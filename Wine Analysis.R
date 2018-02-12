# Ashley Peterson # 
# SAP.io coding challenge #
# Wine dataset #
# February 11, 2018 # 

library(ggplot2)
library(GGally)
library(mice)
library(VIM)
library(MLmetrics)
library(cluster)
library(tidyverse)
library(factoextra)

#import the data
wine <- read.csv('C:/Users/Ashley/Desktop/wine.csv')
# the data has 15 variables and 6,497 observations


# Exploring the Data ------------------------------------------------------
head(wine)

summary(wine)
# three times more white then red in the dataset, red=1599 white=4898
#Variables with missing values: volatile.acidity, astringency.rating, residual.sugar, pH, vintage 

hist(wine$fixed.acidity)
#skewed right 
hist(wine$volatile.acidity)
#skewed right
hist(wine$citric.acid)
#skewed right
hist(wine$astringency.rating)
#skewed right
hist(wine$residual.sugar)
#skewed right
hist(wine$chlorides)
#skewed right
hist(wine$free.sulfur.dioxide)
#skewed right
hist(wine$total.sulfur.dioxide)
#skewed right
hist(wine$density)
# uniform/normal?
hist(wine$pH)
#slight skew right, more normal than other vars
hist(wine$sulphates)
#skewed right
hist(wine$alcohol)
#skewed right
table(wine$vintage)
#wine vintages 2001-2008, every year expect 2001 has about 1000. 2001 has one observation
table(wine$quality)
# quality ranges from 3-9, most in the middle around 5-7
hist(wine$quality)

#calculate the percent missing of a variable
percent_missing <- function(x) { sum(is.na(x))/length(x)*100}
#run the funciton over the columns
apply(wine, 2, percent_missing)
# the variables with missing have less than 5% except residual sugar that has 36% missing. We should consider dropping residual sugar. 
wine$residual.sugar<- NULL



# Train/Test Split --------------------------------------------------------
#split the data into test and training sets
indexes <- sample(1:nrow(wine), size=0.3*nrow(wine))
test <- wine[indexes,]
train <- wine[-indexes,]


# Checking the correlation of variables -----------------------------------

#selecting the continuous variables for the correlation
wine_continuous <- train[c(2:12)]
#running the correlation matrix, this isn't the prettiest to look at so let use ggplot for a better visual. 
cor(wine_continuous, use='complete.obs', method='pearson')

#correlation matrix with plots (takes about a minute to run and plot in a new window)
win.graph()
ggpairs(wine_continuous)
# fixed.acidity and astringency.rating have a correlation of 0.99, free.sulfur.dioxide and total.sulfur.dioxide have a correlation of 0.729

#since they have such a high correlation let's choose one. After googling the difference of the terms, I think that free has a smaller range when measuring the sulfur dioxide in parts per million. Let's 
#keep the total.sulfur.dioxide.
train$free.sulfur.dioxide<- NULL
test$free.sulfur.dioxide<- NULL


# Handling the missing data -------------------------------------------------------

#looking at the pattern of missing data
md.pattern(train)
# it is hard to tell the pattern from just the chart so let's make a plot. 

#plot the pattern to get a better visual
win.graph()
missing_plot <- aggr(train, numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=0.6, gap=3, ylab=c('Histogram of missing data', 'Pattern'))
# from the patern we can say the data is missing at random. There isn't an apparent pattern. most of the obs don't have missing
sum(is.na(train))
# there are 514 missing values in the training set 


#Remove the NA's. 
wine2_train <- na.omit(train)
#the sample size is now 4055


# Mean replacement

#copy the data set for mean imputation
wine_mean_train <- train
#replace with the mean for volatile.acidity
wine_mean_train$volatile.acidity<- ifelse(is.na(wine_mean_train$volatile.acidity), mean(wine_mean_train$volatile.acidity, na.rm=TRUE), wine_mean_train$volatile.acidity)
#check to make sure all missing was replaced. Should sum to 0. 
sum(is.na(wine_mean_train$volatile.acidity))
#replace astringency.rating
wine_mean_train$astringency.rating<- ifelse(is.na(wine_mean_train$astringency.rating), mean(wine_mean_train$astringency.rating, na.rm=TRUE), wine_mean_train$astringency.rating)
sum(is.na(wine_mean_train$astringency.rating))
#replace pH
wine_mean_train$pH<- ifelse(is.na(wine_mean_train$pH), mean(wine_mean_train$pH, na.rm=TRUE), wine_mean_train$pH)
sum(is.na(wine_mean_train$pH))

#since vintage is caegorical we would want to do median replacement.
wine_mean_train$vintage<- ifelse(is.na(wine_mean_train$vintage), median(wine_mean_train$vintage, na.rm=TRUE), wine_mean_train$vintage)
sum(is.na(wine_mean_train$vintage))

summary(wine_mean_train) # no missing! this set has 4548 observations


# Multiple Imputation Using Chaine Equations (MICE)

# impute the values
wine_mice <- mice(train, m=5, maxit=50, seed=173)
summary(wine_mice)

#review the imputed values, visual check for extremes
wine_mice$imp$volatile.acidity
wine_mice$imp$astringency.rating
wine_mice$imp$pH
wine_mice$imp$vintage

#creating the imputed dataframe instead of the imputation object that is created 
wine_mice_cmplt_train <- complete(wine_mice, 5) #replaced with values of the 5th imputed dataset

#checking the imputation 
win.graph()
densityplot(wine_mice)
# we see that the imputed values for each follow the distribution of the original variable
#another visual to look at the imputed values vs the original
win.graph()
stripplot(wine_mice, pch=20, cex=1.2)



# Prepare the Test datasets  -----------------------------------------------

#remove free.sulfur.dioxide because of high correlation with total.sulfur.dioxide
test$free.sulfur.dioxide<- NULL

#missing removed
wine2_test <- na.omit(test)

#MEAN
#copy the data set for mean imputation
wine_mean_test <- test
#replace with the mean for volatile.acidity
wine_mean_test$volatile.acidity<- ifelse(is.na(wine_mean_test$volatile.acidity), mean(wine_mean_train$volatile.acidity, na.rm=TRUE), wine_mean_test$volatile.acidity)
#check to make sure all missing was replaced. Should sum to 0. 
sum(is.na(wine_mean_test$volatile.acidity))
#replace astringency.rating
wine_mean_test$astringency.rating<- ifelse(is.na(wine_mean_test$astringency.rating), mean(wine_mean_train$astringency.rating, na.rm=TRUE), wine_mean_test$astringency.rating)
sum(is.na(wine_mean_test$astringency.rating))
#replace pH
wine_mean_test$pH<- ifelse(is.na(wine_mean_test$pH), mean(wine_mean_train$pH, na.rm=TRUE), wine_mean_test$pH)
sum(is.na(wine_mean_test$pH))

#since vintage is caegorical we would want to do median replacement.
wine_mean_test$vintage<- ifelse(is.na(wine_mean_test$vintage), median(wine_mean_train$vintage, na.rm=TRUE), wine_mean_test$vintage)
sum(is.na(wine_mean_test$vintage))

summary(wine_mean_test) # no missing! this set has 4548 observations


#MICE 
wine_mice_test <- mice(test, m=5, maxit=50, seed=173)
summary(wine_mice_test)
wine_mice_cmplt_test <- complete(wine_mice_test, 5)



# Linear Regression -------------------------------------------------------


#All the variables (minus residual.sugar and free.sulfur.dioxide)
lm_nomiss <- lm(quality ~ ., data=wine2_train)
summary(lm_nomiss)
#adjusted R2 0.2732
#significant variables volatile.acidity, density, sulphates, alcohol (at 0.001)
#(at 0.1) chlorides 

#what are the MAPE values of the predictions?
lm_nomiss_pred <- predict(lm_nomiss, newdata=wine2_test, se.fit=TRUE, interval='prediction', level=0.95)
MAPE_nomiss <- MAPE(lm_nomiss_pred$fit, wine2_test$quality) # 0.207% 

#MEAN imputed data
lm_mean <- lm(quality ~ . , data=wine_mean_train)
summary(lm_mean)
#adjusted R2 0.2658
#significant variables: volatile.acidity, density, sulphates, alcohol (at 0.001)
#(at 0.05) chlorides
lm_mean_pred <- predict(lm_mean, newdata=wine_mean_test, se.fit=TRUE, interval='prediction', level=0.95)
MAPE_mean <- MAPE(lm_mean_pred$fit, wine_mean_test$quality) # 0.20899% 

# MICE data
lm_mice <- lm(quality ~ . , data=wine_mice_cmplt_train)
summary(lm_mice)
#adjusted R2 0.2681
#significant variables: volatile.acidity, chlorides, sulphates, alcohol (at 0.001)
# (at 0.05) fixed.acidity, (at 0.1) chlorides 
#what are the MAPE values of the predictions?
lm_mice_pred <- predict(lm_mice, newdata=wine_mice_cmplt_test, se.fit=TRUE, interval='prediction', level=0.95)
MAPE_mice <- MAPE(lm_mice_pred$fit, wine_mice_cmplt_test$quality) # 0.2072% 

# each of the datsets are preforming similarly. They had R2's around 0.26 and MAPE values of 0.2%


#After finding the significant variables, lets drop the others and see what happens.

# Data with missing removed
linregress <- lm(quality ~ volatile.acidity + density + sulphates + alcohol, data=wine2_train)
summary(linregress)
#adjusted R2 0.2709
#volatile.acidity, sulphaes and alchol most significant
lm_nomiss_pred <- predict(lm_nomiss, newdata=wine2_test, se.fit=TRUE, interval='prediction', level=0.95)
MAPE_nomiss <- MAPE(lm_nomiss_pred$fit, wine2_test$quality) # 0.207% 

# Mean imputed data
lm_mean <- lm(quality ~ volatile.acidity + density + sulphates + alcohol , data=wine_mean_train)
summary(lm_mean)
#adjusted r-squared 0.264
#all vars significant
lm_mean_pred <- predict(lm_mean, newdata=wine_mean_test, se.fit=TRUE, interval='prediction', level=0.95)
MAPE_mean <- MAPE(lm_mean_pred$fit, wine_mean_test$quality) # 0.2105% 


# MICE data 
lm_mice <- lm(quality ~ volatile.acidity + density + sulphates + alcohol , data=wine_mice_cmplt_train)
summary(lm_mice)
#adjusted R2 0.2659
#all vars significant
lm_mice_pred <- predict(lm_mice, newdata=wine_mice_cmplt_test, se.fit=TRUE, interval='prediction', level=0.95)
MAPE_mice <- MAPE(lm_mice_pred$fit, wine_mice_cmplt_test$quality) # 0.2088% 


# Linear regression isn't giving us much explaination only 26%. This could be improvedy by looking at interactions or maybe transforming the variables.
#Also the residual plots should be looked at. 
# For now let's try something else. 



# Logistic Regression -----------------------------------------------------

#Let's see if we can just calssify the wine for 5 and above being 'good' and below 'bad'


# create the binary variable 
wine$good <- ifelse(wine$quality >= 5, 1, 0)
wine$quality <- NULL

#(rerun the split and creating of the 3 datasets to handle the missing with this new variable included.)
#split the data into test and training sets 
indexes <- sample(1:nrow(wine), size=0.3*nrow(wine))
test <- wine[indexes,]
train <- wine[-indexes,]
train$free.sulfur.dioxide<- NULL
test$free.sulfur.dioxide<- NULL

#Remove the NA's. 
wine2_train <- na.omit(train)

#prep the test set
wine2_test <- na.omit(test)

# I only used the complete values and not the imputed sets for this analysis since they preformed similarly for the linear. 
# a next step would be to run logistic with the imputed data sets. 

#LOGISTIC REGRESSION
lr_nomiss<- glm(good~ ., data=wine2_train, family=binomial(link="logit"))
summary(lr_nomiss)
# significant variables: typewhite, volatile.acidity, total.sulfur.dioxide, alcholol (at 0.001)
# (at 0.01) density, pH
# (at 0.1) fixed.acidity, astringency.rating 
lr_nomiss_pred <- predict(lr_nomiss, newdata=wine2_test, type='response', level=0.95)

#need to check how the model is doing with predictions and classifying. What the true positives and true negatives are. 





# K means to Cluster the wines --------------------------------------------

# Standardize the data
wine_std<- wine
wine_std$fixed.acidity <- scale(wine_std$fixed.acidity)
wine_std$volatile.acidity <- scale(wine_std$volatile.acidity)
wine_std$citric.acid <- scale(wine_std$citric.acid)
wine_std$astringency.rating <- scale(wine_std$astringency.rating)
wine_std$chlorides <- scale(wine_std$chlorides)
wine_std$free.sulfur.dioxide <- scale(wine_std$free.sulfur.dioxide)
wine_std$total.sulfur.dioxide <- scale(wine_std$total.sulfur.dioxide)
wine_std$density <- scale(wine_std$density)
wine_std$pH <- scale(wine_std$pH)
wine_std$sulphates <- scale(wine_std$sulphates)
wine_std$alcohol <- scale(wine_std$alcohol)

wine_std <- na.omit(wine_std)
wine_std <- wine_std[2:14]

#determine the number of clusters
#elbow plot to determine number of clusters
set.seed(37329)
wss<- function(k){
  kmeans(wine_std, k, nstart=10)$tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss) #did not converge

#another way to do elbow plot
set.seed(435)
fviz_nbclust(wine_std, kmeans, method='wss') #? inconclusive
#silhouette method, another way to determine number of cluster 
fviz_nbclust(wine_std, kmeans, method='silhouette') # this says 3


#cluster numbers 3, 4, 5, 7 
k3 <- kmeans(wine_std, centers =3, nstart=25)
fviz_cluster(k3, data=wine_std)
k4 <- kmeans(wine_std, centers =4, nstart=25)
fviz_cluster(k4, data=wine_std)
k5 <- kmeans(wine_std, centers =5, nstart=25)
fviz_cluster(k5, data=wine_std)
k8 <- kmeans(wine_std, centers =8, nstart=25)
fviz_cluster(k8, data=wine_std)

# clustering doesn't really tell us much. the wines are fairly grouped together in the middle of the variables which makes it hard to seperate the groups





# Conculsions and Next Steps ----------------------------------------------

# I compared three datasets in linear regressions for ways to handle the missing data. Removing non complete cases, mean imputation, and multiple imputation using chained equations were compared. 
# In linear regression, the three data sets gave similar results with R2's around 0.26. The significant variables were: volatile.acidity, denisty, sulphates, and alcohol.
# Next, I ran logisitic regression to see if it could predict whether a wine would get a quality rating above 5. The accuarcy of the model needs to be checked, but the significan variables were: 
# typeWhite, volatile acidity, total.sulfur.dioxide, and alcohol. 
# Then, I ran k means clustering to see if that would provide any information, but that was inconclusive. 
# From the regressions the variables that were significant in both for prediciting quality rating were: volatile.acidity and alcohol. 
# Next Steps: 
#   Continue checking the logisitic regression results
#   Try a Decision Tree to see if it splits on the same variables that were significant in the regressions
#   Look into additional data to help explain more
#       maybe the judge plays a role in the score
#       more detailed red/white get the type of wine
#       weather- the quality of the harvest of the vintage year


