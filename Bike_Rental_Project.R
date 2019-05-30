#Remove all object stored
rm(list=ls())

#Set Working directory
setwd("D:/Tejasvi/Practise/Project")

#Get Working directory
getwd()

#Install Library
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)

for ( i in x ) {
  print(i)
  library("ggplot2")
  
}

install.packages(c("dplyr","plyr","reshape","ggplot2","data.table","corrgram"))
install.packages("GGally","DataCombine")


# Install libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")


# Load Day.csv file
df=read.csv("D:/Tejasvi/Practise/Project/day.csv", header=T)

#View Day.csv file
View(df)

#summary of data
summary(df)

#View structure of the given data
str(df)

#Create a function uniquevalue for the distribution of numeric values

uniquevalue_numeric= function(num_x)
{
  ggplot(df)+geom_histogram(aes(x=num_x,y=..density..),
                            fill="grey")+
    geom_density(aes(x=num_x,y=..density..))
}


#Visualize the distribution of target variable 'cnt'
uniquevalue_numeric(df$cnt)

#Visualize the distribution of independent variable 'temp'
uniquevalue_numeric(df$temp)


#Visualize the distribution of independent variable 'atemp'
uniquevalue_numeric(df$atemp)


#Visualize the distribution of independent variable 'hum'
uniquevalue_numeric(df$hum)


#Visualize the distribution of independent variable 'windspeed'
uniquevalue_numeric(df$windspeed)

#Visualize the distribution of independent variable 'casual'
uniquevalue_numeric(df$casual)

#Visualize the distribution of independent variable 'registered'
uniquevalue_numeric(df$registered)


#Visulaize the categorical variable 'mnth' with target variable 'cnt'

ggplot(df, aes(x=as.factor(mnth), y=cnt),fill="grey") + 
  stat_summary(fun.y="mean", geom="bar")

ggplot(df)+
  geom_histogram(aes(x=cnt,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=cnt,y=..density..))

#Visulaize the categorical variable 'holiday'
ggplot(df) +
  geom_bar(aes(x=holiday),fill="grey")

#Now Visulaize the categorical variable 'weekday'
ggplot(df) +
  geom_bar(aes(x=weekday),fill="grey") 

#Visulaize the categorical variable 'weathersit'
ggplot(df) +
  geom_bar(aes(x=weathersit),fill="grey") 

# Letsee the dependencies or relationship between 'temp' and 'atemp'
ggplot(df, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()
#* Graph represent both are highly dependent or highly related to each other

# Letsee the dependencies or relationship between 'temp' and 'hum'
ggplot(df, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()
#graph shows humidity is increas till temprature is 0.7 and it is decreasing
#gradually

#Check the relationship between 'temp' and 'windspeed' variable
ggplot(df,aes(x=temp,y=windspeed))+
  geom_point()+
  geom_smooth()

#check the relationship between all numeric variable using pair plot

ggpairs(df[,c('atemp','temp','hum','windspeed','cnt')])

#check relationship between  season and holiday
rel_mnth_holi= table(df$season,df$holiday)

rel_mnth_holi

barplot(rel_mnth_holi)

#check relationship between  season and weekday

rels_cats_2 <- table(df$season,df$weekday)

barplot(rels_cats_2)

#check relationship between  season and weathersit

rels_cats_3 <- table(df$weathersit,df$season)
rels_cats_3

prop.table(rels_cats_3,2)

barplot(rels_cats_3)

#check relationship between  holiday and weathersit

rels_cats_4 <- table(df$weathersit,df$holiday)
rels_cats_4

barplot(df$weathersit,df$holiday)

#to check in proportion

prop.table(rels_cats_4,2)

barplot(rels_cats_4)

#--------------------------------Missing Value Analysis----------------

missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))

missing_val$columns=row.names(missing_val)

missing_val
names(missing_val)[1] =  "Missing_percentage"

missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

# there is no missing value in our data set---

#-----------------------Outlier Analysis--------------------------------

# detect outliers in  'actual' , 'registered' and 'cnt' variables

ggplot(data = df, aes(x = "", y = casual)) + 
  geom_boxplot() 


# boxplot for  Registered  variable

ggplot(data = df, aes(x = "", y = registered)) + 
  geom_boxplot() 

# boxplot for  cnt variable

ggplot(data = df, aes(x = "", y = cnt)) + 
  geom_boxplot() 

# it is  showing that there is no outliers in  cnt variable

#--------------------Handling Outlier-----------------
# analyse relationship between causal and cnt variables before  outlier treatment
ggplot(df, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()
df_out = df

#Remove outliers using boxplot method
val = df_out$casual[df_out$casual %in% boxplot.stats(df_out$casual)$out]
df_out = df_out[which(!df_out$casual %in% val),]

# boxplot for  casual variable

ggplot(data = df_out, aes(x = "", y = casual)) + 
  geom_boxplot() 

# verify the relationship after  outliers
ggplot(df_out, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()

cor(df_out$casual,df_out$cnt)
cor(df_out$casual,df_out$cnt) 

#------------------------------Feature Selection or dimension reduction-----------------
library(corrgram)

# verify correleation between   Numeric variable

corrgram(df[,c('temp','atemp','hum','windspeed','cnt')], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#  dimensional  reduction
df_features = subset(df,select=-c(atemp,hum))

#----------------------------Normality check------------------------
#Normalisation
cnames = c("casual","registered")

for(i in cnames){
  print(i)
  df_features[,i] = (df_features[,i] - min(df_features[,i]))/
    (max(df_features[,i] - min(df_features[,i])))
}

df$casual

#-----------------------------Model Development----------------------

library(DataCombine)
feature_train_columns=c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")

#Divide data into train and test 
library(caret)
install.packages("caret")
library(rpart)
library(MASS)
train.index = createDataPartition(df_features$cnt, p = .80, list = FALSE)
train = df_features[ train.index,]
test  = df_features[-train.index,]

train_feature = train[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

train_feature

test_features = test[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

test_features
#------------------------------------develope Decision tree model---------------------
#rpart for regression
fit = rpart(cnt ~ ., data = train_feature, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test_features[,-12])

print(fit)

#  plotting decision tree

par(cex= 0.8)
plot(fit)
text(fit)

#------------------------------ Evaluate Decision Tree-----------------------------
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test_features[,12], predictions_DT)

#Error Rate: 11.68
#Accuracy: 88.32
#Evaluate  Model using RMSE

RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}
RMSE(test_features[,12], predictions_DT)
#RMSE->490.8318

#-----------------------------Random Forest----------------------
install.packages("randomForest")
library(party)
library(randomForest)

Rental_rf=randomForest(cnt ~ . , data = train_feature)
Rental_rf
plot(Rental_rf)

#--------------------------------Evaluate Random Forest--------------------
#Predict for new test cases
predictions_DT_two = predict(Rental_rf, test_features[,-12])

#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test_features[,12], predictions_DT_two)
#Error Rate:4.19
#Accuracy: 95.81

RMSE(test_features[,12], predictions_DT_two)
#RMSE=190.2036

#------------------------Parameter Tuning for random forest---------------------
Rental_rf_2=randomForest(cnt ~ . , data = train_feature,mtry =7,ntree=500 ,nodesize =10 ,importance =TRUE)
Rental_rf_2

#Predict for new test cases
predictions_RF_two = predict(Rental_rf_2, test_features[,-12])

#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test_features[,12], predictions_RF_two)
#Error Rate: 2.008418
#Accuracy: 97.99

RMSE(test_features[,12], predictions_RF_two)

#RMSE = 92.70644
# check Variable  Importance 

varimp <- importance(Rental_rf_2)

varimp
# sort variable  

sort_var <- names(sort(varimp[,1],decreasing =T))

sort_var
# draw varimp plot 

varImpPlot(Rental_rf_2,type = 2)

#------------------------------Tuning Random Forest Dimensional Reduction--------------
#remove four variables  which is  contributing  less

train_feature_two = train[,c("yr" ,"mnth","weekday","workingday","temp","casual","registered","cnt")]
test_features_two = test[,c("yr" ,"mnth","weekday","workingday","temp","casual","registered","cnt")]

# Develop Random Forest  Model
Rental_rf_3=randomForest(cnt ~ . , data = train_feature_two,mtry =7,ntree=500 ,nodesize =10 ,importance =TRUE)

Rental_rf_3

#Predict for new test cases
predictions_RF_three = predict(Rental_rf_3, test_features_two[,-8])

#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test_features_two[,8], predictions_RF_three)

#Error Rate: 1.650
#Accuracy: 98.35
RMSE(test_features_two[,8], predictions_RF_three)

#RMSE = 74.13

#---------------------------------Develope Linear Regression Model------------------
install.packages('usdm')
library(usdm)
vif(train_feature[,-12])

vifcor(train_feature[,-12], th = 0.9)
# Correleation between two variables is 'season' and 'mnth' is 0.82 so, removing one variable from the model

train_feature_three = train[,c("yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
test_features_three = test[,c("yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

# develop Linear Regression  model
#run regression model
lm_model = lm(cnt ~., data = train_feature_three)


#Summary of the model
summary(lm_model)

# observe the  residuals and   coefficients  of the linear regression model
# Predict  the Test data 
#Predict
predictions_LR = predict(lm_model, test_features_three[,-11])

# Evaluate Linear Regression Model
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test_features_three[,11], predictions_LR)

#Error Rate: 5.958383e-15
#Accuracy: 94.041 

RMSE(test_features_three[,11], predictions_LR)

#RMSE = 3.474482e-13


# COnclusion  For this Dataset  Linear Regression is  Accuracy  is '94.41'
# and RMSE = 6.678262e-13 






