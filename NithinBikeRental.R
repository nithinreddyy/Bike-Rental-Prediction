rm(list = ls())
setwd("C:/Users/Click/Desktop/Bike rental")
getwd()
# #loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "e1071",
      "DataCombine", "pROC", "doSNOW", "class", "readxl","ROSE","dplyr", "plyr", "reshape","xlsx","pbapply", "unbalanced", "dummies", "MASS" , "gbm" ,"Information", "rpart", "miscTools")

# #install.packages if not
#lapply(x, install.packages)

# #load libraries
lapply(x, require, character.only = TRUE)
rm(x)

#Input Data Source
df = data.frame(read_xlsx('Bike_Rental.xlsx', sheet = 1))

#Creating backup of orginal data 
data_Original  = df


###########################################################################
#                  EXPLORING DATA										  #
###########################################################################

#viewing the data
head(df,5)
dim(df)

#structure of data or data types
str(df)  

#Summary of data 
summary(df)

#unique value of each count
apply(df, 2,function(x) length(table(x)))

#By looking at data we can conclude that 
#variables 'instant' and 'dteday' are not significant for our analysis.
#and variables 'casual' and 'registered' sum up to target variable 'cnt'.
#Hence, we can remove these before proceeding.
df = subset(df, select = -c(casual, registered, instant, dteday))

dimnames(df)

# From the above EDA and problem statement categorising data in 2 category "continuous" and "catagorical"
cont_vars = c('temp', 'atemp', 'hum', 'windspeed', 'cnt')

cata_vars = c('season','yr','mnth','holiday','weekday', 'workingday', 'weathersit')


#########################################################################
#                     Visualizing the data                              #
#########################################################################

#library(ggplot2)

#Plot Number of bikes rented Vs. the days of the week.
ggplot(data = df, aes(x = reorder(weekday,-cnt), y = cnt))+
  geom_bar(stat = "identity")+
  labs(title = "Number of bikes rented Vs. days", x = "Days of the week", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#Plot Bikes rented Vs. variation in temperature and hunidity
#It can be observed that people rent bikes mostly when temperature in between 0.5 and0.75 normalized temperature
#and between normalized humidity 0.50 and 0.75
ggplot(df,aes(temp,cnt)) + 
  geom_point(aes(color=hum),alpha=0.5)+
  labs(title = "Bikes rented Vs. variation in temperature and hunidity", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) +
  theme_bw()

#Plot Bikes rented Vs. temperature and weathersite
#Most bikes are rented duing weather site forcast 1
ggplot(data = df, aes(x = temp, y = cnt))+
  geom_point(aes(color=weathersit))+
  labs(title = "Bikes rented Vs. temperature and weathersite", x = "Normalized temperature", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()

#PLot Bikes rented Vs. temperature and workingday
#People rent bikes mostly on working weekdays
ggplot(data = df, aes(x = temp, y = cnt))+
  geom_point(aes(color=workingday))+
  labs(title = "Bikes rented Vs. temperature and workingday", x = "Normalized temperature",y = "Count")+
  #  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()


#################################################################
#         				  Missing data							#
#################################################################

missing_val = sum(is.na(df))
print(missing_val)


################################################################
#               Outlier Analysis			              	   #
################################################################

## BoxPlots - Distribution and Outlier Check
# Boxplot for continuous variables
for (i in 1:length(cont_vars))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cont_vars[i]), x = "cnt"), data = subset(df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cont_vars[i],x="cnt")+
           ggtitle(paste("Box plot for",cont_vars[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
#gridExtra::grid.arrange(gn5)

# #Remove outliers using boxplot method
# #loop to remove from all variables
for(i in cont_vars)
{
  print(i)
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df = df[which(!df[,i] %in% val),]
}

#Replace all outliers with NA and impute
for(i in cont_vars)
{
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df[,i][df[,i] %in% val] = NA
}

# Imputing missing values
df = knnImputation(df,k=3)


################################################################
#               Feature Selection                              #
################################################################

#Here we will use corrgram to find corelation

##Correlation plot
#library('corrgram')

corrgram(df,
         order = F,  #we don't want to reorder
         upper.panel=panel.pie,
         lower.panel=panel.shade,
         text.panel=panel.txt,
         main = 'CORRELATION PLOT')


#We can see that the highly corr related vars in plot are marked in dark blue. 
#Dark blue color means highly positive correlation

##------------------ANOVA testset--------------------------##

## ANOVA testset for Categprical variable
summary(aov(formula = cnt~season,data = df))
summary(aov(formula = cnt~yr,data = df))
summary(aov(formula = cnt~mnth,data = df))
summary(aov(formula = cnt~holiday,data = df))
summary(aov(formula = cnt~weekday,data = df))
summary(aov(formula = cnt~workingday,data = df))
summary(aov(formula = cnt~weathersit,data = df))


################################################################
#               Feature Selection	                     	   #
################################################################

## Dimension Reduction
#temp and atemp have high correlation (>0.7), so we have excluded the atemp column.
#holiday, weekday, workingday have p>0.05

df = subset(df, select = -c(atemp,holiday,weekday,workingday))

################################################################
#               Feature Scaling		                     	   #
################################################################

# Updating the continuous and catagorical variables		 

cont_vars = c('temp', 'hum', 'windspeed', 'cnt')

cata_vars = c('season','yr','mnth', 'weathersit')


#Normality check
#Checking Data for Continuous Variables

################  Histogram   ##################
hist(df$cnt, col="blue", xlab="Count", main="Histogram for Count")
hist(df$hum, col="orange", xlab="Count", main="Histogram for Humidity")
hist(df$windspeed, col="sky blue", xlab="Count", main="Histogram for Windspeed")
hist(df$temp, col="red", xlab="Count", main="Histogram for Temperature")

#We have seen that our data is mostly normally distributed. Hence, we will go for Standardization.	

#Viewing data before Standardization.
head(df)
cont_vars

#Standardization
for(i in cont_vars)
{
  if(i!= "cnt"){
    print(i)
    df[,i] = (df[,i] - mean(df[,i]))/(sd(df[,i]))
  }
}

#Viewing data after Standardization.
head(df)

#Creating dummy variables for categorical variables
library(mlr)
df1 = dummy.data.frame(df, cata_vars)

#Viewing data after adding dummies
head(df1)

################################################################
#          		        Sampling of Data        			   #
################################################################

# #Divide data into trainset and testset using stratified sampling method

#install.packages('caret')
#library(caret)
set.seed(101)
split_index = createDataPartition(df1$cnt, p = 0.8, list = FALSE)
trainset = df1[split_index,]
testset  = df1[-split_index,]

#Checking df Set Target Class
table(trainset$cnt)


####FUNCTION to calculate MAPE####
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}


############################################################################################################################################################
##                                                   Basic approach for ML - Models																		  ##
##               We will first get a basic idea of how different models perform on our preprocesed data and then select the best model and make it        ##
##                                                    more efficient for our Dataset																	  ##
############################################################################################################################################################

#------------------------------------------Decision tree-------------------------------------------#
#Develop Model on training data
fit_DT = rpart(cnt ~., data = trainset, method = "anova")

#Variable importance
fit_DT$variable.importance

#      temp         yr     season       mnth        hum  windspeed weathersit 
# 889796479  637857947  548442480  487529776  195102361  135504939   32013972 

#Lets predict for test data
pred_DT_test = predict(fit_DT, testset)

# For test data 
print(postResample(pred = pred_DT_test, obs = testset$cnt))

#Compute R^2
dt_r2 = rSquared(testset$cnt, testset$cnt - pred_DT_test)
print(dt_r2)

#Compute MSE
dt_mse = mean((testset$cnt - pred_DT_test)^2)
print(dt_mse)

#Compute MAPE
dt_mape = MAPE(testset$cnt, pred_DT_test)
print(dt_mape)

#RMSE  Rsquared       MAE  
#871.4901885   0.8050212 672.9650742 

#R2
#0.8044511

#MSE 
#759495.1

#MAPE
#23.36856


#------------------------------------------Linear Regression-------------------------------------------#

#Develop Model on training data
fit_LR = lm(cnt ~ ., data = trainset)

#Lets predict for test data
pred_LR_test = predict(fit_LR, testset)

# For test data 
print(postResample(pred = pred_LR_test, obs = testset$cnt))

#Compute R^2
lr_r2 = rSquared(testset$cnt, testset$cnt - pred_LR_test)
print(lr_r2)

#Compute MSE
lr_mse = mean((testset$cnt - pred_LR_test)^2)
print(lr_mse)

#Compute MAPE
lr_mape = MAPE(testset$cnt, pred_LR_test)
print(lr_mape)

#RMSE   Rsquared        MAE 
#718.7451054   0.8690861 532.9476425 

#R2
#0.8669913

#MSE
#516594.5

#MAPE
#15.08846

#-----------------------------------------Random Forest----------------------------------------------#

#Develop Model on training data
fit_RF = randomForest(cnt~., data = trainset)

#Lets predict for test data
pred_RF_test = predict(fit_RF, testset)

# For test data 
print(postResample(pred = pred_RF_test, obs = testset$cnt))

#Compute R^2
rf_r2 = rSquared(testset$cnt, testset$cnt - pred_RF_test)
print(rf_r2)

#Compute MSE
rf_mse = mean((testset$cnt - pred_RF_test)^2)
print(rf_mse)

#Compute MAPE
rf_mape = MAPE(testset$cnt, pred_RF_test)
print(rf_mape)

#     RMSE   Rsquared        MAE 
# 547.4775243   0.9258038 422.5625912   

#R2
#0.9228274

#MSE
#299731.6

#MAPE
#14.17132

#--------------------------------------------XGBoost-------------------------------------------#

#Develop Model on training data
fit_XGB = gbm(cnt~., data = trainset, n.trees = 500, interaction.depth = 2)

#Lets predict for test data
pred_XGB_test = predict(fit_XGB, testset, n.trees = 500)

# For test data 
print(postResample(pred = pred_XGB_test, obs = testset$cnt))

#Compute R^2
xgb_r2 = rSquared(testset$cnt, testset$cnt - pred_XGB_test)
print(xgb_r2)

#Compute MSE
xgb_mse = mean((testset$cnt - pred_XGB_test)^2)
print(xgb_mse)

#Compute MAPE
xgb_mape = MAPE(testset$cnt, pred_XGB_test)
print(xgb_mape)

#      RMSE   Rsquared        MAE 
#608.8456581   0.9045705 468.6529898 

#R2
#0.9045568

#MSE
#370693

#MAPE
#14.73772

#################-------------------------------Viewing summary of all models------------------------------###############
# Create variables
Model_name <- c("Decision Tree","Linear Regression", "Random Forest", "XGBoost" )
MSE <- c(dt_mse, lr_mse, rf_mse, xgb_mse)
r2 <- c(dt_r2, lr_r2, rf_r2, xgb_r2)
MAPE <- c(dt_mape, lr_mape, rf_mape, xgb_mape)

# Join the variables to create a data frame
results <- data.frame(Model_name,MSE,r2,MAPE)
results

#     Model_name      MSE        r2     MAPE
#1     Decision Tree 759495.1 0.8044511 23.36856
#2 Linear Regression 516594.5 0.8669913 15.08846
#3     Random Forest 299731.6 0.9228274 14.17132
#4           XGBoost 370693.0 0.9045568 14.73772

########################################################################################
#             			 Saving output to file										   #
########################################################################################

#swrite.csv(submit,file = 'C:/Users/Click/Desktop/Bike rental/Finalcount_R.csv',row.names = F)
#rm(list = ls())
