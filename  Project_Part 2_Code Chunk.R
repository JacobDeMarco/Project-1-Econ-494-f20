#Project Part II#

#Importing Dataset#
library(haven)
library(tseries)
dataset<-read_dta("/Users/jakedemarco/Desktop/Econ 494_Final Project_Part II_Data.dta")
View(dataset)

#Cleaning#
dataset$gr_span <- NULL
dataset$teachers <- NULL
dataset$enrl_tot <- NULL
dataset$calw_pct <- NULL
dataset$meal_pct <- NULL
dataset$computer <- NULL
dataset$comp_stu <- NULL
dataset$read_scr <- NULL
dataset$math_scr <- NULL

#Creating Polynomial#
dataset$str2<-dataset$str^2 

#Creating Logrithmic Transformation#
dataset$ln_str<-log(dataset$str)

#Partioning the data# 
p <- .7 #Fraction of sample to be used for training
obs_count<-dim(dataset)[1] #Identifying the number of rows in the dataframe
trainingdata <- floor(p * obs_count) #Selects the number of observations for the training partition

#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = trainingdata)

Training <- dataset[train_ind, ] 
Testing <- dataset[-train_ind, ] 

#Checking dimensions of data#
dim(Training)
dim(Testing)

#Regression Model 1#
Model1<-lm(testscr ~ str,dataset)
summary(Model1)

#Generating predictions on the training data#
PRED_1_IN <- predict(Model1, Training) 
View(PRED_1_IN)

#Generating predictions on test data#
PRED_1_OUT <- predict(Model1, Testing) #generate predictions on the (out-of-sample) testing data

#Computing RMSE#
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$testscr)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$testscr)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN 
RMSE_1_OUT 

x_grid <- seq(0,8,.1) 
predictions <- predict(Model1, list(displ=x_grid))
plot(Training$testscr ~ Training$str, col='blue', xlab= "Training - Student Teacher Ratio", ylab="Training - Test Scores")
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$testscr ~ Testing$str, col='red', pch=3)

#Test for normality#
jarque.bera.test(Model1$residuals) 

#Regression Model 2#
Model2<-lm(testscr ~ expn_stu + str + avginc + el_pct, Training) #Building Model 2
summary(Model2)

#Generating predictions on the training data#
PRED_2_IN <- predict(Model2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)

#Generating predictions on test data#
PRED_2_OUT <- predict(Model2, Testing) 

#Computing RMSE#
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$testscr)^2)/length(PRED_2_IN))
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$testscr)^2)/length(PRED_2_OUT)) 

RMSE_2_IN 
RMSE_2_OUT 

#Test for normality#
jarque.bera.test(Model2$residuals) 

#Regression Model 3#
Model3<-lm(testscr ~ expn_stu + str + str2 + avginc + el_pct, Training) #Building Model 2
summary(Model3)

#Generating predictions on the training data#
PRED_3_IN <- predict(Model3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)

#Generating predictions on test data#
PRED_3_OUT <- predict(Model3, Testing) 

#Computing RMSE#
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$testscr)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$testscr)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN 
RMSE_3_OUT 

#Test for Normality#
jarque.bera.test(Model3$residuals) 

#Regression Model 4#
Model4 <- lm(testscr ~ ln_str + expn_stu + avginc + el_pct, Training)
summary(Model4)

#generating predictions on the training data#
PRED_4_IN <- predict(Model4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)

#Generating predictions on test data#
PRED_4_OUT <- predict(Model4, Testing) #generate predictions on the (out-of-sample) testing data

#Computing RMSE#
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$testscr)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$testscr)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN 
RMSE_4_OUT 

#Test for normality#
jarque.bera.test(Model4$residuals) 
hist(Model4$residuals, main="Histogram of Model 4 Residuals", xlab="residuals")

#Comparison of in-sample RMSE#
RMSE_1_IN 
RMSE_2_IN 
RMSE_3_IN 
RMSE_4_IN 

#Comparison of out-of-sample RMSE#
RMSE_1_OUT 
RMSE_2_OUT 
RMSE_3_OUT 
RMSE_4_OUT 

