#Prediction using Supervised ML 


#library required
install.packages("Metrics")
install.packages("install.packages("caret", dependencies = TRUE")
library(Metrics)
library(caret)

#loading the data set
scores_data<-read.csv("https://bit.ly/w-data")
scores_data
View(scores_data)
scores<-data.frame(scores_data)
scores

#dividing dataset into training and testing data

scores_size <- floor(0.75 * nrow(scores))
set.seed(123)
scores_ind<- sample(seq_len(nrow(scores)),size = scores_size)
train <- scores[scores_ind, ]
train
test <- scores[-scores_ind, ] 
test


s<-train$Scores
h<-train$Hours
# Plot a graph for the two variables.
plot(h,s)
# Create a linear model for regression.
trmodel<-lm(s~h,data=train)
print(trmodel)
print(summary(trmodel))
cor(s,h)
# add the regression line to the scatterplot.
plot(h,s,col="blue",main=" hours vs percentage ",xlab = "hours studies"
     ,ylab = "percentage scores")
abline(lm(s~h),col="blue")




#for testing data
s1<-test$Scores
h1<-test$Hours
temodel<-lm(s1~h1,data=test)
test_predict<-predict(temodel)
test_predict
print(temodel)
print(summary(temodel))
cor(s1,h1)


#predicting the model and calculating accuracy

actuals_preds <- data.frame(cbind(actuals=test$Scores, predicted=test_predict))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

#What will be predicted score if a student studies for 9.25 hours/day?
a1 <- data.frame(h = 9.25)
result1 <-  predict(trmodel,a1)
result1


#calculating mean absolute error
mae(test$Scores,test_predict)
