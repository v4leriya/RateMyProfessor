#Load the required libraries

library(caTools)
library(ROCR)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)

#read in csv file

rmp=read.csv(file=”ProfessorRatings.csv)

#subset the data that contains non-missing observations for would_take_agains

rmp_log=na.omit(rmp, cols="would_take_agains")

#create a sentiment variable

rmp_log$sentiment=ifelse(rmp_log$would_take_agains=="Yes", 1, 0)

#split the data into training and testing 70/30

split=sample.split(rmp_log$sentiment, SplitRatio=0.7)

train=subset(rmp_log, split==TRUE)

test=subset(rmp_log, split==FALSE)

#train a logistic regression model

log_model=glm(sentiment~student_star, data=train, family="binomial")

#make predictions on a test data

predictions_log=predict(log_model, newdata=test, type="response")

#assess the accuracy of the model

table(test$sentiment, as.numeric(predictions_log>=0.5))

#assess the accuracy of the baseline model

table(test$sentiment)

#plot and calculate AUC curve

pred=prediction(predictions_log, test$sentiment)

as.numeric(performance(pred, "auc")@y.values)

print(log_model)
