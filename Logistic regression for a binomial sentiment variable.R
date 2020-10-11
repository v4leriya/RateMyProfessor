#Load the required libraries
library(caTools)
library(ROCR)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(tidyverse)

# Part 1---------------------------------------------------------------------------------------------

#read in csv file
rmp=read.csv(file=”RateMyProfessor_Sample data.csv)

#subset the data that contains non-missing observations for student_star the binary would_take_agains
rmp=drop_na(rmp, "student_star")
rmp=drop_na(rmp, "would_take_agains")

#create a sentiment variable
rmp_log$sentiment=ifelse(rmp_log$would_take_agains=="Yes", 1, 0)

#split the data into training and testing 70/30
split1=sample.split(rmp_log$sentiment, SplitRatio=0.7)
train1=subset(rmp_log, split1==TRUE)
test1=subset(rmp_log, split1==FALSE)

#train a logistic regression model
log_model1=glm(sentiment~student_star, data=train1, family="binomial")

#make predictions on a test data
predictions_log1=predict(log_model1, newdata=test1, type="response")

#assess the accuracy of the model
table(test1$sentiment, as.numeric(predictions_log1>=0.5))

#assess the accuracy of the baseline model
table(test1$sentiment)

#calculate AUC value
pred1=prediction(predictions_log1, test1$sentiment)
as.numeric(performance(pred1, "auc")@y.values)