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

# Part 2--------------------------------------------------------------------------------------------

#subset the data with only missing observations for the binary would_take_agains
rmp_na=rmp[is.na(rmp$would_take_agains),]

#use the trained logistic model to make predictions on the dataset with missing would_take_agains 
predictions_na=predict(log_model1, newdata=rmp_na, type="response")

#create a sentiment variable
rmp_na$sentiment=ifelse(predictions_na>=0.5, 1, 0)

#create a final dataset that has all of would_take_agains observations filled in
rmp_final=rbind(rmp_log, rmp_na)

#create a corpus based on the student's comments and pre-process it to remove stopwords, etc
corpus=Corpus(VectorSource(rmp_final$comments))
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeWords, stopwords("english"))
corpus=tm_map(corpus, stemDocument)

#create term matrix and remove the sparse terms 

dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm, 0.97)
labeledComments=as.data.frame(as.matrix(dtm))
labeledComments$sentiment=rmp_final$sentiment

#split the data into training and testing 70/30
split2=sample.split(labeledComments$sentiment, SplitRatio=0.7)
train2=subset(labeledComments, split2==TRUE)
test2=subset(labeledComments, split2==FALSE)

#train a logistic regression model on the vectorized dataset of student's comments
log_model2=glm(sentiment~., data=train2, family="binomial")

#make predictions on the test data and check the accuracy of the model
predictions_log2=predict(log_model2, newdata=test2, type="response")
table(test2$sentiment, as.numeric(predictions_log2>=0.5))
table(test2$sentiment)

#calculate AUC value
pred2=prediction(predictions_log2, test2$sentiment)
as.numeric(performance(pred2, "auc")@y.values)

#create new variable "approval rate" for each instructor
rmp_final$approval_rate=mean(rmp_final$sentiment)




