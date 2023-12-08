# Simple Basic Prediction model
# Below example we try to build a simple prediction model based on the training dataset, and to use the prediction model to calculate the error on the test data

# Train dataset - Dataset which is used to train the prediction model
# Test dataset - Dataset on which we try to evaluate the prediction model. This is the dataset prediction model might not have encountered during it's training
# Read the train and test data frames
train<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv")
test<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv")
print(paste("The column names are:"))
colnames(train)
colnames(test)

head(train)

myprediction<-train
# A decision vector is created with length being number of rows in prediction data frame and value being 'F'
decision <- rep('F',nrow(myprediction))
decision
decision[myprediction$Score>40] <- 'D'
decision
decision[myprediction$Score>60] <- 'C'
decision
decision[myprediction$Score>70] <- 'B'

decision[myprediction$Score>80] <- 'A'
decision
myprediction$Grade <- decision
# Evaluating the prediction model on the test dataset
error <- mean(test$Grade!= myprediction$Grade)
print(paste("The value of error computed on testing data is ", error))

# Example demonstrating of how submission file is created
# In this example as well, we follow simple basic prediction model

test<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022testSNoGrade.csv')
submission<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022submission.csv')

print(paste("The column names of test are:"))
colnames(test)

print(paste("The column names of submission are:"))
colnames(submission)

myprediction<-test
decision <- rep('F',nrow(myprediction))
decision[myprediction$Score>40] <- 'D'
decision[myprediction$Score>60] <- 'C'
decision[myprediction$Score>70] <- 'B'
decision[myprediction$Score>80] <- 'A'
#Now make your submission file - it will have the IDs and now the predicted grades
submission$Grade<-decision
submission
# Creating the submission.csv to store submission as csv file on your machine and subsequently submit it on Kaggle
write.csv(submission, 'submission.csv', row.names=FALSE)

# How to build a freestyle model
# Lets move into advanced free style prediction in the hope of reducing error from the previous simple model

# To enhance the accuracy of freestyle prediction models, the key approach is to divide the data into subsets 
# and use the most common outcome of the target variable for predictions. The objective is to isolate segments of data where a specific outcome is predominant. 
# Predicting this prevalent outcome minimizes error. However, discovering these optimal data subsets typically requires a process of trial and error.
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv")

# Definitely start with plots like the boxplot from the section 5 (data exploration).  But then follow up with exploratory queries as in the recent quizzes. Examples here use table()  function and look for situations when one grade is absoutely dominant. This would be your prediction. Thus, the goal is to slice the data using subsetting in such a way that for each slice you get a clear "winner grade". Then combine these subset rules into decision vector - just as we did in snippet above.

# Below some examples of such exploratory queries with clear grade winners.
print("The most frequent grade for all students is F.This would be the most primitive predicion model.It would be correct 405 times out of 1000 cases on the train data as the table below shows")
table(moody$Grade)
print("The most frequent grade for all students who scored above 80 is A. We can predict it in 184 cases from the total of 286 cases. Better but not great")
table(moody[moody$Score>80,]$Grade)
print("But if we restrict to Psychology majors we get A in 50/51 - which is fantastic predition as the table below indicates")
table(moody[moody$Score>80 & moody$Major=='Psychology',]$Grade)
print("The score exceeding 80 is not great predictor anymore for CS students though - it is just 46 out of 111! Clearly we need separate cutpoints for different majors")
table(moody[moody$Score>80 & moody$Major=='CS',]$Grade)
print("We need to be more demanding for CS students Only the score exceeding 95 is a good predictor of A for CS students though - it is 27 out of 33! Reinforces the main message -  we need separate cutpoints for different majors")
table(moody[moody$Score>90 & moody$Major=='CS',]$Grade)
table(moody[moody$Score>95 & moody$Major=='CS',]$Grade)


table(moody[moody$Score<40,]$Grade)
# The code concludes that different subsets require different thresholds for prediction.
# a process of iteratively refining a prediction model by identifying and leveraging the most predictive subsets of data, 
# which is achieved through exploratory analysis and the identification of dominant outcomes within those subsets.
# By employing these more sophisticated techniques, we aim to improve the error rate of our prediction models and achieve better results.