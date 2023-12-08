## STEPS to create the submission file:

## Step 0 - Import Libraries:

install.packages("rpart")
install.packages("rpart.plot")
devtools::install_github("devanshagr/CrossValidation")
library(rpart)
library(rpart.plot)

## Step 1 - Import the train and test set:

train<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv")
test<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022testSNoGrade.csv')
test

## Step 2.1 - Do your analysis on train-set, use freestyle or rpart or both to generate your prediction model:

##freestyle:
myprediction<-train
decision <- rep('F',nrow(myprediction))
decision[myprediction$Score>40] <- 'D'
decision[myprediction$Score>60] <- 'C'
decision[myprediction$Score>70] <- 'B'
decision[myprediction$Score>80 ] <- 'A'
myprediction$Grade <-decision
error <- mean(train$Grade!= myprediction$Grade)
error 
myprediction<-test
decision <- rep('F',nrow(myprediction))
decision[myprediction$Score>40] <- 'D'
decision[myprediction$Score>60] <- 'C'
decision[myprediction$Score>70] <- 'B'
decision[myprediction$Score>80] <- 'A'


##rpart:
tree <- rpart(Grade ~ Major+Score+Seniority, data = train,method = "class", control=rpart.control(minsplit = 200))
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Grade==pred)

## Step 2.2 - Cross Validate the model:

CrossValidation::cross_validate(train, tree, 10, 0.8)

## Step 3 - Apply your model on the test-set:

pred <- predict(tree, test, type="class")

## Step 4.1 - Import the submission file:

submission<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022submission.csv')

## Step 4.2 - Generate the column of the predicted parameter and add it to the submission file:

submission$Grade<-pred
submission
write.csv(submission, 'submission.csv', row.names=FALSE) # to store submission as csv file on your machine and subsequently submit it on Kaggle
# dont forget to set rows.names as FALSE 

