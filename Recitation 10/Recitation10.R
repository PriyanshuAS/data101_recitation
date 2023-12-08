train<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv")
head(train)
summary(train)
# scramble the train frame
v<-sample(1:nrow(train))
v
v[1:5]
trainScrambled<-train[v, ]
trainScrambled
head(trainScrambled)
# one step crossvalidation
trainSample<-trainScrambled[nrow(trainScrambled)-200:nrow(trainScrambled), ]
summary(trainSample)
head(trainSample)
head(train)
myprediction<-trainSample

# prediction model - free style
# How to test how good your model is?
# Crossvalidation:  Divide train data set into two disjoint subsets T (train) and train MINUS T, the complement of T. 
# You use T to derive your prediction model and the complement of T (train MINUS T) to validate (test it).
# We assume that you created prediction model looking just at the subset of training data T=trainScrambled[1:990,  ]. 
# Since for crossvalidation we train on a subset T of the training data set and validate (test) on the complement of T. 
# In this case T= trainScrambled[1:990,  ] and complement of T (to validate/test) is stored as trainSample.
# You can do it multiple times. And observe the error and its stability.
# You build your model using the decision vector.  Here is very SIMPLISTIC MODEL which is just illustration. Your model should have much better error and be more sophisticated. 
# It's important to note that the demonstration provided here depicts only a single step of cross-validation, but in practice, it should be repeated multiple times.


decision <- rep('F',nrow(myprediction))
decision[myprediction$Score>40] <- 'D'

decision[myprediction$Score>60] <- 'C'

decision[myprediction$Score>70] <- 'B'

decision[myprediction$Score>80 ] <- 'A'

myprediction$Grade <-decision
error <- mean(trainSample$Grade!= myprediction$Grade)
error   


# Here you just need the test table (without grades) to apply your prediction model and calculate predicted grades. And submission data frame to fill it in with the predicted #grades

test<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022testSNoGrade.csv')
submission<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/M2022submission.csv')

myprediction<-test
# Here is your model. I just show example of trivial prediction model
decision <- rep('F',nrow(myprediction))

decision[myprediction$Score>40] <- 'D'

decision[myprediction$Score>60] <- 'C'

decision[myprediction$Score>70] <- 'B'

decision[myprediction$Score>80] <- 'A'
# Now make your submission file - it will have the IDs and now the predicted grades

# Cant find accuracy because grade column in the testing set only has NA values
# accuracy<-round(mean(test$Grade==decision),2)
# accuracy

submission$Grade<-decision
submission
write.csv(submission, 'submission.csv', row.names=FALSE) # to store submission as csv file on your machine and subsequently submit it on Kaggle


######################################################################################################################################################################################

## Example 2


train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTrainApr10.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTestFull.csv')

v<-sample(1:nrow(train))
v
v[1:5]
trainScrambled<-train[v, ]
trainScrambled
# one step crossvalidation
trainSample<-trainScrambled[nrow(trainScrambled)-300:nrow(trainScrambled), ]
trainSample
head(trainSample)
head(train)
myprediction<-trainSample


colnames(train)

# Model Training:
decision<-rep('No',nrow(myprediction))
decision[myprediction$College!='BestCollege'& myprediction$Coding!='Weak']<-'Yes'
decision[myprediction$Major!='IT' & myprediction$Impression!='Outgoing' &myprediction$Coding=='Excellent']<-'Yes'
accuracy<-round(mean(myprediction$Hired==decision),2)
print(paste("The accuracy of professor model on traing data is",accuracy))

# Model Testing:
decision<-rep('No',nrow(test))
decision[test$College!='BestCollege'& test$Coding!='Weak']<-'Yes'
decision[test$Major!='IT' & test$Impression!='Outgoing' &test$Coding=='Excellent']<-'Yes'
accuracy<-round(mean(test$Hired==decision),2)
print(paste("The accuracy of professor model on test data is",accuracy))


######################################################################################################################################################################################

## Example 3 (Student Example)

train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTrainApr10.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTestFull.csv')


v<-sample(1:nrow(train))
v
v[1:5]
trainScrambled<-train[v, ]
trainScrambled
# one step crossvalidation
trainSample<-trainScrambled[nrow(trainScrambled)-400:nrow(trainScrambled), ]
trainSample
head(trainSample)
head(train)
myprediction<-trainSample


# Model on training set:
decision = rep('Yes', nrow(myprediction))
decision[myprediction$Impression == "Nerdy" & myprediction$Major == "IT" & myprediction$College == "BestCollege"] <- "No"
decision[myprediction$Coding == "OK" & myprediction$Impression == "Shy" & myprediction$College == "BestCollege"] <-"No"
decision[myprediction$Coding == "OK" & myprediction$Impression == "Nerdy" & myprediction$College == "BestCollege"] <- "No"
decision[myprediction$Coding == 'Weak']<-'No'
decision[myprediction$Coding == "Weak" & myprediction$Impression == "Nerdy" & myprediction$College =="Redbrick"] <- "Yes"


accuracy<-round(mean(myprediction$Hired==decision),2)
print(paste("The accuracy of student model on training data is",accuracy))


# Model on test set:
decision = rep('Yes', nrow(test))
decision[test$Impression == "Nerdy" & test$Major == "IT" & test$College == "BestCollege"] <- "No"
decision[test$Coding == "OK" & test$Impression == "Shy" & test$College == "BestCollege"] <-"No"
decision[test$Coding == "OK" & test$Impression == "Nerdy" & test$College == "BestCollege"] <- "No"
decision[test$Coding == 'Weak']<-'No'
decision[test$Coding == "Weak" & test$Impression == "Nerdy" & test$College =="Redbrick"] <- "Yes"

accuracy<-round(mean(test$Hired==decision),2)
print(paste("The accuracy of student model on test data is",accuracy))

