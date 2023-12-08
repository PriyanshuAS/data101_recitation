install.packages("rpart")
install.packages("rpart.plot")
devtools::install_github("devanshagr/CrossValidation")
library(rpart)

# Now lets import the rpart.plot library to use the rpart.plot() function.
library(rpart.plot)

devtools::install_github("devanshagr/CrossValidation")


#Freestyle (16.4.1)
train<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTrainApr10.csv')
test<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/HireTestFull.csv')
colnames(train)
decision<-rep('No',nrow(train))
decision[train$College!='BestCollege'& train$Coding!='Weak']<-'Yes'
decision[train$Major!='IT' & train$Impression!='Outgoing' &train$Coding=='Excellent']<-'Yes'
accuracy<-round(mean(train$Hired==decision),2)
print(paste("The accuracy of professor model on traing data is",accuracy))
decision<-rep('No',nrow(test))
decision[test$College!='BestCollege'& test$Coding!='Weak']<-'Yes'
decision[test$Major!='IT' & test$Impression!='Outgoing' &test$Coding=='Excellent']<-'Yes'
accuracy<-round(mean(test$Hired==decision),2)
print(paste("The accuracy of professor model on test data is",accuracy))

#Freestyle (16.4.2)
decision = rep('Yes', nrow(train))
decision[train$Impression == "Nerdy" & train$Major == "IT" & train$College == "BestCollege"] <- "No"
decision[train$Coding == "OK" & train$Impression == "Shy" & train$College == "BestCollege"] <-"No"
decision[train$Coding == "OK" & train$Impression == "Nerdy" & train$College == "BestCollege"] <- "No"
decision[train$Coding == 'Weak']<-'No'
decision[train$Coding == "Weak" & train$Impression == "Nerdy" & train$College =="Redbrick"] <- "Yes"
accuracy<-round(mean(train$Hired==decision),2)
print(paste("The accuracy of student model on training data is",accuracy))
decision = rep('Yes', nrow(test))
decision[test$Impression == "Nerdy" & test$Major == "IT" & test$College == "BestCollege"] <- "No"
decision[test$Coding == "OK" & test$Impression == "Shy" & test$College == "BestCollege"] <-"No"
decision[test$Coding == "OK" & test$Impression == "Nerdy" & test$College == "BestCollege"] <- "No"
decision[test$Coding == 'Weak']<-'No'
decision[test$Coding == "Weak" & test$Impression == "Nerdy" & test$College =="Redbrick"] <- "Yes"
accuracy<-round(mean(test$Hired==decision),2)
print(paste("The accuracy of student model on test data is",accuracy))


# R-part

# rpart( formula , method, data, control,...)
# formula: here we mention the prediction column and the other related columns(predictors) on which the prediction will be based on.
           # prediction ~ predictor1 + predictor2 + predictor3 + ...
#method: here we describe the type of decision tree we want. If nothing is provided, the function makes an intelligent guess. 
         #We can use “anova” for regression, “class” for classification, etc.
#data: here we provide the dataset on which we want to fit the decision tree on.
#control: here we provide the control parameters for the decision tree.

tree <- rpart(Hired ~ Impression+Major+Coding+College, data = train,method = "class")
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Hired==pred)
pred <- predict(tree, test, type="class")
head(pred)
mean(test$Hired==pred)

#Cross Validate
#How the function works - The function takes the following arguments:
#1) Data Frame
#2) Decision Tree or rpart object
#3) Number of iterations
#4) Split Ratio


CrossValidation::cross_validate(train, tree, 10, 0.8)

# Min-Split

# minsplit: This parameter specifies the minimum number of observations required in a node for a split to be attempted. 
# If the number of observations in a node is less than minsplit, the node will not be further split. 
# This helps prevent overfitting by avoiding excessive partitioning of small nodes. The default value is usually 20.

# R-part (min-split 200)
tree <- rpart(Hired ~ Impression+Major+College+Coding, data = train,method = "class", control=rpart.control(minsplit = 200))
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Hired==pred)
pred <- predict(tree, test, type="class")
head(pred)
mean(test$Hired==pred)

# R-part (min-split 100)
tree <- rpart(Hired ~ Impression+Major+College+Coding, data = train,method = "class", control=rpart.control(minsplit = 100))
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Hired==pred)
pred <- predict(tree, test, type="class")
head(pred)
mean(test$Hired==pred)

# Min-Bucket 

# minbucket: This parameter sets the minimum number of observations required in any terminal (leaf) node of the tree.
# If a potential split would result in a leaf node with fewer observations than minbucket, the split is not attempted, and the node 
# becomes a terminal node. Similar to minsplit, this parameter helps control the size of the tree and prevent overfitting. 
# The default value is typically one more than minsplit.

# R-part (min-bucket 100)
tree <- rpart(Hired ~ Impression+Major+College+Coding, data = train,method = "class", control=rpart.control(minbucket = 100))
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Hired==pred)
pred <- predict(tree, test, type="class")
head(pred)
mean(test$Hired==pred)

# R-part (min-bucket 200)
tree <- rpart(Hired ~ Impression+Major+College+Coding, data = train,method = "class", control=rpart.control(minbucket = 200))
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Hired==pred)
pred <- predict(tree, test, type="class")
head(pred)
mean(test$Hired==pred)

# CP

# cp: The complexity parameter (cp) is used to control the complexity of the decision tree. 
# It specifies the minimum improvement in the model's accuracy that must be achieved for a split to be considered. 
# If the improvement is below cp, the split is not attempted, leading to a simpler tree. Higher values of cp result in smaller trees. 
# The default value is 0.01.

# R-part (cp - 0.05)
tree <- rpart(Hired ~ Impression+Major+College+Coding, data = train,method = "class", control=rpart.control(cp = 0.05)) 
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Hired==pred)
pred <- predict(tree, test, type="class")
head(pred)
mean(test$Hired==pred)

# R-part (cp - 0.005)
tree <- rpart(Hired ~ Impression+Major+College+Coding, data = train,method = "class", control=rpart.control(cp = 0.005))
rpart.plot(tree)
pred <- predict(tree, train, type="class")
head(pred)
mean(train$Hired==pred)
pred <- predict(tree, test, type="class")
head(pred)
mean(test$Hired==pred)

