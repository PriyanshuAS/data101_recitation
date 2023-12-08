train <- read.csv("/Users/ryuk/Documents/Data 101/Prediction Challenge 1/Couples.csv")
summary(train)


myprediction<-train


decision <- rep('Success', nrow(myprediction))
decision[myprediction$GroomMB == myprediction$BrdieMB & myprediction$GroomInc == myprediction$BrideInc] <- "Failure"


accuracy<-mean(myprediction$Outcome==decision)
print(paste("The accuracy of student model on training data is",accuracy))
