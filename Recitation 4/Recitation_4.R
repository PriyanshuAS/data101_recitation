#Read CSV
Cars <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Cars2022.csv")
summary(Cars)

# FALSE DISCOVERIES USING NARROW QUERIES

#Creating 2 subsets of the data
Cars1<-Cars[1:25000,]
Cars2<-Cars[25001:50000,]

#####################################

#Effect of narrow queries on different subset of a dataset for the average age of a Ford buyer in Chicago
m1<-mean(Cars1[Cars1$Dealership=='Chicago' & Cars1$Car=='Ford',]$Buyer_Age)
cat("The mean age of Ford buyer at Chicago dealership based on Subset Cars1 is:",m1)

m2<-mean(Cars2[Cars2$Dealership=='Chicago' & Cars2$Car=='Ford',]$Buyer_Age)
cat("The mean age of Ford buyer at Chicago dealership based on Subset Cars2 is:",m2)

m_main<-mean(Cars[Cars$Dealership=='Chicago' & Cars$Car=='Ford',]$Buyer_Age)
cat("The mean age of Ford buyer at Chicago dealership based on the overall Cars Dataset is:",m_main)

#####################################

#using varied subsets, you can compare the discrepancies between the narrow queries
#Cars1
round(table(Cars1[Cars1$Dealership=='Chicago'& Cars1$Buyer_Age <22,]$Car)/
        nrow(Cars1[Cars1$Dealership=='Chicago'& Cars1$Buyer_Age<22,]),4)
#You see % of people under age 22 buying Chevrolet is double of people buying Honda or Jeep

#Cars2
round(table(Cars2[Cars2$Dealership=='Chicago'& Cars2$Buyer_Age <22,]$Car)/
        nrow(Cars2[Cars2$Dealership=='Chicago'& Cars2$Buyer_Age<22,]),4)
#But here, you see the trend disappears in the second data subset

#Overall Cars Dataset
#The more narrow is the query, the more discrepancy there is between query results over samples.
round(table(Cars[Cars$Dealership=='Chicago'& Cars$Buyer_Age <22,]$Car)/
        nrow(Cars[Cars$Dealership=='Chicago'& Cars$Buyer_Age<22,]),4)
#Over the full dataset, the trend is different again

#######################################

#Cars1
round(table(Cars1[Cars1$Buyer_Age>45 & Cars1$Buyer=='Female'&Cars1$Dealership=='San Antonio',]$Car)/
        nrow(Cars1[Cars1$Buyer_Age>45 & Cars1$Buyer=='Female'&Cars1$Dealership=='San Antonio',]),4)
#Cars2
round(table(Cars2[Cars2$Buyer_Age>45 & Cars2$Buyer=='Female'&Cars2$Dealership=='San Antonio',]$Car)/
        nrow(Cars2[Cars2$Buyer_Age>45 & Cars2$Buyer=='Female'&Cars2$Dealership=='San Antonio',]),4)
#Overall Cars Dataset
round(table(Cars[Cars$Buyer_Age>45 & Cars$Buyer=='Female'&Cars$Dealership=='San Antonio',]$Car)/
        nrow(Cars[Cars$Buyer_Age>45 & Cars$Buyer=='Female'&Cars$Dealership=='San Antonio',]),4)

#######################################

#Average age distribution grouped by all car makes in Summer in Chicago
#Cars1
tapply(Cars1[Cars1$Dealership=='Chicago' & Cars1$Season == "Summer",]$Buyer_Age, Cars1[Cars1$Dealership=='Chicago' 
                                                                                       & Cars1$Season == "Summer",]$Car, mean)
#Cars2
tapply(Cars2[Cars2$Dealership=='Chicago' & Cars2$Season == "Summer",]$Buyer_Age, Cars2[Cars2$Dealership=='Chicago' 
                                                                                       & Cars2$Season == "Summer",]$Car, mean)
#Overall Cars Dataset
tapply(Cars[Cars$Dealership=='Chicago' & Cars$Season == "Summer",]$Buyer_Age, Cars[Cars$Dealership=='Chicago' 
                                                                                   & Cars$Season == "Summer",]$Car, mean)

# Take example of Jeep vs Ram age distribution

########################################

#Distribution of Female Buyers over age of 30 buying a Honda in LA
#Cars1
round(table(Cars1[Cars1$Dealership == 'LA' & Cars1$Car == 'Honda' & Cars1$Buyer=='Female' & Cars1$Buyer_Age>30,]$Season)/
        nrow(Cars1[Cars1$Dealership == 'LA' & Cars1$Car == 'Honda' & Cars1$Buyer=='Female' & Cars1$Buyer_Age>30,]),4)
#Cars2
round(table(Cars2[Cars2$Dealership == 'LA' & Cars2$Car == 'Honda' & Cars2$Buyer=='Female' & Cars2$Buyer_Age>30,]$Season)/
        nrow(Cars2[Cars2$Dealership == 'LA' & Cars2$Car == 'Honda' & Cars2$Buyer=='Female' & Cars2$Buyer_Age>30,]),4)
#Overall Cars Dataset
round(table(Cars[Cars$Dealership == 'LA' & Cars$Car == 'Honda' & Cars$Buyer=='Female' & Cars$Buyer_Age>30,]$Season)/
        nrow(Cars[Cars$Dealership == 'LA' & Cars$Car == 'Honda' & Cars$Buyer=='Female' & Cars$Buyer_Age>30,]),4)

########################################

# Create narrow queries which seem to discover something  (twice as many Hondas than GMC,  
# 5 years difference in age, more cars sold in Austin over spring than in Chicago over spring etc)


#LAW OF SMALL NUMBERS
#Small Samples are more susceptible to producing extreme results compared to large samples


