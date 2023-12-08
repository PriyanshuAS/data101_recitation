http://tinyurl.com/AttendData101
# Vectors
days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
days

# Numerical Vectors
age <- 1:18
age

# Data Frame
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022.csv")
head(moody)
#

# Data Subsetting
# Return row 5
moody[5, ]

# Return column 1
moody[, 1]

# Rows 1:10 and column 1
moody[1:10, 1]

# Give me rows 1-5 and columns 1 and 3 of moody
moody[1:5, 1:3]

# Table
# Making a table of that counts the number of students for each GPA
gpa <- table(moody$GPA)
gpa
# Joint distribution of Major and GPA
table(moody$Major, moody$GPA)

# Basic Functions
# mean()
mean(moody$GPA)
# length()
length(moody$Major)
# max()
max(moody$GPA)
# min()
min(moody$GPA)
# standard deviation
sd(moody$GPA)

# grades <- table(moody$Grade)
# grades

# Subset
moody[moody$Major=="Statistics", ]
moody[moody$Score >85 & moody$Grade == 'A', ]
# subsetting columns
colnames(moody)
# Removing the first column
moody3<-subset(moody, select = -1)
head(moody3)
dim(moody3)
#Subset of Rows and Columns
moody1<-subset(moody, select = c(1:3), Major=="Statistics")
moody1
colnames(moody1)
#Notice that only 3 columns are remaining
dim(moody)
dim(moody1)

# tapply()
tapply(moody[moody$Seniority == 'Freshman',]$Score, moody[moody$Seniority == 'Freshman' ,]$Grade,mean)
# another example
# Sample data
values <- c(12, 15, 8, 10, 14, 9)
groups <- c("A", "B", "A", "B", "A", "B")
mean_by_group <- tapply(values, groups, mean)
mean_by_group

# Examples of basic queries 
colnames(moody)

summary(moody)

unique(moody$Major)

min(moody[moody$Grade=='B',]$Score)

max(moody[moody$Grade=='A',]$Score)

tapply(moody[moody$Grade=='B',]$Score, moody[moody$Grade=='B',]$Major, min)

moody$ScoreIntervals<-cut(moody$Score,breaks=c(0,50,75,90,100),labels=c("Low","Medium",'Good', "Excellent"))
head(moody)
table(moody$ScoreIntervals)

#Does major affect the grade averages and cutoff points
cat('Grade Averages in Psychology ')
moodyP<-moody[moody$Major=='Psychology',]
tapply(moodyP$Score, moodyP$Grade, mean)
cat('Grade Averages in Computer Science ')
moodyCS<-moody[moody$Major=='CS',]
tapply(moodyCS$Score, moodyCS$Grade, mean)
cat('Grade Averages in Statistics')
moodyStat<-moody[moody$Major=='Statistics',]
tapply(moodyStat$Score, moodyStat$Grade, mean)
cat('Grade Averages in Economics')
moodyEcon<-moody[moody$Major=='Economics',]
tapply(moodyEcon$Score, moodyEcon$Grade, mean)
# So as you can see from the results, the major does affect the grade averages and cutoff points.


