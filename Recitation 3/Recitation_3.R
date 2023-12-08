# getwd()
# setwd("C:/Users/sehga/OneDrive/Documents/Recitations")
partydata <- read.csv("partyR.csv")
head(partydata, n = 10)

# str() and summary() help you find levels for categorical(both) and stats for numerical (summary() only)
str(partydata)
summary(partydata)

unique(partydata$Day)
unique(partydata$DJ)
unique(partydata$Music)

# one of our aims is the find how factors like Day, DJ and Music type impact the attendance
# and to find any hidden relationships.
# We can explore freq distributions too with different subset filters using table() to find high or low values.
# like which Djs plays certain music on specific days.


# summary will show you the mean attendance for the entire dataset, on attendance column if passed without filters. 
summary(partydata$Attendance)
# So the mean is ~56


# So lets try table() to see if there are too few or too many values in any column.
table(partydata$Day)
table(partydata$DJ)
table(partydata$Music)
# This also does not tell us much since things are pretty much uniform.

# How do we compare two columns one cat, one numerical?

# tapply() will help us in this case to compare means of attendance 
# for different unique values of Day, DJ, Music.
tapply(partydata$Attendance, partydata$DJ, mean)
tapply(partydata$Attendance, partydata$Music, mean)
tapply(partydata$Attendance, partydata$Day, mean)
# This does not tell us much since there is little variation.

# So lets dig deeper by applying filters.
# trying other filters along with changes in Day attribute.
unique(partydata$Day)
unique(partydata$DJ)
unique(partydata$Music)

# Tell students to try a dfs approach like below, before spreading and trying different days. 
# For eg:
# So lets pick the day filter as Day = Saturday since it has the highest mean attendance
# on running tapply(partydata$Attendance, partydata$Day, mean)
tapply(partydata$Attendance, partydata$Day, mean)
# So now that we have the mean attendance for Day = saturday, we might want to see
# which DJ has the highest mean attendance to understand which filter to try next.
partyd <- partydata[partydata$Day == "Saturday", ]
tapply(partyd$Attendance, partyd$Music, mean)

partyd <- partydata[partydata$Day == "Saturday" & partydata$DJ=="Blue", ]
tapply(partyd$Attendance, partyd$Music, mean)
partyd <- partydata[partydata$Day == "Saturday" & partydata$DJ=="Monique", ]
tapply(partyd$Attendance, partyd$Music, mean)
partyd <- partydata[partydata$Day == "Saturday" & partydata$DJ=="Brejcha", ]
tapply(partyd$Attendance, partyd$Music, mean)
partyd <- partydata[partydata$Day == "Saturday" & partydata$DJ=="Mski", ]
tapply(partyd$Attendance, partyd$Music, mean)

partyd <- partydata[partydata$Day == "Saturday" & partydata$Music=="Jazz", ]
tapply(partyd$Attendance, partyd$DJ, mean)
partyd <- partydata[partydata$Day == "Saturday" & partydata$Music=="Rock", ]
tapply(partyd$Attendance, partyd$DJ, mean)
partyd <- partydata[partydata$Day == "Saturday" & partydata$Music=="Techno", ]
tapply(partyd$Attendance, partyd$DJ, mean)
partyd <- partydata[partydata$Day == "Saturday" & partydata$Music=="HipHop", ]
tapply(partyd$Attendance, partyd$DJ, mean)


# Next we could pick Day = Friday, since it has least mean attendance on running 
tapply(partydata$Attendance, partydata$Day, mean)

# partyd <- partydata[partydata$Day == "Friday", ]
# tapply(partyd$Attendance, partyd$DJ, mean)

partyd <- partydata[partydata$Day == "Friday" & partydata$DJ=="Blue", ]
tapply(partyd$Attendance, partyd$Music, mean)
partyd <- partydata[partydata$Day == "Friday" & partydata$DJ=="Monique", ]
tapply(partyd$Attendance, partyd$Music, mean)
partyd <- partydata[partydata$Day == "Friday" & partydata$DJ=="Brejcha", ]
tapply(partyd$Attendance, partyd$Music, mean)
partyd <- partydata[partydata$Day == "Friday" & partydata$DJ=="Mski", ]
tapply(partyd$Attendance, partyd$Music, mean)

partyd <- partydata[partydata$Day == "Friday" & partydata$Music=="Jazz", ]
tapply(partyd$Attendance, partyd$DJ, mean)
partyd <- partydata[partydata$Day == "Friday" & partydata$Music=="Rock", ]
tapply(partyd$Attendance, partyd$DJ, mean)
partyd <- partydata[partydata$Day == "Friday" & partydata$Music=="Techno", ]
tapply(partyd$Attendance, partyd$DJ, mean)
partyd <- partydata[partydata$Day == "Friday" & partydata$Music=="HipHop", ]
tapply(partyd$Attendance, partyd$DJ, mean)
# try other combinations also and see if anything is interesting.
# remember to compare it to the mean of the total attendance to decide.





partydsat <- partydata[partydata$DJ=="Mski" & partydata$Music=="HipHop", ]
tapply(partydsat$Attendance, partydsat$Day, mean)
boxplot(partydata$Attendance~partydata$DJ)
boxplot(partydsat$Attendance~partydsat$Day)

# Ans is the following with one having higher attendance and the other having lower attendance.
mean(partydata[partydata$Day == "Saturday" & partydata$DJ=="Mski" & partydata$Music=="HipHop", ]$Attendance)
mean(partydata[partydata$Day == "Saturday" & partydata$DJ=="Blue" & partydata$Music=="HipHop", ]$Attendance)
mean(partydata[partydata$Day == "Friday" & partydata$DJ=="Blue" & partydata$Music=="Jazz", ]$Attendance)


# Confirm and visualise with boxplot
# boxplot does not give mean but it is good to see the spread of values in comparison
boxplot(partydata[partydata$Day == "Saturday" & partydata$DJ=="Mski" & partydata$Music=="HipHop", ]$Attendance)
boxplot(partydata[partydata$Day == "Friday" & partydata$DJ=="Blue" & partydata$Music=="Jazz", ]$Attendance)







# Freq anomaly detection
table(partydata$Day)
table(partydata$DJ)
table(partydata$Music)

partydsat <- partydata[partydata$DJ=="Mski", ]
table(partydsat$Day)
table(partydsat$Music)


partydsat <- partydata[partydata$DJ=="Mski", ]
table(partydsat$Day)
table(partydsat$Music)

# One way to find which days certain music is frequent is using table to find distributions.
table(partydsat$Music, partydsat$Day)

# To find which days are more frequent with which Djs for music = hiphop
partydsat <- partydata[partydata$Music=="HipHop", ]
partydsat
table(partydsat$Day, partydsat$DJ)

partydsat <- partydata[partydata$Music=="Techno", ]
table(partydsat$Day, partydsat$DJ)

partydsat <- partydata[partydata$Music=="Jazz", ]
table(partydsat$Day, partydsat$DJ)

partydsat <- partydata[partydata$Music=="Rock", ]
table(partydsat$Day, partydsat$DJ)





