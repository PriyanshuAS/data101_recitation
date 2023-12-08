# Recitation 7 - Topics - Chi square test and ROR (Reporting Odds Ratio)

# TOPIC 1 = Chi Square test

# import Moody dataset
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022.csv")

# summary of moody
summary(moody)


# what's a contingency table?
# It is a table that displays the count of observations falling into various categories 
# for two or more categorical variables.
# observed : actual count of observations falling into various categories
# expected : count of observations falling into various categories if there was no dependence 
#            between the categories

 

# contingency table (observed)
contingency_table <- table(moody$Grade, moody$Major)
contingency_table



# CHI SQUARE TEST

# Purpose?
# The chi-squared test for independence assesses whether there is a statistically 
# significant association between the variables in the contingency table. 

# Logic?
# If the observed counts significantly differ from expected counts if the variables were independent
# the test indicates a relationship between the variables.

# Example 1: Grades & Major
# 1. Alternative Hypothesis: There is a relationship/dependency between grades and major.

# 2. Null Hypothesis (H0) of Independence: 
#There is no relationship/association between grades & major. 
#In this case, it suggests that grades are independent of the major.

chisq.test(contingency_table)

# X-squared = 60.03 (chi-squared statistic)
# df = 12 (degrees of freedom for the chi-squared test)
# p-value = 2.229e-08 

# observations : 
# p-value = 2.229e-08 < 0.05
# we reject the null hypothesis
# we can say there is relationship between grades & major
# but we can't conclude anything on how exactly are they related

# Example 2: Grades & Seniority
# 1. Alternative Hypothesis: There is a relationship/dependency between grades and Seniority.

# 2. Null Hypothesis (H0) of Independence: 
#There is no relationship/association between grades & Seniority. 
#In this case, it suggests that grades are independent of the Seniority. 

contingency_table2 <- table(moody$Grade, moody$Seniority)
contingency_table2
chisq.test(contingency_table2)

#Observations:
#p-value = 0.001829 < 0.05 (reject null hypothesis, there is dependency between grade and seniority)

# Exercise TOPIC 1: Try testing relationship between Major and Seniority

# TOPIC 2: ROR (Reporting Odds Ratio)

#                   |         Blood Clot (+)  |       Blood Clot (-)  |
#  ---------------------------------------------------------------------
#  Covid Vaccine (+)      |        a                |        b              |
#  Covid Vaccine (-)      |        c                |        d              |
  
#a : number of people who received the vaccine and experienced a blood clot.
#b : number of people who received the vaccine but did not experience a blood clot.
#c : number of people who did not receive the vaccine but experienced a blood clot.
#d : number of people who did not receive the vaccine and did not experience a blood clot.

# EX1: 
a <- 20  
b <- 980  
c <- 5 
d <- 995  

ROR <- (a/b) / (c/d)
ROR

eln_ROR <- log(ROR)  # Natural logarithm of ROR


# Calculate the lower and upper bounds at 95 confidence
lower_CI <- exp(eln_ROR - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
upper_CI <- exp(eln_ROR + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
cat('Lower bound of 95 percent interval')
lower_CI
cat('Upper bound of 95 percent interval')
upper_CI
cat(ROR)
cat(lower_CI,upper_CI)
# 95% chance that ROR lies between the lowerbound and upperbound
# we care about the lower bound value
# observation: LB (95%) 1.518196 > 1
# It seems like 51% higher chance of getting blood clot in person who took vaccine patient 
# as opposed to person who did not take vaccine.


# Lower Bound > 1: When the lower bound of the confidence interval is greater than 1,
# it suggests that the group exposed to a certain factor (ex: Covid vaccine) 
# is at a higher risk of the outcome (ex: Blood Clot) compared to the unexposed group.

# Lower Bound < 1: It suggests that the group exposed to a certain factor (ex: Covid vaccine) 
# is at a lower risk of the outcome (ex: Blood Clot) compared to the unexposed group.

a <- 20  
b <- 980  
c <- 200
d <- 800 

#a : number of people who received the vaccine and experienced a blood clot.
#b : number of people who received the vaccine but did not experience a blood clot.
#c : number of people who did not receive the vaccine but experienced a blood clot.
#d : number of people who did not receive the vaccine and did not experience a blood clot.

ROR <- (a/b) / (c/d)
eln_ROR <- log(ROR)  # Natural logarithm of ROR

# EX2: 
# Calculate the lower and upper bounds
lower_CI <- exp(eln_ROR - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
upper_CI <- exp(eln_ROR + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
cat('Lower bound of 95 percent interval')
lower_CI
cat('Upper bound of 95 percent interval')
upper_CI
cat(ROR)
cat(lower_CI,upper_CI)
# Lower Bound < 1: It suggests that the group exposed to a certain factor (ex: Covid vaccine) 
# is at a lower risk of the outcome (ex: Blood Clot).

