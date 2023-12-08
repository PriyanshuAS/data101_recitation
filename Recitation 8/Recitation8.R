# Recitation Week-8
# Topic: Bayesian Reasoning

# -> Estimating odds (chances, probabilities)
# Basic terms: Observation, Belief
# -> Observation = Something we know
# -> Belief = Something we do not know
# Statement is usually of the form: If Observation, then Belief

# Basic Example

# Observation = Covid Test Result
# Belief = Having Covid

# Probability of having Covid
PriorHaveCovid<-0.01

# 1. Prior odds = odds for the belief before observation
# Prior odds of having Covid
PriorCovidOdds<-PriorHaveCovid/(1-PriorHaveCovid)
cat("The Prior Covid Odds are equal to:",PriorCovidOdds)

# P(a|b) = The probability of event a, given that event b occurs

# 2. True positive is the conditional probability of seeing the observation given that our belief is true
# True positive: Probability of having positive Covid test when having Covid = P(PositiveCovidTest|HaveCovid)
TruePositive<-0.99

# 3. False positive is the probability of observation under condition that the belief is not true
# False positive = Probability of having positive Covid test when not having Covid = P(PostiveCovidTest/DoNotHaveCovid)
FalsePositive<-0.001

# 4. Likelihood Ratio
LikelihoodRatio<-TruePositive/FalsePositive
cat("The Likelihood Ratio is equal to:",LikelihoodRatio)

# 5. Posterior odds = odds for the belief after taking observation into consideration
PosteriorCovidOdds<-LikelihoodRatio*PriorCovidOdds
cat("The Posterior Covid Odds are equal to:",PosteriorCovidOdds)

PosteriorHaveCovid<-PosteriorCovidOdds/(1+PosteriorCovidOdds)
PosteriorHaveCovid    # to get the ratio

################################################################################

# Importing dataset
moody<-read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv')
summary(moody)

# Exercise-1
# Compute the odds that a student with ‘F’ grade is a freshman

# (a) --- Ask students to identify Observation and Belief ---

# Observation is that the student failed the class.
# Belief is that student is a freshman.

nf<-nrow(moody[moody$Seniority=='Freshman',])
nf

n<-nrow(moody)
n

Prior<-nf/n
Prior

PriorOdds<-round(Prior/(1-Prior),2)
cat("The prior odds are equal to:",PriorOdds)

TruePositive<-round(nrow(moody[moody$Grade=='F' & moody$Seniority=='Freshman',])/nrow(moody[moody$Seniority=='Freshman',]),2)
cat("The True Positive is equal to:",TruePositive)

FalsePositive<-round(nrow(moody[moody$Grade=='F'& moody$Seniority!='Freshman',])/nrow(moody[moody$Seniority!='Freshman',]),2)
cat("The False Positive is equal to:",FalsePositive)

LikelihoodRatio<-round(TruePositive/FalsePositive,2)
cat("The Likelihood Ratio is equal to:",LikelihoodRatio)

PosteriorOdds<-LikelihoodRatio * PriorOdds
cat("The Posterior Odds are equal to:",PosteriorOdds)

Posterior<-PosteriorOdds/(1+PosteriorOdds)
Posterior

################################################################################

# Exercise-2
# Compute the odds that a student with ‘A’ grade and score less than 80 is a psychology major

# (b) --- Ask students to identify Observation and Belief ---

# Observation is that a student got an A and less than 80 in score.
# Belief is that student is a psychology major.

np<-nrow(moody[moody$Major=='Psychology',])
np

n<-nrow(moody)
n

Prior<-np/n
Prior

PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds

TruePositive<-round(nrow(moody[moody$Score<80 & moody$Grade=='A'&moody$Major=='Psychology',])/nrow(moody[moody$Major=='Psychology',]),2)
TruePositive

FalsePositive<-round(nrow(moody[moody$Score<80 & moody$Grade=='A'&moody$Major!='Psychology',])/nrow(moody[moody$Major!='Psychology',]),2)
FalsePositive

LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio

PosteriorOdds<-LikelihoodRatio * PriorOdds
PosteriorOdds

Posterior<-PosteriorOdds/(1+PosteriorOdds)
Posterior

################################################################################

# Exercise-3

# Given a contingency table between Seniority and Grade
# The values which label the column represents the observations
# The values which label the row represents the beliefs

t<-table(moody$Seniority, moody$Grade)
cat("Contingency table of Seniority vs Grade:", "\n")
t

# Observation is getting an A, Belief is being a freshman
PriorProb<-sum(t[1, ])/sum(t[ , ])
PriorProb

PriorOdds <- PriorProb/(1-PriorProb)
cat("Prior Odds of being a freshman:", PriorOdds, "\n")

TruePositive<-t[1,1]/sum(t[1, ])
TruePositive

FalsePositive<-sum(t[2:4,1])/sum(t[2:4, ])
FalsePositive

LikelihoodRatio<-TruePositive/FalsePositive
LikelihoodRatio

cat("How odds of being a freshman change if you get A:", LikelihoodRatio, "\n")
# This is how your odds of being a freshman change if you get an A
PosteriorOdds<- LikelihoodRatio * PriorOdds
PosteriorOdds

Posterior <- PosteriorOdds/(1+PosteriorOdds)
Posterior

################################################################################

# Exercise-4

# Reverse the Belief and Observation

# (c) --- Ask students to identify new Observation and new Belief ---

# Belief is getting an A, Observation is being a freshman

PriorProb<-sum(t[ ,1])/sum(t[ , ])
PriorProb

PriorOdds <- PriorProb/(1-PriorProb)
cat("Prior Odds of getting an A:", PriorOdds, "\n")

TruePositive<-t[1,1]/sum(t[ ,1])
TruePositive

FalsePositive<-sum(t[1,2:5])/sum(t[ ,2:5])
FalsePositive

LikelihoodRatio<-TruePositive/FalsePositive
LikelihoodRatio

cat("How odds of getting A change when you are freshman:", LikelihoodRatio, "\n")
# This is how your odds of getting A change if you freshman
PosteriorOdds<- LikelihoodRatio*PriorOdds
PosteriorOdds

Posterior <- PosteriorOdds/(1+PosteriorOdds)
Posterior

################################################################################

# WHAT STUDENTS WILL BE ASKED TO DO -> (a),(b),(c)

################################################################################