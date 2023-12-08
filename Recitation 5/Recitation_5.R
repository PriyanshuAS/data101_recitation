install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

# Null Hypothesis: Initial assumption that there is no significant relationship between the variables being studied. 
# Alternative Hypothesis: Hypothesis that researchers want to test, suggesting that there is a significant relationship between the variables.
# p-value: A statistical measure to assess the strength of evidence against a null hypothesis.
# Z-test: A statistical hypothesis test used to determine whether there is a significant difference between the means of two groups.


# Movies dataset
movies<-read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Movies2022F-4-new.csv")
summary(movies)


# Test the hypothesis that the average IMDB_SCORE is higher when COUNTRY is 'Netherlands' than when COUNTRY is 'Chile'
# Null Hypothesis : The average value of IMDB_SCORE is the same when COUNTRY = 'Netherlands' and when COUNTRY = 'Chile'
# Alternative Hypothesis : The average value of IMDB_SCORE is greater when COUNTRY = 'Netherlands' than when COUNTRY = 'Chile'
mean(movies[movies$country == 'Netherlands', ]$imdb_score)
mean(movies[movies$country == 'Chile', ]$imdb_score)

# permutation_test(df, 'CAT', 'NUM', N, 'v1', 'v2'), where 
# df is the name of data frame, 
# CAT is a categorical variable, 
# NUM is the numerical variable (we are interested in mean(NUM)), 
# N is the number of permutations, 
# v1 and v2 are two values of CAT variable. 
permutation_test(movies, 'country', 'imdb_score', 10000, 'Chile', 'Netherlands')
# Run multiple times; p-values are different every time

z_test_from_data(movies, 'country', 'imdb_score', 'Chile', 'Netherlands')
# p-value returned by z test function is always the same no matter how many times you execute z test function

# If the output is less than 0.05 then we reject the null hypothesis.
# Suggests that there is enough evidence in your sample data to conclude that the alternative hypothesis is more likely.
# Else we fail to reject the null hypothesis; data does not provide sufficient evidence to conclude a significant relationship.


df <- movies[movies$genre == 'Action', ]
# Null Hypothesis : The average value of IMDB_SCORE is the same when COUNTRY = 'USA' and when COUNTRY = 'UK'
# Alternative Hypothesis :
mean(df[df$country == 'USA', ]$imdb_score)
mean(df[df$country == 'UK', ]$imdb_score)
permutation_test(df, 'country', 'imdb_score', 10000, 'USA', 'UK')
z_test_from_data(df, 'country', 'imdb_score', 'USA', 'UK')




#Party dataset
party <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")
summary(party)

df <- party[party$DJ == 'Mski', ]
# Null Hypothesis : The average value of TICKET is the same when DAY = 'Saturday' and when DAY = 'Friday'
# Alternative Hypothesis :
mean(df[df$Day == 'Saturday', ]$Ticket)
mean(df[df$Day == 'Friday', ]$Ticket)
permutation_test(df, 'Day', 'Ticket', 10000, 'Friday', 'Saturday')
z_test_from_data(df, 'Day', 'Ticket', 'Friday', 'Saturday')
# Why is the p-value clode to 1 ??
# Reverse the variables


df <- party[party$Music == 'Jazz', ]
# Null Hypothesis : The average value of RATING is the same when DJ = 'Rohit' and when DJ = 'Mski'
# Alternative Hypothesis :
mean(df[df$DJ == 'Rohit', ]$Rating)
mean(df[df$DJ == 'Mski', ]$Rating)
permutation_test(df, 'DJ', 'Ticket', 10000, 'Mski', 'Rohit')
z_test_from_data(df, 'DJ', 'Ticket', 'Mski', 'Rohit')


