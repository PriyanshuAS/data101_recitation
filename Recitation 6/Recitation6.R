install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest@main")
library(HypothesisTesting)

#Movies dataset
movies<-read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Movies2022F-4-new.csv")
summary(movies)

#Summarizing every column
summary(movies$country)
summary(movies$content)
summary(movies$Gross)
summary(movies$Budget)
summary(movies$genre)

#Calulating the number of different values present in every categorical column
country_count <- length(unique(movies$country))
country_count

content_count <- length(unique(movies$content))
content_count

gross_count <- length(unique(movies$Gross))
gross_count

budget_count <- length(unique(movies$Budget))
budget_count

genre_count <- length(unique(movies$genre))
genre_count

#Calculating the Significance level for Multiple hypothesis using Bonferroni coefficient
#Significance level is equal to 5%/m, where m is the number of hypotheses
#If there are N number of items, then m = N choose 2 i.e m = N*(N-1)/2
m_country <- country_count*(country_count-1)/2
country_significance <- 0.05/m_country
cat("Number of possible combinations for country = ",m_country,"\nSignificance level for country = ",country_significance*100,"%")

#Similarly, we can calculate the significance levels for other features like Content, Gross, Budget, and Genre
m_content <- content_count*(content_count-1)/2
content_significance <- 0.05/m_content
cat("Number of possible combinations for content = ",m_content,"\nSignificance level for content = ",content_significance*100,"%")

m_gross <- gross_count*(gross_count-1)/2
gross_significance <- 0.05/m_gross
cat("Number of possible combinations for gross = ",m_gross,"\nSignificance level for gross = ",gross_significance*100,"%")

m_budget <- budget_count*(budget_count-1)/2
budget_significance <- 0.05/m_budget
cat("Number of possible combinations for budget = ",m_budget,"\nSignificance level for budget = ",budget_significance*100,"%")

m_genre <- genre_count*(genre_count-1)/2
genre_significance <- 0.05/m_genre
cat("Number of possible combinations for genre = ",m_genre,"\nSignificance level for genre = ",genre_significance*100,"%")


#Execute a tapply function of mean IMDB score vs Country
tapply(movies$imdb_score, movies$country, mean)

cat("Significance level for country = ",country_significance*100,"%")
#NULL Hypothesis : The average value of IMDB score is same when country='Chile' than when country='Netherlands'
#Alternative Hypothesis : The average value of IMDB score is greater when country='Netherlands' than when country='Chile'

permutation_test(movies, 'country', 'imdb_score', 10000, 'Chile', 'Netherlands')

permutation_test(movies, 'country', 'imdb_score', 10000, 'Taiwan', 'Brazil')
permutation_test(movies, 'country', 'imdb_score', 10000, 'Italy', 'India')
permutation_test(movies, 'country', 'imdb_score', 10000, 'Spain', 'Sweden')
permutation_test(movies, 'country', 'imdb_score', 10000, 'Hungary', 'USA')

#If the p-value is below the significance level of 0.333%, we have enough evidence to reject the Null Hypothesis
#Else we fail to reject the Null Hypothesis, as the data doesn't provide sufficient evidence to conclude a significant relationship

#Execute a few more tapply functions for mean IMDB score vs Country/Gross/Budget to find more Hypotheses
tapply(movies$imdb_score, movies$genre, mean)
tapply(movies$imdb_score, movies$Gross, mean)
tapply(movies$imdb_score, movies$Budget, mean)
#Try to run permutation tests to reject a Null Hypothesis



#Party Dataset
party <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")
summary(party)

#Calulating the number of different values present in every categorical column
music_count <- length(unique(party$Music))
day_count <- length(unique(party$Day))
dj_count <- length(unique(party$DJ))
dj_count
#Calculating the Significance level for Multiple hypothesis using Bonferroni coefficient
m_music <- music_count*(music_count-1)/2
music_significance <- 0.05/m_music
cat("Number of possible combinations for Music = ",m_music,"\nSignificance level for Music = ",music_significance*100,"%")

m_day <- day_count*(day_count-1)/2
day_significance <- 0.05/m_day
cat("Number of possible combinations for Day = ",m_day,"\nSignificance level for Day = ",day_significance*100,"%")

m_dj <- dj_count*(dj_count-1)/2
dj_significance <- 0.05/m_dj
cat("Number of possible combinations for DJ = ",m_dj,"\nSignificance level for DJ = ",dj_significance*100,"%")

#Execute a tapply function of mean Attendance vs DJ
tapply(party$Attendance, party$DJ, mean)

cat("Significance level for DJ = ",dj_significance*100,"%")
#NULL Hypothesis : The average value of Attendance is same when DJ='Alex' than when DJ='Ania'
#Alternative Hypothesis : The average value of Attendance is greater when DJ='Ania' than when DJ='Alex'
permutation_test(party, 'DJ', 'Attendance', 10000, 'Alex', 'Ania')

permutation_test(party, 'DJ', 'Attendance', 10000, 'Alex', 'Blue')
permutation_test(party, 'DJ', 'Attendance', 10000, 'Rohit', 'Mski')
permutation_test(party, 'DJ', 'Attendance', 10000, 'Carol', 'Blue')

#If the p-value is below the significance level of 0.333%, we have enough evidence to reject the Null Hypothesis
#Else we fail to reject the Null Hypothesis, as the data doesn't provide sufficient evidence to conclude a significant relationship

#Execute a few more tapply functions for mean Attendance vs Music/Day to find more Hypotheses
tapply(party$Attendance, party$Music, mean)
tapply(party$Attendance, party$Day, mean)
#Try to run permutation tests to reject a Null Hypothesis

