library(readr)
library(dplyr)

data <- read.csv("Sentiment_it_tweet_2023.csv", sep = ";", header = TRUE)

#Frequencies calculated before the filtering
frequency <- table(data$sentiment)
print(frequency)
plot(frequency)

#Filters the dataset mantaining only the retweets
data <- data %>% filter(is_retweet == "True")

#Frequencies calculated after the filtering
frequency <- table(data$sentiment)
print(frequency)
plot(frequency)

#Creates a dataset by grouping users who have been retweeted
retweets <- data %>%
  group_by(original_tweet_username) %>%
  summarise(number_of_retweets = n())
print(paste("Number of users in retweets:", nrow(retweets)))


#Creates a dataset consisting only of retweeted users
retweeted_users <- data %>%
  filter(username %in% retweets$original_tweet_username)
print(paste("Number of users in Tweets:", nrow(retweeted_users)))
