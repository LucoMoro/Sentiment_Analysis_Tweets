library(readr)

#setwd(TO SET)

data <- read.csv("Sentiment_it_tweet_2023.csv", sep = ";", header = TRUE)

frequency <- table(data$sentiment)
print(frequency)
plot(frequency)
