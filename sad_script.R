library(readr)

#csv_path <- Sys.getenv("CSV_PATH")

data <- read.csv("Sentiment_it_tweet_2023.csv", sep=";")

frequency <- table(data$sentiment)
print(frequency)
plot(frequency)

