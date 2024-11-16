library(readr)
library(dplyr)
setwd("C:/Users/morel/Desktop/sad_tweets/Sentiment_Analysis_Tweets")
combined_data <- read.csv('./Sentiment_it_tweet_2023.csv',sep = ";", header = TRUE)

combined_df <- combined_data %>%
  filter(combined_data$score >= 0.7)

# Funzione that extacts hastags from the "text" column
extract_hashtag <- function(text) {
  # Uses the regex to find the hastags
  list_of_hashtags <- str_extract_all(text, "#\\w+")
  # Converts the list of hastags into a string divided by a ", "
  paste(list_of_hashtags[[1]], collapse = ", ")
}

# Aggiungi una nuova colonna con la lista degli hashtag formattata come stringa
combined_df$hashtag <- sapply(combined_df$text, extract_hashtag)

# saves the new dataframe in file_con_hastag.csv
write.csv(combined_df, 'file_con_hashtag.csv',sep = ";", header = TRUE, row.names = FALSE, quote = TRUE)

# Prints the first rows of the new dataframe
head(combined_df$hashtag)


#Frequency for each hashtag
combined_hashtag_frequency <- table(combined_df$hashtag)

# Ordina per frequenza decrescente
combined_hashtag_ordered_frequency <- sort(combined_hashtag_frequency, decreasing = TRUE)

# Prendi i primi x elementi
combined_top_50_df <- as.data.frame(head(combined_hashtag_ordered_frequency, 50))


