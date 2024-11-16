library(readr)
library(dplyr)
df <- read.csv('./Sentiment_it_tweet_2023.csv',sep = ";", header = TRUE)

# Funzione that extacts hastags from the "text" column
extract_hashtag <- function(text) {
  # Uses the regex to find the hastags
  list_of_hashtags <- str_extract_all(text, "#\\w+")
  # Converts the list of hastags into a string divided by a ", "
  paste(list_of_hashtags[[1]], collapse = ", ")
}

# Aggiungi una nuova colonna con la lista degli hashtag formattata come stringa
df$hashtag <- sapply(df$text, extract_hashtag)

# saves the new dataframe in file_con_hastag.csv
write.csv(df, 'file_con_hashtag.csv',sep = ";", header = TRUE, row.names = FALSE, quote = TRUE)

# Prints the first rows of the new dataframe
head(df$hashtag)

