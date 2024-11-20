# Carica il pacchetto necessario
library(stringr)

#############################################################################################
################################### Dataset import ##########################################
#############################################################################################
# Leggi il dataset
data <- read.csv('./Sentiment_it_tweet_2023.csv', sep = ";", header = TRUE)


#############################################################################################
############################ Dataset filtering & cleaning ###################################
#############################################################################################

data$text <- tolower(data$text)

df <- data %>%
  filter(score >= 0.7)


#############################################################################################
################################### Unique hashtags extraction ##############################
#############################################################################################

# Estrai tutti gli hashtag dalla colonna 'text'
all_hashtags <- unlist(str_extract_all(df$text, "#\\w+"))

clean_hashtags <- str_replace_all(all_hashtags, "[[:punct:]]$", "")
# Calcola la frequenza di ogni hashtag
hashtag_frequency <- table(clean_hashtags)

# Ordina per frequenza decrescente
hashtag_ordered <- sort(hashtag_frequency, decreasing = TRUE)

# Prendi i top 50 hashtag
top_50_hashtags <- head(hashtag_ordered, 50)

# Converte i risultati in un data frame per la visualizzazione
top_50_df <- as.data.frame(top_50_hashtags)
colnames(top_50_df) <- c("Hashtag", "Frequency")

# Crea il grafico a barre con il pacchetto base di R
barplot(top_50_hashtags,
        names.arg = names(top_50_hashtags),
        las = 2, # Ruota le etichette degli hashtag per una visualizzazione migliore
        col = "steelblue",
        main = "Top 50 Hashtags by Frequency",
        xlab = "Hashtag",
        ylab = "Frequency",
        cex.names = 0.7)  # Imposta la dimensione delle etichette


#############################################################################################
################################### Unique hashtags time series #############################
#############################################################################################

# Adapts the date format to Y-M-D excluding
df$extractedts <- as.Date(df$extractedts)

# Filters all the tweets that contain the Ucraina hashtag
ucraina_hashtag <- df %>%
  filter(str_detect(text, "(?i)#ucraina"))

# Groups the frequency of Ucraina hashtags by day
daily_ucraina_hashtag <- ucraina_hashtag %>%
  group_by(extractedts) %>%
  summarise(Frequency = n())

plot(daily_ucraina_hashtag$extractedts, daily_ucraina_hashtag$Frequency, type = "o", col = "steelblue",
     main = "Daily frequency of #Ucraina",
     xlab = "Date", ylab = "Frequency")


#############################################################################################
################################### Positive tweets time series #############################
#############################################################################################

positive_tweets <- df %>%
  filter(sentiment == "pos")

daily_positive_tweets <- positive_tweets %>%
  group_by(extractedts) %>%
  summarise(Frequency = n())

plot(daily_positive_tweets$extractedts, daily_positive_tweets$Frequency, type = "o", col = "steelblue",
     main = "Daily frequency of positive tweets",
     xlab = "Date", ylab = "Frequency")

nonbot_positive_tweets <- data %>%
  filter(sentiment == "pos")

response_positive_tweets <- df %>%
  filter(sentiment == "pos") %>%
  filter(is_retweet == "True")

#############################################################################################
######################################### Response to RQ_1 ##################################
#############################################################################################

# Extracts the table of hashtags with the frequency (N_x)
rq1_all_hashtags <- unlist(str_extract_all(data$text, "#\\w+"))

rq1_flat_hashtags <- str_replace_all(rq1_all_hashtags, "[[:punct:]]$", "")

rq1_hashtag_table <- as.data.frame(table(rq1_flat_hashtags))
colnames(rq1_hashtag_table) <- c("Hashtag", "Frequency")

rq1_hashtag_table <- rq1_hashtag_table[order(-rq1_hashtag_table$Frequency), ]

rq1_hashtag_rows <- nrow(rq1_hashtag_table)
print(rq1_hashtag_rows)


# Extracts the number of tweets with at least one hashtag (N_y)
rq1_tweets_with_hashtags <- grepl("#\\w+", data$text)

rq1_n_tweets_with_hashtag <- sum(rq1_tweets_with_hashtags)


# Extracts the number of tweets with or without hashtags (N_z)
rq1_data_rows <- nrow(data)
print(rq1_data_rows)




