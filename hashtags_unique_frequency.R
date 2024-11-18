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

df <- data %>%
  filter(score >= 0.7)

df$text <- tolower(df$text)

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



