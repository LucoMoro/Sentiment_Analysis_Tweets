# Carica il pacchetto necessario
library(stringr)

# Leggi il dataset
data <- read.csv('./Sentiment_it_tweet_2023.csv', sep = ";", header = TRUE)

df <- data %>%
  filter(data$score >= 0.7)

# Estrai tutti gli hashtag dalla colonna 'text'
all_hashtags <- unlist(str_extract_all(df$text, "#\\S+"))

# Calcola la frequenza di ogni hashtag
hashtag_frequency <- table(all_hashtags)

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


