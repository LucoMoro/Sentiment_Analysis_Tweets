# Carica il pacchetto necessario
library(stringr)
library(xtable)

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

# Extracts the number of all hashtags used in tweets (N_x)
RQ1_extracted_hashtags <- unlist(str_extract_all(data$text, "#\\w+"))

RQ1_flat_hashtags <- str_replace_all(RQ1_extracted_hashtags, "[[:punct:]]$", "")

RQ1_hashtag_table <- as.data.frame(table(RQ1_flat_hashtags))
colnames(RQ1_hashtag_table) <- c("Hashtag", "Frequency")

RQ1_hashtag_table <- RQ1_hashtag_table[order(-RQ1_hashtag_table$Frequency), ]

RQ1_Nx <- sum(RQ1_hashtag_table$Frequency)
print(RQ1_Nx)


# Extracts the number of tweets with at least one hashtag (N_y)
RQ1_tweets_with_hashtags <- grepl("#\\w+", data$text)

RQ1_Ny <- sum(RQ1_tweets_with_hashtags)
print(RQ1_Ny)


# Extracts the number of tweets with or without hashtags (N_z)
RQ1_Nz <- nrow(data)
print(RQ1_Nz)

# Extract the top 50 hashtags
RQ1_50_hashtags <- head(RQ1_hashtag_table, 50)

# Format the number
RQ1_formatted_numbers <- format(RQ1_50_hashtags$Frequency, big.mark = ",", scientific = FALSE)

# Creates the latex table while truncating to the second decimal
RQ1_latex_table <- data.frame(
  Hashtag = RQ1_50_hashtags$Hashtag,
  Frequency = RQ1_formatted_numbers,
  F_1 = format(round((RQ1_50_hashtags$Frequency * 100) / RQ1_Nx, 2), nsmall = 2),
  F_2 = format(round((RQ1_50_hashtags$Frequency * 100) / RQ1_Ny, 2), nsmall = 2),
  F_3 = format(round((RQ1_50_hashtags$Frequency * 100) / RQ1_Nz, 2), nsmall = 2)
)

RQ1_latex_format <- xtable(RQ1_latex_table, caption = "The most used hashtags in Italian tweets in the streaming data collection", label = "tab:top50hashtags")
print(RQ1_latex_format, file = "RQ1_hashtag_table.tex", include.rownames = FALSE)

print(head(RQ1_latex_table))






