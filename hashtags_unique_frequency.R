install.packages("xtable")
install.packages("corrplot")
install.packages("moments")
install.packages("nortest")

# Carica il pacchetto necessario
library(stringr)
library(xtable)
library(corrplot)
library(dplyr)
library(moments)
library(nortest)


#############################################################################################
################################### Dataset import ##########################################
#############################################################################################
# Leggi il dataset
initial_data <- read.csv("C:\\Users\\morel\\Desktop\\sad_tweets\\Sentiment_Analysis_Tweets\\Sentiment_it_tweet_2023.csv", sep = ";", header = TRUE)


#############################################################################################
############################ Dataset filtering & cleaning ###################################
#############################################################################################

initial_data$text <- tolower(initial_data$text)

# Adapts the date format to Y-M-D excluding
initial_data$extractedts <- as.Date(initial_data$extractedts)

data <- initial_data %>%
  select(-userid, -acctdesc, -location, -following, -followers, -totaltweets, -usercreatedts,
         -tweetid, -tweetcreatedts, -retweetcount,  -hashtags, -language, -favorite_count,
         -original_tweet_id, -original_tweet_userid, -original_tweet_username,
         -in_reply_to_status_id, -in_reply_to_user_id, -in_reply_to_screen_name,
         -quoted_status_id, -quoted_status_userid, -quoted_status_username,
         -is_quote_status, -username)


min_score <- min(data$score)
max_score <- max(data$score)

print(min_score)
print(max_score)


#filter(score >= 0.0)

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
head(RQ1_50_hashtags)

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


####################################### Discrete Empiric Distributiuon function ###################################

################# complete dataset ###############################
RQ1_desc_hashtag_table <- RQ1_hashtag_table %>% arrange(desc(Frequency))
print(head(RQ1_desc_hashtag_table))

# Ensure frequencies are in ascending order
sorted_frequencies <- sort(RQ1_desc_hashtag_table$Frequency)

# Compute the cumulative distribution
step_func <- stepfun(
  sorted_frequencies,
  c(0, cumsum(sorted_frequencies) / sum(sorted_frequencies))
)

# Plot the step function
plot(step_func, main="Stepwise cumulative distribution of hashtags", xlab="Frequency", ylab="Cumulative probability")

# Extract the frequency column
freqs <- RQ1_hashtag_table$Frequency

# Compute skewness
skewness_value <- skewness(freqs)

# Compute kurtosis
kurtosis_value <- kurtosis(freqs)

# Print results
print(paste("Skewness:", skewness_value))
print(paste("Kurtosis:", kurtosis_value))

#outliers
boxplot(
  RQ1_hashtag_table$Frequency,
  horizontal = TRUE,
  main = "Boxplot of Hashtag Frequencies",
  ylab = "Frequency",
  col = "lightblue"
)


################# filtered dataset ###############################
RQ1_50_desc_hashtag_table <- RQ1_50_hashtags %>% arrange(desc(Frequency))
print(head(RQ1_desc_hashtag_table))

# Ensure frequencies are in ascending order
sorted_frequencies <- sort(RQ1_50_desc_hashtag_table$Frequency)

# Compute the cumulative distribution
step_func <- stepfun(
  sorted_frequencies,
  c(0, cumsum(sorted_frequencies) / sum(sorted_frequencies))
)

# Plot the step function
plot(step_func, main="Stepwise cumulative distribution of hashtags", xlab="Frequency", ylab="Cumulative probability")

# Extract the frequency column
freqs <- RQ1_50_hashtags$Frequency

# Compute skewness
skewness_value <- skewness(freqs)

# Compute kurtosis
kurtosis_value <- kurtosis(freqs)

# Print results
print(paste("Skewness:", skewness_value))
print(paste("Kurtosis:", kurtosis_value))


#outliers
boxplot(
  RQ1_50_hashtags$Frequency,
  horizontal = TRUE,
  main = "Boxplot of Hashtag Frequencies",
  ylab = "Frequency",
  col = "lightblue"
)


#############################################################################################
######################################### Response to RQ_2 ##################################
#############################################################################################


extract_hashtags <- function(tweet) {
  hashtags <- unlist(str_extract_all(tweet, "#\\w+"))
  return(hashtags)
}

# Extract the hashtags from column "text"
data$hashtag_list <- lapply(data$text, extract_hashtags)

# Filters hashtags based on RQ1_50_hashtags
data$filtered_hashtag_list <- lapply(data$hashtag_list, function(hashtags) {
  hashtags[hashtags %in% RQ1_50_hashtags$Hashtag]
})

# Creates a new dataset of tweets containing at least one top 50 hashtag
RQ2_data <- data[lengths(data$filtered_hashtag_list) > 0, ]

RQ2_hashtag_vector <- unique(unlist(RQ2_data$filtered_hashtag_list))

# Creates a binary matrix for RQ2_data (1 means that the hasthag is contained in the tweet, 0 otherwise)
RQ2_binary_matrix <- do.call(rbind, lapply(RQ2_data$filtered_hashtag_list, function(hashtags){
  as.numeric(RQ2_hashtag_vector %in% hashtags)
}))
colnames(RQ2_binary_matrix) <- RQ2_hashtag_vector

# Calculates the Perason coefficient
RQ2_correlation_matrix <- cor(RQ2_binary_matrix, method = "pearson")

# Plots the results
corrplot(RQ2_correlation_matrix, method = "color",
         tl.col = "black",
         tl.cex = 0.6,
         tl.srt = 90)

# Puts the diagonal to 0 (otherwise the correlation value is 1)
diag(RQ2_correlation_matrix) <- 0

# Checks wether the correlation is >= .70 and <= -70
RQ2_significant_correlations <- abs(RQ2_correlation_matrix) >= 0.70

# Creates the dataframe
RQ2_significant_pairs <- data.frame(
  hashtag_1 = rownames(RQ2_correlation_matrix)[which(RQ2_significant_correlations, arr.ind = TRUE)[, 1]],
  hashtag_2 = colnames(RQ2_correlation_matrix)[which(RQ2_significant_correlations, arr.ind = TRUE)[, 2]],
  correlation = RQ2_correlation_matrix[which(RQ2_significant_correlations, arr.ind = TRUE)]
)

# Filters out correlations
RQ2_significant_pairs <- RQ2_significant_pairs[
  abs(RQ2_significant_pairs$correlation) >= 0.70,
  ]

# Orders the dataframe
RQ2_significant_pairs_sorted <- RQ2_significant_pairs %>%
  arrange(desc(correlation))

if(nrow(RQ2_significant_pairs_sorted) > 0) {
  # Deletes every even row in order to remove duplicated combinations of hashtags
  RQ2_significant_pairs_reduced <- RQ2_significant_pairs_sorted[seq(1, nrow(RQ2_significant_pairs_sorted), by=1), ]
} else {
  RQ2_significant_pairs_reduced <- RQ2_significant_pairs_sorted
}

RQ2_latex_table <- data.frame(
  Hashtag_1 = RQ2_significant_pairs_reduced$hashtag_1,
  Hashtag_2 = RQ2_significant_pairs_reduced$hashtag_2,
  Correlazione = RQ2_significant_pairs_reduced$correlation
)

RQ2_latex_format <- xtable(RQ2_latex_table, caption = "Le coppie di hashtag con frequenza superiore o uguale a 0.70 ed inferiore o uguale a -0.70", label = "tab:corr_mat")
print(RQ2_latex_format, file = "RQ2_hashtag_table.tex", include.rownames = FALSE)

print(head(RQ2_latex_table))


# Extract correlations for #ucraina
RQ2_ucraina_correlations <- RQ2_correlation_matrix["#ucraina", ]

# Create a data frame with hashtags and correlations
RQ2_ucraina_df <- data.frame(
  hashtag = names(RQ2_ucraina_correlations),
  correlation = RQ2_ucraina_correlations
)

RQ2_ucraina_df <- RQ2_ucraina_df[order(RQ2_ucraina_df$correlation, decreasing = TRUE), ]


RQ2_ucraina_df_latex <- xtable(RQ2_ucraina_df, caption="Correlazione tra #ucraina e i top 50 hashtags", label="tab:ucraina_corr")
print(RQ2_ucraina_df_latex, file = "RQ2_ucraina_df_latex.tex", include.rownames = FALSE)

# View the data frame
print(head(RQ2_ucraina_df_latex))


###################################### Centrality and dispersion measures #########################################

############ complete dataset ##############
data$hashtag_count <- sapply(data$hashtag_list, length)
print(mean(data$hashtag_count))
mean_freq <- mean(data$hashtag_count)
print(median(data$hashtag_count))

mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
print(mode_function(data$hashtag_count))


print(var(data$hashtag_count))
print(sd(data$hashtag_count))
sd_freq <- sd(data$hashtag_count)

cv_freq <- sd_freq / mean_freq
print(cv_freq)

hist(data$hashtag_count,
     breaks = 50,
     main = "Distribution of Hashtag Count",
     xlab = "Number of Hashtags per Tweet",
     col = "lightblue",
     border = "black")

###### normal distribution

ad.test(data$hashtag_count)
qqnorm(data$hashtag_count)
qqline(data$hashtag_count, col = "red")


###### logaritmic

log_data <- log(data$hashtag_count)
log_data <- log_data[!is.na(log_data) & is.finite(log_data)]  # Remove NA and Inf

ad.test(log_data)


###### exponential

data$hashtag_count_jittered <- data$hashtag_count + runif(length(data$hashtag_count), min = -1e-6, max = 1e-6)
ks.test(data$hashtag_count_jittered, "pexp", rate = 1/mean(data$hashtag_count_jittered))





############ filtered dataset ##############
data$filtered_hashtag_count <- sapply(data$filtered_hashtag_list, length)
print(mean(data$filtered_hashtag_count))
mean_freq <- mean(data$filtered_hashtag_count)
print(median(data$filtered_hashtag_count))

mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
print(mode_function(data$filtered_hashtag_count))

print(var(data$filtered_hashtag_count))
print(sd(data$filtered_hashtag_count))
sd_freq <- sd(data$filtered_hashtag_count)

cv_freq <- sd_freq / 2.243576
print(cv_freq)

hist(data$filtered_hashtag_count,
     breaks = 50,
     main = "Distribution of Hashtag Count",
     xlab = "Number of Hashtags per Tweet",
     col = "lightblue",
     border = "black")


###### normal distribution

ad.test(data$filtered_hashtag_count)
qqnorm(data$filtered_hashtag_count)
qqline(data$filtered_hashtag_count, col = "red")


###### logaritmic

log_data <- log(data$filtered_hashtag_count)
log_data <- log_data[!is.na(log_data) & is.finite(log_data)]  # Remove NA and Inf

ad.test(log_data)


###### exponential

data$hashtag_filtered_count_jittered <- data$filtered_hashtag_count + runif(length(data$filtered_hashtag_count), min = -1e-6, max = 1e-6)
ks.test(data$hashtag_filtered_count_jittered, "pexp", rate = 1/mean(data$hashtag_filtered_count_jittered))

#############################################################################################
######################################### Response to RQ_3 ##################################
#############################################################################################

# Filter rows containing "#ucraina" in the filtered_hashtag_list column
RQ3_ucraina_data <- data %>%
  filter(sapply(filtered_hashtag_list, function(x) "#ucraina" %in% x))


# Group by day and sentiment, and count occurrences
RQ3_ucraina_freq <- RQ3_ucraina_data %>%
  group_by(extractedts, sentiment) %>%
  summarise(frequency = n(), .groups = "drop")  # Count rows per group

#Normalize frequency by the maximum frequency within each sentiment group
RQ3_ucraina_normalized <- RQ3_ucraina_freq %>%
  group_by(sentiment) %>%
  mutate(normalized_frequency = frequency / max(frequency)) %>%  # Normalize per sentiment group
  ungroup() %>%  # Remove grouping after mutation
  select(extractedts, sentiment, normalized_frequency)

# Check for NA values in the negative sentiment data
na_rows <- is.na(RQ3_ucraina_freq$frequency) | is.na(RQ3_ucraina_freq$extractedts)
print(RQ3_ucraina_freq[na_rows, ])

# Prepare the plot with the first sentiment (negative) line
plot(RQ3_ucraina_freq$extractedts[RQ3_ucraina_freq$sentiment == "neg"],
     RQ3_ucraina_freq$frequency[RQ3_ucraina_freq$sentiment == "neg"],
     type = "l", col = "blue", xlab = "Date", ylab = "Frequency",
     main = "Sentiment Frequency Over Time for #ucraina")

# Add the second sentiment (neutral) line
lines(RQ3_ucraina_freq$extractedts[RQ3_ucraina_freq$sentiment == "neu"],
      RQ3_ucraina_freq$frequency[RQ3_ucraina_freq$sentiment == "neu"],
      col = "red")

# Add the third sentiment (positive) line
lines(RQ3_ucraina_freq$extractedts[RQ3_ucraina_freq$sentiment == "pos"],
      RQ3_ucraina_freq$frequency[RQ3_ucraina_freq$sentiment == "pos"],
      col = "darkgreen")

# Add a legend
legend("topright", legend = c("Negative", "Neutral", "Positive"),
       col = c("blue", "red", "darkgreen"), lty = 1)


# Prepare the plot with the first sentiment (negative) line
plot(RQ3_ucraina_normalized$extractedts[RQ3_ucraina_normalized$sentiment == "neg"],
     RQ3_ucraina_normalized$normalized_frequency[RQ3_ucraina_normalized$sentiment == "neg"],
     type = "l", col = "blue", xlab = "Date", ylab = "Frequency",
     main = "Normalized Sentiment Frequency Over Time for #ucraina",
     ylim = c(min(RQ3_ucraina_normalized$normalized_frequency), max(RQ3_ucraina_normalized$normalized_frequency)))


# Add the second sentiment (neutral) line
lines(RQ3_ucraina_normalized$extractedts[RQ3_ucraina_normalized$sentiment == "neu"],
      RQ3_ucraina_normalized$normalized_frequency[RQ3_ucraina_normalized$sentiment == "neu"],
      col = "red")

# Add the third sentiment (positive) line
lines(RQ3_ucraina_normalized$extractedts[RQ3_ucraina_normalized$sentiment == "pos"],
      RQ3_ucraina_normalized$normalized_frequency[RQ3_ucraina_normalized$sentiment == "pos"],
      col = "darkgreen")

# Add a legend
legend("topright", legend = c("Negative", "Neutral", "Positive"),
       col = c("blue", "red", "darkgreen"), lty = 1)


# Filter data for positive sentiment
positive_data <- RQ3_ucraina_freq[RQ3_ucraina_freq$sentiment == "pos", ]

# Create a plot for positive sentiment
plot(positive_data$extractedts,
     positive_data$frequency,
     type = "l", col = "blue",
     xlab = "Date", ylab = "Frequency",
     main = "Positive Sentiment Frequency Over Time for #ucraina")


# Filter data for negative sentiment
negative_data <- RQ3_ucraina_freq[RQ3_ucraina_freq$sentiment == "neg", ]

# Create a plot for negative sentiment
plot(negative_data$extractedts,
     negative_data$frequency,
     type = "l", col = "red",
     xlab = "Date", ylab = "Frequency",
     main = "Negative Sentiment Frequency Over Time for #ucraina")



# Filter data for neutral sentiment
neutral_data <- RQ3_ucraina_freq[RQ3_ucraina_freq$sentiment == "neu", ]

# Create a plot for neutral sentiment
plot(neutral_data$extractedts,
     neutral_data$frequency,
     type = "l", col = "darkgreen",
     xlab = "Date", ylab = "Frequency",
     main = "Neutral Sentiment Frequency Over Time for #ucraina")

#############################################################################################
######################################### Response to RQ_4 ##################################
#############################################################################################

bot_tweets <- data %>%
  filter(score >= 0.7)

# Filter rows containing "#ucraina" in the filtered_hashtag_list column
RQ4_zelenskywarcriminal_data <- bot_tweets %>%
  filter(sapply(filtered_hashtag_list, function(x) "#zelenskywarcriminal" %in% x))

# Filter rows containing "#ucraina" in the filtered_hashtag_list column
RQ4_zelenskywarcriminal_retweet_data <- bot_tweets %>%
  filter(is_retweet == "True") %>%
  filter(sapply(filtered_hashtag_list, function(x) "#zelenskywarcriminal" %in% x))

# Group by day and sentiment, and count occurrences
RQ4_zelenskywarcriminal_freq <- RQ4_zelenskywarcriminal_data %>%
  group_by(extractedts, sentiment) %>%
  summarise(frequency = n(), .groups = "drop")

# Group by day and sentiment, and count occurrences
RQ4_zelenskywarcriminal_retweet_freq <- RQ4_zelenskywarcriminal_retweet_data %>%
  group_by(extractedts, sentiment) %>%
  summarise(frequency = n(), .groups = "drop")

# Prepare the plot with the first sentiment (negative) line
plot(RQ4_zelenskywarcriminal_freq$extractedts[RQ4_zelenskywarcriminal_freq$sentiment == "neg"],
     RQ4_zelenskywarcriminal_freq$frequency[RQ4_zelenskywarcriminal_freq$sentiment == "neg"],
     type = "l", col = "blue", xlab = "Date", ylab = "Frequency",
     main = "Negative Frequency Over Time for #zelenskywarcriminal")

# Add the second sentiment (neutral) line
lines(RQ4_zelenskywarcriminal_retweet_freq$extractedts[RQ4_zelenskywarcriminal_retweet_freq$sentiment == "neg"],
      RQ4_zelenskywarcriminal_retweet_freq$frequency[RQ4_zelenskywarcriminal_retweet_freq$sentiment == "neg"],
      col = "red")

# Add a legend
legend("bottomleft", legend = c("Tweets", "Retweets"),
       col = c("blue", "red"), lty = 1)


#############################################################################################
######################################### Response to RQ_5 ##################################
#############################################################################################

gpt_data <- read.csv('C:\\Users\\morel\\Desktop\\sad_tweets\\Sentiment_Analysis_Tweets\\dataset_domanda_risposta.csv', sep = ",", header = TRUE)

# Adapts the date format to Y-M-D excluding
gpt_data$extractedts <- as.Date(gpt_data$extractedts)

data <- gpt_data


gpt_simple_data <- read.csv('C:\\Users\\morel\\Desktop\\sad_tweets\\Sentiment_Analysis_Tweets\\simple_data_conflitto.csv', sep = ",", header = TRUE, quote = "\"")

# Adapts the date format to Y-M-D excluding
gpt_simple_data$extractedts <- as.Date(gpt_simple_data$extractedts)

data <- gpt_simple_data

data$text <- tolower(data$text)





