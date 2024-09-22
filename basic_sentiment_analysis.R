# Load necessary libraries
library(tidytext)
library(dplyr)
library(tidyr)
library(readr)

# 1. Read in the original test_reviews.csv file
test_data <- read_csv("test_reviews.csv")

# 2. Clean the data: remove rows without reviews and set NA for missing ratings
clean_reviews <- test_data %>%
  filter(!is.na(review)) %>%  # Remove rows with missing reviews
  mutate(rating = ifelse(rating == "", NA, rating),  # Set NA for missing ratings
         id = row_number())  # Add id to track each review

# 3. Tokenize, perform sentiment analysis, and calculate sentiment scores
sentiment_data <- clean_reviews %>%
  unnest_tokens(word, review) %>%  # Tokenize the reviews into individual words
  inner_join(get_sentiments("bing"), by = "word") %>%  # Join with Bing lexicon for sentiment
  count(id, sentiment) %>%  # Count positive and negative words per review
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%  # Spread counts
  mutate(sentiment_score = positive - negative) %>%  # Calculate sentiment score
  right_join(clean_reviews, by = "id") %>%  # Merge with cleaned reviews
  mutate(
    sentiment_rating = case_when(
      sentiment_score > 0 ~ "positive",
      sentiment_score < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  ) %>%
  select(id, date, rating, review, sentiment_rating)  # Select relevant columns

# View the final sentiment data
print(sentiment_data)