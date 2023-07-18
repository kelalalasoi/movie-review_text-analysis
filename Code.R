##STEP 1: TOKENISED ALL TEXT DATA (Split the text into individual words or tokens.)

#1.1 Install the tokenizers package if not already installed
install.packages("tokenizers")

#1.2 Load the tokenizers package
library(tokenizers)

#1.3 Set the chunk size
chunk_size <- 1000  # Adjust as needed

#1.4 Initialize an empty list to store tokenized data
tokenized_imdb <- list()

#1.5 Tokenize the data in chunks
for (i in seq(1, nrow(imdb), chunk_size)) {
  chunk <- imdb[i:min(i+chunk_size-1, nrow(imdb)), "review"]
  tokens <- lapply(chunk, tokenize_words)
  tokenized_imdb <- c(tokenized_imdb, tokens)
}

#1.6 Create a new column in the dataset to store the tokens
imdb$tokens <- tokenized_data


#1.7 Print the tokenized dataset
print(imdb)


#-----------------

#STEP 2; PREPROCESSING

# Load the required packages
library(tidytext)
library(dplyr)
library(stringr)

#convert tokens into a character vector
imdb$tokens <- as.character(imdb$tokens)

# Create a tidy text dataframe
tidy_imdb <- imdb %>% unnest_tokens(word, tokens)

# Convert to lowercase
tidy_imdb <- tidy_imdb %>%
  mutate(review = str_to_lower(word))

# Remove punctuation
tidy_imdb <- tidy_imdb %>%
  mutate(word = str_remove_all(word, "[[:punct:]]"))

# Remove numbers
tidy_imdb <- tidy_imdb %>%
  mutate(word = str_remove_all(word, "\\d+"))

# Remove stopwords
tidy_imdb <- tidy_imdb %>%
  anti_join(stop_words)

# Stemming or Lemmatization
# You can use additional packages like 'textstem' for stemming or 'textclean' for lemmatization

# Create a document-term matrix
dtm <- tidy_imdb %>%
  count(review, word)

# Inspect the document-term matrix
dtm

#-----------------

#STEP 3: Sentiment Lexicon: Obtain a sentiment lexicon or dictionary that contains words annotated with their sentiment polarity (positive, negative, or neutral). There are existing sentiment lexicons available, such as the AFINN-111 lexicon or the Vader lexicon.

sentiment_lexicon <- tibble(
  word = c("good", "great", "excellent", "bad", "terrible", "awful"),
  sentiment = c("positive", "positive", "positive", "negative", "negative", "negative")
)

# Join sentiment lexicon
tidy_imdb <- tidy_imdb %>%
  left_join(sentiment_lexicon, by = "word")

#-----------------

#STEP 4 : SENTIMENT SCORING
# Sentiment Scoring
sentiment_scores <- tidy_imdb %>%
  group_by(review) %>%
  summarize(sentiment_score = sum(ifelse(sentiment.x == "positive", 1, -1)), .groups = "drop")

# Merge sentiment scores with the original dataset
imdb_with_scores <- imdb %>%
  left_join(sentiment_scores, by = "review")

#STEP 5: AGGREGATE SCORE
#aggregate the sentiment scores for each review
aggregated_scores <- sentiment_scores %>%
  group_by(review) %>%
  summarise(sentiment_score = sum(sentiment_score), .groups = "drop")

# STEP 6: DETERMINE SENTIMENT
aggregated_scores <- aggregated_scores %>%
  mutate(sentiment = ifelse(sentiment_score >= 0, "positive", "negative"))

# STEP 7: HIERARCHICAL CLUSTERING

# Prepare the data for hierarchical clustering
sentiment_data <- aggregated_scores$sentiment_score


# Perform hierarchical clustering
hc <- hclust(dist(sentiment_data))

# Determine the number of clusters (optional)
# You can use different methods like "knee" or "elbow" method to determine the optimal number of clusters
# Here, let's assume we want 3 clusters
num_clusters <- 3

# Cut the dendrogram to obtain cluster assignments
cluster_assignments <- cutree(hc, k = num_clusters)

# Add the cluster assignments to the aggregated_scores dataframe
aggregated_scores$cluster <- cluster_assignments

# Print the results
print(aggregated_scores)