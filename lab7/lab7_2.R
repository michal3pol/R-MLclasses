library("tidytext")
library("igraph")
library("ggraph")
library(dplyr)
library(tidyr)

fileName <- "ai.txt"
text <- readChar(fileName, file.info(fileName)$size)
text_df <- data_frame(line = 1, text = text)
text_df
tidy_text <- text_df %>%
  unnest_tokens(word, text)
data(stop_words)
de <- data.frame("thy", "OLD_WORDS")
names(de) <- c("word", "lexicon")
stop_words <- rbind(stop_words, de)
de <- data.frame("1", "OLD_WORDS")
names(de) <- c("word", "lexicon")
de <- data.frame("hath", "OLD_WORDS")
names(de) <- c("word", "lexicon")
de <- data.frame("mar'd", "OLD_WORDS")
names(de) <- c("word", "lexicon")
stop_words <- rbind(stop_words,de)
tidy_text <- tidy_text %>%
  anti_join(stop_words)
tidy_text %>%
  count(word, sort = TRUE)

# bigramy
text_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
text_bigrams
text_bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united