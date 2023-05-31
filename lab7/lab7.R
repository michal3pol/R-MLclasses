library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

text <- readLines("ai.txt")
TextDoc <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, ":")
TextDoc <- tm_map(TextDoc, toSpace, ";")
TextDoc <- tm_map(TextDoc, toSpace, ",")
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
TextDoc <- tm_map(TextDoc, removeNumbers)
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
TextDoc <- tm_map(TextDoc, removePunctuation)
TextDoc <- tm_map(TextDoc, stripWhitespace)
TextDoc <- tm_map(TextDoc, stemDocument)
TextDoc <- tm_map(TextDoc, content_transformer(
  function(x) gsub(x, pattern = "mathemat", replacement = "math")))
TextDoc <- tm_map(TextDoc, content_transformer(
  function(x) gsub(x, pattern = " r ", replacement = " Rlanguage ")))
# macierz dokumentu
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# 5 najczestszych slow
head(dtm_d, 5)
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col ="lightgreen",
        main ="20 najczestszych slow w artykule",
        ylab = "Czestotliwosc slow")
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, scale=c(5,0.5),
          min.freq = 1,
          max.words=100, random.order=FALSE,
          rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 30),
           corlimit = 0.5)

# Anal. sentymentu
syuzhet_vector <- get_sentiment(text, method="syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)
# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
# affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)
# emocje
d<-get_nrc_sentiment(as.vector(dtm_d$word))
head (d,10)
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[1:56]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment,
          ylab="count")+ggtitle("Survey sentiments")
barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage"
)


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

library(tidyr)
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