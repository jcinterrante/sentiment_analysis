library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)

data <- read_file("december_2020_bis.txt")
text_df <- tibble(text = data)

bis_word_tokens <- unnest_tokens(text_df, word_tokens, text, token = "words")
bis_sentence_tokens <- unnest_tokens(text_df, word_tokens, text, token = "sentences")
bis_ngram_tokens <- unnest_tokens(text_df, word_tokens, text, token = "ngrams", n = 3)

count(bis_word_tokens, word_tokens, sort = TRUE)
stop_words

sentiment_nrc <- get_sentiments("nrc")
sentiment_afinn <- get_sentiments("afinn")
sentiment_bing <- get_sentiments("bing")

for(s in c("nrc", "afinn", "bing")){
  no_stop_words_bis <- no_stop_words_bis %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
           plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

ggplot(dat = filter(no_stop_words_bis, !is.na(nrc)))+
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "BIS Sentiment (NRC)")


getStemLanguages()

bis_udp <- udpipe(data, "english")
bis_udp$stem <- wordStem(bis_udp$token, language = "porter")

bis_udp_output <- select(bis_udp, "token", "stem", "lemma", "upos")
