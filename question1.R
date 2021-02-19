library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(fmsb)


data <- read_file("december_2020_bis.txt")
text_df <- tibble(text = data)

bis_word_tokens <- unnest_tokens(text_df, word_tokens, text, token = "words")
bis_sentence_tokens <- unnest_tokens(text_df, word_tokens, text, token = "sentences")
bis_ngram_tokens <- unnest_tokens(text_df, word_tokens, text, token = "ngrams", n = 3)

count(bis_word_tokens, word_tokens, sort = TRUE)
stop_words

bis_no_stop_words <- bis_word_tokens %>% 
  anti_join(stop_words, by = c("word_tokens" = "word")) %>%
  count(word_tokens, sort = TRUE)


sentiment_nrc <- get_sentiments("nrc")
sentiment_afinn <- get_sentiments("afinn")
sentiment_bing <- get_sentiments("bing")

for(s in c("nrc", "afinn", "bing")){
  bis_no_stop_words <- bis_no_stop_words %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
           plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

ggplot(data = filter(no_stop_words_bis, !is.na(nrc)))+
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "BIS Sentiment (NRC)")


getStemLanguages()

bis_udp <- udpipe(data, "english")
bis_udp$stem <- wordStem(bis_udp$token, language = "porter")

bis_udp_output <- select(bis_udp, "token", "stem", "lemma", "upos")%>%
  filter(!upos %in% c("PART", "PUNCT", "CCONJ", "SYM", "NUM", "ADP", "AUX", "DET", "PRON", "X", "SCONJ"))

bis_udp_no_stop_words <- bis_udp_output %>% 
  anti_join(stop_words, by = c("token" = "word")) %>%
  count(lemma, sort = TRUE)

for(s in c("nrc", "afinn", "bing")){
  bis_udp_no_stop_words <- bis_udp_no_stop_words%>%
    left_join(get_sentiments(s), by = c("lemma" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

ggplot(data = filter(bis_udp_no_stop_words, !is.na(bing)))+
  geom_histogram(aes(bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "BIS Sentiment (Bing)")

ggplot(data = filter(bis_udp_no_stop_words, !is.na(nrc)))+
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "BIS Sentiment (NRC)")


summary <- bis_udp_no_stop_words %>%
  group_by(nrc)%>%
  summarize(count = sum(n, na.rm = TRUE))%>%
  filter(!is.na(nrc))%>%
  pivot_wider(names_from = "nrc", values_from = count)
# https://www.r-graph-gallery.com/142-basic-radar-chart.html
data <- rbind(rep(250,10) , rep(0,10) , summary)
radarchart(data)
