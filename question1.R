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

for (s in c("nrc", "afinn", "bing")) {
  bis_no_stop_words <- bis_no_stop_words %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

getStemLanguages()

bis_udp <- udpipe(data, "english")
bis_udp$stem <- wordStem(bis_udp$token, language = "porter")

not_list <- bis_udp %>%
  filter(str_to_lower(lemma) == "not")

bis_udp_output <- select(bis_udp, "doc_id", "term_id", "token", "stem", "lemma", "upos") %>%
  filter(!upos %in% c("PART", "PUNCT", "CCONJ", "SYM", "NUM", "ADP", "AUX", "DET", "PRON", "X", "SCONJ")) %>%
  mutate_if(is.character, str_to_lower)

# Remove stop words, and also remove the words "debt" and "outstanding." I found
# that these terms were highly influential on the sentiment scores, even though
# they were being used in a purely technical, value-neutral sense. "Debt" in the
# sense of "sovereign debt" can be bad if it is unsustainable, but it can also
# be good, as in the case of stimulus spending. Similarly, "outstanding" arises
# in the context of "outstanding obligations," rather than as a synonym for
# "good."

bis_udp_no_stop_words <- bis_udp_output %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  count(lemma, sort = TRUE) %>%
  filter(!lemma %in% c("debt", "outstanding"))

for (s in c("nrc", "afinn", "bing")) {
  bis_udp_no_stop_words <- bis_udp_no_stop_words %>%
    left_join(get_sentiments(s), by = c("lemma" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

bis_udp_no_stop_words <- bis_udp_no_stop_words %>%
  mutate(bing = if_else(bing == "positive", 1, -1))

summary(bis_udp_no_stop_words)

bing_graph_data <- bis_udp_no_stop_words %>%
  group_by(lemma, n, bing) %>%
  summarize() %>%
  mutate(bing_score = n * bing)

ggplot(filter(bing_graph_data, abs(bing_score) > 1)) +
  geom_bar(aes(
    y = reorder(lemma, bing_score), x = bing_score,
    fill = factor(bing_score > 0)
  ),
  stat = "identity"
  ) +
  labs(
    title = "Overall Contribution to Sentiment Score by Word (Bing)",
    x = "Bing Score", y = ""
  ) +
  theme(legend.position = "none")

summary <- bis_udp_no_stop_words %>%
  group_by(nrc) %>%
  summarize(count = sum(n, na.rm = TRUE)) %>%
  filter(!is.na(nrc)) %>%
  pivot_wider(names_from = "nrc", values_from = count) %>%
  select(positive, joy, anticipation, disgust, anger, negative, sadness, fear, surprise, trust)

# https://www.r-graph-gallery.com/142-basic-radar-chart.html
data <- rbind(rep(250, 10), rep(0, 10), summary)
radarchart(data,
  pfcol = rgb(0.2, 0.5, 0.5, 0.4),
  pcol = rgb(0.2, 0.5, 0.5, 0.9),
  plwd = 2,
  axislabcol = "gray",
  axistype = 1,
  cglcol = "grey", cglty = 1, cglwd = 0.8, caxislabels = seq(0, 250, 62.5),
  title = "Sentiments in the December 2020 BIS (NRC)",
  vlcex = 1
)