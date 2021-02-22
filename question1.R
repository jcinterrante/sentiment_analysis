library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(fmsb)
library(scales)
getStemLanguages()

data <- read_file("december_2020_bis.txt")

bis_udp <- udpipe(data, "english")
bis_udp$stem <- wordStem(bis_udp$token, language = "porter")

# I considered negating "not" but I realized that in the cases in which
# it was used, the phrases usually didn't negate the sentiment. eg:
# "this not only puts a floor under short-term rates but also 
# limits the potential decline." The "not" shouldn't negate
# the sentiment of the phrase.

not_list <- bis_udp %>%
  cbind_dependencies(type = "parent_rowid", recursive = TRUE)%>%
  filter(str_to_lower(token) == "not")

bis_udp_output <- dplyr::select(bis_udp, "doc_id", "term_id", "token", "stem", "lemma", "upos") %>%
  filter(!upos %in% c("PART", "PUNCT", "CCONJ", "SYM", "NUM", "ADP", "AUX", "DET", "PRON", "X", "SCONJ")) %>%
  mutate_if(is.character, str_to_lower)

# Remove stop words, and also remove the words "debt," "asset", and "outstanding." 
# I found that these terms were highly influential on the sentiment scores, even though
# they were being used in a purely technical, value-neutral sense.

bis_udp_word_filters <- bis_udp_output %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  #count(lemma, sort = TRUE) %>%
  filter(!lemma %in% c("debt", "outstanding", "asset"))

for (s in c("nrc", "afinn", "bing")) {
  bis_udp_word_filters <- bis_udp_word_filters %>%
    left_join(get_sentiments(s), by = c("lemma" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

bis_udp_word_filters <- bis_udp_word_filters %>%
  mutate(bing = if_else(bing == "positive", 1, -1))

bing_summary <- bis_udp_word_filters %>%
  group_by(lemma, term_id, bing) %>%
  summarize()

afinn_summary <- bis_udp_word_filters %>%
  group_by(lemma, term_id, afinn) %>%
  summarize()

nrc_summary <- bis_udp_word_filters %>%
  group_by(nrc) %>%
  summarize(count = n()) %>%
  filter(!is.na(nrc)) %>%
  mutate(count = count / sum(count)) %>%
  pivot_wider(names_from = "nrc", values_from = count) %>%
  dplyr::select(positive, joy, anticipation, disgust, anger, negative, sadness, fear, surprise, trust) %>%
  ungroup()

# Summary Statistics
print("NRC: Sentiment Proportions")
nrc_summary%>%pivot_longer(everything(),names_to = "sentiment")%>%
  mutate(value = value/sum(value)) %>%
  arrange(-value)%>%
  mutate(value = percent(value))

summary(dplyr::select(afinn_summary, afinn))
summary(dplyr::select(bing_summary, bing))

#graphs
bing_graph_data <- bing_summary %>%
  group_by(lemma)%>%
  summarize(bing_score = sum(bing, na.rm = TRUE))%>%
  filter(abs(bing_score) >= 2)

ggplot(bing_graph_data) +
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
ggsave("question_1_barplot.png")

#removed a similar graph for AFINN
#ggplot(filter(afinn_summary, abs(afinn_score) > 1)) +
 # geom_bar(aes(
  #  y = reorder(lemma, afinn_score), x = afinn_score,
   # fill = factor(afinn_score > 0)
#  ),
 # stat = "identity"
  #) +
  #labs(
   # title = "Overall Contribution to Sentiment Score by Word (Afinn)",
    #x = "Afinn Score", y = ""
#  ) +
 # theme(legend.position = "none")

# https://www.r-graph-gallery.com/142-basic-radar-chart.html
data <- rbind(rep(250, 10), rep(0, 10), nrc_summary)

png(file="question_1_radar_plot.png", width = 800, height = 800)
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
dev.off()
