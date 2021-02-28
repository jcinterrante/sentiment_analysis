# Really great work!
# 98/100

library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(fmsb)
library(rvest)
library(stringr)
library(RColorBrewer)
library(GISTools)
getStemLanguages()

request_page <- function(url, delay = 0.1) {
  request <- read_html(url)
  article <- html_node(request, "#cmsContent")
  paragraphs <- html_nodes(article, "p")
  text_list <- html_text(paragraphs)
  text <- paste(text_list, collapse = "\n")

  writeLines(text, paste0(
    "bis 20", str_extract(url, "\\d{2}(?=\\d)"), "-",
    str_extract(url, "\\d{2}(?!\\d)"), ".txt"
  ))
  Sys.sleep(delay)
  text
}

summarize_nrc <- function(df) {
  nrc_summary <- df %>%
    group_by(doc_date, nrc) %>%
    summarize(count = n()) %>%
    filter(!is.na(nrc)) %>%
    group_by(doc_date) %>%
    mutate(count = count / sum(count)) %>%
    pivot_wider(names_from = "nrc", values_from = count) %>%
    dplyr::select(positive, joy, anticipation, disgust, anger, negative, sadness, fear, surprise, trust) %>%
    ungroup()


  print("NRC: Sentiment Proportions")
  print(nrc_summary %>%
    pivot_longer(-doc_date, names_to = "sentiment") %>%
    arrange(-value) %>%
    mutate(value = percent(value)) %>%
    pivot_wider(names_from = doc_date))
  nrc_summary
}

summarize_bing <- function(df) {
  bing_summary <- df %>%
    mutate(bing = if_else(bing == "positive", 1, -1)) %>%
    group_by(doc_date, doc_id, lemma, term_id, bing) %>%
    summarize()

  for (i in levels(factor(bing_summary$doc_date))) {
    print(i)

    subset <- bing_summary %>%
      ungroup() %>%
      filter(doc_date == i) %>%
      dplyr::select(bing)

    print(summary(subset))
  }
  bing_summary
}

analyze_sentiments <- function(text, exclude) {
  bis_udp <- udpipe(text$text, "english")

  doc_dates <- text %>%
    mutate(
      doc_date = ymd(
        paste0(
          "20", str_extract(url, "\\d{2}(?=\\d)"), "-",
          str_extract(url, "\\d{2}(?!\\d)"), ", 1"
        )
      ),
      doc_id = paste0("doc", row_number())
    )

  bis_udp_output <- bis_udp %>%
    filter(!upos %in% c("PART", "PUNCT", "CCONJ", "SYM", "NUM", "ADP", "AUX", "DET", "PRON", "X", "SCONJ")) %>%
    mutate_if(is.character, str_to_lower) %>%
    left_join(dplyr::select(doc_dates, doc_id, doc_date), by = "doc_id") %>%
    dplyr::select(doc_id, doc_date, term_id, token, lemma, upos) %>%
    mutate(doc_id = factor(doc_id))

  bis_udp_no_stop_words <- bis_udp_output %>%
    anti_join(stop_words, by = c("lemma" = "word")) %>%
    filter(!lemma %in% exclude)

  for (s in c("nrc", "afinn", "bing")) {
    bis_udp_no_stop_words <- bis_udp_no_stop_words %>%
      left_join(get_sentiments(s), by = c("lemma" = "word")) %>%
      plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
  }
  bis_udp_no_stop_words
}

generate_summary_plot <- function(data) {
  ggplot(data, aes(x = factor(quarter(doc_date, with_year = TRUE)))) +
    labs(
      title = "Overall Positivity of Report (Bing)", subtitle = "Bar Indicates Average Sentiment",
      x = "Document", y = "Postivity (1 = Positive, -1 = Negative)", fill = ""
    ) +
    geom_violin(aes(y = bing, fill = factor(quarter(doc_date, with_year = TRUE)))) +
    stat_summary(aes(y = bing), color = "chocolate2", fun = "mean", geom = "crossbar", size = 2) +
    scale_fill_brewer(palette = "Dark2") +
    theme(legend.position = "none")
  ggsave("question_2_barplot.png")
}

generate_radar_plot <- function(df) {
  nrc_names <- df$doc_date
  nrc_summary <- rbind(rep(.4, 10), rep(0, 10), dplyr::select(df, -doc_date))

  png(file = "question_2_radar_plot.png", width = 800, height = 800)
  radarchart(nrc_summary,
    plwd = 2,
    axislabcol = "gray",
    axistype = 1,
    cglcol = "gray", cglty = 1, cglwd = 0.8,
    caxislabels = paste(seq(0, 40, 10), "%"),
    title = "How Often Was a Sentiment Detected as a Percent of All Sentiments?\n(NRC)",
    vlcex = 1,
    plty = 1,
    pfcol = add.alpha(brewer.pal(8, "Pastel2"), 0.3),
    pcol = brewer.pal(8, "Dark2")
  )

  legend(
    x = 1, y = 1.25, legend = nrc_names, bty = "n", pch = 20,
    col = brewer.pal(8, "Dark2"), cex = 1.2, pt.cex = 3
  )
  dev.off()
}

delay <- 0.1
url_df <- tibble(url = c(
  "https://www.bis.org/publ/qtrpdf/r_qt1912a.htm",
  "https://www.bis.org/publ/qtrpdf/r_qt2012a.htm"
))

exclude_words <- c("debt", "outstanding", "asset")

bis_text <- url_df %>%
  mutate(text = map_chr(url, request_page, delay = delay))

sentiments <- analyze_sentiments(bis_text, exclude_words)
write_csv(sentiments, "sentiments.csv")
view(sentiments)
# Note: unlike in the dec 2020 document, the dec 2019 document contains 
# nots that do negate, ie Nots that are used as participles. I recognize
# this As a problem, but wasn't able to implement a solution in time!
nrc_summary <- summarize_nrc(sentiments)

bing_summary <- summarize_bing(sentiments)

generate_summary_plot(bing_summary)

generate_radar_plot(nrc_summary)
