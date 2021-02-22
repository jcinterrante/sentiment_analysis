library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(fmsb)
library(rvest)
library(stringr)
library(lubridate)
getStemLanguages()

request_page <- function(url, delay = 0.1) {
  request <- read_html(url)
  article <- html_node(request, "#cmsContent")
  paragraphs <- html_nodes(article, "p")
  text_list <- html_text(paragraphs)
  text <- paste(text_list, collapse = " ")

  writeLines(text, paste0(
    "bis 20", str_extract(url, "\\d{2}(?=\\d)"), "-",
    str_extract(url, "\\d{2}(?!\\d)"), ".txt"
  ))
  Sys.sleep(delay)
  text
}

generate_summary_stats<- function(df){
    bing_summary <- df %>%
        mutate(bing = if_else(bing == "positive", 1, -1))%>%
        group_by(doc_id, lemma, n, bing) %>%
        summarize() %>%
        mutate(bing_score = n * bing)
    
    for(i in levels(bing_summary$doc_id)){
        print(i)
        
        subset <- bing_summary %>%
            ungroup()%>%
            filter(doc_id == i)%>%
            select(bing_score)
        
        print(summary(subset))
    }
    
    nrc_summary <- df %>%
        group_by(doc_id, nrc) %>%
        summarize(count = sum(n, na.rm = TRUE)) %>%
        filter(!is.na(nrc)) %>%
        pivot_wider(names_from = "nrc", values_from = count) %>%
        select(positive, joy, anticipation, disgust, anger, negative, sadness, fear, surprise, trust)
    
    print("NRC: Sentiment Proportions")
    print(nrc_summary %>%
              pivot_longer(-doc_id,names_to = "sentiment")%>%
              mutate(value = value/sum(value)) %>%
              arrange(-value)%>%
              mutate(value = percent(value))%>%
              pivot_wider(names_from=doc_id))
}

analyze_sentiments<- function(text, exclude){
    # analyze sentiments
    bis_udp <- udpipe(text$text, "english")
    
    bis_udp_output <- select(bis_udp, "doc_id", "term_id", "token", "lemma", "upos") %>%
        filter(!upos %in% c("PART", "PUNCT", "CCONJ", "SYM", "NUM", "ADP", "AUX", "DET", "PRON", "X", "SCONJ")) %>%
        mutate_if(is.character, str_to_lower)%>%
        mutate(doc_id = factor(doc_id))
    
    bis_udp_no_stop_words <- bis_udp_output %>%
        anti_join(stop_words, by = c("lemma" = "word")) %>%
        count(doc_id, lemma, sort = TRUE) %>%
        filter(!lemma %in% exclude)
    
    for (s in c("nrc", "afinn", "bing")) {
        bis_udp_no_stop_words <- bis_udp_no_stop_words %>%
            left_join(get_sentiments(s), by = c("lemma" = "word")) %>%
            plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
    }
    bis_udp_no_stop_words
}

generate_summary_plot(data){
ggplot(filter(bing_summary, abs(bing_score) > 1)) +
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
ggsave("question_2_barplot.png")

}

generate_radar_plot <- function(df) {
  summary <- df %>%
    group_by(doc_id, nrc) %>%
    summarize(count = sum(n, na.rm = TRUE)) %>%
    filter(!is.na(nrc)) %>%
    pivot_wider(names_from = "nrc", values_from = count) %>%
    ungroup() %>%
    select(-doc_id)

  for (i in nrow(summary)) {
    graph <- list(rbind(
      rep(250, 10),
      rep(0, 10),
      summary[i, ]
    ))
    # A vectorized approach that could be used instead of the for loop
    # graphs <- summary %>%
    #   rowid_to_column() %>%
    #  mutate(table = list(rbind(rep(250,10), rep(0,10), summary[rowid, 2:11])),
    #        graph = radarchart(data.frame(table)))
    radarchart(data.frame(graph))
  }
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

generate_summary_stats(sentiments)

generate_summary_plot(sentiments)

generate_radar_plot(sentiments)