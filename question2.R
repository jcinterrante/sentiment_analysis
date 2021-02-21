library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(fmsb)
library(rvest)
library(stringr)
library(lubridate)
getStemLanguages()

request_page <- function(url, delay = 0.1){
    print(url)
    request <- read_html(url)
    article <- html_node(request, "#cmsContent")
    paragraphs<-html_nodes(article, "p")
    text_list <- html_text(paragraphs)
    text <- paste(text_list, collapse = "")
    
    writeLines(text, paste0("bis 20", str_extract(url, "\\d{2}(?=\\d)"), "-",
                            str_extract(url, "\\d{2}(?!\\d)"),".txt"))
    Sys.sleep(delay)
    text
    
}

generate_radar_plots <- function(df){
    
    summary <- df %>%
        group_by(doc_id, nrc)%>%
        summarize(count = sum(n, na.rm = TRUE))%>%
        filter(!is.na(nrc))%>%
        pivot_wider(names_from = "nrc", values_from = count)
    
    #graphs <- summary %>% 
     #   rowid_to_column() %>%
      #  mutate(table = list(rbind(rep(250,10), rep(0,10), summary[rowid, 2:11])),
       #        graph = radarchart(data.frame(table)))
    
    for(i in summary.nrow()){
        graph <- summary %>% 
            mutate(table = list(rbind(rep(250,10), 
                                      rep(0,10), 
                                      summary[i, 2:11])))
        radarchart(data.frame(graph))
    }
}

delay = 0.1
url_df <- tibble(url = c("https://www.bis.org/publ/qtrpdf/r_qt1912a.htm",
              "https://www.bis.org/publ/qtrpdf/r_qt2012a.htm"))

bis_text <- url_df %>%
    mutate(text = map_chr(url, request_page, delay = delay))

bis_word_tokens <- unnest_tokens(bis_text, word_tokens, text, token = "words") %>%
    mutate(report_date = ymd(paste0("20", str_extract(url, "\\d{2}(?=\\d)"), ".",
                                   str_extract(url, "\\d{2}(?!\\d)"),".1")))%>%
    select(report_date, word_tokens)

# analyze sentiments
bis_udp <- udpipe(bis_text$text, "english")

bis_udp$stem <- wordStem(bis_udp$token, language = "porter")

bis_udp_output <- select(bis_udp, "doc_id", "token", "stem", "lemma", "upos") %>%
    filter(!upos %in% c("PART", "PUNCT", "CCONJ", "SYM", "NUM", "ADP", "AUX", "DET", "PRON", "X", "SCONJ"))

bis_udp_no_stop_words <- bis_udp_output %>% 
    anti_join(stop_words,by = c("token" = "word"), copy = TRUE) %>% 
    group_by(doc_id)%>%
    count(lemma, sort = TRUE)

for(s in c("nrc", "afinn", "bing")){
    bis_udp_no_stop_words <- bis_udp_no_stop_words%>%
        left_join(get_sentiments(s), by = c("lemma" = "word")) %>%
        plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

generate_summary_plots

generate_radar_plots(bis_udp_no_stop_words)



ggplot(data = filter(bis_udp_no_stop_words, !is.na(bing)))+
    geom_histogram(aes(bing), stat = "count") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title = "BIS Sentiment (Bing)")

ggplot(data = filter(bis_udp_no_stop_words, !is.na(nrc)))+
    geom_histogram(aes(nrc), stat = "count") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title = "BIS Sentiment (NRC)")



    summary <- bis_udp_no_stop_words %>%
        group_by(doc_id, nrc)%>%
        summarize(count = sum(n, na.rm = TRUE))%>%
        filter(!is.na(nrc))%>%
        pivot_wider(names_from = "nrc", values_from = count)
    
    
    graphs <- summary %>% 
        mutate(
            table = list(rbind(rep(250,10) , rep(0,10), .[1])),
               graph = radarchart(table))
    
    
    graphs <- summary %>% 
        rowid_to_column()%>%
        mutate(table = list(rbind(rep(250,10), rep(0,10), summary[rowid, 2:11])),
               graph = radarchart(data.frame(table)))
graphs$table
    