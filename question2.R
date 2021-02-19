library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(fmsb)
library(rvest)
library(stringr)


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

#analyze_sentiments

delay = 0.1
url_list <- tibble(url = c("https://www.bis.org/publ/qtrpdf/r_qt1912a.htm",
              "https://www.bis.org/publ/qtrpdf/r_qt2012a.htm"))

url_list <- url_list %>%
    mutate(text = map_chr(url, request_page, delay = 0.5))


