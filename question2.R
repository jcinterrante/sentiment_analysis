library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(fmsb)
library(rvest)
library(stringr)


request_page <- function(url){
    request <- read_html(url)
    article <- html_node(request, "#cmsContent")
    paragraphs<-html_nodes(article, "p")
    text_list <- html_text(paragraphs)
    text <- paste(text_list, collapse = "")
    
    writeLines(text, paste0("bis 20", str_extract(url, "\\d{2}(?=\\d)"), "-",
                            str_extract(url, "\\d{2}(?!\\d)"),".txt"))
    text
}





url_list <- c("https://www.bis.org/publ/qtrpdf/r_qt1912a.htm",
              "https://www.bis.org/publ/qtrpdf/r_qt2012a.htm")

bis_texts <- list()

for(i in seq_along(url_list)){
    bis_texts[i] <- request_page(url_list[i])
}

