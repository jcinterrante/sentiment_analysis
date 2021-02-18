library(tidyverse)
library(tidytext)

data <- read_file("december_2020_bis.txt")

text_df <- tibble(text = data)
bis_word_tokens <- unnest_tokens(text_df, word_tokens, text, token = "words")
bis_sentence_tokens <- unnest_tokens(text_df, word_tokens, text, token = "sentences")
bis_ngram_tokens <- unnest_tokens(text_df, word_tokens, text, token = "ngrams", n = 3)

count(bis_word_tokens, word_tokens, sort = TRUE)
stop_words

no_stop_words_bis <- anti_join(bis_word_tokens, stop_words, by = c("word_tokens" = "word"))
