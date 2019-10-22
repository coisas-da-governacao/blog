stopword_list <- readr::read_delim("content/post/meetup/empty.txt", "\n", col_names = "words")$words
library(ggplot2)
# TM
programas_df_renamed <- transmute(manifests_texts, doc_id = party, text = text)

source <- tm::DataframeSource(programas_df_renamed)

# Corpus and some preprocessing
corpus <- tm::VCorpus(source) %>%
  tm::tm_map(tm::removePunctuation) %>% 
  tm::tm_map(tm::removeWords, stopword_list)

programas_tdm <- tm::TermDocumentMatrix(corpus)

most_frequent_words <- tm::findMostFreqTerms(programas_tdm, n = 10L)

tf_idf <- tm::weightTfIdf(programas_tdm)

party_names <- tibble(id = programas_df_renamed$doc_id,
                      word = programas_df_renamed$doc_id) %>% 
  mutate(word = case_when(
    word == "be" ~ "bloco|esquerda", 
    word == "iniciativa-liberal" ~ "iniciativa|liberal",
    word == "alianca" ~ "aliança",
    word == "pnr" ~ "pnr|partido|nacional|renovador",
    word == "verdes" ~ "verdes|pev|cdu",
    TRUE ~ word))

most_frequent_words_tf_idf <- tm::findMostFreqTerms(tf_idf, n = 13L)

parties <- names(most_frequent_words_tf_idf)

for (i in seq_along(parties)) {
  words <- most_frequent_words_tf_idf[[i]]
  
  words <- words[!grepl(names(words), pattern = filter(party_names, id == parties[i])$word)] %>% head(10)
  
  png(sprintf("%s/content/post/meetup/tfidf/%s.png", getwd(), parties[i]), 
      width = 800, height = 800, unit = "px", bg = "#F5F5F5")

  wordcloud::wordcloud(words = names(words), freq = words,
                       rot.per = 0,
                       random.order = FALSE,
                       colors = rgb(23, 34, 65, maxColorValue = 255),
                       scale = c(10,0.5))
  dev.off()
}

