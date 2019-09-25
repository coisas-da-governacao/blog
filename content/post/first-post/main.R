library("dplyr")
source("content/post/first-post/text_clean_up.R")
source("content/post/first-post/pdf_to_text.R")

programas_pdf <- list.files(path = "content/post/first-post/resources/2019/", full.names = TRUE)
programas_raw_df <- pdf_to_text(programas_pdf)
programas_df <- mutate(programas_raw_df, programa = text_clean_up(programa))


# TM
programas_df_renamed <- transmute(programas_df, doc_id = partido, text = programa)

source <- tm::DataframeSource(programas_df_renamed)

# Corpus and some preprocessing

corpus <- tm::VCorpus(source) %>%
  tm::tm_map(tm::removeWords, tm::stopwords("portuguese")) %>% 
  tm::tm_map(tm::removeWords, c("através", "ser", "programa", "deve", "forma", "cada", "ção"))

programas_tdm <- tm::TermDocumentMatrix(corpus)

#save(programas_df_renamed, file = "content/post/first-post/programas_df.RData")
#save(programas_tdm, file = "content/post/first-post/programas_tdm.RData")

save(programas_df_renamed, file = "content/post/second-post/programas_df_updated.RData")
save(programas_tdm, file = "content/post/second-post/programas_tdm_updated.RData")

