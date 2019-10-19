library("dplyr")
library("lsa")
source("content/post/first-post/pdf_to_text.R")

# Data 
programas_pdf <- list.files(path = "content/post/first-post/resources/2019/", full.names = TRUE)

pdf_to_text <- function(file_paths) {
  purrr::map2_df(
    .x = file_paths,
    .y = c(3, 6, 5, 4, 1, 3, 7, 10, 1, 1, 3, 6, 1), 
    .f = function(x, y) { 
      pdf_as_text <- pdftools::pdf_text(x)
      pages <- length(pdf_as_text)
      df = tibble(
        partido = gsub(".pdf", "", basename(x)),
        programa = stringr::str_flatten(pdf_as_text[y:pages]))
      return(df)}
  )
}

programas_raw_df <- pdf_to_text(programas_pdf)

text_clean_up <- function(raw_text) {
  
  raw_text %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(pattern = "-\\s+\\\n", "") %>%
    stringr::str_replace_all(pattern = "-\\\n", "") %>%
    stringr::str_replace_all(pattern = "\\\n|\\\b", " ") %>%
    stringr::str_replace_all(pattern = ";", ".") %>%
    stringr::str_replace_all(pattern = "[^[:alnum:][:space:](.|?|!)]", " ") %>%
    stringr::str_replace_all(pattern = "[[:digit:]]+", "") %>%
    #stringr::str_replace_all(pattern = "\\w{0,3}", " ") %>%
    #gsub(pattern = '\\b\\w{1,3}\\b', replacement = ' ') %>%
    stringr::str_squish() #%>%
  #    textstem::lemmatize_strings(dictionary = portuguese_lemmas)
}

programas_df <- mutate(programas_raw_df, programa = text_clean_up(programa))

verdes_txt <- programas_df %>% filter(partido == "verdes") %>% pull(programa)
ps_txt <- programas_df %>% filter(partido == "ps") %>% pull(programa)
be_txt <- programas_df %>% filter(partido == "be") %>% pull(programa)
cds_txt <- programas_df %>% filter(partido == "cds") %>% pull(programa)
pan_txt <- programas_df %>% filter(partido == "pan") %>% pull(programa)
psd_txt <- programas_df %>% filter(partido == "psd") %>% pull(programa)
livre_txt <- programas_df %>% filter(partido == "livre") %>% pull(programa)

genericSummary2(verdes_txt, k=1, min = 5, split = c(".", "?"))

sum_verdes <- lexRankr::lexRank(verdes_txt, removeNum = FALSE, stemWords = FALSE, rmStopWords = FALSE, sentencesAsDocs = TRUE)
sum_be <- lexRankr::lexRank(be_txt, removeNum = FALSE, stemWords = FALSE, rmStopWords = FALSE, sentencesAsDocs = TRUE)
sum_cds <- lexRankr::lexRank(cds_txt, removeNum = FALSE, stemWords = FALSE, rmStopWords = FALSE, usePageRank = FALSE)
sum_cds2 <- lexRankr::lexRank(cds_txt, removeNum = FALSE, stemWords = FALSE, rmStopWords = FALSE)


sum_pan <- lexRankr::lexRank(pan_txt, removeNum = FALSE, stemWords = FALSE, rmStopWords = FALSE, sentencesAsDocs = TRUE)

