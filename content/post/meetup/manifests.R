library("dplyr")

paths <- list.files("content/post/first-post/resources/2019", pattern = ".pdf", full.names = TRUE)

manifests <- function(path) {
  pages_data <- pdftools::pdf_data(path)
  party <- gsub(".pdf", "", basename(path))
  
  pages_data %>% 
    purrr::map_df(.f = ~ {
      .x %>% 
        mutate(text = stringr::str_replace_all(string = text, pattern = "\b|-|\\{|–", replacement = "")) %>% 
        mutate(text = if_else(space, 
                              text,
                              stringr::str_replace_all(string = text, pattern = "\\d+", ".")),
               text = stringr::str_replace_all(text, ":|;", ".")) %>%
    mutate(id = party) %>%
    group_by(id) %>% 
    summarise(text = paste(text, collapse = " "))
    }) %>% 
    group_by(id) %>% 
    summarise(text = paste(text, collapse = ".")) %>% 
    mutate(text = stringr::str_squish(text),
           text = stringr::str_replace_all(text, stringr::fixed(" ."), "."),
           text = stringr::str_replace_all(text, "\\.+", ". "),
           text = stringr::str_replace_all(text, stringr::fixed(";"), "."))
}

manifests_texts <- paths %>% purrr::map_df(manifests) %>% 
  filter(id != "pdr")

sentences_manifests <- tidytext::unnest_tokens(manifests_texts, 
                                               output = "sentences", 
                                               input = "text", 
                                               token = "sentences") %>% 
  filter(stringr::str_count(sentences, " ") > 5) %>% 
  mutate(sentences = stringr::str_remove_all(sentences, "programa eleitoral do pan legislativas."),
         sentences = stringr::str_remove_all(sentences, "programa eleitoral do bloco de esquerda."), 
         sentences = stringr::str_remove_all(sentences, "eleições legislativas"), 
         sentences = stringr::str_remove_all(sentences, "eleições legislativas"), 
         sentences = stringr::str_remove_all(sentences, "programa político chega.")) %>% 
  group_by(id) %>% 
  summarize(text = paste(sentences, collapse = " "))

