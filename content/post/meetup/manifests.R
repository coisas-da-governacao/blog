library("dplyr")
library("tidytext")

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
        filter(nchar(text) > 1) %>%
        filter(stringr::str_detect(text, pattern = "\\d{1}", negate = TRUE)) %>% 
        mutate(party = party) %>%
        group_by(party) %>% 
        summarise(text = paste(text, collapse = " "))
    }) %>% 
    group_by(party) %>% 
    summarise(text = paste(text, collapse = ".")) %>% 
    mutate(text = stringr::str_squish(text),
           text = stringr::str_replace_all(text, " i+ ", "."), 
           text = stringr::str_replace_all(text, stringr::fixed(" ."), "."),
           text = stringr::str_replace_all(text, stringr::fixed(";"), "."),
           text = stringr::str_replace_all(text, 
                                          pattern =  paste0(c("1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "10."), 
                                                            collapse = "|"), "."),
           text = stringr::str_replace_all(text, "\\.+", ". "))
}

manifests_texts <- paths %>% purrr::map_df(manifests) %>% 
  filter(party != "pdr") %>% 
  mutate(text = tolower(text))
