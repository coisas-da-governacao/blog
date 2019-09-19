# Clean up

#'
#' @param raw_text chr
text_clean_up <- function(raw_text) {

  portuguese_lemmas <- readr::read_delim("content/post/first-post/resources/lemmatization-pt.txt",
                                         delim = "\t",
                                         col_names = c("lemma", "token")) %>%
    select(token, lemma) %>%
    distinct(token, .keep_all = TRUE)

  raw_text %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(pattern = "\\\n|\\\b", " ") %>%
    stringr::str_replace_all(pattern = "[[:punct:]]", " ") %>%
    stringr::str_replace_all(pattern = "[[:digit:]]+", "") %>%
    stringr::str_squish() #%>%
#    textstem::lemmatize_strings(dictionary = portuguese_lemmas)
}
