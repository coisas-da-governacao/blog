library("dplyr")
devtools::install_github("quartin/ggthemes")


paths <- list.files("content/post/first-post/resources/2019", pattern = ".pdf", full.names = TRUE)

words_per_page <- function(path) {
  pages_data <- pdftools::pdf_data(path)
  party <- gsub(".pdf", "", basename(path))
  
  pages_data %>% 
    purrr::map_df(.f = ~ {
      .x %>% 
      filter(stringr::str_starts(text, pattern = "\b", negate = TRUE)) %>% 
        count(name = "number_words")
    }) %>%
    mutate(party = party,
           page_number = row_number()) %>% 
    select(party, page_number, number_words)
  }

all_counts <- paths %>% purrr::map_df(words_per_page) %>% 
  filter(party != "pdr")

ggplot2::ggplot(all_counts, aes(x = number_words)) +
  ggthemes::theme_fivethirtyeight(base_size = 14) +
  ggplot2::geom_freqpoly(binwidth = 30, colour = ggthemes::talkdesk_pal()(4)[4]) +
  ggplot2::facet_wrap(~ party)

ggplot2::ggplot(all_counts, aes(x = page_number, y = number_words)) +
  ggthemes::theme_hc(base_size = 12) +
  ggplot2::geom_point(colour = ggthemes::talkdesk_pal()(4)[2], alpha = 0.8) +
  ggplot2::facet_wrap(~ party) +
  ggplot2::theme(strip.background = element_rect(fill = ggthemes::talkdesk_pal()(4)[1])) +
  ggplot2::theme(strip.text = element_text(size = 20, colour = 'white')) +
  ggplot2::ylab("Number of words") +
  ggplot2::xlab("Page number")

