pdf_to_text <- function(file_paths) {
  purrr::map_df(
    .x = file_paths,
    .f = ~ { tibble(
      partido = gsub(".pdf", "", basename(.x)),
      programa = stringr::str_flatten(pdftools::pdf_text(.x))
    )}
  )
}
