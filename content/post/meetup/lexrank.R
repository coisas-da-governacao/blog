lex_rank <- function(party, n = 3) {
  lexRankr::lexRank(filter(sentences_manifests, party == party)$sentences,
                    removeNum = FALSE, 
                    stemWords = FALSE, 
                    rmStopWords = FALSE, 
                    sentencesAsDocs = TRUE,
                    n = n)
}


lex_rank_ps <- lex_rank("ps", 5)

lex_rank_verdes <- lex_rank("pan", 10)
