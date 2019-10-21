library("dplyr")
library("mallet")


lda_model <- mallet::MalletLDA(num.topics = 25)

mallet.instances <- mallet.import(
  sentences_manifests$id,
  sentences_manifests$text,
  stoplist.file = "content/post/meetup/empty.txt")

lda_model$loadDocuments(mallet.instances)

lda_model$train(200)
lda_model$maximize(10)

doc.topics <- mallet.doc.topics(lda_model, smoothed = T, normalized = T)
topic_words <- mallet.topic.words(lda_model, smoothed = T, normalized = T)

vocabulary <- lda_model$getVocabulary()
word.freqs <- mallet.word.freqs(lda_model)

1:25 %>% purrr::map(~ {mallet.top.words(lda_model, topic_words[.x,], num.top.words = 15)})

stopword_list <- read.table("content/post/meetup/empty.txt")
words_per_doc <- sentences_manifests %>% 
    tidytext::unnest_tokens(output = words,
                  input = text, 
                  token = "words") %>%
    anti_join(stopword_list, by = c("words" = "V1")) %>%
  group_by(id) %>%
  summarise(n = n())

library(LDAvis)

# create the JSON object to feed the visualization:
json <- LDAvis::createJSON(phi = topic_words, 
                   theta = doc.topics, 
                   doc.length = words_per_doc$n, 
                   vocab = vocabulary, 
                   term.frequency = word.freqs$term.freq)

LDAvis::serVis(json, out.dir = 'vis', open.browser = FALSE)
