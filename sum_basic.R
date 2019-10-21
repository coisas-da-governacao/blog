pre_process_sentences <- function(df){
  df %>% 
    lexRankr::unnest_sentences(output = "sentence", input = "transcription_string") %>% 
    rename(id = sent_id)
}

words_without_stopwords <- function(df_sentences){
  df_sentences %>% 
    unnest_tokens(output = words,
                  input = sentence, 
                  token = "words") 
}

df_count_words_per_sentence <- function(df_words){
  df_words %>% 
    group_by(id, words) %>%
    summarise(count_per_sentence = n()) %>% 
    ungroup()
}

df_count_words <- function(df_words){
  df_words %>% 
    group_by(words) %>% 
    summarise(count = n()) %>% 
    ungroup()
}


# Sum basic specific auxiliary functions
word_prob_vector <- function(df){
  df %>%
    mutate(prob_word = count/sum(as.numeric(count)))
}

sentence_weight_vector <- function(word_prob){
  word_prob %>% 
    group_by(id) %>% 
    mutate(weight_sentence = sum(prob_word/count_per_sentence)) %>% 
    ungroup() 
}

choose_word_sum_basic <- function(sentences_weights){
  sentences_weights %>% 
    top_n(n = 1, wt = weight_sentence) %>% 
    filter(prob_word == max(prob_word)) %>% 
    head(1) %>% 
    select(id, words)
}

update_word_probs <- function(word_prob, chosen_word){
  word_prob %>% 
    mutate(prob_word = ifelse(words == unlist(chosen_word$words), prob_word*prob_word, prob_word))
}

sum_basic_whole_texts <- function(df, n_of_sentences){
  ps <- pre_process_sentences(df)
  
  pps <- words_without_stopwords(ps)
  
  word_counts_per_sentence <- df_count_words_per_sentence(pps)
  
  word_counts <- df_count_words(pps)
  
  word_prob <- word_counts_per_sentence %>% 
    inner_join(word_counts, by = "words")
  
  # first step: compute the probability of each word
  word_prob <- word_prob %>%
    mutate(prob_word = count/sum(as.numeric(count)))
  
  summary_sentences <- c()
  
  for (i in 1:n_of_sentences) {
    # second step: compute the weights of a sentence
    sentences_weights <- word_prob %>% 
      group_by(id) %>% 
      mutate(weight_sentence = sum(prob_word/count_per_sentence)) %>% 
      ungroup() 
    
    # third step: pick the highest probability word in all corpus and then pick the corresponding highest score sentence
    chosen_sentence <- sentences_weights %>% 
      filter(prob_word == max(prob_word)) %>% 
      top_n(n = 1, wt = weight_sentence) %>% 
      head(1) %>% 
      select(id)
    
    summary_sentences <- c(summary_sentences, unlist(chosen_sentence$id))
    
    # fourth step: update the highest word probability of the previous sentence
    word_prob <- word_prob %>% 
      mutate(prob_word = ifelse(id == unlist(chosen_sentence$id), prob_word*prob_word, prob_word))
  }
  
  ps %>% 
    filter(id %in% summary_sentences)
}
