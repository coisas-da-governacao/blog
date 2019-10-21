install.packages("rJava", type = "source")

library("mallet")


lda_model <- mallet::MalletLDA(num.topics = 14)

