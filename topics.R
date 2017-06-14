require(topicmodels)
require(tm)
require(tidyverse)

extracts <- read_csv('extracts.csv')

extracts_c <- Corpus(VectorSource(extracts$enextracts))

enDTM <- DocumentTermMatrix(extracts_c, control = list(stopwords = stopwords('en'), removePunctuation = function(x) removePunctuation(x, preserve_intra_word_dashes = TRUE), stemming = TRUE))

enDTM <- removeSparseTerms(enDTM, (nrow(enDTM)-3) / nrow(enDTM))

enLDA <- LDA(enDTM, 2)
terms(enLDA, 20)
topics(enLDA)

extracts$entitles[topics(enLDA) == 2]
extracts$entitles[topics(enLDA) == 1]
