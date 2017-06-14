require(tidyverse)
require(tidytext)
require(tm)
require(e1071)
require(class)
require(MLmetrics)


sa200k <- read_csv('SA200K.csv')
sa200k$Sentiment <- as.factor(sa200k$Sentiment)

### Only sample 20K

sa200k <- sa200k[sample(1:nrow(sa200k), 5000),]

dtm <- DocumentTermMatrix(Corpus(VectorSource(sa200k$SentimentText)))
clean_dtm <- removeSparseTerms(dtm, ((nrow(dtm) - 15) / nrow(dtm)))

set.seed(777)
training <- sample(1:nrow(dtm), floor(nrow(dtm) * 0.7))

trainingX <- as.matrix(clean_dtm[training,])
trainingy <- as.factor(sa200k$Sentiment[training])

testX <- as.matrix(clean_dtm[-training,])
testy <- as.factor(sa200k$Sentiment[-training])


#svmm <- svm(trainingX, trainingy, scale = FALSE)
svmm <- naiveBayes(x = trainingX, y = trainingy)
predict_y <- predict(svmm, trainingX)

table(predict_y, trainingy)


F1_Score(trainingy, predict_y)

predicttest_y <- predict(svmm, testX)

table(predicttest_y, testy)
F1_Score(predicttest_y, testy)

### Make prediction

input <- 'its amazing how supportive ur family is,and ur friendship with Cheryl is truly touching too'
input <- 'i feel sad'

DocumentTermMatrix(Corpus(VectorSource(c(input))), control = list(dictionary = Terms(clean_dtm))) %>% as.matrix %>% predict(svmm, .)

### make it a function

bot <- function(input) {
    sentiment <- DocumentTermMatrix(Corpus(VectorSource(c(input))), control = list(dictionary = Terms(clean_dtm))) %>% as.matrix %>% predict(svmm, .)
    ifelse(sentiment == 1, "Yeah!", "Don't be sad")
}

"HKAECT is so cool" %>% bot

"My PhD study is bad" %>% bot

### Negation

"My PhD Study is not bad" %>% bot
