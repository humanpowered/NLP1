library(tidyverse)
library(lexicon)


sms = read_csv("spamraw.csv")
str(sms)
table(sms$type)


library(tm)
sms_corpus <- VCorpus(VectorSource(sms$text))

#To receive a summary of specific messages we make use of inspect() function
inspect(sms_corpus[1:2])
#To view actual message text we use this
as.character(sms_corpus[[1]]) #Double bracket is must
#to view multiple messages
lapply(sms_corpus[1:2],as.character)

sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower)) #converting to lower case letters
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers) #removing numbers
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords()) #removing stop words
sms_corpus_clean <- tm_map(sms_corpus_clean,removePunctuation) #removing punctuation
#to work around the default behavior of remove punctuations, simply create a function i.e.,
                #replacePunctuations <- function(x){
                               #gsub("[:punct:]",+," ",*)
                               #}


library(SnowballC)
#single word stemming
wordStem(c("learn","learned","learning"))
#Doing stemming to entire corpus
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)#removing spaces after doing above process

#final step is to split the messages into individual components through process called Tokenization
#In this case tokens are words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
#What happened here??
#Ans:-DocumentTermMatrix function will take a corpus and create a data structure called DTM in which,
      #Rows indicate Documents(sms messages) and column indicates Terms(words)
