####### tutuorial found at https://www.kaggle.com/shravan3273/natural-language-processing-with-r/notebook ----------

# Understanding the Niave Bayes
#
# Classifier based on Bayesian methods utilize training data to calculate an observed probability of each outcomebased on the evidence provided by feature values.When the classifier is later applied to unlabeled data, it uses the observed probabilities to predict the most likely class for the new features.In fact, Bayesian classifier have been used for:
#
# Text classification, such as junk email(spam) filtering
#
# Intrusion or anamaly detection in computer networks
#
# Diagnosing medical conditions
#
# Naive bayes algorithm usage has become the Defacto standard for "Text classification"
#
# Step1:- Collecting data ------------------------------

#To develop the naive bayes classifier, we will use data adapted from the sms spam collection
#We have total of 5559 observations and 2 variables
# data sourced @ https://www.kaggle.com/shravan3273/natural-language-processing-with-r/data


# Step2:-Exploring and Preparing the data --------------------

library(tidyverse)
library(lexicon)
sms = read.csv("spamraw.csv")
str(sms)
sms$text = as.character(sms$text)
str(sms)
table(sms$type)

# first step in processing text data involves creating a corpus, which is collection of documents. In our case, the corpus will be collection of sms messages.
# What is a Document??
#
# For example, i have 1000 messages and in each message i have 10 common words. Then,
#
# 1000 messages = 1000 Documents(rows)
#
# 1000 * 10 = 10,000 variables or Columns

library(tm)
sms_corpus <- VCorpus(VectorSource(sms$text))

#To receive a summary of specific messages we make use of inspect() function
inspect(sms_corpus[1:2])
#To view actual message text we use this
as.character(sms_corpus[[1]]) #Double bracket is must
#to view multiple messages
lapply(sms_corpus[1:2],as.character)

# Now, the real game starts ------------------------------

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


# Final step -------------------------------------

#final step is to split the messages into individual components through process called Tokenization
#In this case tokens are words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
#What happened here??
#Ans:-DocumentTermMatrix function will take a corpus and create a data structure called DTM in which,
      #Rows indicate Documents(sms messages) and column indicates Terms(words)


# Single Shot process-----------------------------------
sms_dtm2 <- DocumentTermMatrix(sms_corpus,
                              control = list(tolower = TRUE,
                                            removeNumbers = TRUE,
                                            stopwords = TRUE,
                                            removePunctuation = TRUE,
                                            stemming = TRUE)
)
#Difference in doing step-by-step and single shot is,we can see slight change in no. of terms in matrix.

# Dividing the data into train and test ------------------------------------------------
# divide into train and test sets not the best method as it isn't randomly assigned using caret split would be better also wonder how disproportionate ham to spam will affect training

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]
sms_train_labels <- sms[1:4169,]$type
sms_test_labels <- sms[4170:5559,]$type
#lets check whether the subsets are representing complete set of sms data
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))


# Visualising words ------------------------------
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE)

#let's visualise spam and ham messages
spam <- subset(sms,type == "spam")
ham <- subset(sms,type == "ham")
wordcloud(spam$text,max.words = 40,scale = c(3,0.5)) #max.words is most common words
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

findFreqTerms(sms_dtm_train,5)
sms_freq_words <- findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)
#this command will display the words appearing at least five times in sms_dtm_train matrix

sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]
convert_counts <- function(x){
                 x <- ifelse(x>0,"Yes","No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# What happened after visualisation??
#
# Train and Test sets(i.e., sms_dtm_freq_train & sms_dtm_freq_test) includes 1136 features, which corresponds to words appearing in atleast five messages.
#
# The Naive bayes Classifier is typically trained on data with categorical features. Since the cells in the sparse matrix
#
# (It is a matrix in which most of the elements are zero) are numeric.
#
# convert_counts() function is used to convert counts(i.e., 0 or 1) to Yes/No strings.
#
# Margin = 1 ---------->Rows
#
# Margin = 2 ---------->Columns


# Step3:- Training a model on the data ---------------------------
#install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train,sms_train_labels)


# Step4:- Evaluating model performance --------------------------------

sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
table(sms_test_pred,sms_test_labels)
