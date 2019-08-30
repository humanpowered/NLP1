library(lexicon)
library(stringr)
library(textclean)
library(tm)

CleanData <- function(x){
  # words to be removed from corpus

  rem <- c(stopwords("en"),
  as.character(lexicon::pos_df_pronouns[,1]),
  as.character(lexicon::pos_interjections),
  as.character(lexicon::sw_python))

  # cleaning text
  x <- stringr::str_replace(x, '(http|https)[^([:blank:]|\\"|<|&|#\n\r)]+', ' ') # remove urls
  x2 <- textclean::replace_symbol(x) # replace symbols like %, @ and & with words
  x2 <- tolower(x2) # force lower case
  x2 <- tm::removeNumbers(x2)
  x2 <- tm::removePunctuation(x2)
  x2 <- tm::removeWords(x2, rem) # remove common stopwords
  x2 <- tm::stripWhitespace(x2)
  cleandata <- stringr::str_replace_all(x2, '[^a-zA-Z\\s]', ' ') # remove all non-characters

  return(cleandata)
}
