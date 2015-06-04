formatProcureText <- function(procure, text.var) {
  TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
  text <- mapply(paste, procure[, text.var], collapse = " ")
  text <- stripWhitespace(text)
  text <- removePunctuation(text)  
  text <- tolower(text)
  text <- Corpus(VectorSource(text))
  text <- tm_map(text, removeWords, c("the", stopwords("english"))) 
  text <- tm_map(text, removeNumbers) 
  dtm <- DocumentTermMatrix(text, control=list(weighting=weightTf, 
                                               tokenize = TrigramTokenizer))
  dtm <- removeSparseTerms(dtm, 0.999)
  return(dtm)
}
