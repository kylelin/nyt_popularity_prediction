####################################################
# UTIL FUNCTIONS
# AUTHOR Yanpeng Lin
####################################################


# Remove leading and training white space
trim <- function(str) {
  return(gsub("^\\s+|\\s+$", "", str))
}

# Remove diacritics from letters
replaceHTML <- function(str) {
  return(gsub("&([[:alpha:]])(acute|cedil|circ|grave|uml);", "\\1", str))
}

# Remove special characters, such as dashes, quotes, and ellipses
ignoreHTML <- function(str) {
  return(gsub("&((m|n)dash|hellip|(l|r)squo;)", "", str))
}

# Extract the text between HTML tags
extractText <- function(str) {
  return(gsub("<.*?>", "", str))
}

# Clean up: extract, remove, replace, and trim
cleanupText <- function(str) {
  return(trim(replaceHTML(ignoreHTML(extractText(str)))))
}

# Replace multiple string patterns
# the ellipsis indicates that we can supply gsub-specific parameters to phaseSub
phaseSub <- function(x, pattern, replacement, ...) {
  if (length(pattern) != length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result = x
  for (i in 1:length(pattern)) {
    result = gsub(pattern[i], replacement[i], result, ...)
  }
  return(result)
}

# Analyse frequency and popularity of string patterns
analyseTerms <- function(terms, ignoreCase=TRUE)
{
  idxHeadTrain = which(grepl(terms, NewsTrain$Headline, ignore.case=ignoreCase))
  idxTextTrain = which(grepl(terms, NewsTrain$Text,     ignore.case=ignoreCase))
  idxHeadTest  = which(grepl(terms, NewsTest$Headline,  ignore.case=ignoreCase))
  idxTextTest  = which(grepl(terms, NewsTest$Text,      ignore.case=ignoreCase))

  avgPopHeadTrain = mean(as.numeric(as.character(NewsTrain$Popular[idxHeadTrain])))
  numArtHeadTrain = length(idxHeadTrain)
  avgPopTextTrain = mean(as.numeric(as.character(NewsTrain$Popular[idxTextTrain])))
  numArtTextTrain = length(idxTextTrain)
  numArtHeadTest  = length(idxHeadTest)
  numArtTextTest  = length(idxTextTest)

  return(c(avgPopHeadTrain, numArtHeadTrain,
           avgPopTextTrain, numArtTextTrain,
           numArtHeadTest, numArtTextTest))
}

# Calculate the accuracy and AUC of the model on the training set
calcAUC <- function(model, truth)
{
  suppressPackageStartupMessages(require(ROCR))

  model.pred = predict(model, type="prob")
  model.accu = tr(table(truth, model.pred[, 2]>0.5))/nrow(model.pred)
  model.auc  = as.numeric(performance(prediction(model.pred[, 2], truth), "auc")@y.values)

  return(c(model.accu, model.auc))
}

# N-Gram tokenizer for mono-, bi-, and tri-grams
NGTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

# Prepare a CSV file with predictions for submission
generateSubmission <- function(predictions) {
  fileName = paste0("Submission_", deparse(substitute(predictions)), ".csv")
  submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predictions)
  write.csv(submission, fileName, row.names=FALSE)
}
