#' Text mining functions
#'
#' Load Harvard IV psychological dictionary
#' @param stemDictionary If \code{TRUE}, each word is stemmed to its root form (default).
#' @return list of positive and negative stems
#' @importFrom utils data
#' @export
loadHarvardDictionary <- function(stemDictionary = TRUE) {
  data(dictionaryHarvard, envir=environment())
  if (stemDictionary) {
    return(list("Positive" = unique(tm::stemDocument(dictionaryHarvard$PositiveWords, language="english")),
                "Negative" = unique(tm::stemDocument(dictionaryHarvard$NegativeWords, language="english"))))
  } else {
    return(dictionaryHarvard)
  }
}

#' Load LM dictionary
#' @param stemDictionary If \code{TRUE}, each word is stemmed to its root form (default).
#' @return list of positive and negative stems
#' @importFrom utils data
#' @export
loadLMDictionary <- function(stemDictionary = TRUE) {
  data(dictionaryLM, envir=environment())
  if (stemDictionary) {
    return(list("Positive" = unique(tm::stemDocument(dictionaryLM$PositiveWords, language="english")),
                "Negative" = unique(tm::stemDocument(dictionaryLM$NegativeWords, language="english"))))
  } else {
    return(dictionaryLM)
  }
}

#' Load Henry dictionary
#' @param stemDictionary If \code{TRUE}, each word is stemmed to its root form (default).
#' @return list of positive and negative stems
#' @importFrom utils data
#' @export
loadHenryDictionary <- function(stemDictionary = TRUE) {
  data(dictionaryHenry, envir=environment())
  if (stemDictionary) {
    return(list("Positive" = unique(tm::stemDocument(dictionaryHenry$PositiveWords, language="english")),
                "Negative" = unique(tm::stemDocument(dictionaryHenry$NegativeWords, language="english"))))
  } else {
    return(dictionaryHenry)
  }
}

#' Load SentiWordNet dictionary
#' @param stemDictionary If \code{TRUE}, each word is stemmed to its root form (default).
#' @return list of positive and negative stems
#' @note Numeric terms and terms with neutral sentiment are excluded.
#' @importFrom utils data
#' @import dplyr
#' @export
loadSentiWordNetDictionary <- function(stemDictionary = TRUE) {
  data(sentiWordNetDictionary, envir=environment())
  if (stemDictionary) {
    sentiWordNetDictionary$Word <- tm::stemDocument(sentiWordNetDictionary$Word, language="english")

    sentiWordNetDictionary <- sentiWordNetDictionary %>% dplyr::group_by(Word) %>% dplyr::summarise(mean(PosScore), mean(NegScore)) %>% as.data.frame()
    sentiWordNetDictionary$Sentiment <- sentiWordNetDictionary$`mean(PosScore)` - sentiWordNetDictionary$`mean(NegScore)`
    colnames(sentiWordNetDictionary) <- c("Word", "PosScore", "NegScore", "Sentiment")
  }
  return(sentiWordNetDictionary)
}

#' Load MIT English dictionary
#' @param stemDictionary If \code{TRUE}, each word is stemmed to its root form (default).
#' @return Vector of valid English words
#' @importFrom utils data
#' @export
loadEnglishDictionary <- function(stemDictionary = TRUE) {
  data(englishWords, envir=environment())
  if (stemDictionary) {
    return(as.vector(unique(tm::stemDocument(englishWords, language="english"))))
  } else {
    return(englishWords)
  }
}

#' Calculates the automated readability index from a given text.
#' @param text An input character string.
#' @export
automatedReadabilityIndex = function(text) {
  textCleaned = tm::stripWhitespace(gsub("\n"," ", text))
  sentences = unlist(strsplit(textCleaned, ". ", fixed = T))
  cleanedSentences = tm::removePunctuation(sentences)
  cleanedSentences = tm::removeNumbers(cleanedSentences)
  cleanedSentences = tm::stripWhitespace(cleanedSentences)

  words = strsplit(cleanedSentences, " ")

  numSentences = length(cleanedSentences)
  numWords = length(unlist(words))
  charactersPerSentence = sapply(words, nchar)
  charactersInText = sum(sapply(charactersPerSentence, sum))

  readability = 4.71 * (charactersInText / numWords) + 0.5 * (numWords / numSentences) - 21.43
  return(readability)
}

#' Tokenize a string vector.
#' @param x A vector to be tokenized.
#' @return Tokenized vector.
#' @importFrom stringr str_replace_all
#' @export
tokenize <- function(x) {
  tokens <-
    sapply(x, function(y)
      stringr::str_replace_all(y, "[^[:alnum:]]", ""))
  tokens <- sapply(names(tokens), function(x)
    paste(x, "_", sep = ""))
  return(as.vector(tokens))
}
