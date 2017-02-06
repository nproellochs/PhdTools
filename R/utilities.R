#' Utility functions
#'
#' Splits a sequence into equal parts.
#' @param x A vector to be splitted.
#' @param n An integer specifying the number of chunks.
#' @export
chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))

#' Produces a vector of a desired length with an arbitrary number of labels.
#' Each label occurs equally frequent in the resulting sequence.
#' @param vectorLength An integer specifying the length of the vector.
#' @param numberOfChunks An integer specifying the number of labels
#' @export
labeledVector = function(vectorLength, numberOfChunks){
  temp = chunk(1:vectorLength,numberOfChunks)
  return(unlist(lapply(1:length(temp), function(x) rep(x,length(temp[[x]])))))
}

#' Produces polarity labels for a numeric input vector.
#' @param x A numeric input vector.
#' @export
polarity <- function(x, labelPos = 1, labelNeg = -1, labelNeutral = NA) {
  out <- x
  out[out > 0] <- labelPos
  out[out < 0] <- labelNeg
  out[out == 0] <- NA
  return(out)
}
