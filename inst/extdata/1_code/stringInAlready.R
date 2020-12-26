stringInAlready <- function(stringTestVec, stringList, threshold = 0.1) {
  stringTestVec <- stringTestVec %>% unlist()
  names(stringList) <- stringTestVec
  stringIn <- lapply(seq_along(stringList), function(ii) {sum(stringdist::stringdist(names(stringList)[[ii]], stringList[[ii]], method = "cosine") < threshold) > 0}) %>% unlist()
  stringIn[is.na(stringIn)] <- FALSE
  return(stringIn)
}