numToBreaks <- function(vecIn, breaks) {
  vecIn <- cut(vecIn, breaks, right = F)
  return(as.character(vecIn))
}