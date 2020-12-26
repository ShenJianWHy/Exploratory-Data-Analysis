dateConv <- function(x) {
  return(as.Date(parsedate::parse_date(x)))
}