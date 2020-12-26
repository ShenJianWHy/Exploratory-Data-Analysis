toSeason <- function(dat) {
  
  stopifnot(class(dat) == "Date")
  
  scalarCheck <- function(dat) {
    m <- as.POSIXlt(dat)$mon + 1        # correct for 0:11 range
    d <- as.POSIXlt(dat)$mday           # correct for 0:11 range
    if ((m == 3 & d >= 21) | (m == 4) | (m == 5) | (m == 6 & d < 21)) {
      r <- 1
    } else if ((m == 6 & d >= 21) | (m == 7) | (m == 8) | (m == 9 & d < 21)) {
      r <- 2
    } else if ((m == 9 & d >= 21) | (m == 10) | (m == 11) | (m == 12 & d < 21)) {
      r <- 3
    } else {
      r <- 4
    }
    r
  }
  
  res <- sapply(dat, scalarCheck)
  res <- ordered(res, labels=c("Spring", "Summer", "Fall", "Winter"))
  invisible(res)
}


