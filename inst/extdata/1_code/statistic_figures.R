# Construction of the stat figures for different data base
statistic_figures <- function(datIn, varsIn, costVar, keyVar) {
  
  datIn$FinCostVar <- datIn[[costVar]]
  datIn$finKey     <- datIn[[keyVar]]
  
  total <- datIn %>%
    distinct(finKey, .keep_all = TRUE) %>%
    ungroup() %>%
    summarise(nScores    = n(),
              Score      = sum(FinCostVar, na.rm = T),
              Freq       = nScores / nScores,
              p0_Score   = quantile(FinCostVar, probs = 0, na.rm = T),
              p25_Score  = quantile(FinCostVar, probs = 0.25, na.rm = T),
              p50_Score  = quantile(FinCostVar, probs = 0.50, na.rm = T),
              p75_Score  = quantile(FinCostVar, probs = 0.75, na.rm = T),
              p100_Score = quantile(FinCostVar, probs = 1.00, na.rm = T),
              ave_Score  = Score / nScores) %>%
    select(-c(Freq, Score))
  
  lstOut <- list()
  
  for (var in varsIn) {
    datTem <- datIn
    if (var %in% c("service_type", "motif")) { datTem <- datIn %>% distinct(finKey, .keep_all = TRUE) }
    this_ua <- datIn %>%
      group_by_(var) %>%
      summarise(nScores   = n(),
                p0_Score   = quantile(FinCostVar, probs = 0, na.rm = T),
                p25_Score  = quantile(FinCostVar, probs = 0.25, na.rm = T),
                p50_Score  = quantile(FinCostVar, probs = 0.50, na.rm = T),
                p75_Score  = quantile(FinCostVar, probs = 0.75, na.rm = T),
                p100_Score = quantile(FinCostVar, probs = 1.00, na.rm = T), 
                ave_Score = mean(FinCostVar, na.rm = TRUE)) %>%
      mutate(Freq = nScores / sum(nScores)) %>%
      arrange(-Freq)
    lstOut <- append(lstOut, list(this_ua))
  }
  names(lstOut) <- varsIn
  return(list(lstOut = lstOut, totals = total))
}