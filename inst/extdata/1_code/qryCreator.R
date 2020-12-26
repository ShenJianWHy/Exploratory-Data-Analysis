qryCreator <- function(table, colsToSelect = "*", colsToFilter = NULL, conditionsToFilter = NULL) {
  select <- paste0(colsToSelect, collapse = ", ")
  qry    <- paste0("SELECT ", select, " FROM ", table)
  
  if (!is.null(colsToFilter)) {
    qry <- paste0(qry, " WHERE ", paste(colsToFilter, conditionsToFilter, collapse = " AND "))
  }
  
  return(qry)
}