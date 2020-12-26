propFunc <- function (datIn, vars, totToReturn, asDF = TRUE) 
{
  outpList <- list()
  for (vr in vars) {
    varSelected <- datIn[[vr]]
    varSelected[grepl(x = varSelected, pattern = "NA", ignore.case = TRUE)] <- NA
    outp <- as.data.frame(prop.table(table(varSelected, useNA = "always")))
    NAValue <- outp$Freq[is.na(outp$varSelected)]
    outp <- outp[!is.na(outp$varSelected), ]
    outp <- outp[order(-outp$Freq), ]
    FinalOutp <- data.frame(varSelected = "NA", Freq = NAValue)
    ender <- min((totToReturn - 1), nrow(outp))
    toDrop <- nrow(outp) - ender
    toDropData <- data.frame(varSelected = c("Rest", "RestCount"), 
                             Freq = c(ifelse(nrow(outp) == 0, 0, toDrop/nrow(outp)), toDrop))
    if (nrow(outp) > 0) {
      FinalOutp <- rbind(FinalOutp, outp[1:ender, ], toDropData)
    } else {
      FinalOutp <- rbind(FinalOutp, toDropData)
    }
    
    FinalOutp$Freq <- round(FinalOutp$Freq, 4)
    tempList <- list(FinalOutp$Freq)
    names(tempList) <- vr
    names(tempList[[1]]) <- FinalOutp$varSelected
    outpList <- append(outpList, tempList)
  }
  
  if (asDF) {
    for (ii in 1:length(outpList)) {
      rowVals <- outpList[ii]
      rowValsNames <- names(rowVals[[1]])
      tempDf <- as.data.frame(rowVals)
      rownames(tempDf) <- rowValsNames
      tempDf <- t(tempDf)
      RestRestCount <- t(as.data.frame(tempDf[, c(c("Rest", "RestCount"))]))
      rownames(RestRestCount) <- "1"
      otherDat <- tempDf[, !(colnames(tempDf) %in% c("Rest", "RestCount"))]
      finDat <- t(data.frame(paste0(names(otherDat), " : ", otherDat)))
      rownames(finDat) <- "1"
      colnames(finDat) <- paste0("Var_", seq(1:dim(finDat)[2]))
      finDat <- cbind(finDat, RestRestCount)
      finDat <- as.data.frame(finDat)
      
      if (ii == 1) {
        combinedFinDat <- finDat
      } else {
        combinedFinDat <- dplyr::bind_rows(combinedFinDat, finDat)
      }
    }
    rownames(combinedFinDat) <- names(outpList)
    outpList <- combinedFinDat
    outpList <- outpList %>% dplyr::select(-c(Rest, RestCount), everything())
  }
  return(outpList)
}