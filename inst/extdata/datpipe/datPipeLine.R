dataPipeLine <- function(){
  startTime <- Sys.time()
  library(knitr)
  library(rmdformats)
  options(max.print = "75")
  opts_chunk$set(echo    = TRUE,
                 cache   = FALSE,
                 prompt  = FALSE,
                 tidy    = FALSE,
                 comment = NA,
                 message = FALSE,
                 warning = FALSE)
  opts_knit$set(width = 75)
  
  # read necessary files(raw data, ref data, ymal file and other files)
  
  # Setup and Load Packages
  options(scipen = 999, tibble.width = Inf)
  if (!is.element("yaml",     .packages(all.available = TRUE))) { install.packages("yaml")     }
  if (!is.element("devtools", .packages(all.available = TRUE))) { install.packages("devtools") }
  if (!is.element("fs",       .packages(all.available = TRUE))) { install.packages("fs")       }
  
  # Load Meta Data
  metaData <- yaml::read_yaml(file = "metadat.yaml")
  # Connect to DB -----------------------------------------------------------
  connect_TRT <- FALSE
  connect_GIP <- TRUE
  connect_SPO <- FALSE
  for (file in list.files(path = "inst/extdata/1_code", full.names = TRUE)) source(file)
  devtools::install_github(repo = metaData$connectionDetails$github$repo, auth_token = metaData$connectionDetails$github$token)
  eaR::pkgInstaller(
    libs = c("RPostgres", "tidyverse", "lubridate", "parsedate", "readxl", "data.table", "RODBC", "sqldf", "fixerapi", "rjson", "stringdist", "kableExtra", "plotly"),
    "C:/Rpackages")
  
  
  library(dplyr)
  title_var <- "Neonet Analysis"
  extractDate <- as.Date("2020-05-22")
  
  # load data
  ref_coverage <- readr::read_csv(file = "inst/extdata/ref/coverageFormulaLookup.csv")  %>% lapply(function(x) eaR::cleanNames(datIn = x, sep = "_")) %>% as.data.frame()
  ref_prodType <- readr::read_csv(file = "inst/extdata/ref/productTypeGroupLookup.csv") %>% lapply(function(x) eaR::cleanNames(datIn = x, sep = "_")) %>% as.data.frame()
  ref_replacem <- readr::read_csv(file = "inst/extdata/ref/replacementLookup.csv")      %>% lapply(function(x) eaR::cleanNames(datIn = x, sep = "_")) %>% as.data.frame()
  ref_prodTypeMap <- readr::read_csv(file = "inst/extdata/ref/productTypeMapping.csv")
  pricingGrid     <- readr::read_csv(file = "inst/extdata/ref/pricingGrid.csv")
  
  
  ## Sales Data
  # Load Data and basic cleaning
  salesDat <- readr::read_csv(file = "inst/extdata/raw/sales_mobileOnly.csv") %>% 
    rbind(readr::read_csv(file = "inst/extdata/raw/sales_nonMobile.csv")) %>%
    mutate(policyNumber        = policyNumber        %>% eaR::cleanNames(sep = "_"),
           productName         = productName         %>% eaR::cleanNames(sep = "_"),
           productType         = productType         %>% eaR::cleanNames(sep = "_"),
           productBrand        = productBrand        %>% eaR::cleanNames(sep = "_"),
           coverageType        = coverageType        %>% eaR::cleanNames(sep = "_"),
           group               = group               %>% eaR::cleanNames(sep = "_"),
           datePolSold         = datePolSold         %>% lubridate::dmy(),
           dateCoverageStart   = dateCoverageStart   %>% lubridate::dmy(),
           dateCoverageEnd     = dateCoverageEnd     %>% lubridate::dmy(),
           dateReport          = dateReport          %>% lubridate::dmy(),
           timeToCoverStart    = dateCoverageStart - datePolSold %>% as.numeric(),
           yearDateReport      = yearDateReport      %>% as.numeric(),
           riskPeriodTwo       = riskPeriodTwo       %>% as.numeric(),
           premGross           = premGross           %>% as.numeric(),
           premComNeoent       = premComNeoent       %>% as.numeric(),
           premNet             = premNet             %>% as.numeric(),
           premComBroker       = premComBroker       %>% as.numeric(),
           premComAgent        = premComAgent        %>% as.numeric(),
           premTotCom          = premTotCom          %>% as.numeric(),
           premOperationalCost = premOperationalCost %>% as.numeric(),
           expo                = ifelse(extractDate > dateCoverageEnd, dateCoverageEnd - dateCoverageStart, extractDate - dateCoverageStart) / (dateCoverageEnd - dateCoverageStart) %>% as.numeric(),
           burningCost         = premNet - premComBroker - premComAgent - premOperationalCost,
           burningCostEarned   = burningCost * expo) %>%
    filter(dateCoverageStart >= as.Date("2015-07-01"), coverageType != "assistance") %>%
    left_join(ref_coverage %>% dplyr::distinct(policyNumber, .keep_all = TRUE)) %>% # These needs to be distinct, sometimes they
    left_join(ref_prodType %>% dplyr::distinct(policyNumber, .keep_all = TRUE)) %>% # aren't and we need to figure out why this is
    left_join(ref_replacem %>% dplyr::distinct(policyNumber, .keep_all = TRUE))     # the case
  # Add store code information
  storeCode                <- salesDat$policyNumber %>% str_split(pattern = "_") %>% as.data.frame() %>% t() %>% as.data.frame()
  salesDat$storeCode       <- storeCode$V3
  # Identify cancelled policies (potential cancelled)
  polsMapping <- salesDat %>%
    group_by(policyNumber) %>%
    summarise(count   = n(), 
              netPrem = sum(premNet))
  potentialCancelledPolicies <- salesDat %>%
    filter(policyNumber %in% polsMapping$policyNumber[polsMapping$count > 1])
  # Remove cancelled policies and get product price
  salesDat <- salesDat %>%
    filter(policyNumber %in% polsMapping$policyNumber[polsMapping$count == 1], premGross > 0) %>% 
    left_join(ref_prodTypeMap) %>%
    left_join(pricingGrid) %>%
    mutate(coverageDescr    = coverage,
           productTypeGroup = productTypeDescr %>% eaR::cleanNames(sep = "_"),
           productPrice     = ifelse(grossMinPrem > premGross, grossMinPrem / grossPremPerc, premGross / grossPremPerc)) %>%
    select(-c(coverage, productTypeDescr, grossMinPrem, grossPremPerc, productCode))
  # Create extended and accidental datasets
  formulaParts <- salesDat$coverageFormula %>% str_split(pattern = "_") %>% as.data.frame() %>% t() %>% as.data.frame()
  formulaParts <- lapply(formulaParts, as.numeric) %>% as.data.frame()
  salesDat$formulaPartOne <- formulaParts$V1
  salesDat$formulaPartTwo <- formulaParts$V2
  salesDat$trueCover <- salesDat$coverageType
  cascoDat <- salesDat %>% 
    filter(coverageType == "gn") %>%
    mutate(totalDur              = formulaPartOne + 2 * formulaPartTwo,
           trueCover             = "casco",
           premGross             = ((formulaPartOne + formulaPartTwo) / totalDur) * premGross,
           premComNeoent         = ((formulaPartOne + formulaPartTwo) / totalDur) * premComNeoent,
           premNet               = ((formulaPartOne + formulaPartTwo) / totalDur) * premNet,
           premComBroker         = ((formulaPartOne + formulaPartTwo) / totalDur) * premComBroker,
           premComAgent          = ((formulaPartOne + formulaPartTwo) / totalDur) * premComAgent,
           premTotCom            = ((formulaPartOne + formulaPartTwo) / totalDur) * premTotCom,
           premOperationalCost   = ((formulaPartOne + formulaPartTwo) / totalDur) * premOperationalCost,
           expo                  = ifelse(extractDate > dateCoverageEnd, dateCoverageEnd - dateCoverageStart, extractDate - dateCoverageStart) / (dateCoverageEnd - dateCoverageStart) %>% as.numeric(),
           burningCost           = premNet - premComBroker - premComAgent - premOperationalCost,
           burningCostEarned     = burningCost * expo) %>%
    select(-totalDur)
  ewDat <- salesDat %>% 
    filter(coverageType == "gn") %>%
    mutate(totalDur            = formulaPartTwo,
           trueCover           = "pg",
           dateCoverageStart   = dateCoverageStart %m+% lubridate::years(formulaPartOne),
           premGross           = (formulaPartTwo / totalDur) * premGross,
           premComNeoent       = (formulaPartTwo / totalDur) * premComNeoent,
           premNet             = (formulaPartTwo / totalDur) * premNet,
           premComBroker       = (formulaPartTwo / totalDur) * premComBroker,
           premComAgent        = (formulaPartTwo / totalDur) * premComAgent,
           premTotCom          = (formulaPartTwo / totalDur) * premTotCom,
           premOperationalCost = (formulaPartTwo / totalDur) * premOperationalCost,
           expo                = ifelse(extractDate > dateCoverageEnd, dateCoverageEnd - dateCoverageStart, extractDate - dateCoverageStart) / (dateCoverageEnd - dateCoverageStart) %>% as.numeric(),
           burningCost         = premNet - premComBroker - premComAgent - premOperationalCost,
           burningCostEarned   = burningCost * expo) %>%
    select(-totalDur) %>%
    rbind(salesDat %>% filter(coverageType != "gn"))
  salesDat %>% 
    head() %>%
    kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    scroll_box(width = "100%")
  
  ## Claims Data
  claimsDatRaw <- readr::read_csv(file = "inst/extdata/raw/claims_all.csv") %>%
    mutate(policyNumber = policyNumber %>% eaR::cleanNames(sep = "_"),
           coverType    = coverType    %>% eaR::cleanNames(sep = "_"),
           dateIncurred = dateIncurred %>% lubridate::dmy()) %>%
    filter(coverType != "assistance") %>%
    group_by(policyNumber, coverType, dateIncurred) %>%
    summarise(claimCount = n(),
              costTotal  = sum(costTotal))
  claimsDat <- claimsDatRaw %>%
    select(policyNumber, coverType, costTotal) %>%
    rename(trueCover = coverType) %>%
    group_by(policyNumber, trueCover) %>%
    summarise(claimCount = n(),
              claimCost  = sum(costTotal))
  claimsDat %>% 
    head() %>%
    kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    scroll_box(width = "100%")
  
  ## Merged Data
  finDatCasco <- cascoDat %>% 
    left_join(claimsDat) %>%
    mutate(claimCount = ifelse(is.na(claimCount), 0, claimCount))
  finDatEW <- ewDat %>% 
    left_join(claimsDat) %>%
    mutate(claimCount = ifelse(is.na(claimCount), 0, claimCount))
  
  coreDat <- cascoDat %>%
    select(policyNumber, dateCoverageStart, dateCoverageEnd)
  cascoDatUWView <- setDT(coreDat)[ , list(policyNumber = policyNumber, coverStart = eaR::seqlast(dateCoverageStart, dateCoverageEnd, by = "month")), by = 1:nrow(coreDat)] %>% 
    as.tibble() %>% 
    group_by(policyNumber) %>%
    mutate(term = 12 * ((cumsum(nrow) + 11) %/% 12)) %>%
    ungroup() %>%
    select(-nrow) %>%
    left_join(coreDat) %>%
    mutate(extractDt     = extractDate,
           startOfPeriod = coverStart,
           endOfPeriod   = pmin(coverStart %>% lubridate::ceiling_date("month") - 1, dateCoverageEnd, extractDt),
           periodExpo    = as.numeric(endOfPeriod - startOfPeriod + 1) / lubridate::days_in_month(startOfPeriod)) %>%
    select(c(policyNumber, startOfPeriod, term, endOfPeriod, periodExpo)) %>%
    filter(periodExpo > 0) %>%
    group_by(policyNumber) %>%
    mutate(totExpo = sum(periodExpo, na.rm = TRUE) %>% as.numeric())
  cascoDatUWView <- cascoDat %>%
    left_join(cascoDatUWView) %>%
    mutate(premGross           = (periodExpo / totExpo) * premGross,
           premComNeoent       = (periodExpo / totExpo) * premComNeoent,
           premNet             = (periodExpo / totExpo) * premNet,
           premComBroker       = (periodExpo / totExpo) * premComBroker,
           premComAgent        = (periodExpo / totExpo) * premComAgent,
           premTotCom          = (periodExpo / totExpo) * premTotCom,
           premOperationalCost = (periodExpo / totExpo) * premOperationalCost,
           expoPer             = (periodExpo / totExpo) * expo,
           burningCost         = (periodExpo / totExpo) * burningCost,
           burningCostEarned   = (periodExpo / totExpo) * burningCostEarned)
  cascoDatUWView <- sqldf::sqldf("SELECT a.policyNumber, a.trueCover, a.startOfPeriod, a.endOfPeriod,
                                       b.policyNumber AS polNo, b.coverType, b.dateIncurred, SUM(b.claimCount) AS claimCount, SUM(b.costTotal) AS costTotal
                                FROM cascoDatUWView a
                                LEFT JOIN claimsDatRaw b 
                                    ON a.policyNumber  = polNo           AND
                                       a.trueCover     = b.coverType     AND
                                       b.dateIncurred <= a.endOfPeriod   AND 
                                       b.dateIncurred >= a.startOfPeriod
                               GROUP BY a.policyNumber, a.trueCover, a.startOfPeriod, a.endOfPeriod") %>% 
    as.tibble() %>% select(-c(polNo, coverType, dateIncurred)) %>%
    left_join(cascoDatUWView)
  coreDat <- ewDat %>%
    select(policyNumber, dateCoverageStart, dateCoverageEnd)
  ewDatUWView <- setDT(coreDat)[ , list(policyNumber = policyNumber, coverStart = eaR::seqlast(dateCoverageStart, dateCoverageEnd, by = "month")), by = 1:nrow(coreDat)] %>% 
    as.tibble() %>% 
    group_by(policyNumber) %>%
    mutate(term = 12 * ((cumsum(nrow) + 11) %/% 12)) %>%
    ungroup() %>%
    select(-nrow) %>%
    left_join(coreDat) %>%
    mutate(extractDt     = extractDate,
           startOfPeriod = coverStart,
           endOfPeriod   = pmin(coverStart %>% lubridate::ceiling_date("month") - 1, dateCoverageEnd, extractDt),
           periodExpo    = as.numeric(endOfPeriod - startOfPeriod + 1) / lubridate::days_in_month(startOfPeriod)) %>%
    select(c(policyNumber, startOfPeriod, term, endOfPeriod, periodExpo)) %>%
    filter(periodExpo > 0) %>%
    group_by(policyNumber) %>%
    mutate(totExpo = sum(periodExpo, na.rm = TRUE) %>% as.numeric())
  ewDatUWView <- cascoDat %>%
    left_join(ewDatUWView) %>%
    mutate(premGross           = (periodExpo / totExpo) * premGross,
           premComNeoent       = (periodExpo / totExpo) * premComNeoent,
           premNet             = (periodExpo / totExpo) * premNet,
           premComBroker       = (periodExpo / totExpo) * premComBroker,
           premComAgent        = (periodExpo / totExpo) * premComAgent,
           premTotCom          = (periodExpo / totExpo) * premTotCom,
           premOperationalCost = (periodExpo / totExpo) * premOperationalCost,
           expoPer             = (periodExpo / totExpo) * expo,
           burningCost         = (periodExpo / totExpo) * burningCost,
           burningCostEarned   = (periodExpo / totExpo) * burningCostEarned)
  ewDatUWView <- sqldf::sqldf("SELECT a.policyNumber, a.trueCover, a.startOfPeriod, a.endOfPeriod,
                                       b.policyNumber AS polNo, b.coverType, b.dateIncurred, SUM(b.claimCount) AS claimCount, SUM(b.costTotal) AS costTotal
                                FROM ewDatUWView a
                                LEFT JOIN claimsDatRaw b 
                                    ON a.policyNumber  = polNo           AND
                                       a.trueCover     = b.coverType     AND
                                       b.dateIncurred <= a.endOfPeriod   AND 
                                       b.dateIncurred >= a.startOfPeriod
                               GROUP BY a.policyNumber, a.trueCover, a.startOfPeriod, a.endOfPeriod") %>% 
    as.tibble() %>% select(-c(polNo, coverType, dateIncurred)) %>%
    left_join(ewDatUWView)
  
  finDatUWView <- rbind(cascoDatUWView, ewDatUWView)
  print(Sys.time() - startTime)
  return(finDatUWView)
}