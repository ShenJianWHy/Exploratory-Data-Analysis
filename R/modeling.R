modeling <- function(data, target, features, exposure, family) {
  
  if(family =="gamma") {
    ModDat <- data %>% filter(!!sym(target) > 0)
    family <- Gamma(link="log")
  } else {
    ModDat <- data
    family <-poisson()
  }
  
  ModDat <- ModDat[,c(target, features, exposure)]
  ModDatDummy <- ModDat %>% caret::dummyVars(formula = "~.", fullRank = F)
  ModDat <- predict(ModDatDummy, ModDat) %>% as.data.frame()
  
  idx <- sample(seq_len(nrow(ModDat)), size = floor(0.8 * nrow(ModDat)))
  ModDatTrn <- ModDat[idx,]
  ModDatTst <- ModDat[-idx,]
  
  feat <- names(ModDat)
  feat <- feat[!feat %in% c(target, features, exposure)]

  formula <- paste(target, "~", paste(feat, collapse = "+"), "+offset(log(",exposure,"))") %>%
    as.formula()
  fit <- glm(formula, family = family, data = ModDat)
  
  
  pred <- predict(
    fit,
    newdata  = ModDatTst[c(feat, exposure)]
  )
  
  predAll <- predict(
    fit,
    newdata  = ModDat[c(feat, exposure)]
  )
  
  gini <- MLmetrics::NormalizedGini(y_pred = pred, y_true = ModDatTst[[target]])
  rmse <- RMSE(pred, ModDatTst[[target]])
  
  adjust <- sum(ModDat[target])/sum(predAll)
  
  return(list(
   model = fit,
   metrics = c("gini"=gini, "rmse"=rmse),
   prediction = pred,
   predictionAll = predAll,
   adjust = adjust
  ))
}



# rm(list=ls())
load("insData.rda")

freq_res <-
  modeling(
    data = SingaporeAuto,
    target = "Clm_Count",
    features = c("SexInsured", "VehicleType", "AutoAge0",  "VAgeCat"),
    # features = c("SexInsured", "VehicleType", "VAgeCat" ),
    exposure = "Exp_weights",
    family = "poisson"
  )
freq <- freq_res$model
sev_res <-
  modeling(
    data = SingaporeAuto,
    target = "claimSev",
    features = c("SexInsured", "VehicleType", "AutoAge0",  "VAgeCat"),
    # features = c("SexInsured", "VehicleType", "VAgeCat" ),
    exposure = "Clm_Count",
    family = "gamma"
  )
sev <- sev_res$model

freq_coef <- freq$coefficients %>% exp() %>% as.data.frame()
freq_coef$names <- rownames(freq_coef)
rownames(freq_coef)=NULL
names(freq_coef) = c("coeff_freq", "names")

sev_coef <- sev$coefficients %>% exp() %>% as.data.frame()
names(sev_coef) = "coeff_sev"

sev_coef <- sev$coefficients %>% exp() %>% as.data.frame()
sev_coef$names <- rownames(sev_coef)
rownames(sev_coef)=NULL
names(sev_coef) = c("coeff_sev", "names")


rateTbl <- freq_coef %>% full_join(sev_coef, by = "names") %>% 
  mutate(relsSev  = ifelse(is.na(coeff_sev),  1, coeff_sev),
         relsFreq = ifelse(is.na(coeff_freq), 1, coeff_freq),
         purePrem = relsSev * relsFreq) %>% 
  select(names, relsFreq, relsSev, purePrem)


rateTblAdj <- rateTbl %>% 
  mutate(
    purePrem = purePrem * sev_res$adjust * freq_res$adjust
  )
