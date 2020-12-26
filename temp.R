library(insuranceData)
library(dplyr)
library(caret)


data(SingaporeAuto)
SingaporeAuto$claimSev <- rgamma(n = nrow(SingaporeAuto), shape = 3, scale = 250)
SingaporeAuto$claimSev[SingaporeAuto$Clm_Count == 0] <- 0

# glm sev

SingaporeAuto <- SingaporeAuto %>% filter(claimSev > 0)
targetFreq     <- "claimSev"
feats          <- c("SexInsured", "VehicleType", "AutoAge0",  "VAgeCat")
expo           <- "Clm_Count"

ModDatFreq       <- SingaporeAuto[,c(targetFreq, feats, expo)]
ModDatDummyFreq  <- ModDatFreq %>% dummyVars(formula = "~.", fullRank = F)
ModDatFreq       <- predict(ModDatDummyFreq, ModDatFreq) %>% as.data.frame()

feat <- names(ModDatFreq)
feat <- feat[!feat %in% c(targetFreq, expo)]

formula <- paste(targetFreq, "~", paste(feat, collapse = "+"), "+offset(log(",expo,"))") %>% 
  as.formula()
fit <- glm(formula, family = Gamma(link="log"), data = ModDatFreq)

summary(fit)


# sev ---------------------------------------------------------------------

SingaporeAuto <- SingaporeAuto %>% filter(claimSev > 0)
targetFreq     <- "claimSev"
feats          <- c("SexInsured", "VehicleType", "AutoAge0",  "VAgeCat")
expo           <- "Clm_Count"

ModDatFreq       <- SingaporeAuto[,c(targetFreq, feats, expo)]
ModDatDummyFreq  <- ModDatFreq %>% dummyVars(formula = "~.", fullRank = F)
ModDatFreq       <- predict(ModDatDummyFreq, ModDatFreq) %>% as.data.frame()

set.seed(2020)

SplitIndexFreq <-
  createDataPartition(ModDatFreq[[targetFreq]],
                      p     = 0.8,
                      list  = FALSE,
                      times = 1)

TrainFreqDF    <- ModDatFreq[SplitIndexFreq,]
TestFreqDF     <- ModDatFreq[-SplitIndexFreq,]
FinFeatsFreq   <- names(TrainFreqDF)[!(names(TrainFreqDF) %in% c(targetFreq, expo))]


fit <- glmnet(
  x      = as.matrix(TrainFreqDF[FinFeatsFreq]),
  y      = TrainFreqDF[[targetFreq]],
  family = "gaussian",
  offset = TrainFreqDF[[expo]]
)

summary(fit)
plot(fit)

cvfit <- cv.glmnet(
  x      = as.matrix(TrainFreqDF[FinFeatsFreq]),
  y      = TrainFreqDF[[targetFreq]],
  family = "gaussian",
  offset = TrainFreqDF[[expo]]
)

# plot(cvfit)

opt.lam = c(cvfit$lambda.min, cvfit$lambda.1se)
coef(cvfit, s = opt.lam)
pred <- predict(
  fit,
  newx = as.matrix(TestFreqDF[FinFeatsFreq]),
  type = "response",
  s = opt.lam,
  newoffset = TestFreqDF[[expo]]
)
pred <- pred %>% as.data.frame()

gini <- MLmetrics::NormalizedGini(y_pred = pred$`1`, y_true = TestFreqDF[[targetFreq]])
rmse <- RMSE(pred$`1`, TestFreqDF[[targetFreq]])




# freq --------------------------------------------------------------------

targetFreq     <- "Clm_Count"
feats          <- c("SexInsured", "VehicleType", "AutoAge0",  "VAgeCat")
expo           <- "Exp_weights"


ModDatFreq       <- SingaporeAuto[,c(targetFreq, feats, expo)]
ModDatDummyFreq  <- ModDatFreq %>% dummyVars(formula = "~.", fullRank = F)
ModDatFreq       <- predict(ModDatDummyFreq, ModDatFreq) %>% as.data.frame()

set.seed(2020)

SplitIndexFreq <-
  createDataPartition(ModDatFreq[[targetFreq]],
                      p     = 0.8,
                      list  = FALSE,
                      times = 1)

TrainFreqDF    <- ModDatFreq[SplitIndexFreq,]
TestFreqDF     <- ModDatFreq[-SplitIndexFreq,]
FinFeatsFreq   <- names(TrainFreqDF)[!(names(TrainFreqDF) %in% c(targetFreq, expo))]

fit <- glmnet(
  x      = as.matrix(TrainFreqDF[FinFeatsFreq]),
  y      = TrainFreqDF[[targetFreq]],
  family = "poisson",
  offset = TrainFreqDF[[expo]]
)

plot(fit)

cvfit <- cv.glmnet(
  x      = as.matrix(TrainFreqDF[FinFeatsFreq]),
  y      = TrainFreqDF[[targetFreq]],
  family = "poisson",
  offset = TrainFreqDF[[expo]]
)

# plot(cvfit)

opt.lam = c(cvfit$lambda.min, cvfit$lambda.1se)
coef(cvfit, s = opt.lam)
pred <- predict(
  fit,
  newx = as.matrix(TestFreqDF[FinFeatsFreq]),
  type = "response",
  s = opt.lam,
  newoffset = TestFreqDF[[expo]]
)
pred <- pred %>% as.data.frame()

gini <- MLmetrics::NormalizedGini(y_pred = pred$`1`, y_true = TestFreqDF[[targetFreq]])
rmse <- RMSE(pred$`1`, TestFreqDF[[targetFreq]])


