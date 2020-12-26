library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(bs4Dash)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(GGally)
library(pushbar)
library(rlang)
library(pivottabler)
library(tidyr)
library(skimr)
library(visdat)
library(naniar)
library(lubridate)
library(rlang)
library(plotly)
library(ggmosaic)
library(ggpubr)
library(forcats)
library(histogram)
library(rhandsontable)
library(shinydashboardPlus)
library(scales)
library(summarytools)
library(insuranceData)
library(caret)
library(glmnet)
library(iml)

# rm(list = ls())
# files.sources = paste0("R/", list.files(path = "R"))
# sapply(files.sources, source, local = TRUE)


js4fixhightlightBug <- '
$(document).on("shiny:value", function(e){
  if(e.name === "tsAnalysis-tsPlot"){
    setTimeout(function(){
      $("#tsAnalysis-tsPlot").prev().children()[1].style.display = "none";
    }, 0);
  }
});
'


user_base <- data_frame(
  user = c("1", "user2"),
  password = c("1", "pass2"), 
  password_hash = sapply(c("1", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

# load("allData.rda")

# load("df_10000.rda")
load("df_10000a.rda")
varVec <- c("target", "features", "exposure", "date")

options(shiny.autoreload = T)
#options(shiny.autoreload.pattern = glob2rx("ui.R"))
options(shiny.maxRequestSize=50*1024^2)




