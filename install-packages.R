packs <- c("tidytext", "tidyverse","ggplot2","dplyr","tibble","wordcloud",
           "stringr","widyr","janeaustenr","lexiconPT", "readxl",
           "tidyr","jsonlite","tm","e1071","gmodels","caret","reshape2", 
           "lubridate", "R.utils", "widyr", "ggraph", "keras", 
           "readr", "purrr", "caret", "tidymodels", "devtools",
           "googleLanguageR", "stopwords", "tensorflow", "randomForest",
           "SnowballC", "xgboost", "lubridate", "PerformanceAnalytics", 
           "fastDummies", "car", "olsrr", "nortest", "kableExtra",
           "jtools", "lmtest", "overdisp", "MASS", "plotly", "correlation",
           "glmnet", "forecast", "fitdistrplus", "moments", "car", 
           "xgboost", "reshape2", "syn", "httr", "questionr", "rvest", 
           "memoise", "ggside", "tidyquant", "viridis", "scales",
           "purrr", "ggplot2misc", "statmod", "tweedie", "univariateML")

if(sum(as.numeric(!packs %in% installed.packages())) != 0){
  installer <- packs[!packs %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(packs, require, character = T) 
} else {
  sapply(packs, require, character = T) 
}
Sys.setenv(GL_AUTH = "data/pc-api-5426176469399848792-157-c59489ab0207.json")

install.packages("ExtDist")
