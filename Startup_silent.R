### to reduce knit time

  require(MASS)
  require(ggplot2)
  require(RColorBrewer) 
  require(grid)
  require(plyr)
  require(dplyr)
  require(reshape2)
  require(xlsx)
  require(knitr)
  require(gtable)
  require(foreach)
  require(gridExtra)
  require(magrittr)
  require(agricolae)
  require(stringr)
  require(data.table)
  require(lubridate)
  require(scales)
  require(pforeach)
  require(devtools)
  require(tidyr)
  require(slackr)
  require(RCurl)
  require(broom)
  library(latex2exp)
  
  OFFline <- F
  home.dir <- function() setwd("~/Dropbox/R")
  
  # install required version {rmarkdown} for MRO, only the first time.
  # install.packages("rmarkdown", repos = "https://cran.revolutionanalytics.com") 
  
  # startups for Mac
  system("defaults write org.R-project.R force.LANG en_US.UTF-8")
  # in order to avoid warning message
  # http://stackoverflow.com/questions/9689104/installing-r-on-mac-warning-messages-setting-lc-ctype-failed-using-c
  
  # set font family for Mac
  quartzFonts(Hiragino = c("HiraMaruProN-W4", "HiraKakuStdN-W8", "HiraMinProN-W3", "HiraginoSansGB-W3"))
  quartzFonts(YuGo = c("YuGo-Medium", "YuGo-Bold", "YuGo-Bold", "YuGo-Bold"))
  
  
  options(help_type="html")
  options(warn=1)
  
  # set working directory "/dropbox/R"
  
  # online
  MyFunctions <-
    getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/functions.txt") %>%
    str_split(pattern = "\n") %>%
    .[[1]] %>%
    {.[!str_detect(., pattern = "Startup")]} %>%
    paste0("https://raw.githubusercontent.com/KeachMurakami/Sources/master/", .)
  MyFunctions[!str_detect(MyFunctions, pattern = "Startup")] %>%
    a_ply(., .fun = source_url, .margins = 1)
  