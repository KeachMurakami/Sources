temp.dir <- getwd()

library(MASS)
library(ggplot2)
library(RColorBrewer) 
library(grid)
library(plyr)
library(dplyr)
library(plyr)
library(dplyr)
library(reshape2)
library(xlsx)
library(knitr)
library(gtable)
library(foreach)
library(gridExtra)
library(magrittr)
library(agricolae)
library(stringr)
library(data.table)
library(lubridate)
library(scales)
library(pforeach)
library(devtools)
library(tidyr)
library(slackr)
library(RCurl)

readURL <- function(URL) eval(parse(text = getURL(URL, ssl.verifypeer = FALSE)))

OFFline <- class(try(source_url(url = "https://raw.githubusercontent.com/KeachMurakami/Sources/master/NetworkTest.txt"), silent = T)) == "try-error"
OSMac <- .Platform$OS.type == "unix"
if(OSMac){
  # startups for Mac
  system("defaults write org.R-project.R force.LANG en_US.UTF-8")
  # in order to avoid warning message
  # http://stackoverflow.com/questions/9689104/installing-r-on-mac-warning-messages-setting-lc-ctype-failed-using-c
  home.dir <- function() setwd("/Users/keach/Dropbox/R")
  slackr_setup(config_file = ".slackr")
} else {
  # startups for Win
  home.dir <- function() setwd("C:/Users/KeachMurakami/Dropbox/R")
}
home.dir()


options(help_type="html")
options(warn=1)

# set working directory "/dropbox/R"

if(OFFline){
  if(OSMac){
    # offline and Mac
    MyFunctions <- 
      dir("~/GitHub/BeeLabR/Sources/", pattern = "R$", full.names = T)
    MyFunctions[!str_detect(MyFunctions, pattern = "Startup")] %>%    
      a_ply(., .fun = "source", .margins = 1)
    cat("#####warning######\nThe functions are read from local files\nIt might not be the latest ver.")
  } else {
    # offline and Win
    cat("#####warning######\nThe functions are read from local files\nIt might not be the latest ver.")
  }
} else {
  # online
  MyFunctions <-
    getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/functions.txt") %>%
    str_split(pattern = "\n") %>%
    .[[1]] %>%
    {.[!str_detect(., pattern = "Startup")]}

  for(i in 1:length(MyFunctions)){
    funct <-
      MyFunctions[i] %>%
      paste0("https://raw.githubusercontent.com/KeachMurakami/Sources/master/", .)
    
    eval(parse(text = getURL(funct, ssl.verifypeer = FALSE)))
  }
  cat("#####loaded######\nThe latest ver. functions are loaded from github")
}

if(OSMac){
  # set font family for Mac
  quartzFonts(Hiragino = c("HiraMaruProN-W4", "HiraKakuStdN-W8", "HiraMinProN-W3", "HiraginoSansGB-W3")) 
  quartzFonts(YuGo = c("YuGo-Medium", "YuGo-Bold", "YuGo-Bold", "YuGo-Bold")) 
} else {
  # set font family for Win
}
setwd(temp.dir)
