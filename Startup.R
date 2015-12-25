temp.dir <- getwd()

library(MASS)
library(ggplot2) #ggplot2
library(RColorBrewer) 
library(grid)
library(plyr)
library(dplyr)
require(plyr)
require(dplyr)
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

OFFline <- 
  class(try(source("http://www7b.biglobe.ne.jp/~homunculus/Rsource/tameshi.txt"), silent = T)) == "try-error"
  #class(try(source_url(url = "https://raw.githubusercontent.com/KeachMurakami/Sources/master/NetworkTest.txt")), silent = T)) == "try-error"

if(.Platform$OS.type == "unix"){
  # startups for Mac
  system("defaults write org.R-project.R force.LANG en_US.UTF-8")
  # in order to avoid warning message
  # http://stackoverflow.com/questions/9689104/installing-r-on-mac-warning-messages-setting-lc-ctype-failed-using-c
  home.dir <- function() setwd("/Users/keach/Dropbox/R")
  library(slackr)
  slackr_setup(config_file = ".slackr")
  setwd(temp.dir)
} else {
  # startups for Win
  home.dir <- function() setwd("C:/Users/KeachMurakami/Dropbox/R")
}
home.dir()
}

options(help_type="html")
options(warn=1)

# set working directory "/dropbox/R"

if(OFFline){
  source("./Sources/functions.R")
  source("./Sources/labels.R")
  cat("#####warning######\nThe functions are read from local files\nIt might not be the latest ver.")  
} else {
  library(RCurl)
  eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/functions.R", ssl.verifypeer = FALSE)))
  eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/labels.R", ssl.verifypeer = FALSE)))
}