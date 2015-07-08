mean2 <- function(x) mean(x, na.rm = T)
sd2 <- function(x) sd(x, na.rm = TRUE)
se <- function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))
length2 <- function(x) sum(!is.na(x))

library(shiny)

# web scraping
library(RCurl)
library(googlesheets)
library(rdrop2)

# data handlings
library(plyr)
library(dplyr)
library(data.table)
library(stringr)
library(magrittr)
library(tidyr)
library(reshape2)


# visualize
library(ggplot2)
library(rCharts)
library(googleVis)

# statistical
library(agricolae)