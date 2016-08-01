# MyWrite
require(ggplot2)
require(plyr)
require(dplyr)
require(knitr)
require(magrittr)
require(stringr)
require(data.table)
require(lubridate)
require(tidyr)

WriteLog <- function(PubName = "", LogFile = "~/Dropbox/R/my.data/MyWrite.csv"){
  if(PubName == ""){
    fread(LogFile) %>%
    gather(Pub, words, -Date) %>%
    na.omit %>%
    mutate(Date = mdy(Date)) %>%
    ggplot(aes(Date, words, group = Pub, col = Pub)) +
    theme_bw() +
    theme(legend.position = c(.8, .4),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank()) +
    geom_line() +
    geom_point() +
    ylab("Words in the body")
  } else {
  fread(LogFile) %>%
    gather(Pub, words, -Date) %>%
    na.omit %>%
    mutate(Date = mdy(Date)) %>%
    filter(Pub %in% PubName) %>%
    ggplot(aes(Date, words, group = Pub, col = Pub)) +
    theme_bw() +
    theme(legend.position = c(.8, .4),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank()) +
    geom_line() +
    geom_point() +
    ylab("Words in the body")
  }
}