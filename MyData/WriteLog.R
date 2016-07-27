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

WriteLog <- function(PubName = ""){
  if(PubName == "") stop()
  fread("~/Dropbox/R/my.data/MyWrite.csv") %>%
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
    geom_point()
}