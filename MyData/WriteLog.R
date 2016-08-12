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
  input <-
    fread(LogFile) %>%
    gather(Pub, words, -Date) %>%
    na.omit %>%
    separate(col = words, into = c("words", "comment"), sep = "_") %>%
    mutate(Date = mdy(Date),
           words = as.numeric(words))

    Task_all <- 
      input$Pub %>%
      unique %>%
      as.character
      
    PubName <-
      Vectorize(ifelse)(PubName == "", Task_all, PubName)
  
  input %>%  
    filter(Pub %in% PubName) %>%
    ggplot(aes(Date, words, group = Pub, col = Pub, label = comment)) +
    theme_bw() +
    theme(legend.position = "none",
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank()) +
    geom_line() +
    geom_point() +
    geom_text(aes(y = as.numeric(words) + 100)) +
    ylab("Words in the body")
  }