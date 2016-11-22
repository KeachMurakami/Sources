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
require(plotly)

WriteLog <- function(PubName = "", LogFile = "~/Dropbox/R/my.data/MyWrite.csv", smooth = F, comment = T){
  def_warn <- options()$warn
  options(warn = -1)
  input <-
    fread(LogFile) %>%
    gather(Pub, words, -Date) %>%
    na.omit %>%
    separate(col = words, into = c("words", "comment"), sep = "_") %>%
    mutate(Date = ymd(Date),
           words = as.numeric(words))

    Task_all <- 
      input$Pub %>%
      unique %>%
      as.character
      
    PubName <-
      Vectorize(ifelse)(PubName == "", Task_all, PubName)

    input <-  
      input %>%  
      filter(Pub %in% PubName) %>%
      na.omit
    
    Fig <-
      input %>%
      ggplot(aes(Date, words, group = Pub, col = Pub, label = comment)) +
      theme_bw() +
      theme(legend.position = "none",
            legend.key = element_blank(),
            legend.title = element_blank(),
            legend.background = element_blank()) +
      geom_line() +
      geom_point() +
      ylab("Words in the body")
    
    if(comment){
      Fig <-
        Fig + geom_text(aes(y = as.numeric(words) + 100))
    }
    # if(smooth){
    #   Fig <-
    #     Fig +
    #     geom_smooth(method = "loess")
    # }
  options(warn = def_warn)
  return(Fig)
  # ggplotly(Fig)
  }