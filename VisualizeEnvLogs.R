# Wrapper for readGL(s) and readMCH(s) visualization

EnvLog <-
  function(result_readXXX){
    
    ### readMCH doesnt contain 'ch' column
    if(sum(colnames(result_readXXX) == "ch") == 0){
      result_readXXX %<>%
        mutate(ch = "")
    }
    
    result_readXXX %>%
      ggplot(aes(x = Time, y = Mean, col = paste(ID, ch, DayNight), group = paste(ID, ch))) +
      geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = rel(0.5)) +
      geom_line() +
      geom_point(alpha = .5) +
      facet_grid(variable ~ ., scale = "free")
  }

EnvHist <-
  function(result_readXXX){
    
    ### readMCH doesnt contain 'ch' column
    if(sum(colnames(result_readXXX) == "ch") == 0){
      result_readXXX %<>%
        mutate(ch = "")
    }
    
    result_readXXX %>%
      mutate(label = paste0(ID, ch, ", ", DayNight)) %>%
      ggplot(aes(x = Mean, fill = label, group = label)) +
      geom_histogram(position = "identity", alpha = .5) +
      facet_grid(variable ~ .)
  }
