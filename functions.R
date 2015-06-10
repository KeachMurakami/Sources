summariser <- 
  function(df, labels = NULL){
  Label_name <- colnames(df)[labels]
  data_name <- colnames(df)[-labels]
  
  if(length(labels) == 1) {
    
    colnames(df)[labels] <- "MainTrtm" 
    
    df <- mutate_each(df, funs(as.character), matches("Trtm"))
    
    Stat <-
      df %>%
      group_by(MainTrtm) %>%
      summarise_each(funs(mean2, sd2, se, length2)) %>%
      arrange(MainTrtm) %>%
      ungroup
    
    Signif <-
      lapply(1:length(data_name), function(i){
        name <- data_name[i]
        formula <- formula(paste(name, " ~ MainTrtm"))
        df %>%
          aov(formula = formula, data = .) %>%
          HSD.test(y = ., trt = "MainTrtm") %>% .$groups %>%
          arrange(trt)
      }) %>%
      rbind_all %>%
      select(MainTrtm = trt, Tukey = M) %>%
      mutate(MainTrtm = str_trim(as.character(MainTrtm)), variables = name) %>%
      arrange(MainTrtm)
    
    Returns <-
      Stat %>%
      select(MainTrtm, mean2, sd2, se, length2) %>%
      join(., Signif, by = "MainTrtm", type = "full") %>%
      arrange(MainTrtm)
    
    colnames(Returns) <- c(Label_name, "ave", "SD", "SE", "n", "Tukey", "variables")
  } else {
    Group <- paste0("Trtm", 1:(length(labels) - 1))
    colnames(df)[labels][1] <- "MainTrtm" 
    colnames(df)[labels][-1] <- Group 
    
    df <- mutate_each(df, funs(as.character), matches("Trtm"))
    
    Stat <-
      df %>%
      tidyr::unite(data = .,
                   col = "Trtms",
                   eval(parse(text = paste0("c(", str_join(Group, collapse = ","), ")"))),
                   sep = ":", remove = F) %>%
      group_by_(.dots = as.list(c("MainTrtm", "Trtms", Group))) %>%
      summarise_each(funs(mean2, sd2, se, length2)) %>%
      arrange(Trtms, MainTrtm) %>%
      ungroup
    
    Signif <-
      lapply(1:length(data_name), function(i){
        name <- data_name[i]
        formula <- formula(paste(name, " ~ MainTrtm"))
        lapply(1:length(unique(Stat$Trtms)), function(j){
          # formulaを文字列で設定
          df %>%
            tidyr::unite(data = .,
                         col = "Trtms",
                         eval(parse(text = paste0("c(", str_join(Group, collapse = ","), ")"))),
                         sep = ":", remove = F) %>%
            filter(Trtms == unique(Stat$Trtms)[j]) %>%
            aov(formula = formula, data = .) %>%
            HSD.test(y = ., trt = "MainTrtm") %>% .$groups %>%
            arrange(trt) %>% mutate(Trtms = unique(Stat$Trtms)[j])
        }) %>%
          rbind_all %>%
          mutate(variables = name)
      }) %>%
      rbind_all %>%
      select(MainTrtm = trt, Trtms, Tukey = M, variables) %>%
      mutate(MainTrtm = str_trim(as.character(MainTrtm))) %>%
      arrange(Trtms, MainTrtm)
    
    Returns <-
      Stat %>%
      select(MainTrtm, Trtms, mean2, sd2, se, length2) %>%
      join(., Signif, by = c("MainTrtm", "Trtms"), type = "full") %>%
      arrange(Trtms)
    
    colnames(Returns) <- c("MainTrtm", "Trtms", "ave", "SD", "SE", "n", "Tukey", "variables") 
  }
  Returns
}