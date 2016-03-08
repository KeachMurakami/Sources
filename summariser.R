summariser <- function(df = ., labels = NULL){
#   # comment
#   if(1 == 2){
#     "
#     Arguments detail
#     df: input data (df)
#     labels: Number of columns which contain factors (Treatment, variable conditions, etc).
#     + If the experiment has single level
#       -> 'labels' is NULL (default)
#     + If the experiment has single factor (several levels),
#      -> 'labels' is the column number (ex. 1, c(7)).
#     + If the experiment has several factor (several levels)
#      -> 'labels' are the column number (ex. 1:2, c(1, 9)).
#     *** The first column will be used for HSD test grouping. ***
#
#     Outputs: Returns (df)
#     Ave, SD, SE, sample size, significance between the treatment (by Tukey test)
#     
#     Future prospect
#     Handle other statistical test.
#     replace c(a, a, a) to c("", "", "") in Tukey.  
#     "
#   } 

DefaultWarnSetting <- options()$warn
options(warn = -1)
  
Label_name <- colnames(df)[labels]
data_name <- colnames(df)[-labels]

if(length(labels) == 1L) {
  
  colnames(df)[labels] <- "MainTrtm" 
  
  df_group <- 
    mutate_each(df, funs(as.character), matches("Trtm")) %>%
    group_by(MainTrtm)
  
  ave <-
    df_group %>%
    summarise_each(funs(mean2)) %>%
    melt(id.vars = c("MainTrtm"), value.name = "ave")
  SD <-
    df_group %>%
    summarise_each(funs(sd2)) %>%
    melt(id.vars = c("MainTrtm"), value.name = "SD")
  SE <-
    df_group %>%
    summarise_each(funs(se)) %>%
    melt(id.vars = c("MainTrtm"), value.name = "SE")
  sample_size <-
    df_group %>%
    summarise_each(funs(length2)) %>%
    melt(id.vars = c("MainTrtm"), value.name = "n")
  
  Stat <-
    join(ave, SD, by = c("MainTrtm", "variable")) %>%
    join(., SE, by = c("MainTrtm", "variable")) %>%
    join(., sample_size, by = c("MainTrtm", "variable")) %>%
    mutate(variable = as.character(variable))  
  
  Signif <-
    lapply(1:length(data_name), function(i){
      name <- data_name[i]
      formula <- formula(paste(name, " ~ MainTrtm"))
      df %>%
        aov(formula = formula, data = .) %>%
        HSD.test(y = ., trt = "MainTrtm") %>% .$groups %>%
        mutate(variable = name)
    }) %>%
    rbind_all %>%
    transmute(MainTrtm = str_trim(as.character(trt)),
              Tukey = M,
              variable = variable) %>%
    arrange(MainTrtm)
  
  Returns <-
    Stat %>%
    join(., Signif, by = c("MainTrtm", "variable"), type = "full") %>%
    arrange(MainTrtm) %>%
    set_names(c(Label_name, "variable", "ave", "SD", "SE", "n", "Tukey"))
} else {
  Group <- paste0("Trtm", 1:(length(labels) - 1))
  colnames(df) <- c("MainTrtm", Group, data_name) 
  
  df <- mutate_each(df, funs(as.character), matches("Trtm"))
  
  df_group <-
    df %>%
    tidyr::unite(data = .,
                 col = "Trtms",
                 eval(parse(text = paste0("c(", str_c(Group, collapse = ","), ")"))),
                 sep = ":", remove = F) %>%
    select(-matches(paste0("Trtm", "[0-9]$"))) %>%
    group_by_(.dots = as.list(c("MainTrtm", "Trtms")))
  
  ave <-
    df_group %>%
    summarise_each(funs(mean2)) %>%
    melt(id.vars = c("MainTrtm", "Trtms"), value.name = "ave")
  SD <-
    df_group %>%
    summarise_each(funs(sd2)) %>%
    melt(id.vars = c("MainTrtm", "Trtms"), value.name = "SD")
  SE <-
    df_group %>%
    summarise_each(funs(se)) %>%
    melt(id.vars = c("MainTrtm", "Trtms"), value.name = "SE")
  sample_size <-
    df_group %>%
    summarise_each(funs(length2)) %>%
    melt(id.vars = c("MainTrtm", "Trtms"), value.name = "n")
  
  Stat <-
    join(ave, SD, by = c("MainTrtm", "Trtms", "variable")) %>%
    join(., SE, by = c("MainTrtm", "Trtms", "variable")) %>%
    join(., sample_size, by = c("MainTrtm", "Trtms", "variable")) %>%
    mutate(variable = as.character(variable))
  
  Signif <-
    lapply(1:length(data_name), function(i){
      name <- data_name[i]
      formula <- formula(paste(name, " ~ MainTrtm"))
      lapply(1:length(unique(Stat$Trtms)), function(j){
        temp <-
          df %>%
          tidyr::unite(data = .,
                       col = "Trtms",
                       eval(parse(text = paste0("c(", str_c(Group, collapse = ","), ")"))),
                       sep = ":", remove = F) %>%
          filter(Trtms == unique(Stat$Trtms)[j])
        if(n_distinct(temp$MainTrtm) == 1){
          temp %>%
            slice(1) %>%
            transmute(trt = MainTrtm, M = " ", Trtms = Trtms) %>%
            data.frame(., mean = mean(temp[, name])) %>%
            select(trt, means = mean, M, Trtms)
        } else {
          temp %>%
            aov(formula = formula, data = .) %>%
            HSD.test(y = ., trt = "MainTrtm") %>% .$groups %>%
            arrange(trt) %>% mutate(Trtms = unique(Stat$Trtms)[j])
          # if(n_distinct(temp$M) == 1){ temp$M <- " " }
        }            
      }) %>%
        rbind_all %>%
        mutate(variable = name)
    }) %>%
    rbind_all %>%
    select(MainTrtm = trt, Trtms, variable, Tukey = M) %>%
    mutate(MainTrtm = str_trim(as.character(MainTrtm)))
  
  options(warn = DefaultWarnSetting)
  
  Returns <-
    join(Stat, Signif, by = c("MainTrtm", "Trtms", "variable")) %>%
    tidyr::separate(data = ., col = Trtms, 
                    into = paste0(Label_name[-1]),
                    sep = ":", remove = T)
  colnames(Returns)[1] <- Label_name[1]
  }
  Returns %>%
  return
}
