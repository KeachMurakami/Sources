mean2 <- 
  function(x) mean(x, na.rm = T)

sd2 <- 
  function(x) sd(x, na.rm = TRUE)

se <- 
  function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

length2 <- 
  function(x) sum(!is.na(x))

melt2 <- 
  function(df, cols, ...) {
    # syntax sugar for melt
    # input numerics to id.vars
    
    if(length(cols) >= dim(df)[2]) stop(message = "undefined columns selected")

    cols %>%
    as.numeric %>%
    colnames(df)[.] %>%
    reshape2::melt(df, id.vars = ., ...)
  }