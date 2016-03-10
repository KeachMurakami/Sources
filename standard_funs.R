mean2 <- 
  function(x) mean(x, na.rm = TRUE)

sum2 <-
  function(x) sum(x, na.rm = TRUE)

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
    
    #input df was converted into data.table for speed
    
    if(length(cols) >= dim(df)[2]) stop(message = "undefined columns selected")

    cols %>%
    as.numeric %>%
    colnames(df)[.] %>%
    data.table::melt(as.data.table(df), id.vars = ., ...)
  }

rbind_all2 <-
  function(df) {
    temp <- options()$warn
    options(warn = -1)
    res <-
      rbind_all(df) %>%
      as.data.table
    options(warn = temp)
    return(res)
  }


`%!in%` <-
  function (x, table){
    !match(x, table, nomatch = 0L) > 0L
  }
