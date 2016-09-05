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

distinct2 <-
  function(.data, ...) dplyr::distinct(.data, ..., .keep_all = T)


`%!in%` <-
  function (x, table){
    !match(x, table, nomatch = 0L) > 0L
  }
