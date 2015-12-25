mean2 <- function(x) mean(x, na.rm = T)
sd2 <- function(x) sd(x, na.rm = TRUE)
se <- function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))
length2 <- function(x) sum(!is.na(x))