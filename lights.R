PPS <-
  function(df, digits = 3){
  lapply(1:(dim(df)[2] - 1), function(X){
    df <- merge(phy.equ, df, by = "wavelength")
    sig.r <- sum(df[, (X + 3)] * df[, 2], na.rm = TRUE)
    sig.fr <- sum(df[, (X + 3)] * df[, 3], na.rm = TRUE)
    equ <- sig.r / (sig.r + sig.fr)
    return(equ)
  }) %>%
    unlist() %>%
    round(digits = digits)  
}

PFD <-
  function(df, from = 350, to = 800, digits = 1){
  lapply(1:(dim(df)[2] - 1), function(X){
    df %>%
      filter(wavelength >= from, wavelength <= to) %>%
      .[, X + 1] %>%
      sum() / 1000
  }) %>%
  unlist() %>%
  round(digits = digits)
}

PPFD <- function(df, digits = 1) PFD(df, 400, 700, digits)

LIGHT <- function(df, digits = c(1,1,3)){
  colnames(df)[1] <- "wavelength"
  rbind(PPFD(df, digits[1]),
        PFD(df, digits = digits[2]),
        PPS(df, digits = digits[3])
        ) %>%
    as.data.frame %>%
    cbind(label = c("PPFD", "PFD", "PPS"), .)
}