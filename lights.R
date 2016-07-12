if(OFFline){
  tmp_dir <- getwd(); home.dir()
  phy.equ <-
    read.csv("~/Dropbox/R/Sources/data/phytochrome.csv")
  setwd(tmp_dir)
} else {
  txt <-
    getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/data/phytochrome.csv", ssl.verifypeer = FALSE)
  write(txt, smp <- tempfile()) # write the Sager's data to temporary file
  phy.equ <- read.csv(smp)
  
  rm(smp, txt)
}


PPS <-
  function(df, digits = 3){
    colnames(df)[1] <- "wavelength"
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
    colnames(df)[1] <- "wavelength"
    lapply(1:(dim(df)[2] - 1), function(X){
      df %>%
        filter(between(wavelength, from, to)) %>%
        .[, X + 1] %>%
        sum() / 1000
    }) %>%
      unlist() %>%
      round(digits = digits)
  }

PPFD <- function(df, digits = 1) PFD(df, 400, 700, digits)

LIGHT <-
  function(df, digits = c(1,1,3)){
    colnames(df)[1] <- "wavelength"
    rbind(PPFD(df, digits[1]),
          PFD(df, digits = digits[2]),
          PPS(df, digits = digits[3])
    ) %>%
      as.data.frame %>%
      cbind(label = c("PPFD", "PFD", "PPS"), .)
  }

SPD <-
  function(df, from = 350, to = 800, ...){
    colnames(df)[1] <- "wavelength"
    df %>%
      melt2(1) %>%
      ggplot(aes(x = wavelength, y = value, group = variable, col = variable, ...)) +
      geom_line() +
      xlab("wavelength [nm]") + xlim(c(from, to))
  }

SPDnorm <- 
  function(df, from = 350, to = 800, integratedPFD = 100){
    if(colnames(df)[1] != "wavelength") stop("1st column must be 'wavelength'")
    
    df <- as.data.frame(df)
    
    light_number <-
      dim(df)[2] - 1
    Sums <-
      colwise(sum)(df) %>%
      unlist
    
    for(i in 2:(light_number+1)){
      df[, i] <- df[, i] * integratedPFD / Sums[i]
    }
    
    df %>%
      return
  }
