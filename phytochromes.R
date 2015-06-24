library(RCurl)
txt <- getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/data/phytochrome.csv", ssl.verifypeer = FALSE)
write(txt, smp <- tempfile()) # write the Sager's data to temporary file
phy.equ <- read.csv(smp)
rm(smp, txt)

phytochome <- function(df){
  # comment
  if(1 == 2){
    "Arguments detail
    df: data.frame. first column must be wavelength.
    
    This function able to handle plural dataset like belew
    wavelength  data1   data2
    350         4       3
    351         5       2
    352         8       2
    353         10      3
    ...

    Outputs
    Outputs of this function are...
    List
    $phytochrome equilibrium
    $PPFD (photons in 400-700 nm)
    $PFD (photons in wavelength below 800 nm)
    $spectrum (ggplot2 figure.
    legend indicates the values of PFD, PPFD, and phytochromePSS.
        
    "
  }

  data.num <- dim(df)[2] - 1
  colnames(df) <- c("wavelength", paste("dat", 1:data.num, sep = ""))
  
  df0 <- join(phy.equ, df, by = "wavelength")
  
  PPS <-
    lapply(1:data.num, function(X){
      sig.r <- sum(dat[, (X + 3)] * dat[, 2], na.rm = TRUE)
      sig.fr <- sum(dat[, (X + 3)] * dat[, 3], na.rm = TRUE)
      equ <- sig.r / (sig.r + sig.fr)
      return(equ)
    }) %>%
    unlist()
  
#   PPFD <-
#     lapply(1:data.num, function(X){
#       dat %>%
#         filter(wavelength <= 700) %>%
#         filter(wavelength >= 400) %>%
#         .[, X + 3] %>%
#         sum() / 1000
#     }) %>%
#     unlist() %>%
#     round(digits = 1)
#   
#   PFD <-
#     lapply(1:data.num, function(X){
#       dat %>%
#         filter(wavelength <= 800) %>%
#         .[, X + 3] %>%
#         sum() / 1000
#     }) %>%
#     unlist() %>%
#     round(digits = 1)
#   
#   colnames(input) <- c("wavelength", paste(as.character(PFD[1:data.num]),
#                                            as.character(PPFD[1:data.num]),
#                                            as.character(phytochrome.equ[1:data.num]),
#                                            sep = "\n")
#   )
  
  spectrum <-
    input %>%
    melt(data = ., id.vars = "wavelength") %>%
    ggplot(data = ., aes(x = wavelength, y = value, color = variable, group = variable)) +
    theme(legend.position = "top",
          legend.text = element_text(size = 5)) +
    geom_line() + xlim(c(350, 850)) + ylim(c(0, ymax)) +
    xlab("wavelength [nm]") + ylab(uSPFD)
  
  results <- 
    list(PPS, spectrum) %>%
    set_names("Phtochrome PPS", "Spectrum") %>%
  
#   win.metafile(paste("from.", filename, ".emf", sep = ""),
#                width = 10, height = 12, family = font)
#   print(spectrum)
#   dev.off()
  
  return
  }
