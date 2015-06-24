
# calculate phytochrome equiribilium
phy.equ.pre <- read.xlsx("./calibration/phytochrome_data.xlsx", sheetIndex = 1)
phy.equ <-
  data.frame(wavelength = phy.equ.pre[, 1],
             coef.r = phy.equ.pre[, 2],
             coef.fr = phy.equ.pre[, 3])

phy.spectra <- function(filename, skip = 13, font = "serif", ymax = 15){
  # comment
  if(1 == 2){
    "Arguments detail
    filename: Filename of spectrum data from MS-720 (EIKO Seiki inc).
    skip: Number of comment row in the .csv file.
    Default value (13) is for MS-720.
    
    Outputs
    Outputs of this function are...
    List
    $phytochrome equilibrium
    $PPFD (photons in 400-700 nm)
    $PFD (photons in wavelength below 800 nm)
    $spectrum (ggplot2 figure.
    legend indicates the values of PFD, PPFD, and phyto. equ.
    
    File
    spectrum figure in emf format
    
    This function able to handle plural dataset like belew
    wavelength  data1   data2
    350         4       3
    351         5       2
    352         8       2
    353         10      3
    ...
    
    Future prospect
    check the range of wavelength and reflect the range in results 
    "
  }
  
  input <- 
    read.csv(filename, colClasses = "character", skip = skip) %>%
    colwise(as.numeric)(.)
  data.num <- dim(input)[2] - 1
  colnames(input) <- c("wavelength", paste("dat", 1:data.num, sep = ""))
  
  dat <- merge(phy.equ, input, by = "wavelength")
  
  phytochrome.equ <-
    lapply(1:data.num, function(X){
      sig.r <- sum(dat[, (X + 3)] * dat[, 2], na.rm = TRUE)
      sig.fr <- sum(dat[, (X + 3)] * dat[, 3], na.rm = TRUE)
      equ <- sig.r / (sig.r + sig.fr)
      return(equ)
    }) %>%
    unlist() %>%
    round(digits = 3)
  
  PPFD <-
    lapply(1:data.num, function(X){
      dat %>%
        filter(wavelength <= 700) %>%
        filter(wavelength >= 400) %>%
        .[, X + 3] %>%
        sum() / 1000
    }) %>%
    unlist() %>%
    round(digits = 1)
  
  PFD <-
    lapply(1:data.num, function(X){
      dat %>%
        filter(wavelength <= 800) %>%
        .[, X + 3] %>%
        sum() / 1000
    }) %>%
    unlist() %>%
    round(digits = 1)
  
  colnames(input) <- c("wavelength", paste(as.character(PFD[1:data.num]),
                                           as.character(PPFD[1:data.num]),
                                           as.character(phytochrome.equ[1:data.num]),
                                           sep = "\n")
  )
  
  spectrum <-
    input %>%
    melt(data = ., id.vars = "wavelength") %>%
    ggplot(data = ., aes(x = wavelength, y = value / 1000, color = variable, group = variable)) +
    theme_bw(20) +
    theme(legend.position = "top",
          legend.text = element_text(size = 5)) +
    geom_line() + xlim(c(350, 850)) + ylim(c(0, ymax)) +
    xlab("wavelength [nm]") + ylab(uSPFD)
  
  results <- list(phytochrome.equ, PPFD, PFD, spectrum)
  names(results) <- c("Phtochrome equilibrium", "PPFD (400-700 nm)", "PFD (350-800 nm)", "Spectrum")
  
  win.metafile(paste("from.", filename, ".emf", sep = ""),
               width = 10, height = 12, family = font)
  print(spectrum)
  dev.off()
  
  return(results)  
  print("metafile output was correctly operated")
  }
