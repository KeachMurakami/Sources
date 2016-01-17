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
}

rm(smp, txt, tmp_dir)

phytochome <- 
  function(df, fill = TRUE, alpha = 0.5, waveband = c(350, 850), peakPFD = 10){
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
    
    labels <- colnames(df) 
    data.num <- dim(df)[2] - 1
    colnames(df) <- c("wavelength", paste("dat", 1:data.num, sep = ""))
    
    df0 <-
      join(phy.equ, df, by = "wavelength") %>%
      na.omit
    
    PPS <-
      lapply(1:data.num, function(X){
        sig.r <- sum(df0[, (X + 3)] * df0[, 2], na.rm = TRUE)
        sig.fr <- sum(df0[, (X + 3)] * df0[, 3], na.rm = TRUE)
        equ <- sig.r / (sig.r + sig.fr)
        return(equ)
      }) %>%
      unlist()
    
    if(fill == TRUE){
      spectrum <- 
        df %>%
        set_names(labels) %>%
        melt(data = ., id.vars = "wavelength") %>%
        ggplot(data = ., aes(x = wavelength, y = value, col = variable, fill = variable, group = variable)) +
        theme(legend.position = c(0.1,0.8),
              legend.text = element_text(size = 5)) +
        geom_area(alpha = .5, position = "identity") +
        coord_cartesian(xlim = waveband, ylim = c(0, peakPFD)) +
        xlab("wavelength [nm]") + ylab(expression(paste("spectral photon flux density [", mu, "mol ",  m^-2, "", s^-1, "]")))
    } else {
      spectrum <-
        df %>%
        set_names(labels) %>%
        melt(data = ., id.vars = "wavelength") %>%
        ggplot(data = ., aes(x = wavelength, y = value, color = variable, group = variable)) +
        theme(legend.position = c(0.1,0.8),
              legend.text = element_text(size = 5)) +
        geom_line() +
        coord_cartesian(xlim = waveband, ylim = c(0, peakPFD)) +
        xlab("wavelength [nm]") + ylab(expression(paste("spectral photon flux density [", mu, "mol ",  m^-2, "", s^-1, "]")))
    }
    
    
    
    list(PPS, spectrum) %>%
      set_names(., c("Phtochrome PSS", "Spectrum")) %>%
      
      #   win.metafile(paste("from.", filename, ".emf", sep = ""),
      #                width = 10, height = 12, family = font)
      #   print(spectrum)
      #   dev.off()
      
      return
    }