IntSmoother <-
  function(df, smooth){
    if(!smooth){
      return(df)
    } else {
      df %>%
        filter(abs(0.5 - wavelength %% 1) > 0.4) %>%
        mutate(wavelength2 = round(wavelength %% 1, 2),
               wavelength = round(wavelength, 0)) %>%
        spread(wavelength2, value) %>%
        transmute(wavelength, value = (`0.07` * 0.032 + `0.97` * 0.068) / 0.1)
    }
  }

OceanOpt <-
  function(files, sep = c("_"), smooth = TRUE){
    file_name <-
      files %>%
      str_split(pattern = sep) %>%
      .[[1]]
    
    Trtm <-
      file_name[1] %>%
      str_extract(., "[A-Za-z]+")
    
    PlantID <-
      file_name[1] %>%
      str_extract(., "[0-9]+")
    
    files %>%
      fread(skip = 63, nrows = 9129) %>%
      rename(wavelength = V1, value = V2) %>%
      IntSmoother(., smooth) %>%
      mutate(Trtm = Trtm, PlantID = PlantID)
  }
