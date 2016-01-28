readMCH <- function(ID, StartDay, EndDay,
                    StartTime = "150000", EndTime = "150000",
                    ONtime = 7, OFFtime = 23,
                    LogPath = "~/Dropbox/MCH_log/"){
  
  
  # ID: chr
  #   logger's ID
  #
  # ch: num
  #   channel
  # 
  # StartDay, EndDay: num 150911 (2015.9.11.)
  # 
  # ONtime, OFFtime:
  # lighting span. default: ONtime = 7, OFFtime = 23
  # 
  # StartTime, EndTime: chr "150000" (15:00:00)
  
  # Future prospect
  # read data from cloud drive
  
  library(ggplot2)
  library(tidyr)
  library(data.table)
  library(lubridate)
  
  # read calibration data
  Calb <- 
    dir(LogPath, pattern = "Calb|calb", full.names = T) %>%
    fread %>%
    filter(MCH_ID == as.numeric(ID)) %>%
    select(starts_with("Slope"), starts_with("Intercept")) %>%
    unlist
  
  # suppress warning for tidyr::separate
  warn.value <- as.numeric(options("warn"))
  options(warn = -1)
  
  
  # distinguish day and night time from hour
  DNdet <- function(Hour, ONtime, OFFtime){
    Hour <- as.numeric(Hour)
    ifelse(test = (Hour >= ONtime && Hour < OFFtime) , yes = "Day", no = "Night")
  }
  
  # data folder path maker
  DataPath <- function(ID){
    paste0(LogPath, "No", formatC(ID, digits = 1, flag = "0"), "/")
  }
  
  #################
  # data handling #
  #################
  
  AllDataDirs <-
    DataPath(ID) %>%
    dir %>%
    as.numeric
  
  # three cases can be for reading
  # 1) measurement is started during logging (StartDay != any(AllDataDirs))
  # 2) measurement is started with logging start (StartDay == one(AllDataDirs))
  # 3) no data files
  
  inRanges <-
    between(AllDataDirs, StartDay, EndDay) %>%  
    which %>%
    range
  
  if(is.infinite(inRanges[1])) {
    DataDirs <-
      (AllDataDirs < StartDay) %>%
      sum %>%
      AllDataDirs[.] %>%
      paste0(DataPath(ID), ., "/") %>%
      dir(full.names = TRUE)
  } else {
    Used <-
      AllDataDirs[(inRanges[1] - 1):inRanges[2]]
    DataDirs <-
      AllDataDirs[(inRanges[1] - 1):inRanges[2]] %>% # contains (min - 1 ~ max)
      paste0(DataPath(ID), ., "/") %>%
      dir(full.names = TRUE)
    rm(Used)
  }
  
  # if there is no data in the target span
  if(length(DataDirs) == 0){
    print("no data files detected")
    break
  }
  
  CutFrom <- ymd_hms(paste0(StartDay, " ", StartTime), tz = Sys.timezone())
  CutTill <- ymd_hms(paste0(EndDay, " ", EndTime), tz = Sys.timezone())
  
  Raw <-
    lapply(1:length(DataDirs), function(i){
      if (class(try(fread(input = DataDirs[i]), silent = TRUE)) == "try-error") {
        # if error in fread(), break loop
        data.frame(Time = "NA", val = NA) %>%
          return
      } else {
        temp <-
          fread(input = DataDirs[i])
        
        temp %>%
          select(Date, Time, ends_with("Value")) %>%
          setnames(c("Day", "Time", "RH", "Temp", "CO2")) %>%
          return
      }
    }) %>%
    rbind_all %>%
    filter(CO2 != 0) %>%
    na.omit %>%
    separate(col = Time, into = c("Hour", "Min", "Sec"), sep = ":", remove = F) %>%
    mutate(Time = paste0(Day, " ", Time),
           LightOn = ONtime, LightOff = OFFtime,
           Time = ymd_hms(Time, tz = Sys.timezone()),
           DayNight = Vectorize(DNdet)(Hour, LightOn, LightOff), # day night
           RH = (RH - Calb[4]) / Calb[1], # calbrate
           Temp = (Temp - Calb[5]) / Calb[2], # calbrate
           CO2 = (CO2 - Calb[6]) / Calb[3] # calbrate
    ) %>%
    select(-starts_with("Light")) %>%
    filter(between(Time, CutFrom, CutTill)) %>% # extract data between (StartTime and EndTime)
    mutate(ID = ID, start = CutFrom, end = CutTill) # memos for subsequent data analysis
  
  Raw %>%
{
  # hourly
  Hourly <<-
    mutate(., DayHour = paste0(Day, " ", Hour)) %>%
    group_by(ID, DayNight, Day, Hour, DayHour) %>%
    summarise(MeanRH = mean(RH), SDRH = sd(RH),
              MeanTemp = mean(Temp), SDTemp = sd(Temp),
              MeanCO2 = mean(CO2), SDCO2 = sd(CO2)) %>%
    ungroup %>%
    mutate(Time = paste0(DayHour, "-00-00"),
           Time = ymd_hms(Time)) 
  # daily
  Daily <<-
    group_by(., ID, DayNight, Day) %>%
    summarise(MeanRH = mean(RH), SDRH = sd(RH),
              MeanTemp = mean(Temp), SDTemp = sd(Temp),
              MeanCO2 = mean(CO2), SDCO2 = sd(CO2)) %>%
    ungroup %>%
    mutate(Time = ymd(Day))
  # all span
  FullSpan <<-
    group_by(., ID, DayNight) %>%
    summarise(MeanRH = mean(RH), SDRH = sd(RH),
              MeanTemp = mean(Temp), SDTemp = sd(Temp),
              MeanCO2 = mean(CO2), SDCO2 = sd(CO2)) %>%
    ungroup
}

options(warn = warn.value) # reset warning

Raw %>%
  select(ID, DayNight, RH, CO2, Temp, Time) %>%
  list(Raw = ., Hourly = Hourly, Daily = Daily, Span = FullSpan,
       inputs = list(ID = ID, StartDay = StartDay, EndDay = EndDay,
                     StartTime = StartTime, EndTime = EndTime, ONtime, OFFtime, LogPath)) %>%
  return
}

# > sessionInfo()
# R version 3.1.2 (2014-10-31)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] datasets  grid      utils     stats     graphics  grDevices methods   base     
# 
# other attached packages:
# [1] lubridate_1.3.3    tidyr_0.3.1        data.table_1.9.6   RCurl_1.95-4.7     bitops_1.0-6      
# [6] stringr_1.0.0      agricolae_1.2-3    GGally_0.5.0       magrittr_1.5       gridExtra_2.0.0   
# [11] foreach_1.4.3      gtable_0.1.2       knitr_1.10.5       xlsx_0.5.7         xlsxjars_0.6.1    
# [16] rJava_0.9-7        reshape2_1.4.1     dplyr_0.4.3        plyr_1.8.3         mvtnorm_1.0-3     
# [21] RColorBrewer_1.1-2 gcookbook_1.0      ggplot2_1.0.1      MASS_7.3-45       
# 
# loaded via a namespace (and not attached):
# [1] AlgDesign_1.1-7.3 assertthat_0.1    boot_1.3-17       chron_2.3-47      cluster_2.0.3    
# [6] coda_0.18-1       codetools_0.2-14  colorspace_1.2-6  combinat_0.0-8    DBI_0.3.1        
# [11] deldir_0.1-9      digest_0.6.8      iterators_1.0.8   klaR_0.6-12       labeling_0.3     
# [16] lattice_0.20-33   lazyeval_0.1.10   LearnBayes_2.15   Matrix_1.2-2      memoise_0.2.1    
# [21] munsell_0.4.2     nlme_3.1-122      parallel_3.1.2    proto_0.3-10      R6_2.1.1         
# [26] Rcpp_0.12.2       reshape_0.8.5     scales_0.3.0      sp_1.1-1          spdep_0.5-88     
# [31] splines_3.1.2     stringi_1.0-1     tools_3.1.2