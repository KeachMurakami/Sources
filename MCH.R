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
  
  # hourly
  Hourly <-
    Raw %>%
    mutate(., DayHour = paste0(Day, " ", Hour)) %>%
    gather(variable, value, -(Day:Sec), -(DayNight:DayHour)) %>%
    group_by(ID, DayNight, Day, Hour, DayHour, variable) %>%
    summarise(Mean = mean(value), SD = sd(value)) %>%
    ungroup %>%
    mutate(Time = paste0(DayHour, "-00-00"),
           Time = ymd_hms(Time)) 
  # daily
  Daily <-
    Raw %>%
    gather(variable, value, -(Day:Sec), -(DayNight:end)) %>%
    group_by(., ID, DayNight, Day, variable) %>%
    summarise(Mean = mean(value), SD = sd(value)) %>%
    ungroup %>%
    mutate(Time = ymd(Day))
  # all span
  FullSpan <-
    Raw %>%
    gather(variable, value, -(Day:Sec), -(DayNight:end)) %>%
    group_by(., ID, DayNight, variable) %>%
    summarise(Mean = mean(value), SD = sd(value)) %>%
    ungroup
  
  
  options(warn = warn.value) # reset warning
  
  Raw %>%
    select(ID, DayNight, RH, CO2, Temp, Time) %>%
    gather(variable, Mean, -ID, -DayNight, -Time) %>%
    mutate(SD = 0) %>%
    list(Raw = ., Hourly = Hourly, Daily = Daily, Span = FullSpan,
         inputs = list(ID = ID, StartDay = StartDay, EndDay = EndDay,
                       StartTime = StartTime, EndTime = EndTime, ONtime, OFFtime, LogPath)) %>%
    return
}

# Wrapper for multi-MCH logging
readMCHs <- function(ID, StartDay, EndDay,
                    StartTime = "150000", EndTime = "150000",
                    ONtime = 7, OFFtime = 23,
                    LogPath = "~/Dropbox/MCH_log/"){
  
  results <-
    Vectorize(readMCH)(ID, StartDay, EndDay,
                      StartTime, EndTime,
                      ONtime, OFFtime,
                      LogPath) %>%
    .[-5, ] %>%
    alply(.margins = 1, .fun = "rbind_all") %>%
    .[1:4]
  
  names(results) <- c("Raw", "Hourly", "Daily", "Span")
  
  return(results)
}