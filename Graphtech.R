readGL <- function(ID, ch, StartDay, EndDay,
                   StartTime = "150000", EndTime = "150000",
                   ONtime = 7, OFFtime = 23,
                   LogPath = "~/Dropbox/GL_log/"){  
  
  
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
  
  # A: chamA-F @419
  #   ch1 = chamF, ch2 = chamE, ... ch6 = chamA 
  # B: chamK-N @420
  #   ch1 = chamK, ch2 = chamL, ... ch4 = chamN 
  # D: chamG-J @420
  #   ch1 = chamJ, ch2 = chamI, ... ch3 = chamG 
  
  library(ggplot2)
  library(tidyr)
  library(data.table)
  library(lubridate)
  
  # read calibration data
  Calb <- 
    dir(LogPath, pattern = "Calb|calb", full.names = T) %>%
    fread %>%
    select(everything(), LoggerIDs = GL_ID) %>%
    filter(LoggerIDs == ID, GL_ch == ch) %>%
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
    paste0(LogPath, ID, "/")
  }
  
  #################
  # data handling #
  #################
  
  AllDataDirs <-
    DataPath(ID) %>%
    dir %>%
    as.numeric
  
  
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
  
  selected_ch <- paste0("ch", ch)
  CutFrom <- ymd_hms(paste0(StartDay, " ", StartTime), tz = Sys.timezone())
  CutTill <- ymd_hms(paste0(EndDay, " ", EndTime), tz = Sys.timezone())
  
  Raw <-
    lapply(1:length(DataDirs), function(i){
      if (class(try(fread(input = DataDirs[i], skip = 33), silent = TRUE)) == "try-error") {
        # if error in fread(), break loop
        data.frame(Time = "NA", val = NA) %>%
          return
      } else {
        temp <-
          fread(input = DataDirs[i], skip = 33)
        
        temp %>%
          setnames(c("No", "Time", "ms", paste0("ch", 1:(dim(temp)[2] - 3)))) %>%
          select_("Time", val = selected_ch) %>%
          mutate(val = extract_numeric(val)) %>%
          return
      }
    }) %>%
    rbind_all %>%
    na.omit %>%
    separate(col = Time, into = c("Day", "time"), sep = " ", remove = F) %>%  
    separate(col = time, into = c("Hour", "Min", "Sec"), sep = ":") %>%
    mutate(LightOn = ONtime, LightOff = OFFtime,
           Time = ymd_hms(Time, tz = Sys.timezone()),
           DayNight = Vectorize(DNdet)(Hour, LightOn, LightOff), # day night
           Temp= (val - Calb[2]) / Calb[1] # calbrate
    ) %>%
    select(-starts_with("Light")) %>%
    filter(between(Time, CutFrom, CutTill)) %>% # extract data between (StartTime and EndTime)
    mutate(ID = ID, ch = ch, start = CutFrom, end = CutTill) # memos for subsequent data analysis
  
  # hourly
  Hourly <-
    Raw %>%
    mutate(., DayHour = paste0(Day, " ", Hour)) %>%
    group_by(ID, ch, DayNight, Day, Hour, DayHour) %>%
    summarise(Mean = mean(Temp), SD = sd(Temp)) %>%
    ungroup %>%
    mutate(Time = paste0(DayHour, "-00-00"),
           Time = ymd_hms(Time),
           variable = "Temp") 
  # daily
  Daily <-
    Raw %>%
    group_by(., ID, ch, DayNight, Day) %>%
    summarise(Mean = mean(Temp), SD = sd(Temp)) %>%
    ungroup %>%
    mutate(Time = ymd(Day),
           variable = "Temp")
  # all span
  FullSpan <-
    Raw %>%
    group_by(., ID, ch, DayNight) %>%
    summarise(Mean = mean(Temp), SD = sd(Temp)) %>%
    ungroup %>%
    mutate(variable = "Temp")
  
  options(warn = warn.value) # reset warning
  
  Raw %>%
    select(ID, ch, DayNight, Mean = Temp, Time) %>%
    mutate(SD = 0, variable = "Temp") %>%
    list(Raw = ., Hourly = Hourly, Daily = Daily, Span = FullSpan,
         inputs = list(ID = ID, ch = ch, StartDay = StartDay, EndDay = EndDay,
                       StartTime = StartTime, EndTime = EndTime, ONtime, OFFtime, LogPath)) %>%
    return
}

# Wrapper for multi-channel logging
readGLs <- function(ID, ch, StartDay, EndDay,
                    StartTime = "150000", EndTime = "150000",
                    ONtime = 7, OFFtime = 23,
                    LogPath = "~/Dropbox/GL_log/"){
  
  results <-
    Vectorize(readGL)(ID, ch, StartDay, EndDay,
                      StartTime, EndTime,
                      ONtime, OFFtime,
                      LogPath) %>%
    .[-5, ] %>%
    alply(.margins = 1, .fun = "rbind_all") %>%
    .[1:4]
  
  names(results) <- c("Raw", "Hourly", "Daily", "Span")
  
  return(results)
}