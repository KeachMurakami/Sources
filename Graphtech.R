readGL <- function(ID, ch, StartDay, EndDay,
                   StartTime = "150000", EndTime = "150000",
                   ONtime = 7, OFFtime = 23,
                   LogPath = "~/Dropbox/GL_log/"){  

# 将来的にはクラウド化したい。

# ID:
#   表示させたいロガーのID
#   例: "A"
# 
# ch: 
#   表示させたいロガーのチャネル
#   例: 1
# 
# StartDay, EndDay:
#   見たい区間を指定
#   例: 150911 (2015年9月11日の場合)
# 
# ONtime, OFFtime:
#   光源のON/OFF時間を指定
#   例: ONtime = 10, OFFtime = 20
#   デフォルトは7時にON、23時にOFF
# 
# StartTime, EndTime: 
#   測定開始時間・終了時間を指定
#   例: "150000" (15:00:00の場合)

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

# キャリブレーションシートの読み込み
Calb <- 
  paste0(LogPath, "CalbGL.csv") %>%
  fread %>%
  filter(GL_ID == ID, GL_ch == ch) %>%
  select(Slope, Intercept) %>%
  unlist


# 警告設定を緩和
# tidyr::separateで多く出るので
warn.value <- as.numeric(options("warn"))
options(warn = -1)


# 時間に応じて昼夜を追加する関数
  DNdet <- function(Hour, ONtime, OFFtime){
    Hour <- as.numeric(Hour)
    ifelse(test = (Hour >= ONtime && Hour < OFFtime) , yes = "Day", no = "Night")
  }

# データハンドリング
AllDataDirs <-
  dir(paste0(LogPath, ID, "/")) %>%
  as.numeric

inRanges <-
  ((AllDataDirs > StartDay) & (AllDataDirs <= EndDay)) %>%
    which %>%
    range

  if(is.infinite(inRanges[1])) {
    DataDirs <-
     (AllDataDirs < StartDay) %>%
       which %>%
       max %>%
       AllDataDirs[.] %>%
       paste0(LogPath, ID, "/", ., "/") %>%
       dir(full.names = TRUE)
  } else {
    Used <-
      AllDataDirs[(inRanges[1] - 1):inRanges[2]]
    DataDirs <-
      AllDataDirs[(inRanges[1] - 1):inRanges[2]] %>% # minの１つ前までにデータが含まれる
      paste0(LogPath, ID, "/", ., "/") %>%
      dir(full.names = TRUE)
    rm(Used)
  }

# 指定期間のログデータがない場合の処理。
if(length(DataDirs) == 0){
  print("no CSV files detected")
  break
}

StartFolder <- dirname(DataDirs)[1]

selected_ch <- paste0("ch", ch)

Raw <-
  lapply(1:length(DataDirs), function(i){
    if (class(try(fread(input = DataDirs[i], skip = 33), silent = TRUE)) == "try-error") {
      # fread関数がエラーになった場合はループ抜け
      # データ数が１のときとかにエラーが出るので、例外処理
      data.frame(Time = "NA", val = NA) %>%
        return
    } else {
      # エラーに該当しなければファイルの読み込みを進行する
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
         Time = ymd_hms(Time, locale="C", tz="Asia/Tokyo"),
         DayNight = Vectorize(DNdet)(Hour, LightOn, LightOff), # 昼夜表示
         val= (val - Calb[2]) / Calb[1] # 校正
        ) %>%
  select(-starts_with("Light")) %>%
  # 処理開始、終了時間でカットする
  filter(Time > ymd_hms(paste0(StartDay, " ", StartTime, tz = "Asia/Tokyo"),
         Time < ymd_hms(paste0(EndDay, " ", EndTime), tz = "Asia/Tokyo"))) %>%
  mutate(ID = ID, ch = ch,
         start = ymd_hms(paste0(StartDay, " ", StartTime)),
         end = ymd_hms(paste0(EndDay, " ", EndTime)))

  Raw %>%
    {
    # 1時間平均
    meanHour <<-
      mutate(., IDs = paste0(Day, " ", Hour)) %>%
      group_by(DayNight, ID, ch, start, end, IDs) %>%
      summarise(value = mean(val), SD = sd(val)) %>%
      ungroup %>%
      mutate(Time = paste0(IDs, "-00-00"),
             Time = ymd_hms(Time)) 
    # 1日平均
    meanDay <<-
      group_by(., ID, ch, start, end, Day, DayNight) %>%
      summarise(value = mean(val), SD = sd(val)) %>%
      ungroup %>%
      mutate(Time = ymd(Day))
    # 期間平均
    meanAll <<-
      group_by(., ID, ch, start, end, DayNight) %>%
      summarise(value = mean(val), SD = sd(val))
    }
  
  options(warn = warn.value) # 警告設定を元に戻す
  
  list(Raw = Raw, Hourly = meanHour, Daily = meanDay, Span = meanAll) %>%
    return

}



# 要求パッケージなど。
# エラーが出たらここらへんをインストールしてください。
# だめなら村上まで。

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
