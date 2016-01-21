library(EBImage)

binary_area <-
  function(pict_file = NULL, FilteringColor = "G", SizeRatio_Min_vs_Max = 20){
    
    dsp <- function(x, ...) display(x, method = "raster", ...)
    
    SetSize <- function(){
      dsp(pict0)
      repeat{
        dsp(pict0)
        cat("\n\n\nset left top: click in the picture window, then push Esc")
        left_top <-
          locator() %>%
          unlist %T>%
          print
        abline(v = left_top[1], col = "red"); abline(h = left_top[2], col = "red")
        
        cat("\n\n\nset right bottom: click in the picture window, then push Esc")
        right_bottom <-
          locator() %>%
          unlist %T>%
          print
        abline(v = right_bottom[1], col = "red"); abline(h = right_bottom[2], col = "red")
        
        if(readline("OK? (y/n)") == "y") break
        else cat("\n\nretry")
      }
      pict <<- pict0[left_top[1]:right_bottom[1], left_top[2]:right_bottom[2],]
      
      if(!FlagBinary) coloring()
      else if(readline("set color or skip? (y = set it)") == "y") coloring()
      else Calculates()
    }
    
    
    coloring <- function(){
      colorMode(pict) <- 2
      
      repeat{
        pict_bin <-
          between(pict[, , FilterCol], ColRange[1], ColRange[2]) %>%
          fillHull %>%
          toRGB
        
        colorMode(pict_bin) <- 0
        overlay <-
          paintObjects(x = pict_bin, tgt = pict/1.5, opac = c(1, .1))
        colorMode(overlay) <- 2
        overlay %>% dsp
        
        if(readline("Good binary? (y/n)\n") == "y") break
        else {
          ColRange <-
            readline("threshold Low and High\nformat: Low(0~1),High(0~1)\n") %>%
            str_split(pattern = ",") %>%
            .[[1]] %>%
            as.numeric %>%
            {c(max(.[1], 0), min(.[2], 1))} # エラー抑制:　High Lowを0~1の範囲にしとく。
        }
      }
      pict_bin <<-
        between(pict[, , FilterCol], ColRange[1], ColRange[2]) %>%
        fillHull %>%
        toRGB
      FlagBinary <<- TRUE
      
      Calculates()  
    }
    
    
    Calculates <- function(){
      if(!FlagBinary) {coloring()}
      # selected several segments to suppress too many labels in the following plot
      large_segment <<-
        pict_bin %>%
        bwlabel %>%
        computeFeatures.shape %>%
        .[, "s.area", drop = F] %>%
        {. > max(.) / SizeRatio_Min_vs_Max}
      
      segment_xy <<-
        pict_bin %>%
        bwlabel %>%
        computeFeatures.moment(ref = pict_bin) %>%
        .[, c("m.cx", "m.cy")]
      
      
      segment_labels <<-
        data.frame(segment_xy) %>%
        transmute(x = m.cx, y = m.cy,
                  labels = 
                    (1:nrow(large_segment) * large_segment) %>%
                    as.character %>%
                    str_replace(pattern = "^0$", replacement = "")
        )
      
      for(i in 1:nrow(large_segment)){
        text(x = segment_labels[i, 1],
             y = segment_labels[i, 2],
             label = segment_labels[i, 3],
             adj = c(0,1), col = "red", cex = 2)
      }
      
      
      used_segment <-
        readline("select segments to be calculated\nformat: 5,6,7 ") %>%
        str_split(pattern = ",") %>%
        .[[1]] %>%
        as.numeric
      
      results <<-
        pict_bin %>%
        bwlabel %>%
        computeFeatures.shape %>%
        .[, "s.area", drop = F] %>%
        data.frame %>%
        slice(used_segment) %>%
        mutate(segmentID = as.character(used_segment), ColLow = ColRange[1], ColHigh = ColRange[2],
               X_min = left_top[1], X_max = right_bottom[1], 
               Y_min = left_top[2], Y_max = right_bottom[2], 
               current_dir = getwd(), filepath = pict_file)
      
      print(results)
      
      CheckData <- readline("Problems in data? (no/size/color)")
      if(CheckData == "no") return(results)
      else if(CheckData == "size") {
        pict <<- pict0
        SetSize()
      }
      else coloring()
    }
    if(is.null(pict_file)){  
      pict_file <-
        file.choose()
    }
    
    pict0 <- readImage(pict_file)
    pict <- pict0
    
    center_xy <- dim(pict)[1:2] / 5
    ColRange <- c(0.1, 0.9)
    FilterCol <- switch(FilteringColor, "R" = 1, "G" = 2, "B" = 3)
    FlagBinary <- FALSE
    
    SetSize()
  }