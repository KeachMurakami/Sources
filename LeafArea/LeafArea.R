library(EBImage)

binary_area <-
  function(pict_file = NULL, FilteringColor = "G"){
    
    dsp <- function(x, ...) display(x, method = "raster", ...)
    dsp_sized <- function() dsp(pict[(center_xy[1] - d_pix[1]):(center_xy[1] + d_pix[3]), (center_xy[2] - d_pix[2]):(center_xy[2] + d_pix[4]),])
    
    SetSize <- function(){
      dsp(pict0)
      cat("\n\n\nclick the center of view in the picture window, then push Esc")
      repeat{
        center_xy <<-
          locator() %>%
          unlist %T>%
          print
        points(center_xy[1], center_xy[2], col = 5, pch = 19)  
        if(readline("Captured the centers well? (y/n)\n") == "y") break
        else cat("\n\nretry")
      }
      
      d_pix_max <- c(center_xy, dim(pict0)[1:2] - center_xy)
      d_pix <- d_pix_max * .8
      
      repeat{
        dsp_sized()
        if(readline("Good x range? (y/n)\n") == "y") break
        else {
          d_pix_x0 <-
            readline(paste0("\nx range set \n format: left pix, right pix \n displayed left: ", round(d_pix[1], 1), ", right: ", round(d_pix[3], 1))) %>%
            str_split(pattern = ",") %>%
            .[[1]] %>%
            as.numeric
          d_pix[c(1, 3)] <<- c(min(d_pix_max[1], d_pix_x0[1]), min(d_pix_max[3], d_pix_x0[2]))
        }
      }
      repeat{
        dsp_sized()
        if(readline("Good y range? (y/n)\n") == "y") break
        else {
          d_pix_y0 <-
            readline(paste0("\ny range set \n format: upper pix, lower pix \n displayed upper: ", round(d_pix[2], 1), ", lower: ", round(d_pix[4], 1))) %>%
            str_split(pattern = ",") %>%
            .[[1]] %>%
            as.numeric
          d_pix[c(2, 4)] <<- c(min(d_pix_max[2], d_pix_y0[1]), min(d_pix_max[4], d_pix_y0[2]))
        }
      }
      pict <<- pict0[(center_xy[1] - d_pix[1]):(center_xy[1] + d_pix[3]), (center_xy[2] - d_pix[2]):(center_xy[2] + d_pix[4]),]
      
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
        {. > max(.)/10}
      
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
             adj = c(0,1), col = "black", cex = 2)
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
               X_min = center_xy[1] - d_pix[1], X_max = center_xy[1] + d_pix[1], 
               Y_min = center_xy[2] - d_pix[2], Y_max = center_xy[2] + d_pix[2], 
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