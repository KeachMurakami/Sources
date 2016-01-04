library(EBImage)

dsp <- function(x, ...) display(x, method = "raster", ...)

binary_area <-
  function(pict_file = NULL){
    if(is.null(pict_file)){  
      pict_file <-
        file.choose()
    }
    
    pict <- readImage(pict_file)
    
    dsp(pict)
    repeat{
      center_xy <-
        locator() %>%
        unlist %T>%
        print
      points(center_xy[1], center_xy[2], col = 5, pch = 19)  
      if(readline("Captured the centers well? (y/n)\n") == "y") break
      else print("retry")
    }
    
    d_pix[1:2] <- 400
    repeat{
      pict[(center_xy[1] - d_pix[1]):(center_xy[1] + d_pix[1]), (center_xy[2] - d_pix[2]):(center_xy[2] + d_pix[2]),] %>%
        dsp
      if(readline("Good size? (y/n)\n") == "y") break
      else {
        d_pix <<-
          readline(paste0("set dims <- \n format: x,y \n(displayed: ", d_pix[1], ",", d_pix[2], ")")) %>%
          str_split(pattern = ",") %>%
          .[[1]] %>%
          as.numeric
      }
    }
    
    pic <- pict[(center_xy[1] - d_pix[1]):(center_xy[1] + d_pix[1]), (center_xy[2] - d_pix[2]):(center_xy[2] + d_pix[2]),]
    
    colorMode(pic) <- 2
    
    BlueGreen <- c(0.9, 0.9)
    repeat{
      pic_bin <-
        (pic[,,2] < BlueGreen[2] & pic[,,3] < BlueGreen[1]) %>%
        fillHull %>%
        toRGB
      
      colorMode(pic_bin) <- 0
      
      overlay <-
        paintObjects(x = pic_bin, tgt = pic)
      colorMode(overlay) <- 2
      
      overlay %>% dsp
      
      if(readline("Good binary? (y/n)\n") == "y") break
      else {
        BlueGreen <-
          readline("threshold in B- and G-bands\nformat: B(0~1),G(0~1)\n") %>%
          str_split(pattern = ",") %>%
          .[[1]] %>%
          as.numeric
      }
    }
    
    pic_bin <-
      (pic[,,2] < BlueGreen[2] & pic[,,3] < BlueGreen[1]) %>%
      fillHull %>%
      toRGB
    
    # selected several segments to suppress too many labels in the following plot
    large_segment <-
      pic_bin %>%
      bwlabel %>%
      computeFeatures.shape %>%
      .[, "s.area", drop = F] %>%
      {. > max(.)/10}
    
    segment_xy <-
      pic_bin %>%
      bwlabel %>%
      computeFeatures.moment(ref = pic_bin) %>%
      .[, c("m.cx", "m.cy")]
    
    
    segment_labels <-
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
    
    shown_pixels <- 4 * d_pix[1] * d_pix[2]
    
    used_segment <-
      readline("select segments to be calculated\nformat: 5,6,7 ") %>%
      str_split(pattern = ",") %>%
      .[[1]] %>%
      as.numeric
    
    pic_bin %>%
      bwlabel %>%
      computeFeatures.shape %>%
      .[, "s.area", drop = F] %>%
      data.frame %>%
      slice(used_segment) %>%
      mutate(B = Blue, G = Green, FullPixel = shown_pixels, segmentID = as.character(used_segment), filepath = pict_file) %>%
      return
  }