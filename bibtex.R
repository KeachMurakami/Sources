bib_update <-
  function(input_dir = "~/Dropbox/Paper/bibtex/", output_dir = "~/GitHub/BeeLabR/BibTex/"){
    OS <- .Platform$OS.type
    if(OS != "unix"){
      stop("Win用にパスを設定していないのでMacでやる")
    } else {
      
      paste0("cat ", input_dir, "*.bib > ", output_dir, "full_with.bib") %>% system
      paste0("cat ", output_dir, "*with.bib > ", output_dir, "with.bib") %>% system
      
      paste0("mv ", output_dir, "full_with.bib ", output_dir, "full_without.bib") %>% system
      paste0("cat ", output_dir, "*without.bib > ", output_dir, "without.bib") %>% system
      
      paste0("mv ", output_dir, "full_without.bib ", output_dir, "full.bib") %>% system
      
    }
    cat("\nCitations have been updated.\n\n")
  }

abbrv_update <-
  function(abbrv_file = "~/GitHub/BeeLabR/BibTex/MyList.xlsx"){
    abbrv_list <-
      xlsx::read.xlsx(abbrv_file, sheetIndex = 1)
    abbrv_dir <-
      dirname(abbrv_file)
    
    abbrv_list$Abbrv_with_period.1 %>%
      as.character %>%
      write(file = paste0(abbrv_dir, "/abbrv_with.bib"))
    
    abbrv_list$Abbrv_without_period.1 %>%
      as.character %>%
      write(file = paste0(abbrv_dir, "/abbrv_without.bib"))
    
    cat("\nAbbreviations have been updated.\n\n")
  }