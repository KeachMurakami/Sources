update_bib <-
  function(input_dir = "~/GitHub/BeeLabR/BibTex/list_bib/", output_dir = "~/GitHub/BeeLabR/BibTex/"){
    OS <- .Platform$OS.type
    if(OS != "unix"){
      stop("Win用にパスを設定していないのでMacでやる")
    } else {
      
      paste0("cat ", input_dir, "*.bib > ", output_dir, "full_with.bib") %>% system
      paste0("cat ", output_dir, "*with.bib > ", output_dir, "With.bib") %>% system
      
      paste0("mv ", output_dir, "full_with.bib ", output_dir, "full_without.bib") %>% system
      paste0("cat ", output_dir, "*without.bib > ", output_dir, "Without.bib") %>% system
      
      paste0("mv ", output_dir, "full_without.bib ", output_dir, "tempFull.bib") %>% system
      paste0("cat ", output_dir, "*Full.bib > ", output_dir, "Full.bib") %>% system
      paste0("rm ", output_dir, "tempFull.bib") %>% system
    }
    cat("\nCitations have been updated.\n\n")
  }

update_abbrv <-
  function(abbrv_file = "~/GitHub/BeeLabR/BibTex/MyAbbrvList.csv"){
    abbrv_dir <-
      dirname(abbrv_file)

    abbrv_list <-
      read.csv(abbrv_file) %>%
      mutate(Full = paste0('@string{', Abbrv, '=\"', Full, '\"}'),
             With = paste0('@string{', Abbrv, '=\"', Abbrv_with_period, '\"}'),
             Without = paste0('@string{', Abbrv, '=\"', Abbrv_without_period, '\"}')) %>%
    {
      .$Full %>% as.character() %>% write(file = paste0(abbrv_dir, "/abbrev_Full.bib"))
      .$With %>% as.character() %>% write(file = paste0(abbrv_dir, "/abbrev_with.bib"))
      .$Without %>% as.character() %>% write(file = paste0(abbrv_dir, "/abbrev_without.bib"))
    }
    
    cat("\nAbbreviations have been updated.\n\n")
  }

update_cite <- 
  function(){
    update_bib()
    update_abbrv()
  }
  
