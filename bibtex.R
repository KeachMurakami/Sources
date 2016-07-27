bib_update <-
  function(input_dir = "~/Dropbox/Paper/bibtex/", output_dir = "~/GitHub/BeeLabR/BibTex/"){
    OS <- .Platform$OS.type
    if(OS != "unix"){
      stop("Win用にパスを設定していないのでMacでやる")
    } else {
      paste0("cat ", input_dir, "*.bib > ", output_dir, "full_with.bib") %>%
        system
      paste0("cat ", input_dir, "*.bib > ", output_dir, "full_without.bib") %>%
        system
      
      paste0("cat ", output_dir, "*with.bib > ", output_dir, "with_period.bib") %>%
        system
      paste0("cat ", output_dir, "*without.bib > ", output_dir, "without_period.bib") %>%
        system
    }
    cat("\ncitations have been updated.\n\n")
  }
