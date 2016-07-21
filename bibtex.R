bib_update <-
  function(input_dir = "~/Dropbox/Paper/bibtex/", output_file = "~/Dropbox/R/List.bib"){
    OS <- .Platform$OS.type
    if(OS == "unix"){
      paste0("cat ", input_dir, "*.bib > ", output_file) %>%
        system
    } else {
      stop("Win用にパスを設定していないのでMacでやる")
    }
    
    cat("\ncitations have been updated.\n\n")
  }
