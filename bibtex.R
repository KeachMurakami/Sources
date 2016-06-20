bib_update <-
  function(OS = "Mac"){
    ifelse(OS == "Mac", "~/Dropbox/", "") %>%
    {paste0("cat ", ., "Paper/bibtex/*.bib > ", ., "R/List.bib")} %>%
    system
    
    cat("\ncitations have been updated.\n\n")
  }
