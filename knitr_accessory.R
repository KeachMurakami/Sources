# html fig captions
knit_hooks$set(
  html.cap = function(before, options, envir) {
    if(!before) {
      paste('<p class="caption">',options$html.cap,"</p>",sep="")
    }
  })