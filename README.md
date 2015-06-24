Please type and read functions from Github.

library(RCurl)
eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/*******.R", ssl.verifypeer = FALSE)))

** summariser **
group_by() %>% summarise_each(funs(mean, sd, se, length))  
with Tukey' test by the first column.  

** phytochrome **
Calculate phytochrome photostationally state according to the spectral distribution of the light and [Sager et al. (1988) (pdf download link)]("https://elibrary.asabe.org/azdez.asp?JID=3&AID=30952&ConfID=t1988&v=31&i=6&T=2").  
