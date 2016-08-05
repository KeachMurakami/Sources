### add Eqn, Fig, Table number automatically

EqnHead <- ""
FigHead <- "Fig. "
TableHead <- "Table "
EqnNum_count <- 0
FigNum_count <- 0
TableNum_count <- 0

EqnSupHead <- "S"
FigSupHead <- "Fig. S"
TableSupHead <- "Table S"
EqnSupNum_count <- 0
FigSupNum_count <- 0
TableSupNum_count <- 0

Labeling <- function(ID, pre = "Fig. ") paste0(pre, EqnHead, ID)


EqnNum <- 
  function(head = EqnHead){
    EqnNum_count <<- EqnNum_count + 1
    return(paste0(head, EqnNum_count))
  }

EqnSupNum <- 
  function(head = EqnSupHead){
    EqnSupNum_count <<- EqnSupNum_count + 1
    return(paste0(head, EqnSupNum_count))
  }



FigNum <- 
  function(Caption, head = FigHead){
    FigNum_count <<- FigNum_count + 1
    return(paste0(head, FigNum_count, " ", Caption))
  }

FigSupNum <- 
  function(Caption, head = FigSupHead){
    FigSupNum_count <<- FigSupNum_count + 1
    return(paste0(head, FigSupNum_count, " ", Caption))
  }



TableNum <- 
  function(Caption, head = TableHead){
    TableNum_count <<- TableNum_count + 1
    return(paste0(head, TableNum_count, " ", Caption))
  }

TableSupNum <- 
  function(Caption, head = TableSupHead){
    TableSupNum_count <<- TableSupNum_count + 1
    return(paste0(head, TableSupNum_count, " ", Caption))
  }