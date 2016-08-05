### add Eqn, Fig, Table number automatically

EqnNum <- 
  function(head = EqnHead){
    EqnNum_count <<- EqnNum_count + 1
    return(paste0(head, EqnNum_count))
  }

FigNum <- 
  function(Caption, head = FigHead){
    FigNum_count <<- FigNum_count + 1
    return(paste0(head, FigNum_count, " ", Caption))
  }

TableNum <- 
  function(Caption, head = TableHead){
    TableNum_count <<- TableNum_count + 1
    return(paste0(head, TableNum_count, " ", Caption))
  }
