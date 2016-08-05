### add Eqn number automatically

EqnNum <- 
  function(head = EqnHead){
    EqnNum_count <<- EqnNum_count + 1
    return(paste0(head, EqnNum_count))
  }
