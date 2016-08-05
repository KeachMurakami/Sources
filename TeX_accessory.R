### add Eqn number automatically

EqnNum <- 
  function(head = EqnHead){
    Num_count <<- Num_count + 1
    return(paste0(head, Num_count))
  }
