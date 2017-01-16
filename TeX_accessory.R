### cross reference insert

fig_ref <-
  function(fig_label, range = F, prefix = T){
    fig_num <- length(fig_label)
    separater <- ifelse(range, "--", ", ")
    if(!prefix) fig_prefix <- c("", "")
    
    if(fig_num == 1){
      paste0(fig_prefix[1], " \\ref{fig:", fig_label, "}")
    } else {
        paste0("\\ref{fig:", fig_label, "}") %>%
          str_c(., collapse = separater) %>%
          paste(fig_prefix[2], .)
    }
  }

table_ref <-
  function(table_label, range = F, prefix = T){
    table_num <- length(table_label)
    separater <- ifelse(range, "--", ", ")
    if(!prefix) table_prefix <- c("", "")
    
    if(table_num == 1){
      paste0(table_prefix[1], " \\ref{table:", table_label, "}")
    } else {
      paste0("\\ref{table:", table_label, "}") %>%
        str_c(., collapse = separater) %>%
        paste(table_prefix[2], .)
    }
  }



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

setPrefix <-
  function(Sep = ""){
    EqnHead <<- paste0(Sep)
    FigHead <<- paste0("Fig.", Sep)
    TableHead <<- paste0("Table", Sep)
    EqnSupHead <<- paste0("S", Sep)
    FigSupHead <<- paste0("Fig. S", Sep)
    TableSupHead <<- paste0("Table S", Sep)
    
    EqnNum_count <<- 0
    FigNum_count <<- 0
    TableNum_count <<- 0
    EqnSupNum_count <<- 0
    FigSupNum_count <<- 0
    TableSupNum_count <<- 0
  }

