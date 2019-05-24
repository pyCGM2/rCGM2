#---- all cycle table ------


#' @title
#' addCumulatedCycleindex
#' @description
#' add new col CycleCum, cumulating cycle number from both left and right contexts
#'
#' @param table [dataframe] all-cycle table.
#' @param groupByList [list] group-by independant variables .
#' @return update all-cycle table
#' @examples
#' addCumulatedCycleindex(kinematicTable, c("Id"))

addCumulatedCycleindex<-function(table, groupByList){

  nbCycles = table%>%
    filter(Context =="Left")%>%
    group_by_at(vars(groupByList)) %>%
    summarise(ncycles = max(Cycle)+1)


  tmp = left_join(table,nbCycles)

  outTable = tmp %>%
    rowwise() %>%
    mutate(CycleCum  = ifelse(Context == "Left",Cycle,Cycle + ncycles))

  return (outTable)
}

#' @title
#' constructOverallTable
#' @description
#' merge left and right contexts. \code{Left} and \code{Right} modalities are replaced by \code{Overall}
#'
#' @param table [dataframe] all-cycle table.
#' @return update all-cycle table.
#' @examples
#' constructOverallTable(kinematicTable)
#' @section Warning:
#' function addCumulatedCycleindex needs to be run before

constructOverallTable<- function(table){

  if ("CycleCum" %in% names(table)) {


    table$Label = substring(table$Label,2,  nchar(table$Label))
    table$Context = "Overall"
    table$Cycle = table$CycleCum

     return (table)
  }else{ stop("need to run function addCumulatedCycleindex before ") }
}


#' @title
#' homogeniseCycles
#' @description
#' homogenise the number of cycle for a combinaison of independant variables
#'
#' @param table [dataframe] all-cycle table
#' @param groupByList [list] group-by independant variables .
#' @return update all-cycle table
#' @examples
#' homogeniseCycles(kinematicTable, c("Id","Operator","Session"))

homogeniseCycles<-function(table,groupByList){

  cycleCumtable = addCumulatedCycleindex(table,groupByList)


  tmp =  cycleCumtable %>%
    group_by_at(vars(groupByList)) %>%
    summarise(maxCycles = max(CycleCum))

  minMax = min (tmp$maxCycles)

  outTable = filter (cycleCumtable, CycleCum <= minMax)

  return (outTable)

}


#' @title
#' getNumberOfCycles
#' @description
#' return number of cycles for a label, its axis and a combinaison of factor
#'
#' @param table [dataframe] all-cycle table
#' @param label [string] label of the frame sequence
#' @param axis [string] axis of the frame sequence
#' @param groupByList [list] group-by independant variables
#' @return  dataframe.
#' @examples
#' getNumberOfCycles(kinematicTable,"LHipAngles","X", c("Id","Operator","Session"))


getNumberOfCycles<-function(table,label,axis,groupByList){

  outTable =  table%>%
    group_by_(groupByList) %>%
    filter( Label==label & Axis == axis  )%>%
    summarize(ncycles = length(Label))

  return(outTable)
}



















