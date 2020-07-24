#---- all cycle table ------



#' @title
#' defineComparisonfactor
#' @description
#' define the comparison factor as a new column
#'
#' @param table [dataframe] all-cycle table.
#' @param groupBy [vector] column names to gather .
#' @return updated table.
#' @examples
#' defineComparisonfactor(kinematicTable,c("id","Context"))

defineComparisonfactor<- function(table,groupBy){



  table = table %>%
    unite(ComparisonFactor, groupBy,remove = FALSE)

  table$ComparisonFactor = as.factor(table$ComparisonFactor)



  return (table)

}


#' @title
#' constructOverallTable
#' @description
#' merge left and right contexts. \code{Left} and \code{Right} modalities are replaced by \code{Overall}
#'
#' @param table [dataframe] all-cycle table.
#' @param groupBy [vector] group-by independant variables .
#' @return updated table.
#' @examples
#' constructOverallTable(kinematicTable,c("Ipp","ComparisonFactor"))



setVariableType<-function(table){



  outTable = table %>%
    rowwise()%>%
    mutate(Domain = ifelse(str_detect(Label,"Angle"),"ANGLES",
                           ifelse(str_detect(Label,"Moment"), "MOMENT",
                                  ifelse(str_detect(Label,"Power"), "POWER","UNKNOWN"))))



  return(outTable)
}




setAffectedSide<-function(table,subjectTable,subjectID="Id"){


  table = left_join(table,subjectTable,by=subjectID)

  table = table %>%
    rowwise()%>%
    mutate(SideType = ifelse(AffectedSide=="B","Affected",
                         ifelse(AffectedSide =="L" & EventContext=="Left", "Affected",
                                ifelse(AffectedSide =="R" & EventContext=="Right", "Affected","Unaffected"))))


  return(table)
}


separateEmgLabel<-function(table){

  outTable = table %>%
    mutate(Label = sub("_", ",", Label)) %>%
    separate (Label,c("Label", "Method"),sep=",")



  return(outTable)
}





constructOverallTable<- function(table,groupBy,removeFirstCharacter=TRUE){


  # remove first letter  and change EventContext
  if (removeFirstCharacter)
    table$Label = substring(table$Label,2,  nchar(table$Label))

  table$EventContext0 = table$EventContext
  table$Cycle0 = table$Cycle
  table$EventContext = "Overall"

  if (isColExist(table,"Axis"))
    groupBy = append(groupBy,c("Label","Axis"))
  else
    groupBy = append(groupBy,c("Label"))

  table = table %>%
    group_by_at(groupBy)%>%
    mutate(Cycle = 1:n())

  return (table)

}


constructOverallStpTable<- function(table,groupBy){


  # remove first letter  and change EventContext

  table$EventContext0 = table$EventContext
  table$Cycle0 = table$Cycle
  table$EventContext = "Overall"


  table = table %>%
    group_by_at(groupBy)%>%
    mutate(Cycle = 1:n())

  return (table)

}


constructOverallDiscreteTable<- function(table,groupBy){

  # remove first letter  and change EventContext
  table$Label = substring(table$Label,2,  nchar(table$Label))
  table$EventContext0 = table$EventContext
  table$Cycle0 = table$Cycle

  table$EventContext = "Overall"


  groupBy = append(groupBy,c("Label"))

  table = table %>%
    group_by_at(groupBy)%>%
    mutate(Cycle = 1:n())

  return (table)

}



constructOverallDiscreteEmgTable<- function(table,groupBy){

  # remove first letter  and change EventContext
  table$Label = substring(table$Label,2,  nchar(table$Label))

  table$EventContext0 = table$EventContext
  table$Cycle0 = table$Cycle

  table$EventContext = "Overall"


  groupBy = append(groupBy,c("Label","Phase"))

  table = table %>%
    group_by_at(groupBy)%>%
    mutate(Cycle = 1:n())

  return (table)

}


removeUnmatchedContext<-function(table){


  left = table[substr(emgTable$Label,1,1)=="L" & table$EventContext == "Left" ,]
  right = table[substr(emgTable$Label,1,1)=="R" & table$EventContext == "Right" ,]

  outTable = rbind(left,right)


      return(outTable)
}



# -----------------------------------------

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
#' @param groupBy [list] group-by independant variables
#' @return  dataframe.
#' @examples
#' getNumberOfCycles(kinematicTable,"LHipAngles","X", c("Id","Operator","Session"))


getNumberOfCycles<-function(table,label,axis,groupBy){

  outTable =  table%>%
    group_by_at(groupBy) %>%
    filter( Label==label & Axis == axis  )%>%
    summarize(ncycles = length(Label))

  return(outTable)
}












