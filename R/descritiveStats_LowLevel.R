#---- Scalar ----

#' @title
#' computeDescritiveStats
#' @description
#' return descriptive statistics for a series of dependant variables and a combinaison of factors
#'
#' @param table [dataframe] all-cycle table.
#' @param dependantVariables [list] dependant variables
#' @param groupByList [list] group-by independant variables
#' @param bySubjectFlag [bool] group results by subject
#' @return  table
#' @examples
#' TODO

computeDescritiveStats<- function(table,dependantVariables,groupByList,bySubjectFlag=TRUE){

  if (bySubjectFlag) {
    groupByList = c(groupByList,"Id","EventContext")
  } else{
    groupByList = c(groupByList,"EventContext")
  }


  # mean
  mean=table%>%
    group_by_at(vars(groupByList)) %>%
    select(dependantVariables)%>%
    summarise_all(funs(mean))

  sd=table%>%
    group_by_at(vars(groupByList)) %>%
    select(dependantVariables)%>%
    summarise_all(funs(sd))


  mean$Stats ="mean"
  sd$Stats ="std"

  out = bind_rows(mean, sd)
  if (!(bySubjectFlag)) {
    out$Id = "All"
  }

  out$Index = seq(1,nrow(out))
  return( out)


}




#' @title
#' gather_DescritiveStats
#' @description
#' applicatio of dplyr::gather function  on descriptive stats table. ( return a \textit{long} table)
#'
#' @param descStatTable [dataframe] descriptive stats table.
#' @param dependantVariables [list] dependant variables
#' @param groupByList [list] group-by independant variables
#' @return  table
#' @examples
#' TODO

gather_descritiveStats<- function(descStatTable,dependantVariables,groupByList){

  meanGather = descStatTable  %>%
    filter(Stats == "mean")%>%
    gather_("Factor", "Mean",  groupByList)


  sdGather = descStatTable  %>%
    filter(Stats == "std")%>%
    gather_("Factor", "Sd",  groupByList)


  meanGather$Stats=NULL
  sdGather$Stats=NULL
  final = left_join(meanGather,sdGather, by=c("Id",dependantVariables,"EventContext","Factor") )

  return(final)
}

#---- FRAME sequences ----

#' @title
#' computeDescritiveStats_onFrameSequences
#' @description
#' return descriptive stats for all frame sequences
#'
#' @param table [dataframe] all-cycle table
#' @param groupByList [list] group-by independant variables
#' @param bySubjectFlag [bool] output table grouped by subject
#' @return  table
#' @examples
#' TODO

computeDescritiveStats_onFrameSequences<- function(table,groupByList,bySubjectFlag=TRUE){

  if (bySubjectFlag) {
    groupByList = c(groupByList,"Id","EventContext","Label","Axis")
  } else{
    groupByList = c(groupByList,"EventContext","Label","Axis")
  }


  meanTrace=table%>%
    group_by_at(vars(groupByList)) %>%
    select(Frame0:Frame100)%>%
    summarise_all(funs(mean))

  sdTrace=table%>%
    group_by_at(vars(groupByList)) %>%
    select(Frame0:Frame100)%>%
    summarise_all(funs(sd))


  meanTrace$Stats ="mean"
  sdTrace$Stats ="std"

  out = bind_rows(meanTrace, sdTrace)
  if (!(bySubjectFlag)) {
    out$Id = "All"
  }

  out$Cycle = 1
  out$Index = seq(1,nrow(out))

  return( out)
}

#' @title
#' gather_descriptiveStats_FrameSequences
#' @description
#' #' applicatio of dplyr::gather function  on frame sequence descriptive stats table.
#' ( return a \textit{long} table)
#'
#' @param frameSeqDescStats [dataframe] frame sequence descriptive stats
#' @return  dataframe.
#' @examples
#' TODO



gather_descriptiveStats_FrameSequences<- function(frameSeqDescStats){


  meanTraceGather = frameSeqDescStats  %>%
    filter(Stats == "mean")%>%
    gather(Frame, Values,  Frame0:Frame100)

  sdTraceGather = frameSeqDescStats  %>%
    filter(Stats == "std")%>%
    gather(Frame, Values,  Frame0:Frame100)

  final = bind_rows(meanTraceGather,sdTraceGather)


  return(final)

}


#' @title
#' getStdCorridorLimits_fromDescStatFrameSequences
#' @description
#' return corridor limits of frame seqence descriptive stats
#' @param frameSeqDescStats [dataframe] frame sequence descriptive stats

#' @return  dataframe.
#' @examples
#' TODO


getStdCorridorLimits_fromDescStatFrameSequences<- function(frameSeqDescStats){
  table_transform = frameSeqDescStats  %>%
    gather(Frame, Values,  Frame0:Frame100)%>%
    select(-Index) %>%
    spread(Stats, Values)%>%
    mutate(Max= mean+std, Min = mean-std )

  return(table_transform)
}
