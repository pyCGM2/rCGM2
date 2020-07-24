#---- gait-applied descriptive stats ----

#' @title
#' descriptiveGait
#' @description
#' return descriptive stats of both frame sequence and gait event scalars
#'
#' @param table [dataframe] all-cycle table
#' @param bySubjectFlag [bool] group-by independant variables
#' @return  [list] Frames : descriptive stats of frame-based variables , Events : descriptive stats of events
#' @examples
#' TODO
#'

descriptiveGaitOld<- function(table,groupByList,bySubjectFlag=TRUE){

  descFrames = computeDescritiveStats_onFrameSequences(table,groupByList,bySubjectFlag = bySubjectFlag) # frames
  descEvents = computeDescritiveStats(table,c("stancePhase","doubleStance1","doubleStance2"),groupByList,bySubjectFlag =bySubjectFlag) #gait events

  #out= left_join(descriptiveKinematics, descriptivegaitEvents, by=c("Id","EventContext","ComparisonFactor","Stats"))

  return (list(Frames = descFrames, Events = descEvents ))
}
