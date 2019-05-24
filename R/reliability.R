#' @title
#' ComputeSessionAverage
#'
#' @description
#' compute average of all sessions
#' @param  table [dataframe] all-table
#' @return [dataframe]
#' @examples
#' TODO
#' @section Warning:
#'
ComputeSessionAverage<-function(table){
  outDf = table %>%
  group_by(Participant,Assessor,Session,Label,Axis,Context,Frames)%>%
  summarise(Avg = mean(Value,na.rm = TRUE))
  return(outDf)
}


#' @title
#' withinSubjectStandardDeviation
#'
#' @description
#' compute the within-subject standard deviation
#' @param  sessionAverage [dataframe] session average table
#' @param  Anova [Bool] computation done with anova
#' @return [dataframe]
#' @examples
#'

#' @section Warning:
#' within-subject standard deviation matches Richard Baker's spreadsheet processing.
#' You might also compute this value through  Anova outputs. In this case, slight differences might occur


withinSubjectStandardDeviation <-function(sessionAverage,Anova=FALSE){


  if (!Anova){
    frames = sessionAverage %>%
    group_by(Participant,Label,Axis,Context,Frames)%>%
    summarize(Sem = sd(Avg))
  } else {
    frames = sessionAverage %>%
      group_by(Participant,Label,Axis,Context,Frames)%>%
      do(anova = aov(Avg ~ Assessor, data=.))%>%
      mutate(Sem = sigma(anova))
  }


  framesAvg = frames %>%
    group_by(Participant,Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem,na.rm = TRUE))


  return(list(atFrame=frames, overall = framesAvg))
}


#' @title
#' withinOperatorStandardDeviation
#'
#' @description
#' compute the within-Operator standard deviation
#' @param  sessionAverage [dataframe] session average table
#' @param  Anova [Bool] computation done with anova
#' @return [dataframe]
#' @examples
#'
#' @section Warning:
#' within-operator standard deviation matches Richard Baker's spreadsheet processing.
#' You might also compute this value through  Anova outputs. In this case, slight differences might occur

withinOperatorStandardDeviation<-function(sessionAverage,Anova=FALSE){
  # It s also the SEM for individual Assessor

  if (!Anova){

    frames = sessionAverage %>%
      group_by(Participant,Assessor,Label,Axis,Context,Frames)%>%
      summarise(Sem = sd(Avg,na.rm = TRUE))
  } else  {

    frames = sessionAverage %>%
      group_by(Participant,Assessor,Label,Axis,Context,Frames)%>%
      summarise(Sem = sd(Avg))
  }

  framesAvg = frames %>%
    group_by(Participant,Assessor,Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem,na.rm = TRUE))


  return(list(atFrame=frames, overall = framesAvg))

}

#' @title
#' Sem_byAssessor
#'
#' @description
#' compute the standard error of meausrement by assessors
#' @param  sessionAverage [dataframe] session average table
#' @param  Anova [Bool] computation done with anova
#' @return
#' @examples
#'
Sem_byAssessor<-function(sessionAverage,Anova=TRUE){

  if (Anova){
    frames = sessionAverage %>%
      group_by(Assessor,Label,Axis,Context,Frames)%>%
      do(anova = aov(Avg ~ Participant, data=.))%>%
      mutate(Sem = sigma(anova,na.rm = TRUE))

    framesAvg = frames %>%
      group_by(Assessor,Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))


  } else {
    wod = withinOperatorStandardDeviation(sessionAverage)
    colnames(wod$atFrame)[colnames(wod$atFrame)=="Sem"] <- "Sem_wod"

    #SEM by assessor
    frames = wod$atFrame %>%
      group_by(Assessor,Label,Axis,Context,Frames)%>%
      summarise(Sem = sqrt(sum(Sem_wod^2)/length(Sem_wod)))

    framesAvg = frames %>%
      group_by(Assessor,Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))

  }
  return(list(atFrame=frames, overall = framesAvg))

}


#' @title
#' Sem_allAssessors
#'
#' @description
#' compute the standard error of measurement  for all assessors
#' @param  sessionAverage [dataframe] session average table
#' @param  Anova [Bool] computation done with anova
#' @return
#' @examples
#'
Sem_allAssessors<-function(sessionAverage, Anova=TRUE){

  if (Anova){


    frames = sessionAverage %>%
      group_by(Label,Axis,Context,Frames)%>%
      do(anova = aov(Avg ~ Participant, data=.))%>%
      mutate(Sem = sigma(anova,na.rm = TRUE))

    framesAvg = frames %>%
      group_by(Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))

    } else {

    wsd = withinSubjectStandardDeviation(sessionAverage)
    colnames(wsd$atFrame)[colnames(wsd$atFrame)=="Sem"] <- "Sem_wsd"

    #all Ass
    frames = wsd$atFrame %>%
      group_by(Label,Axis,Context,Frames)%>%
      summarise(Sem = sqrt(sum(Sem_wsd^2)/length(Sem_wsd)))


    framesAvg = frames %>%
      group_by(Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))
    }


  return(list(atFrame=frames, overall = framesAvg))

}




#' @title
#' betweenAssessors
#'
#' @description
#' assess between assessor difference
#' @param  sessionAverage [dataframe] session average table
#' @return
#' @examples
#'
#'
betweenAssessors<-function(sessionAverage){

  assessor_frames = sessionAverage %>%
    group_by(Assessor,Label,Axis,Context,Frames)%>%
    summarise(Average = mean(Avg,na.rm = TRUE))


  assessor_framesAvg = assessor_frames %>%
    group_by(Assessor,Label,Axis,Context)%>%
    summarise(FrameAverage = mean(Average,na.rm = TRUE))



  allAssessors_frames = assessor_frames %>%
    group_by(Frames,Label,Axis,Context)%>%
    summarise(All = mean(Average), Agreement = max(Average)-min(Average))


  allAssessors_framesAvg = allAssessors_frames %>%
    group_by(Label,Axis,Context)%>%
    summarise(AllAverage = mean(All), FrameAgreement = mean(Agreement))


  return(list(byAssessor_atFrame=assessor_frames,
              byAssessor_overall = assessor_framesAvg,
              allAssessors_atFrame=allAssessors_frames,
              allAssessors_overall = allAssessors_framesAvg))
}


#' @title
#' betweenAssessorsReport
#'
#' @description
#' report of  between assessor differences
#' @param  betweenAssessorAssement [dataframe] between assessor assessment table
#' @return
#' @examples
#'
#'
#'
betweenAssessorsReport<-function(betweenAssessorAssement){

  byAssessor = betweenAssessorAssement$byAssessor_overall
  allAssessors = betweenAssessorAssement$allAssessors_overall


  byAssessorJoin = left_join(byAssessor,allAssessors, by=c("Label","Axis","Context"))


  byAssessorJoin = byAssessorJoin%>%
    rowwise()%>%
    mutate(Case = ifelse(FrameAverage>AllAverage, "greater", "lesser"),
           AbsoluteDifference = FrameAverage - AllAverage
           )

  return (byAssessorJoin)


}

#' @title
#' withinAssessorReport
#' @description
#' Report of the within asessor differences
#'
#' @param  sembyAssessordf [dataframe] standard error of measurement table computed by assessor
#' @param  nAssesor [integer] number of assessors
#' @param  nParticipant [integer] number of participants
#' @return []
#' @examples
#'
#' @section Warning:
#'
withinAssessorReport<-function(sembyAssessordf,nAssesor,nParticipant){

  ic_upper = sqrt(((nAssesor-1)*nParticipant)/(qchisq(0.05, df=(nAssesor-1)*nParticipant)))

  ##  within Assessor
  overallDf = sembyAssessordf$overall

  overallDf$IC_up = overallDf$SemAvg *ic_upper


  return (overallDf)


}

#' @title
#' accrossAssessorReport
#' @description
#' Report of across asessor differences
#'
#' @param  semAllAssessordf [dataframe] standard error of measurement table computed for ALL assessors
#' @param  nAssesor [integer] number of assessors
#' @param  nParticipant [integer] number of participants
#' @return []
#' @examples
#'
#' @section Warning:
#'
accrossAssessorReport<-function(semAllAssessordf,nAssesor,nParticipant){

  ic_upper = sqrt(((nAssesor-1)*nParticipant)/(qchisq(0.05, df=(nAssesor-1)*nParticipant)))

  ##  within Assessor
  overallDf = semAllAssessordf$overall

  overallDf$IC_up = overallDf$SemAvg *ic_upper


  return (overallDf)


}



