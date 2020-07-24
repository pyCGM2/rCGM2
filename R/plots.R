#' @title
#' consistencyPlot
#' @description
#' plot all cycle of a frame sequence
#' @param  table [dataframe] all-cycle table
#' @param iLabel [string] label of the frame sequence
#' @param iEventContext [string] context of the frame sequence
#' @param iAxis [string] axis of the frame sequence
#' @param iTitle [string] plot title
#' @param yLabel [string] label of the Y-axis
#' @param legendPosition [string] position of the legend (see legend.position of ggplot2)
#' @param ylimits [list] limits of the y-axis
#' @param colorFactor [string] line color according an independant variable
#' @param facetFactor [string] create \textit{facet} plot ( see ggplot2) for an  independant variable
#' @param linetypeFactor [string] line type definied according an independant variable

#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
consistencyPlot<-function(table, iEventContext , iLabel,iAxis,
                          iTitle="",yLabel="Deg", legendPosition = "none",
                          ylimits=NULL,
                          colorFactor=NULL,facetFactor=NULL,linetypeFactor=NULL){


  gatherFramesTbl = table %>% gather(Frame, Values,  Frame0:Frame100)


  iData = filter(gatherFramesTbl,
                 Label == iLabel & Axis == iAxis  & EventContext == iEventContext)

  fig = ggplot() +
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    ggtitle(iTitle)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition)

  iData$inter <- interaction(iData$Index) # !!!! I spent lot of time to find that
  #iData$inter <- interaction(iData$Id,iData$Label,iData$EventContext,iData$Axis,iData$ComparisonFactor, iData$Cycle) # !!!! I spent lot of time to find that

  if ((!is.null(colorFactor)) &&   (!is.null(linetypeFactor))) {

    fig = fig + geom_line(data=iData,
                          aes_string(x="Frame",y="Values",
                                     color = colorFactor,
                                     linetype = linetypeFactor,
                                     group = "inter"),
                          size=0.5)
  } else if ((!is.null(colorFactor)) &&   (is.null(linetypeFactor))){

    fig = fig + geom_line(data=iData,
                          aes_string(x="Frame",y="Values",
                                     color = colorFactor,
                                     group = "inter"),
                          size=0.5)
  } else if ((is.null(colorFactor)) &&   !(is.null(linetypeFactor))){

    fig = fig + geom_line(data=iData,
                          aes_string(x="Frame",y="Values",
                                     linetype = linetypeFactor,
                                     group = "inter"),
                          size=0.5)
  }  else {

    fig = fig + geom_line(data=iData,
                          aes_string(x="Frame",y="Values",
                                     color = "EventContext",
                                     group = "inter"),
                          size=0.5)

  }
}



#' @title
#' descriptivePlot
#' @description
#' plot descriptive statistics of a frame sequence
#' @param  descStatsFrameSequence [dataframe] descriptive stats table of frame sequence
#' @param iLabel [string] label of the frame sequence
#' @param iEventContext [string] context of the frame sequence
#' @param iAxis [string] axis of the frame sequence
#' @param iTitle [string] plot title
#' @param yLabel [string] label of the Y-axis
#' @param legendPosition [string] position of the legend (see legend.position of ggplot2)
#' @param ylimits [list] limits of the y-axis
#' @param colorFactor [string] line color according an independant variable
#' @param facetFactor [string] create \textit{facet} plot ( see ggplot2) for an  independant variable
#' @param linetypeFactor [string] line type definied according an independant variable
#' @param lineWidth [float] line width value
#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
#'
descriptivePlot<-function(descStatsFrameSequence, iEventContext , iLabel,iAxis,
                          iTitle="",yLabel="Deg", legendPosition = "none",
                          colorFactor=NULL,facetFactor=NULL,linetypeFactor=NULL,
                          ylimits=NULL,
                          lineWidth=0.5){

  gatherFramesTbl = transform_frameData_descritiveStats(descStatsFrameSequence)

  iData = filter(gatherFramesTbl,
                 Stats == "mean" &
                   Label == iLabel & Axis == iAxis & EventContext == iEventContext )



  fig = ggplot() +
    ggtitle(iTitle)+
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition)


  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}



  iData$inter <- interaction(iData$Index) # !!!! I spent lot of time to find that

  if ((!is.null(colorFactor)) &&   (!is.null(linetypeFactor))) {

      fig = fig + geom_line(data=iData,
                          aes_string(x="Frame",y="Value",
                                     color = colorFactor,
                                     linetype = linetypeFactor,
                                     group = "inter"),
                          size=lineWidth)
  } else if ((!is.null(colorFactor)) &&   (is.null(linetypeFactor))){

    fig = fig + geom_line(data=iData,
                          aes_string(x="Frame",y="Value",
                                     color = colorFactor,
                                     group = "inter"),
                          size=lineWidth)
  } else if ((is.null(colorFactor)) &&   !(is.null(linetypeFactor))){

  fig = fig + geom_line(data=iData,
                        aes_string(x="Frame",y="Value",
                                   linetype = linetypeFactor,
                                   group = "inter"),
                        size=lineWidth)
  } else {


    fig = fig + geom_line(data=iData,
                          aes_string(x="Frame",y="Value",
                                     color = "EventContext",
                                     group = "inter"),
                          size=lineWidth)

    if (iEventContext=="Left"){
      fig = fig + scale_color_manual(values=c("red"))
    } else if (iEventContext=="Right"){
      fig = fig + scale_color_manual(values=c("blue"))
      }

  }



  if (!is.null(facetFactor)){ fig =  fig+facet_grid(paste0(".~", facetFactor))}


  return(fig)

}

#' @title
#' consistencyPlot_bothContext
#' @description
#' plot all left and right cycles of a frame sequence
#' @param  table [dataframe] all-cycle table
#' @param LabelLeft [string] label of the frame sequence for the left context
#' @param AxisLeft [string] axis of the frame sequence for the left context
#' @param LabelRight [string] label of the frame sequence for the right context
#' @param AxisRight [string] axis of the frame sequence for the Left context
#' @param iTitle [string] plot title
#' @param yLabel [string] label of the Y-axis
#' @param legendPosition [string] position of the legend (see legend.position of ggplot2)
#' @param ylimits [list] limits of the y-axis

#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
consistencyPlot_bothContext<-function(table,  LabelLeft,AxisLeft, LabelRight,AxisRight,
                                      iTitle="",yLabel="Deg", legendPosition = "none",
                                      ylimits=NULL){



  gatherFramesTbl = table %>% gather(Frame, Values,  Frame0:Frame100)

  iData_L = filter(gatherFramesTbl,
                 Label == LabelLeft & Axis == AxisLeft)

  iData_R = filter(gatherFramesTbl,
                     Label == LabelRight & Axis == AxisRight)


  iData = bind_rows(iData_L, iData_R)

  fig = ggplot() +
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    ggtitle(iTitle)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition)



  fig = fig + geom_line(data=iData,
                        aes(x=Frame,y=Values,
                            color = EventContext,
                            group=interaction(Index)),
                        size=0.5)


  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}



  fig

  return(fig)


}



#' @title
#' descriptivePlot_bothContext
#' @description
#' plot left and right descriptivestats  of a frame sequence
#' @param  descStatsFrameSequence [dataframe] descriptive stats table of frame sequence
#' @param LabelLeft [string] label of the frame sequence for the left context
#' @param AxisLeft [string] axis of the frame sequence for the left context
#' @param LabelRight [string] label of the frame sequence for the right context
#' @param AxisRight [string] axis of the frame sequence for the Left context
#' @param iTitle [string] plot title
#' @param yLabel [string] label of the Y-axis
#' @param legendPosition [string] position of the legend (see legend.position of ggplot2)
#' @param ylimits [list] limits of the y-axis

#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
descriptivePlot_bothContext<-function(descStatsFrameSequence,  LabelLeft,AxisLeft, LabelRight,AxisRight,
                                      iTitle="",yLabel="Deg", legendPosition = "none",
                                      ylimits=NULL){


  gatherFramesTbl = gather_descriptiveStats_FrameSequences( descStatsFrameSequence)


  iData_L = filter(gatherFramesTbl,
                   Stats == "mean" &
                     Label == LabelLeft & Axis == AxisLeft & EventContext == "Left" )

  iData_R = filter(gatherFramesTbl,
                   Stats == "mean" &
                     Label == LabelRight & Axis == AxisRight & EventContext == "Right")



  iData = bind_rows(iData_L, iData_R)


  fig = ggplot() +
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    ggtitle(iTitle)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition)

  fig = fig + geom_line(data=iData,
                        aes(x=Frame,y=Values,
                            color = EventContext,
                            group=interaction(Index)),
                        size=0.5)

  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}





  return(fig)

}



#---- figure add-on----


#' @title
#' addGaitDescriptiveEventsLines
#' @description
#' add gait events of a specific context as vertical lines to a figure
#' @param  [fig] ggplot2 figure
#' @param  [descStatsPhaseTable] descriptive stats table of gait phase scalar
#' (must include c("stancePhase","doubleStance1","doubleStance2")
#' @param  [EventContext] selected context
#' @param colorFactor [string] line color according an independant variable
#' @param linetypeFactor [string] line type definied according an independant variable
#'
#' @return []
#' @examples
#'
#' @section Warning:
#'
addGaitDescriptiveEventsLines<-function(fig,descStatsPhaseTable,iEventContext,
                                        colorFactor=NULL,linetypeFactor=NULL ){


  if ("ComparisonFactor" %ni% names(descStatsPhaseTable))
  {
    descStatsPhaseTable$ComparisonFactor =  descStatsPhaseTable$EventContext
  }


  gaitEventsTableFilt =  filter(descStatsPhaseTable, EventContext == iEventContext)


  gaitEvents = transform_scalar_DescriptiveStats(gaitEventsTableFilt,STP)

  gaitEvents = filter(gaitEvents, Stats == "mean" )


  if ((!is.null(colorFactor)) &&   (!is.null(linetypeFactor))) {
    fig = fig + geom_vline( data = filter(gaitEvents,Variable  == "stancePhase"),
                            aes_string(xintercept="Value",
                                color=colorFactor,
                                linetype = linetypeFactor),show_guide = FALSE)

  } else   if (!(is.null(colorFactor)) &&   (is.null(linetypeFactor))) {
    fig = fig + geom_vline( data = filter(gaitEvents,Variable  == "stancePhase"),
                            aes_string(xintercept="Value",
                                       color=colorFactor),show_guide = FALSE)

  } else  if ((is.null(colorFactor)) &&   !(is.null(linetypeFactor))) {

    fig = fig + geom_vline( data = filter(gaitEvents,Variable  == "stancePhase"),
                            aes_string(xintercept="Value",
                                       linetype=linetypeFactor),show_guide = FALSE)
  } else {

    fig = fig + geom_vline( data = filter(gaitEvents,Variable  == "stancePhase"),
                            aes_string(xintercept="Value",
                                       color="EventContext"),show_guide = FALSE)

  }

  return(fig)
}



#' @title
#' addGaitDescriptiveEventsLines_bothContext
#' @description
#' add gait events from left and right context as vertical lines to a figure
#' @param  [fig] ggplot2 figure
#' @param  [descStatsPhaseTable] descriptive stats table of gait phases
#' (must include c("stancePhase","doubleStance1","doubleStance2")
#'
#' @return []
#' @examples
#'
#' @section Warning:
#'
geom_vline_descriptiveEvents_bothContext<-function(descStatsPhaseTable){

  if ("ComparisonFactor" %ni% names(descStatsPhaseTable))
  {
    descStatsPhaseTable$ComparisonFactor =  descStatsPhaseTable$EventContext
  }


  gaitEvents =gather_descritiveStats(descStatsPhaseTable,"ComparisonFactor",c("stancePhase","doubleStance1","doubleStance2"))


  geom_vline( data = filter(gaitEvents,Factor == "stancePhase"),
                          aes(xintercept=Mean, color=ComparisonFactor))



}



#' @title
#' geom_normative_ribbon
#' @description
#' new ggplot2 geom displaying  corridor of normative data
#' @param  data [dataframe] normative data table
#' @return []
#' @examples
#'
#' @section Note:
#' programming as a new geom ( see https://rpubs.com/hadley/97970)

geom_normative_ribbon <- function(data) {
  # programming as a new geom ( see https://rpubs.com/hadley/97970)
  list(
    geom_ribbon(data = data,
                aes(ymin = Min, ymax = Max, x= Frame,group=interaction(Label,Axis)),
                fill = "grey70",alpha = 0.4)
    )
}


#' @title
#' geom_stdRibbon
#' @description
#' new ggplot2 geom displaying std corridor
#' @param  table [dataframe] all cycle table
#' @return []
#' @examples
#'
#' @section Note:
#' programming as a new geom ( see https://rpubs.com/hadley/97970)
geom_stdRibbon <- function(table,bySubjectFlag) {


  data = getStdCorridorLimits(table)


  if ("ComparisonFactor" %ni% names(data))
  {
    data$ComparisonFactor =data$EventContext
  }

  geom_ribbon(data = data,
              aes(ymin = Min, ymax = Max,fill = ComparisonFactor, x= Frame,group=interaction(ComparisonFactor,Label,Axis)),
              alpha = 0.4)


}

