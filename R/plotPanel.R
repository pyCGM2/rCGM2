### GAIT PLOT PANEL  ####



#' @title
#' descriptiveKinematicGaitPanel
#' @description
#' convenient descriptive plot panel of gait kinematics for a specific context
#' @param  descStatsFrameSequence [dataframe] descriptive stats table of all frame sequences
#' @param  descStatsPhases [dataframe] descriptive stats table of gait phase scalar ()
#' @param iContext [string] context of the frame sequence
#' @param colorFactor [string] line color according an independant variable
#' @param linetypeFactor [string] line type definied according an independant variable
#' @param normativeData [dataframe] table of a normative dataset
#' @param stdCorridorFlag [Bool] add std corridor to plot
#' @param manualLineType [list] manual line type ( see ggplot2 doc)
#' @param manualSizeType [float] manual line size ( see ggplot2 doc)

#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
#'
#'
descriptiveKinematicGaitPanel<- function(descStatsFrameSequence,descStatsPhases, iContext,
                                         colorFactor=NULL, linetypeFactor=NULL,
                                         normativeData=NULL,stdCorridorFlag=FALSE,
                                         manualLineType=NULL,manualSizeType=NULL){




  if (iContext== "Left"){
    prefixe = "L"
  } else if (iContext== "Right"){
    prefixe = "R"
  } else if (iContext== "Overall"){
    prefixe = ""}

  # trace uni
  Pelvis_X = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"PelvisAngles"),"X",
                             iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(0,60),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Pelvis_Y = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"PelvisAngles"),"Y",
                             iTitle="Pelvic obliquity",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Pelvis_Z = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"PelvisAngles"),"Z",
                             iTitle="Pelvis rotation",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Hip_X = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"HipAngles"),"X",
                          iTitle="Hip flexion",yLabel="Deg", legendPosition="none",ylimits=c(-20,70),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Hip_Y = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"HipAngles"),"Y",
                          iTitle="Hip Abd",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Hip_Z = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"HipAngles"),"Z",
                          iTitle="Hip rot",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)


  Knee_X = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"KneeAngles"),"X",
                           iTitle="Knee flexion",yLabel="Deg", legendPosition="none",ylimits=c(-15,75),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Knee_Y = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"KneeAngles"),"Y",
                           iTitle="Knee Abd",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Knee_Z = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"KneeAngles"),"Z",
                           iTitle="Knee rot",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)


  Ankle_X = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"AnkleAngles"),"X",
                            iTitle="Ankle flexion",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Ankle_Y = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"AnkleAngles"),"Y",
                            iTitle="Ankle eversion",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  FootProgress_Z = descriptivePlot(descStatsFrameSequence,  iContext , paste0(prefixe,"FootProgressAngles"),"Z",
                                   iTitle="Foot progression",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                                   colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  if (!(is.null(descStatsPhases))){
    Pelvis_X=addGaitDescriptiveEventsLines(Pelvis_X,descStatsPhases,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Pelvis_Y=addGaitDescriptiveEventsLines(Pelvis_Y,descStatsPhases,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Pelvis_Z=addGaitDescriptiveEventsLines(Pelvis_Z,descStatsPhases,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Hip_X=addGaitDescriptiveEventsLines(Hip_X,descStatsPhases,iContext,
                                        colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Hip_Y=addGaitDescriptiveEventsLines(Hip_Y,descStatsPhases,iContext,
                                        colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Hip_Z=addGaitDescriptiveEventsLines(Hip_Z,descStatsPhases,iContext,
                                        colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Knee_X=addGaitDescriptiveEventsLines(Knee_X,descStatsPhases,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Knee_Y=addGaitDescriptiveEventsLines(Knee_Y,descStatsPhases,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Knee_Z=addGaitDescriptiveEventsLines(Knee_Z,descStatsPhases,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Ankle_X=addGaitDescriptiveEventsLines(Ankle_X,descStatsPhases,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Ankle_Y=addGaitDescriptiveEventsLines(Ankle_Y,descStatsPhases,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    FootProgress_Z=addGaitDescriptiveEventsLines(FootProgress_Z,descStatsPhases,iContext,
                                                 colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

  }


  if (!(is.null(normativeData))){
    Pelvis_X = Pelvis_X+geom_normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "X"))
    Pelvis_Y = Pelvis_Y+geom_normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Y"))
    Pelvis_Z = Pelvis_Z+geom_normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Z"))

    Hip_X = Hip_X+geom_normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "X"))
    Hip_Y = Hip_Y+geom_normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Y"))
    Hip_Z = Hip_Z+geom_normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Z"))

    Knee_X = Knee_X+geom_normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "X"))
    Knee_Y = Knee_Y+geom_normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Y"))
    Knee_Z = Knee_Z+geom_normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Z"))

    Ankle_X = Ankle_X+geom_normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "X"))
    Ankle_Y = Ankle_Y+geom_normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "Y"))
    FootProgress_Z = FootProgress_Z+geom_normative_ribbon(filter(normativeData,Label=="FootProgressAngles" & Axis == "Z"))
  }

  if (stdCorridorFlag){

    data = getStdCorridorLimits_fromDescStatFrameSequences(descStatsFrameSequence)

    # Pelvis_X = Pelvis_X +
    #   geom_ribbon(data = filter(data,Label=="PelvisAngles" & Axis == "X"),
    #               aes(ymin = Min, ymax = Max,fill = ComparisonFactor, x= Frame,group=interaction(ComparisonFactor,Label,Axis)),
    #               alpha = 0.4)

    Pelvis_X = Pelvis_X + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"PelvisAngles") & Axis == "X"))
    Pelvis_Y = Pelvis_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"PelvisAngles") & Axis == "Y"))
    Pelvis_Z = Pelvis_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"PelvisAngles") & Axis == "Z"))

    Hip_X = Hip_X + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"HipAngles") & Axis == "X"))
    Hip_Y = Hip_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"HipAngles") & Axis == "Y"))
    Hip_Z = Hip_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"HipAngles") & Axis == "Z"))

    Knee_X = Knee_X + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"KneeAngles") & Axis == "X"))
    Knee_Y = Knee_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"KneeAngles") & Axis == "Y"))
    Knee_Z = Knee_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"KneeAngles") & Axis == "Z"))

    Ankle_X = Ankle_X + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"AnkleAngles") & Axis == "X"))
    Ankle_Y = Ankle_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"AnkleAngles") & Axis == "Y"))
    FootProgress_Z = FootProgress_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"FootProgressAngles") & Axis == "Z"))

  }

  if (!(is.null(manualLineType))){
    Pelvis_X = Pelvis_X + scale_linetype_manual(values=manualLineType)
    Pelvis_Y = Pelvis_Y + scale_linetype_manual(values=manualLineType)
    Pelvis_Z = Pelvis_Z + scale_linetype_manual(values=manualLineType)

    Hip_X = Hip_X + scale_linetype_manual(values=manualLineType)
    Hip_Y = Hip_Y + scale_linetype_manual(values=manualLineType)
    Hip_Z = Hip_Z + scale_linetype_manual(values=manualLineType)

    Knee_X = Knee_X + scale_linetype_manual(values=manualLineType)
    Knee_Y = Knee_Y + scale_linetype_manual(values=manualLineType)
    Knee_Z = Knee_Z + scale_linetype_manual(values=manualLineType)

    Ankle_X = Ankle_X + scale_linetype_manual(values=manualLineType)
    Ankle_Y = Ankle_Y + scale_linetype_manual(values=manualLineType)
    FootProgress_Z = FootProgress_Z + scale_linetype_manual(values=manualLineType)

  }

  if (!(is.null(manualSizeType))){
    Pelvis_X = Pelvis_X + scale_size_manual(values=manualSizeType)
    Pelvis_Y = Pelvis_Y + scale_size_manual(values=manualSizeType)
    Pelvis_Z = Pelvis_Z + scale_size_manual(values=manualSizeType)

    Hip_X = Hip_X + scale_size_manual(values=manualSizeType)
    Hip_Y = Hip_Y + scale_size_manual(values=manualSizeType)
    Hip_Z = Hip_Z + scale_size_manual(values=manualSizeType)

    Knee_X = Knee_X + scale_size_manual(values=manualSizeType)
    Knee_Y = Knee_Y + scale_size_manual(values=manualSizeType)
    Knee_Z = Knee_Z + scale_size_manual(values=manualSizeType)

    Ankle_X = Ankle_X + scale_size_manual(values=manualSizeType)
    Ankle_Y = Ankle_Y + scale_size_manual(values=manualSizeType)
    FootProgress_Z = FootProgress_Z + scale_size_manual(values=manualSizeType)

  }




  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)


  legend_shared <- get_legend(Pelvis_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))

  fig = plot_grid(p1, p2,p3, p4,
                  legend_shared,
                  nrow = 5,rel_heights = c(1,1,1,1, .2))

  fig

  return(fig)

}



#' @title
#' descriptiveKinematicGaitPanel_bothContext
#' @description
#' convenient descriptive plot panel of gait kinematics for left and right contexts
#' @param  descStatsFrameSequence [dataframe] descriptive stats table of all frame sequences
#' @param  descStatsPhases [dataframe] descriptive stats table of gait phase scalar ()
#' @param normativeData [dataframe] table of a normative dataset
#' @param stdCorridorFlag [Bool] add std corridor to plot


#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
#'
#'

descriptiveKinematicGaitPanel_bothContext<- function(descStatsFrameSequence,descStatsPhases=NULL,
                                              normativeData=NULL,stdCorridorFlag=TRUE){
  #


  Pelvis_X = descriptivePlot_bothContext(descStatsFrameSequence, "LPelvisAngles","X", "RPelvisAngles","X",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(0,60))

  Pelvis_Y = descriptivePlot_bothContext(descStatsFrameSequence, "LPelvisAngles","Y", "RPelvisAngles","Y",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))

  Pelvis_Z = descriptivePlot_bothContext(descStatsFrameSequence, "LPelvisAngles","Z", "RPelvisAngles","Z",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))


  Hip_X = descriptivePlot_bothContext(descStatsFrameSequence, "LHipAngles","X", "RHipAngles","X",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-20,70))

  Hip_Y = descriptivePlot_bothContext(descStatsFrameSequence, "LHipAngles","Y", "RHipAngles","Y",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))

  Hip_Z = descriptivePlot_bothContext(descStatsFrameSequence, "LHipAngles","Z", "RHipAngles","Z",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))



  Knee_X = descriptivePlot_bothContext(descStatsFrameSequence, "LKneeAngles","X", "RKneeAngles","X",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-15,75))

  Knee_Y = descriptivePlot_bothContext(descStatsFrameSequence, "LKneeAngles","Y", "RKneeAngles","Y",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))

  Knee_Z = descriptivePlot_bothContext(descStatsFrameSequence, "LKneeAngles","Z", "RKneeAngles","Z",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))


  Ankle_X = descriptivePlot_bothContext(descStatsFrameSequence, "LAnkleAngles","X", "RAnkleAngles","X",
                                        iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))

  Ankle_Y = descriptivePlot_bothContext(descStatsFrameSequence, "LAnkleAngles","Y", "RAnkleAngles","Y",
                                        iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))

  FootProgress_Z = descriptivePlot_bothContext(descStatsFrameSequence, "LFootProgressAngles","Z", "RFootProgressAngles","Z",
                                               iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))

  if (!(is.null(descStatsPhases))){
    Pelvis_X = Pelvis_X+geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    Pelvis_Y = Pelvis_Y+ geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    Pelvis_Z= Pelvis_Z +geom_vline_descriptiveEvents_bothContext(descStatsPhases)

    Hip_X= Hip_X + geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    Hip_Y=Hip_Y + geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    Hip_Z=Hip_Z + geom_vline_descriptiveEvents_bothContext(descStatsPhases)

    Knee_X=Knee_X + geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    Knee_Y=Knee_Y + geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    Knee_Z=Knee_Z + geom_vline_descriptiveEvents_bothContext(descStatsPhases)

    Ankle_X=Ankle_X + geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    Ankle_Y=Ankle_Y + geom_vline_descriptiveEvents_bothContext(descStatsPhases)
    FootProgress_Z=FootProgress_Z +  geom_vline_descriptiveEvents_bothContext(descStatsPhases)
  }

  if (!(is.null(normativeData))){
    Pelvis_X = Pelvis_X+geom_normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "X"))
    Pelvis_Y = Pelvis_Y+geom_normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Y"))
    Pelvis_Z = Pelvis_Z+geom_normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Z"))

    Hip_X = Hip_X+geom_normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "X"))
    Hip_Y = Hip_Y+geom_normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Y"))
    Hip_Z = Hip_Z+geom_normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Z"))

    Knee_X = Knee_X+geom_normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "X"))
    Knee_Y = Knee_Y+geom_normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Y"))
    Knee_Z = Knee_Z+geom_normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Z"))

    Ankle_X = Ankle_X+geom_normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "X"))
    Ankle_Y = Ankle_Y+geom_normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "Y"))
    FootProgress_Z = FootProgress_Z+geom_normative_ribbon(filter(normativeData,Label=="FootProgressAngles" & Axis == "Z"))
  }

  if (stdCorridorFlag){

    data = getStdCorridorLimits_fromDescStatFrameSequences(descStatsFrameSequence)

    Pelvis_X = Pelvis_X + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LPelvisAngles","RPelvisAngles") & Axis == "X"))
    Pelvis_Y = Pelvis_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LPelvisAngles","RPelvisAngles") & Axis == "Y"))
    Pelvis_Z = Pelvis_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LPelvisAngles","RPelvisAngles") & Axis == "Z"))

    Hip_X = Hip_X + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LHipAngles","RHipAngles") & Axis == "X"))
    Hip_Y = Hip_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LHipAngles","RHipAngles") & Axis == "Y"))
    Hip_Z = Hip_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LHipAngles","RHipAngles") & Axis == "Z"))

    Knee_X = Knee_X + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LKneeAngles","RKneeAngles") & Axis == "X"))
    Knee_Y = Knee_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LKneeAngles","RKneeAngles") & Axis == "Y"))
    Knee_Z = Knee_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LKneeAngles","RKneeAngles") & Axis == "Z"))

    Ankle_X = Ankle_X + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LAnkleAngles","RAnkleAngles") & Axis == "X"))
    Ankle_Y = Ankle_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LAnkleAngles","RAnkleAngles") & Axis == "Y"))
    FootProgress_Z = FootProgress_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LFootProgressAngles","RFootProgressAngles") & Axis == "Z"))


  }


  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)



  legend_shared <- get_legend(Pelvis_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))

  fig = plot_grid(p1, p2,p3, p4,
                  legend_shared,
                  nrow = 5,rel_heights = c(1,1,1,1, .2))

  fig

  return(fig)

}

#' @title
#' descriptiveKineticGaitPanel
#' @description
#' convenient descriptive plot panel of gait kinetics for a specific context
#' @param  descStatsFrameSequence [dataframe] descriptive stats table of all frame sequences
#' @param  descStatsPhases [dataframe] descriptive stats table of gait phase scalar ()
#' @param iContext [string] context of the frame sequence
#' @param colorFactor [string] line color according an independant variable
#' @param linetypeFactor [string] line type definied according an independant variable
#' @param normativeData [dataframe] table of a normative dataset
#' @param stdCorridorFlag [Bool] add std corridor to plot
#' @param manualLineType [list] manual line type ( see ggplot2 doc)
#' @param manualSizeType [float] manual line size ( see ggplot2 doc)

#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
#'
#'


descriptiveKineticGaitPanel<- function(descStatsFrameSequence,descStatsPhases, iContext,
                                       colorFactor=NULL, linetypeFactor=NULL,
                                       normativeData=NULL,stdCorridorFlag=FALSE,
                                       manualLineType=NULL,manualSizeType=NULL){




  if (iContext== "Left"){
    prefixe = "L"
  } else if (iContext== "Right"){
    prefixe = "R"
  } else if (iContext== "Overall"){
    prefixe = ""}


  Hip_X = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"X",
                          iTitle="Hip extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-2.0,3.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Hip_Y = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"Y",
                          iTitle="Hip abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,2.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Hip_Z = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"Z",
                          iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Hip_Power = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"HipPower"),"Z",
                              iTitle="Hip power",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0),
                              colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)


  Knee_X = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"X",
                           iTitle="Knee extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Knee_Y = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"Y",
                           iTitle="Knee abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Knee_Z = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"Z",
                           iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Knee_Power = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"KneePower"),"Z",
                               iTitle="Knee power",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0),
                               colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)



  Ankle_X = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"X",
                            iTitle="Ankle extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,3.0),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Ankle_Y = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"Y",
                            iTitle="Ankle abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Ankle_Z = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"Z",
                            iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Ankle_Power = descriptivePlot(descStatsFrameSequence, iSubjects, "Overall" , paste0(prefixe,"AnklePower"),"Z",
                                iTitle="Ankle power",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-2.0,5.0),
                                colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)


  if (!(is.null(descStatsPhases))){
    Hip_X=addGaitDescriptiveEventsLines(Hip_X,descStatsPhases,iContext,
                                        colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Hip_Y=addGaitDescriptiveEventsLines(Hip_Y,descStatsPhases,iContext,
                                        colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Hip_Z=addGaitDescriptiveEventsLines(Hip_Z,descStatsPhases,iContext,
                                        colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Hip_Power=addGaitDescriptiveEventsLines(Hip_Power,descStatsPhases,iContext,
                                            colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Knee_X=addGaitDescriptiveEventsLines(Knee_X,descStatsPhases,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Knee_Y=addGaitDescriptiveEventsLines(Knee_Y,descStatsPhases,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Knee_Z=addGaitDescriptiveEventsLines(Knee_Z,descStatsPhases,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Knee_Power=addGaitDescriptiveEventsLines(Knee_Power,descStatsPhases,iContext,
                                             colorFactor=colorFactor, linetypeFactor=linetypeFactor  )



    Ankle_X=addGaitDescriptiveEventsLines(Ankle_X,descStatsPhases,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    Ankle_Y=addGaitDescriptiveEventsLines(Ankle_Y,descStatsPhases,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Ankle_Z=addGaitDescriptiveEventsLines(Ankle_Z,descStatsPhases,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )


    Ankle_Power=addGaitDescriptiveEventsLines(Ankle_Power,descStatsPhases,iContext,
                                              colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

  }
  if (!(is.null(normativeData))){

    Hip_X = Hip_X+geom_normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "X"))
    Hip_Y = Hip_Y+geom_normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Y"))
    Hip_Z = Hip_Z+geom_normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Z"))
    Hip_Power = Hip_Power+geom_normative_ribbon(filter(normativeData,Label=="HipPower" & Axis == "Z"))


    Knee_X = Knee_X+geom_normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "X"))
    Knee_Y = Knee_Y+geom_normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Y"))
    Knee_Z = Knee_Z+geom_normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Z"))
    Knee_Power = Knee_Power+geom_normative_ribbon(filter(normativeData,Label=="KneePower" & Axis == "Z"))

    Ankle_X = Ankle_X+geom_normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "X"))
    Ankle_Y = Ankle_Y+geom_normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Y"))
    Ankle_Z = Ankle_Z+geom_normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Z"))
    Ankle_Power = Ankle_Power+geom_normative_ribbon(filter(normativeData,Label=="AnklePower" & Axis == "Z"))


  }



  if (stdCorridorFlag){

    data = getStdCorridorLimits_fromDescStatFrameSequences(descStatsFrameSequence)


    Hip_X = Hip_X + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"HipMoment") & Axis == "X"))
    Hip_Y = Hip_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"HipMoment") & Axis == "Y"))
    Hip_Z = Hip_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"HipMoment") & Axis == "Z"))
    Hip_Power = Hip_Power + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"HipPower") & Axis == "Z"))


    Knee_X = Knee_X + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"KneeMoment") & Axis == "X"))
    Knee_Y = Knee_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"KneeMoment") & Axis == "Y"))
    Knee_Z = Knee_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"KneeMoment") & Axis == "Z"))
    Knee_Power = Knee_Power + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"KneePower") & Axis == "Z"))


    Ankle_X = Ankle_X + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"AnkleMoment") & Axis == "X"))
    Ankle_Y = Ankle_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"AnkleMoment") & Axis == "Y"))
    Ankle_Z = Ankle_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"AnkleMoment") & Axis == "Z"))
    Ankle_Power = Ankle_Power + geom_stdRibbon(filter(descStatsFrameSequence,Label==paste0(prefixe,"AnklePower") & Axis == "Z"))

  }

  if (!(is.null(manualLineType))){


    Hip_X = Hip_X + scale_linetype_manual(values=manualLineType)
    Hip_Y = Hip_Y + scale_linetype_manual(values=manualLineType)
    Hip_Z = Hip_Z + scale_linetype_manual(values=manualLineType)
    Hip_Power = Hip_Power + scale_linetype_manual(values=manualLineType)

    Knee_X = Knee_X + scale_linetype_manual(values=manualLineType)
    Knee_Y = Knee_Y + scale_linetype_manual(values=manualLineType)
    Knee_Z = Knee_Z + scale_linetype_manual(values=manualLineType)
    Knee_Power = Knee_Power + scale_linetype_manual(values=manualLineType)


    Ankle_X = Ankle_X + scale_linetype_manual(values=manualLineType)
    Ankle_Y = Ankle_Y + scale_linetype_manual(values=manualLineType)
    Ankle_Z = Ankle_Z + scale_linetype_manual(values=manualLineType)
    Ankle_Power = Ankle_Power + scale_linetype_manual(values=manualLineType)

  }

  if (!(is.null(manualSizeType))){
    Hip_X = Hip_X + scale_size_manual(values=manualSizeType)
    Hip_Y = Hip_Y + scale_size_manual(values=manualSizeType)
    Hip_Z = Hip_Z + scale_size_manual(values=manualSizeType)
    Hip_Power = Hip_Power + scale_size_manual(values=manualSizeType)

    Knee_X = Knee_X + scale_size_manual(values=manualSizeType)
    Knee_Y = Knee_Y + scale_size_manual(values=manualSizeType)
    Knee_Z = Knee_Z + scale_size_manual(values=manualSizeType)
    Knee_Power = Knee_Power + scale_size_manual(values=manualSizeType)


    Ankle_X = Ankle_X + scale_size_manual(values=manualSizeType)
    Ankle_Y = Ankle_Y + scale_size_manual(values=manualSizeType)
    Ankle_Z = Ankle_Z + scale_size_manual(values=manualSizeType)
    Ankle_Power = Ankle_Power + scale_size_manual(values=manualSizeType)


  }





  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_power,ncol=4)

  legend_shared <- get_legend(Hip_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))

  fig = plot_grid(p1,p2,p3,legend_shared,
                  nrow = 4, rel_heights = c(1,1,1,.2))

  fig

  return(fig)



}

#' @title
#' descriptiveKineticGaitPanel_bothContext
#' @description
#' convenient descriptive plot panel of gait kinematics for left and right contexts
#' @param  descStatsFrameSequence [dataframe] descriptive stats table of all frame sequences
#' @param  descStatsPhases [dataframe] descriptive stats table of gait phase scalar ()
#' @param normativeData [dataframe] table of a normative dataset
#' @param stdCorridorFlag [Bool] add std corridor to plot


#' @return fig [ggplot2 figure]
#' @examples
#'
#' @section Warning:
#'
#'
#'
descriptiveKineticGaitPanel_bothContext<- function(descStatsFrameSequence,descStatsPhases=NULL,
                                            normativeData=NULL,stdCorridorFlag=FALSE){


  Hip_X = descriptivePlot_bothContext(descStatsFrameSequence, "LHipMoment","X", "RHipMoment","X",

                                      iTitle="Hip extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-2.0,3.0))

  Hip_Y = descriptivePlot_bothContext(descStatsFrameSequence, "LHipMoment","Y", "RHipMoment","Y",

                                      iTitle="Hip abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,2.0))

  Hip_Z = descriptivePlot_bothContext(descStatsFrameSequence, "LHipMoment","Z", "RHipMoment","Z",

                                      iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))

  Hip_Power = descriptivePlot_bothContext(descStatsFrameSequence, "LHipMoment","Z", "RHipMoment","Z",

                                          iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0))


  Knee_X = descriptivePlot_bothContext(descStatsFrameSequence, "LKneeMoment","X", "RKneeMoment","X",

                                       iTitle="Knee extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0))

  Knee_Y = descriptivePlot_bothContext(descStatsFrameSequence, "LKneeMoment","Y", "RKneeMoment","Y",

                                       iTitle="Knee abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0))

  Knee_Z = descriptivePlot_bothContext(descStatsFrameSequence, "LKneeMoment","Z", "RKneeMoment","Z",

                                       iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))

  Knee_Power = descriptivePlot_bothContext(descStatsFrameSequence, "LKneeMoment","Z", "RKneeMoment","Z",

                                           iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0))

  Ankle_X = descriptivePlot_bothContext(descStatsFrameSequence, "LAnkleMoment","X", "RAnkleMoment","X",

                                        iTitle="Ankle extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,3.0))

  Ankle_Y = descriptivePlot_bothContext(descStatsFrameSequence, "LAnkleMoment","Y", "RAnkleMoment","Y",

                                        iTitle="Ankle abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))

  Ankle_Z = descriptivePlot_bothContext(descStatsFrameSequence, "LAnkleMoment","Z", "RAnkleMoment","Z",

                                        iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))

  Ankle_Power = descriptivePlot_bothContext(descStatsFrameSequence, "LAnkleMoment","Z", "RAnkleMoment","Z",

                                            iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-5.0,5.0))


  if (!(is.null(descStatsPhases))){

    Hip_X=addGaitDescriptiveEventsLines_bothContext(Hip_X,descStatsPhases)
    Hip_Y=addGaitDescriptiveEventsLines_bothContext(Hip_Y,descStatsPhases)
    Hip_Z=addGaitDescriptiveEventsLines_bothContext(Hip_Z,descStatsPhases)
    Hip_Power=addGaitDescriptiveEventsLines_bothContext(Hip_Power,descStatsPhases)

    Knee_X=addGaitDescriptiveEventsLines_bothContext(Knee_X,descStatsPhases)
    Knee_Y=addGaitDescriptiveEventsLines_bothContext(Knee_Y,descStatsPhases)
    Knee_Z=addGaitDescriptiveEventsLines_bothContext(Knee_Z,descStatsPhases)
    Knee_Power=addGaitDescriptiveEventsLines_bothContext(Knee_Power,descStatsPhases)

    Ankle_X=addGaitDescriptiveEventsLines_bothContext(Ankle_X,descStatsPhases)
    Ankle_Y=addGaitDescriptiveEventsLines_bothContext(Ankle_Y,descStatsPhases)
    Ankle_Z=addGaitDescriptiveEventsLines_bothContext(Ankle_Z,descStatsPhases)
    Ankle_Power=addGaitDescriptiveEventsLines_bothContext(Ankle_Power,descStatsPhases)

  }

  if (!(is.null(normativeData))){

    Hip_X = Hip_X+geom_normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "X"))
    Hip_Y = Hip_Y+geom_normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Y"))
    Hip_Z = Hip_Z+geom_normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Z"))
    Hip_Power = Hip_Power+geom_normative_ribbon(filter(normativeData,Label=="HipPower" & Axis == "Z"))


    Knee_X = Knee_X+geom_normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "X"))
    Knee_Y = Knee_Y+geom_normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Y"))
    Knee_Z = Knee_Z+geom_normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Z"))
    Knee_Power = Knee_Power+geom_normative_ribbon(filter(normativeData,Label=="KneePower" & Axis == "Z"))

    Ankle_X = Ankle_X+geom_normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "X"))
    Ankle_Y = Ankle_Y+geom_normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Y"))
    Ankle_Z = Ankle_Z+geom_normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Z"))
    Ankle_Power = Ankle_Power+geom_normative_ribbon(filter(normativeData,Label=="AnklePower" & Axis == "Z"))


  }
  if (stdCorridorFlag){

    data = getStdCorridorLimits_fromDescStatFrameSequences(descStatsFrameSequence)

    Hip_X = Hip_X + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LHipMoment","RHipMoment") & Axis == "X"))
    Hip_Y = Hip_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LHipMoment","RHipMoment") & Axis == "Y"))
    Hip_Z = Hip_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LHipMoment","RHipMoment") & Axis == "Z"))
    Hip_Power = Hip_Power + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LHipPower","RHipPower") & Axis == "Z"))


    Knee_X = Knee_X + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LKneeMoment","RKneeMoment") & Axis == "X"))
    Knee_Y = Knee_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LKneeMoment","RKneeMoment") & Axis == "Y"))
    Knee_Z = Knee_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LKneeMoment","RKneeMoment") & Axis == "Z"))
    Knee_Power = Knee_Power + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LKneePower","RKneePower") & Axis == "Z"))


    Ankle_X = Ankle_X + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LAnkleMoment","RAnkleMoment") & Axis == "X"))
    Ankle_Y = Ankle_Y + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LAnkleMoment","RAnkleMoment") & Axis == "Y"))
    Ankle_Z = Ankle_Z + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LAnkleMoment","RAnkleMoment") & Axis == "Z"))
    Ankle_Power = Ankle_Power + geom_stdRibbon(filter(descStatsFrameSequence,Label %in% c("LAnklePower","RAnklePower") & Axis == "Z"))


  }

  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_Power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_Power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_Power,ncol=4)

  legend_shared <- get_legend(Hip_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))

  fig = plot_grid(p1,p2,p3,legend_shared,
                  nrow = 4, rel_heights = c(1,1,1,.2))

  fig

  return(fig)

}



