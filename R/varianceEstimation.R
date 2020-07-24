#' @title
#' VarianceEstCore
#' @description
#'
#' @param  []
#' @return []
#' @examples
#'
#' @section Warning:
#'
VarianceEstCore <- function(parameterTable, formStr){
  vcompNames=c(rev(gsub("1 \\| ","",attr(terms(as.formula(paste0("1",formstr))),"term.labels"))),"Residual")

  data = select(parameterTable,Frame0:Frame100)
  X = parameterTable
  Ynames = names(select(data,Frame0:Frame100))

  V=lapply(1:ncol(data),function(j){
    form=as.formula(paste0(Ynames[j],formstr))
    lmm=lmer(form,data=X,control=lmerControl())
    out=as.data.frame(VarCorr(lmm))[,c("grp","sdcor")]
    names(out)[2]=Ynames[j]
    out$grp=factor(vcompNames,levels=vcompNames[c((length(vcompNames)-1):1,length(vcompNames))])
    out
  })
  pV=Reduce(merge,V)
  pV=tidyr::gather(pV,Frames,sd,-grp,convert=T)
  pV$sd=round(pV$sd,5)

  sdData  = tidyr::spread(pV,Frames,sd)

  return(list(gather = pV,spread = sdData))
}

#' @title
#' plotVariance
#' @description
#'
#' @param  []
#' @return []
#' @examples
#'
#' @section Warning:
#'
plotVariance<-function(varianceTable){
  ggplot(data = varianceTable$gather,aes(x=Frames, y=sd,group = grp,color=grp ))+geom_line()+
  theme(panel.grid.minor = element_blank(),
        axis.title.x =      element_text(size = 10),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y =  element_text(size = 10),
        axis.text.y  = element_text(size=8),
        legend.text = element_text(size = 8),
        legend.title=element_blank(),
        plot.title = element_text(family="Times", face="plain", size=10),
        legend.position='top')
}

#' @title
#' getGlobalVariance
#' @description
#'
#' @param  []
#' @return []
#' @examples
#'
#' @section Warning:
#'

getGlobalVariance<-function(varianceTable){
  outTable  = varianceTable$gather %>%
    group_by(grp)%>%
    summarize(globalSD = round(sqrt(mean(sd^2)),1))
  return(outTable)
}

# TODO - Resume this code----------
# core model fucntion
# VarianceEstCore <- function(.data, formStr){
#   form=as.formula(paste0("Value~",formStr))
#   lmm=lmer(form,data=.data,control=lmerControl())
#   out=as.data.frame(VarCorr(lmm))[,c("grp","sdcor")]
#
#   return(out)
#
# }
#
#
#  main variance estimation
# varianceEstimation <-function(Data,Label,Axis,EventContext,formStr){
#   if (EventContext == "Overall"){
#     angles = filter(Data, Label==Label & Axis == Axis )
#
#     VarianceAtEachFrames = angles %>%
#       gather(Frames,Value,Frame0:Frame100)%>%
#       group_by(Frames) %>%
#       do(data.frame(vari=VarianceEstCore(.,formStr)))
#
#     VarianceAtEachFrames["Label"]=Label
#     VarianceAtEachFrames["Axis"]=Axis
#
#     VarianceGlobal = VarianceAtEachFrames  %>%
#       group_by(Label,Axis,vari.grp) %>%
#       summarize( sd =  round(sqrt(mean(vari.sdcor^2)),1))
#
#
#   } else {
#     angles = filter(Data, Label==Label & Axis == Axis & EventContext==EventContext)
#
#     VarianceAtEachFrames = angles %>%
#       gather(Frames,Value,Frame0:Frame100)%>%
#       group_by(Frames) %>%
#       do(data.frame(vari=VarianceEstCore(.,formStr)))
#
#     VarianceAtEachFrames["Label"]=Label
#     VarianceAtEachFrames["Axis"]=Axis
#     VarianceAtEachFrames["EventContext"]=EventContext
#
#
#     VarianceGlobal = VarianceAtEachFrames  %>%
#       group_by(Label,Axis,EventContext,vari.grp) %>%
#       summarize( sd =  round(sqrt(mean(vari.sdcor^2)),1))
#   }
#
#
#   return (list(frames=VarianceAtEachFrames,overall =VarianceGlobal))
# }
