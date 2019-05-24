#---- convenient metrics ----


#' @title
#' MeanAbsoluteVariability
#' @description
#' compute the mean absolute variability (see Mantovani2016) for all modalities
#' of the independant variables  (\textbf{ComparisonFactor})
#'
#' @param table [dataframe] all-cycle table
#' @return [dataframe]
#' @examples
#' MeanAbsoluteVariability(kinematicCyles)
#' @section Warning:
#' need construction of the factor ComparisonFactor

MeanAbsoluteVariability <- function(table){
  # compute mav for each comparison factor

  wide_Table = table %>%
    select(ComparisonFactor,Id,Label,Axis,Cycle,Context,Frame0:Frame100)%>%
    gather(Frames, Value, Frame0:Frame100)


  out = wide_Table%>%
    group_by(ComparisonFactor,Id,Label,Context,Axis,Frames)%>%
    summarise(diff = max(Value)-min(Value))%>%
    group_by(ComparisonFactor,Label,Context,Axis)%>%
    summarize(MAV = mean(diff))

  return (out)
}



#' @title
#' MinDetectableChange f
#' @description
#' compute the min detectable change (see Mantovani2016) for specific dependant variables
#'
#' @param table [dataframe] all-cycle table
#' @param dependantVariables [list] dependant variables
#' @return [dataframe]
#' @examples
#' mdc = MinDetectableChange(kinematicTable,c("Frame0","Frame001") )
#' @section Warning:
#' need construction of the factor ComparisonFactor

MinDetectableChange <- function(table, dependantVariables){

  gkt = table  %>%
    select("Id","Label","Context","Axis","Cycle","ComparisonFactor",dependantVariables)%>%
    gather_("Factor", "Values",  dependantVariables)%>%
    mutate(Factor = paste0("Sem_",Factor))%>%
    group_by(Label,Axis,Context,Factor)%>%
    do(anova = aov(Values ~ ComparisonFactor, data=.))%>%
    mutate(Sem = 1.96* sqrt(2)*sigma(anova))%>%
    select(-anova)%>%
    spread(Factor,Sem)

  return(gkt)

}



#--- Metrics on Frame Sequence ----

#' @title
#' computeMetrics_onFrameSequences
#' @description
#' compute basic metrics between two modalities of the independant variable (\textbf{ComparisonFactor})
#'  for all frame sequences
#'
#' @param table [dataframe] all-cycle table
#' @param metricsFunction [string] metrics ( eg, mae, rmse) to apply
#' @param modality1 [string] 1st selected modality of the independant variable (ComparisonFactor)
#' @param modality2 [string] 2nd selected modality of the independant variable (ComparisonFactor)
#' @param comparisonLabel [string] label given to the comparison
#' @param frameIndexes [list] selected frame indexes
#' @return [dataframe]
#' @examples
#' computeMetrics_onFrameSequences(kinematicTable,"rmse",  "modality1","modality2",comparisonLabel = "mod1_mod2")
#' @section Warning:
#' need construction of the factor ComparisonFactor

computeMetrics_onFrameSequences<- function(table,metricsFunction,modality1,modality2,comparisonLabel=NULL,frameIndexes=NULL){

  apply_metrics<-function(df,metricsFunction ,modality1,modality2,frameIndexes=NULL){

    if (is.null(frameIndexes)){
      b1 = which( colnames(wide_Table)==paste(modality1,"_Frame0",sep="" ))
      e1 = which( colnames(wide_Table)==paste(modality1,"_Frame100",sep="" ))

      b2 = which( colnames(wide_Table)==paste(modality2,"_Frame0",sep="" ))
      e2 = which( colnames(wide_Table)==paste(modality2,"_Frame100",sep="" ))
    } else {
      b1 = which( colnames(wide_Table)==paste0(modality1,"_Frame",frameIndexes[1]))
      e1 = which( colnames(wide_Table)==paste0(modality1,"_Frame",frameIndexes[2]))

      b2 = which( colnames(wide_Table)==paste0(modality2,"_Frame",frameIndexes[1]))
      e2 = which( colnames(wide_Table)==paste0(modality2,"_Frame",frameIndexes[2]))

    }

    valueA = as.numeric(df[b1:e1])
    valueB = as.numeric(df[b2:e2])

    res =  eval(parse(text= paste0("Metrics::",metricsFunction,"(valueA,valueB)")))
    #res = Metrics::rmse(valueA,valueB)
    return (res)
  }


  if ("Stats" %in% names(table)){
    table =  filter(table, Stats == "mean")
  }


  wide_Table = table %>%
    select(ComparisonFactor,Id,Label,Axis,Cycle,Context,Frame0:Frame100)%>%
    gather(Frames, Value, Frame0:Frame100)%>%
    unite(temp, ComparisonFactor, Frames) %>%
    spread(temp, Value)
  wide_Table$Index=seq(1,nrow(wide_Table))

  #ta =apply(wide_Table,1, processRms,indexA_begin,indexA_end,indexB_begin,indexB_end)

  out = wide_Table%>%
    rowwise()%>%
    do(data.frame(metricsFunction = apply_metrics(., metricsFunction,modality1,modality2,frameIndexes = frameIndexes)))%>%
    bind_cols(wide_Table%>%
                select(Id,Label,Axis,Cycle,Context))

  if (is.null(comparisonLabel)){comparisonLabel = paste0(modality1,"_",modality2)}
  out["ComparisonLabel"] = comparisonLabel

  names(out)[names(out) == "metricsFunction"] <- metricsFunction

  return(out)
}


#' @title
#' metrics_onFrames_local
#' @description
#' compute basic metrics between two modalities of the ComparisonFactor Factor for a specific point
#'
#' @param table [dataframe] all-cycle table
#' @param metricsFunction [string] metrics ( eg, mae, rmse) to apply
#' @param Label [string] label of the frame sequence
#' @param Context [string] context of the frame sequence
#' @param Axes [string] axis of the frame sequence
#' @param modality1 [string] 1st selected modality of the independant variable (ComparisonFactor)
#' @param modality2 [string] 2nd selected modality of the independant variable (ComparisonFactor)
#' @param comparisonLabel [string] label given to the comparison
#' @param frameIndexes [list] selected frame indexes
#' @return [dataframe]
#' @examples
#' computeMetrics_onFrameSequence(kinematicTable,"rmse", "LKneeAngles","Left",c("X","Y","Z"),"Rigid","THIsta")
#' @section Warning:
#' need construction of the factor ComparisonFactor


computeMetrics_onFrameSequence <- function(table,metricsFunction,Label,Context,Axes,modality1,modality2,comparisonLabel=NULL){

  #

  out = data.frame()

  if (is.null(comparisonLabel)){comparisonLabel = paste0(modality1,"_",modality2)}

  for (Axis in Axes){

    table1 = filter(table,ComparisonFactor == modality1 & Label == Label & Axis == Axis[1] & Context == Context)
    table2 = filter(table,ComparisonFactor == modality2 & Label == Label & Axis == Axis[1] & Context == Context)

    d1 = select(table1,starts_with("Frame0"): ends_with("Frame100"))
    d2 = select(table2,starts_with("Frame0"): ends_with("Frame100"))

    if (nrow(d1) != nrow(d2))
      stop("not the same number of cycle")

    value=c()
    for (i in 1:nrow(d1))
      value[i] =  eval(parse(text= paste0("Metrics::",metricsFunction,"(d1[i,],d2[i,])")))

    out = bind_rows(out,data.frame("Label" = Label, "Axis" = Axis, "Context" = Context,
                                   "comparison" = comparisonLabel,
                                   metricsFunction =  value))
  }

  names(out)[names(out) == "metricsFunction"] <- metricsFunction
  return (out)

}


#' @title
#' LinearFit_onFrameSequences
#' @description
#' Linear fitting between two modalities of the independant variable (\textbf{ComparisonFactor})
#'
#' @param table [dataframe] all-cycle table
#' @param modality1 [string] 1st selected modality of the independant variable (ComparisonFactor)
#' @param modality2 [string] 2nd selected modality of the independant variable (ComparisonFactor)
#' @param comparisonLabel [string] label given to the comparison
#' @param frameIndexes [list] selected frame indexes
#' @return [dataframe]
#' @examples
#' TODO
#' @section Warning:
#' need construction of the factor ComparisonFactor
#'
LinearFit_onFrameSequences<- function(table,modality1,modality2,comparisonLabel=NULL,frameIndexes=NULL){


  apply_lm<-function(df,modality1,modality2,frameIndexes=NULL){

    if (is.null(frameIndexes)){
      b1 = which( colnames(wide_Table)==paste(modality1,"_Frame0",sep="" ))
      e1 = which( colnames(wide_Table)==paste(modality1,"_Frame100",sep="" ))

      b2 = which( colnames(wide_Table)==paste(modality2,"_Frame0",sep="" ))
      e2 = which( colnames(wide_Table)==paste(modality2,"_Frame100",sep="" ))
    } else {
      b1 = which( colnames(wide_Table)==paste0(modality1,"_Frame",frameIndexes[1]))
      e1 = which( colnames(wide_Table)==paste0(modality1,"_Frame",frameIndexes[2]))

      b2 = which( colnames(wide_Table)==paste0(modality2,"_Frame",frameIndexes[1]))
      e2 = which( colnames(wide_Table)==paste0(modality2,"_Frame",frameIndexes[2]))

    }

    valueA = as.numeric(df[b1:e1])
    valueB = as.numeric(df[b2:e2])


    model = lm(valueA~valueB)
    model_sum = summary(model)
    out = list(a0 = model$coefficients[1], a1 = model$coefficients[2], R2 = model_sum$adj.r.squared )

    return (out)
  }


  if ("Stats" %in% names(table)){
    table =  filter(table, Stats == "mean")
  }


  wide_Table = table %>%
    select(ComparisonFactor,Id,Label,Axis,Cycle,Context,Frame0:Frame100)%>%
    gather(Frames, Value, Frame0:Frame100)%>%
    unite(temp, ComparisonFactor, Frames) %>%
    spread(temp, Value)
  wide_Table$Index=seq(1,nrow(wide_Table))

  #ta =apply(wide_Table,1, processRms,indexA_begin,indexA_end,indexB_begin,indexB_end)

  out = wide_Table%>%
    rowwise()%>%
    do(data.frame(a0 = apply_lm(.,modality1,modality2,frameIndexes)$a0, a1 = apply_lm(.,modality1,modality2,frameIndexes = frameIndexes)$a1, R2 = apply_lm(.,factor1,factor2,frameIndexes = frameIndexes)$R2))%>%
    bind_cols(wide_Table%>%
                select(Id,Label,Axis,Cycle,Context))

  if (is.null(comparisonLabel)){comparisonLabel = paste0(modality1,"_",modality2)}
  out["ComparisonLabel"] = comparisonLabel


  return(out)
}



#---- METRICS on Scalar  ----


#' @title
#' computeMetrics_onScalar
#' @description
#' compute basic metrics between two modalities of the independant variable (\textbf{ComparisonFactor})
#'  for a specific Scalar
#'
#' @param table [dataframe] all-cycle table
#' @param metricsFunction [string] metrics ( eg, mae, rmse) to apply
#' @param modality1 [string] 1st selected modality of the independant variable (ComparisonFactor)
#' @param modality2 [string] 2nd selected modality of the independant variable (ComparisonFactor)
#' @param comparisonLabel [string] label given to the comparison
#' @return [dataframe]
#' @examples
#' TODO
#' @section Warning:
#' need construction of the factor ComparisonFactor
#'


#' @section Warning:
#' need construction of the factor ComparisonFactor
#'
computeMetrics_onScalar<- function(table,metricsFunction,DiscreteLabel,modality1,modality2,comparisonLabel=NULL){

  apply_metrics<-function(df,metricsFunction ,modality1,modality2){

    valueA = as.numeric(df[modality1])
    valueB = as.numeric(df[modality2])

    res =  eval(parse(text= paste0("Metrics::",metricsFunction,"(valueA,valueB)")))     #res = Metrics::rmse(valueA,valueB)

    return (res)
  }


  if ("Stats" %in% names(table)){
    table =  filter(table, Stats == "mean")
  }


  wide_Table = table %>%
    select("ComparisonFactor","Id","Label","Axis","Cycle","Context",DiscreteLabel)%>%
    gather_("Method", "Value", DiscreteLabel)%>%
    unite_("temp", "ComparisonFactor", DiscreteLabel) %>%
    spread(temp, Value)
  wide_Table$Index=seq(1,nrow(wide_Table))


  out = wide_Table%>%
    group_by(Id,Label,Context,Axis,Cycle)%>%
    do(data.frame(metricsFunction = apply_metrics(., metricsFunction,modality1,modality2)))

  if (is.null(comparisonLabel)){comparisonLabel = paste0(modality1,"_",modality2)}
  out["ComparisonLabel"] = comparisonLabel

  names(out)[names(out) == "metricsFunction"] <- metricsFunction

  return(out)
}



