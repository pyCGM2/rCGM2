#' @title
#' normativeDataSetTransformation
#' @description
#' transform  the normative table  as long table with new columns max (mean+std) and min (mean-std)
#' @param normativeTable [dataframe] normative table
#' @return  dataframe.
#' @examples
#' TODO

normativeDataSetTransformation<- function(normativeTable){
  table_transform = normativeTable  %>%
    gather(Frame, Values,  Frame0:Frame100)%>%
    unite(temp, StatType) %>%
    spread(temp, Values)%>%
    mutate(Max= mean+sd, Min = mean-sd )

  return(table_transform)
}
