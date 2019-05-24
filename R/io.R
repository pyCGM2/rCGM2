#' @title
#' constructTableFromXls
#' @description
#' load annd concatenate a list of excel spreadsheet
#'
#' @param  fullXlsFile [String] full filename (path+fileame) of excel spreadsheets
#' @param  sheet [String] sheet name
#' @return []
#' @examples
#'
#'


constructTableFromXls <- function(fullXlsFiles, sheet){

  # construit une dataframe a partir d une feuille de plusieurs ficheirs excel
  table = data.frame()
  for (xlsfile in fullXlsFiles){
    data = read_excel(xlsfile, sheet = sheet ,col_names = TRUE)
    table = rbind(table,data)
  }

  table$Index = seq(1,nrow(table))
  return (  table )
}

#---- Normatibe dataset ----

#' @title
#' loadNormativeDataSet
#' @description
#' load  normative dataset
#'
#' @param  fullXlsFile [String] full filename (path+fileame) of the selected dataset
#' @return []
#' @examples
#'
#'


loadNormativeDataSet <- function(fullXlsFile, sheet){

  data = read_excel(fullXlsFile, sheet = sheet ,col_names = TRUE)

  return (  data )
}
