
`%+%` <- function(a, b) paste0(a, b)

`%ni%` <- Negate(`%in%`)


viewTable <- function(table) utils::View(table)


isColExist <- function(table, colname){

  if (colname %in% names(table)) {return(TRUE)}  else {return(FALSE)}
}


getColumIndex <- function(table, colname){
    index = which( colnames(table)==colname)
    return(index)
}



