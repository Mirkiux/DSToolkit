#' Filtro1obs Function
#'
#' Exports a csv table that resumes observations per variable and
#' returns BDModelo with variables more than 1 observations
#'
#' @param BDModelo Uncleaned DataBase
#'
#' @importFrom utils write.csv
#' @return BDModelo
#' @export
#'
#' @examples
Filtro1obs<-function(BDModelo){
  nobsxvar=data.frame()
  for(i in 1:ncol(BDModelo)){
    nobss=length(unique(BDModelo[,i]))
    nombre=colnames(BDModelo[i])
    nobss=cbind(i,nobss,nombre)
    nobsxvar=rbind(nobsxvar,nobss)
  }
  nobsxvar$nobss1=as.numeric(as.character(nobsxvar$nobss))
  nameFile <- paste("lista_var_inicial",".csv",sep = "" )

  listavar=as.data.frame(nobsxvar)
  write.csv(listavar, file = nameFile,row.names = F)

  nobsxvar=nobsxvar[nobsxvar$nobss1>1,]

  BDModelo <-BDModelo[, which(names(BDModelo) %in% as.character(nobsxvar[,3]))]

  return(BDModelo)
}
