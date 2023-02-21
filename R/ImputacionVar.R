#' ImputacionVar Function
#'
#' Funcion imputacion de variables:
#' - numericas con el promedio
#' - categoricas con la categoria de mayor frecuencia.
#'
#' @param BDModelo Uncleaned Dataset
#'
#' @return BDModelo
#' @export
#'
#' @examples
ImputacionVar<-function(BDModelo){
  typevar=data.frame()
  for(i in 1:ncol(BDModelo)){
    tipvar=class((BDModelo[,i]))
    typevar2=as.data.frame(cbind(i,tipvar))
    typevar=rbind(typevar,typevar2)
  }
  typevar=cbind(typevar,names(BDModelo))
  table(typevar$tip)

  for (i in 1:ncol(BDModelo)){
    if  (class(BDModelo[,i])== "factor"){
      tabla1=as.data.frame(table(BDModelo[,i]))
      tabla1$freqporc=tabla1$Freq/sum(tabla1$Freq)
      var_imput=tabla1[tabla1$freqporc==max(tabla1$freqporc),]
      BDModelo[,i][is.na(BDModelo[,i])]<-var_imput$Var1
    } else {
      BDModelo[,i][is.na(BDModelo[,i])]<-0
    }
  }
  return(BDModelo)
}
