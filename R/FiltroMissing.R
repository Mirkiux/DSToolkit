#' FiltroMissing Function
#'
#' Returns the number of missings per variable
#'
#' @param BDModelo Uncleaned DataBase
#' @param p Threshold of minimimun missing observations
#' @param nmissxvar Name of the filtered Data Base
#'
#' @return nmissxvar
#' @export
#'
#' @examples
FiltroMissing<-function(BDModelo,p,nmissxvar){
  nmissxvar=data.frame()
  for(i in 1:ncol(BDModelo)){
    nmiss=as.numeric(sum(is.na(BDModelo[,i])))
    nobs=as.numeric(length(BDModelo[,i])) #ERROR? nrow()
    temp1=as.data.frame(cbind(i,nmiss,nobs))
    temp1$porcmiss=as.numeric(as.character(temp1$nmiss))/as.numeric(as.character(temp1$nobs))
    nmissxvar=rbind(nmissxvar,temp1)    }

  #Excluyo a las variables que tienen mas del 50% de missings:
  nmissxvar=cbind(nmissxvar,names(BDModelo))
  nmissxvar=nmissxvar[nmissxvar$porcmiss<=0.50,]
  nmissxvar$rango_miss=ifelse(nmissxvar$porcmiss==0,"a.0 miss",
                              ifelse(nmissxvar$porcmiss<0.1,"b.0-10% miss",
                                     ifelse(nmissxvar$porcmiss<0.2,"c.10-20% miss",
                                            ifelse(nmissxvar$porcmiss<0.3,"d.20-30% miss",
                                                   ifelse(nmissxvar$porcmiss<0.4,"e.30-40% miss",
                                                          ifelse(nmissxvar$porcmiss<0.5,"f.40-50% miss","g.50% miss"))))))

  #Se decide no considerar a las variables con % missing >p
  #nmissxvar=nmissxvar[nmissxvar$porcmiss<=p,]
  return(nmissxvar)
}
