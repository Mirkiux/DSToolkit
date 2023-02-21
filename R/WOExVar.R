#' WOExVar Function
#'
#' @param Btrain Train DataSet
#' @param varNames Variable Names from Train Dataset
#' @param varResponse Default = RESULTADO
#'
#' @return Btrain
#' @export
#'
#' @importFrom stats quantile
#'
#' @examples
WOExVar <- function(Btrain,varNames, varResponse = "RESULTADO"){
  var_respuesta=Btrain[, which(names(Btrain) %in% varResponse)]

  for (i in varNames){
    unicos<-matrix(unique((Btrain[,i])))
    num_var=nrow(unicos)
    if  (class(Btrain[,i])== "numeric"){
      if (num_var==2){
        Btrain[[paste0("R_",i)]]<-Btrain[,i]
        table2=gbpct(Btrain[,paste0("R_",i)],var_respuesta)

        table2=table2[,c("Names","WOE")]
        names(table2)<-c(i,paste0("WOE_",i))
        Btrain=merge(x = Btrain, y = table2, by =i , all.x = TRUE)
      }
      else{
        breaks<-c(unique(quantile(Btrain[,i],probs=seq(0,1,length=10))))
        #breaks<-c(unique(quantile(Btrain[,i],probs=seq(0,1,length=20))))
        Btrain[[paste0("R_",i)]]<- (cut(Btrain[,i] ,breaks,include.lowest=TRUE,right=TRUE))
        #table(Btrain[[paste0("R_",i)]])

        table1=gbpct(Btrain[,paste0("R_",i)],var_respuesta)

        table1=table1[,c("Names","WOE")]
        names(table1)<-c(paste0("R_",i),paste0("WOE_",i))
        #table(Btrain[[paste0("R_",i)]])

        Btrain=merge(x = Btrain, y = table1, by =paste0("R_",i) , all.x = TRUE)
      }}
    else {
      Btrain[[paste0("R_",i)]]<-Btrain[,i]
      #table(Btrain[[paste0("R_",i)]])

      table2=gbpct(Btrain[,paste0("R_",i)],var_respuesta)

      table2=table2[,c("Names","WOE")]
      names(table2)<-c(i,paste0("WOE_",i))
      Btrain=merge(x = Btrain, y = table2, by =i , all.x = TRUE)
    }
  }
  return(Btrain)
}
