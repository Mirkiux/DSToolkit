#' IVWOE Function
#'
#' @param Btrain Train DataSet
#' @param varNames Variable Names from Train Dataset
#' @param VarRpta Variable Names
#'
#' @return IV
#' @export
#'
#' @examples
IVWOE<-function(Btrain,varNames,VarRpta){
  var_respuesta=Btrain[, which(names(Btrain) %in% VarRpta)]
  IV=data.frame()
  for (i in varNames){
    table1=gbpct(Btrain[,i],var_respuesta)

    table_IV=table1[table1$WOE!=Inf,]
    table_IV=table_IV[table_IV$WOE!=-Inf,]
    table_IV=table_IV[!is.na(table_IV$IV),]
    variable=as.character(i)
    IV_value=sum(table_IV$IV)

    # set1 <- paste("VarObj"," ~ ", i,sep = "")
    # model1a <- glm(set1,Btrain,family=binomial())
    # Btrain$m1a_score<-predict(model1a,type='response',Btrain)
    # m1a_pred<-prediction(Btrain$m1a_score,Btrain$VarObj)
    # m1a_perf<-performance(m1a_pred,"tpr","fpr")
    #
    # #KS,Gini y AUC:
    # m1a_KS=round(max(attr(m1a_perf,'y.values')[[1]]-attr(m1a_perf,'x.values')[[1]])*100,2)
    # m1a_AUROC <- round(performance(m1a_pred, measure = "auc")@y.values[[1]]*100, 2)
    # m1a_Gini <- (2*m1a_AUROC - 100)
    #
    # IVxvar=cbind(variable,IV_value,m1a_KS,m1a_Gini,m1a_AUROC)
    # IV=rbind(IV,IVxvar)
    # rm(IVxvar)

  }
  IV$IV=as.numeric(as.character(IV$IV_value))
  #IV$GINI=as.numeric(as.character(IV$m1a_Gini))

  return(IV)
}
