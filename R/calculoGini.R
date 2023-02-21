#' calculoGini Function
#'
#' @param newData DataBase
#' @param modelo Estimation output
#' @param VarDep Default = RESULTADO
#'
#' @importFrom ROCR performance
#' @importFrom ROCR prediction
#' @importFrom magrittr %>%
#' @importFrom stats predict
#'
#' @return res_perf
#' @export
#'
#' @examples
calculoGini<-function(newData,modelo,VarDep="RESULTADO"){
  newData$m1a_score<-predict(modelo,type='response',newData)
  m1a_pred<-prediction(newData$m1a_score,newData[[VarDep]])
  m1a_perf<-performance(m1a_pred,"tpr","fpr")

  #KS,Gini y AUC:
  m1a_KS=round(max(attr(m1a_perf,'y.values')[[1]]-attr(m1a_perf,'x.values')[[1]])*100,2)
  m1a_AUROC <- round(performance(m1a_pred, measure = "auc")@y.values[[1]]*100, 2)
  m1a_Gini <- (2*m1a_AUROC - 100)

  res_perf=as.data.frame(cbind(m1a_KS,m1a_AUROC,m1a_Gini))
  colnames(res_perf)=c("KS","ROC","GINI")
  rownames(res_perf)=paste0("Indicadores")
  return(res_perf)
}
