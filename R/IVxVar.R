#' IVxVar Function
#'
#' @param Btrain  train DataSet
#' @param varNames Variable Names from Train Dataset
#' @param varResponse Variable Names
#'
#' @importFrom stats glm
#' @importFrom stats quantile
#' @importFrom stats binomial
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#' @importFrom stats predict
#'
#' @return
#' @export
#'
#' @examples
IVxVar<-function(Btrain,varNames,varResponse = "RESULTADO"){
  var_respuesta=Btrain[, which(names(Btrain) %in% varResponse)]
  IV=data.frame()
  for (i in varNames){
    if  (class(Btrain[,i])== "numeric"){
      breaks<-c(-Inf,unique(quantile(Btrain[,i],probs=seq(0,1,length=10),na.rm = TRUE)),Inf)
      Btrain[[paste0("R_",i)]]<- (cut(Btrain[,i] ,breaks,include.lowest=TRUE,right=TRUE))
      table1=gbpct(Btrain[,paste0("R_",i)],var_respuesta)

      table_IV=table1[table1$WOE!=Inf,]
      table_IV=table_IV[table_IV$WOE!=-Inf,]
      table_IV=table_IV[!is.na(table_IV$IV),]
      variable=as.character(i)
      IV_value=sum(table_IV$IV)
      #CALCULO GINI
      set1 <- paste(varResponse," ~ ", i,sep = "")
      model1a <- glm(set1,Btrain,family=binomial())
      Btrain$m1a_score<-predict(model1a,type='response',Btrain)
      m1a_pred<-prediction(Btrain$m1a_score,Btrain[[varResponse]])
      m1a_perf<-performance(m1a_pred,"tpr","fpr")

      #KS,Gini y AUC:
      m1a_KS=round(max(attr(m1a_perf,'y.values')[[1]]-attr(m1a_perf,'x.values')[[1]])*100,2)
      m1a_AUROC <- round(performance(m1a_pred, measure = "auc")@y.values[[1]]*100, 2)
      m1a_Gini <- (2*m1a_AUROC - 100)
      #FIN CALCULO GINI
      # m1a_KS = NA
      # m1a_Gini = NA
      # m1a_AUROC = NA

      IVxvar=cbind(variable,IV_value,m1a_KS,m1a_Gini,m1a_AUROC)
      IV=rbind(IV,IVxvar)
      rm(IVxvar)
    }
    else {
      Btrain[[paste0("R_",i)]]<-Btrain[,i]
      table2=gbpct(Btrain[,paste0("R_",i)],var_respuesta)

      table_IV=table2[table1$WOE!=Inf,]
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
      m1a_KS = NA
      m1a_Gini = NA
      m1a_AUROC = NA
      IVxvar=cbind(variable,IV_value,m1a_KS,m1a_Gini,m1a_AUROC)
      IV=rbind(IV,IVxvar)
      rm(IVxvar)
    }
  }
  IV$IV=as.numeric(as.character(IV$IV_value))
  IV$GINI=as.numeric(as.character(IV$m1a_Gini))

  return(IV)
}
