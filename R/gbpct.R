#' gbpct function
#'
#' @param x x independent variable(vector)
#' @param y y dependent variable(vector)
#'
#' @return
#' @export
#'
#' @examples
gbpct <- function(x, y){
  mt <- as.matrix(table(as.factor(x), as.factor(y)))
  Total <- mt[,1] + mt[,2]                          # Total observations
  Total_Pct <- round(Total/sum(mt)*100, 2)          # Total PCT
  VarObj_pct <- round((mt[,"1"]/sum(mt[,"1"]))*100, 2)     # PCT of BAd or event or response
  VarNObj_pct <- round((mt[,"0"]/sum(mt[,"0"]))*100, 2)   # PCT of Good or non-event
  VarObj_Rate <- round((mt[,"1"]/(mt[,"1"]+mt[,"0"]))*100, 2) # Bad rate or response rate
  grp_score <- round((VarNObj_pct/(VarNObj_pct + VarObj_pct))*10, 2) # score for each group
  WOE <- ifelse(VarObj_pct==0,0,round(log(VarNObj_pct/VarObj_pct)*100, 2))      # Weight of Evidence for each group
  g_b_comp <- ifelse(mt[,"1"] == mt[,"0"], 0, 1)
  IV <- ifelse(g_b_comp == 0, 0, (VarNObj_pct - VarObj_pct)*(WOE/100)) # Information value for each group
  Efficiency <- abs(VarNObj_pct - VarObj_pct)/2                       # Efficiency for each group
  otb<-as.data.frame(cbind(mt, VarNObj_pct,  VarObj_pct,  Total,
                           Total_Pct,  VarObj_Rate, grp_score,
                           WOE, IV, Efficiency ))
  otb$Names <- rownames(otb)
  rownames(otb) <- NULL
  otb[,c(12,2,1,3:11)] # return IV table
  return (otb) # Modificado el 20/01/2022
}
