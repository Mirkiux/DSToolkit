#' getPicturesAttrition Function
#'
#' @param attritionBase
#' @param varResponseRaw
#' @param varNamesModelRaw
#' @param varNamesModelLenRaw
#' @param nameFileRaw
#'
#' @importFrom dplyr summarise
#' @importFrom stats quantile
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom plyr ddply
#' @importFrom plyr .
#'
#' @return
#' @export
#'
#' @examples
getPicturesAttrition <- function (attritionBase, varResponseRaw, varNamesModelRaw, varNamesModelLenRaw, nameFileRaw)
{

  # no visible global function definition for
  decil <- POBLACION <- frecuencia <- frecuencia_f <- porcentaje <- porcentaje_f <- NULL

  nameFile <- nameFileRaw
  varResponse <- varResponseRaw
  varNames <- varNamesModelRaw
  varLen <- varNamesModelLenRaw
  #i=21
  #i<- 10
  z=10
  for ( i in 1:varLen){

    nameActualVar <-  varNames[i]

    cat(paste(i,') ' ,nameActualVar,"\n",sep=""))


    tryCatch(
      {
        png(paste(nameFile,nameActualVar,".png", sep=""))

        if (is.numeric(attritionBase[,nameActualVar])) {
          if (length(unique(attritionBase[,nameActualVar]))>2) {

            #attritionBase$decil <- discretize(attritionBase[,nameActualVar],method="frequency",categories=10)

            breaks<-unique(quantile(attritionBase[,nameActualVar],probs=seq(0,1,length=z)))
            #breaks<-unique(quantile(BDModelo[,nameActualVar],probs=seq(0,1,length=z)))

            #BDModelo$decil<-(cut(BDModelo[,c(nameActualVar)] ,breaks,include.lowest=TRUE,right=TRUE))
            attritionBase$decil<-(cut(attritionBase[,c(nameActualVar)] ,breaks,include.lowest=TRUE,right=TRUE))

            #BDModelo$decil <- discretize(BDModelo[,nameActualVar],method="frequency",categories=10)

            baseGraph<- data.frame(ddply(attritionBase,
                                         .(decil),
                                         summarise,
                                         porcentaje = sum(get(varResponse))/sum(POBLACION) * 100,#FLAGCANCELAVOLUNTARA)/sum(POBLACION) * 100,
                                         frecuencia = sum(POBLACION)
            ))

            baseGraph$porcentaje_f <- as.character(round(as.numeric(baseGraph$porcentaje),2))
            baseGraph$frecuencia_f <- prettyNum(baseGraph$frecuencia,big.mark=",",scientific=FALSE)

            colnames(baseGraph)[1] <- varNames[i]


            p1<-ggplot2::ggplot(data=baseGraph, ggplot2::aes(x= get(varNames[i]))) +
              ggplot2::geom_bar(ggplot2::aes(y= frecuencia),
                       stat = "identity",
                       color = "black",
                       fill = "#66CCCC")  +
              ggplot2::xlab(varNames[i])+
              ggplot2::theme_bw() +
              ggplot2::theme(legend.position="top") +
              ggplot2::geom_text(ggplot2::aes(y=frecuencia, label = frecuencia_f), size = 4, position=ggplot2::position_dodge(width=0.9), vjust=-0.5) +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
            # geom_line(aes( y= porcentaje, group = 1),
            #           color = "red",
            #           ylim = 0.10) +
            # scale_y_continuous(
            #   "mpg (US)",
            #   sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
            # )


            p2<- ggplot2::ggplot(data=baseGraph, ggplot2::aes(x= get(varNames[i]), group=1)) +
              ggplot2::geom_line(ggplot2::aes( y= porcentaje, group = 1),
                        color = "red",
                        size = 1) +
              ggplot2::theme_bw() +
              ggplot2::theme(panel.grid=ggplot2::element_blank()) +
              ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA))  +
              ggplot2::scale_y_continuous(limits=c(0,90)) +
              ggplot2::geom_text(ggplot2::aes(y=porcentaje, label = porcentaje_f), size = 4, position=ggplot2::position_dodge(width=0.9), vjust=-0.5)

            plot(double_axis_graph(p1,p2))

          }
          else {

            baseGraph<- data.frame(ddply(attritionBase,
                                         .(get(varNames[i])),
                                         summarise,
                                         porcentaje = sum(get(varResponse))/sum(POBLACION) * 100,
                                         frecuencia = sum(POBLACION)
            ))

            baseGraph$porcentaje_f <- as.character(round(as.numeric(baseGraph$porcentaje),2))
            baseGraph$frecuencia_f <- prettyNum(baseGraph$frecuencia,big.mark=",",scientific=FALSE)

            colnames(baseGraph)[1] <- varNames[i]


            p1<-ggplot2::ggplot(data=baseGraph, ggplot2::aes(x= get(varNames[i]))) +
              ggplot2::geom_bar(ggplot2::aes(y= frecuencia),
                       stat = "identity",
                       color = "black",
                       fill = "#66CCCC")  +
              ggplot2::xlab(varNames[i])+
              ggplot2::theme_bw() +
              ggplot2::theme(legend.position="top") +
              ggplot2::geom_text(ggplot2::aes(y=frecuencia, label = frecuencia_f), size = 4, position=ggplot2::position_dodge(width=0.9), vjust=-0.5) +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
            # geom_line(aes( y= porcentaje, group = 1),
            #           color = "red",
            #           ylim = 0.10) +
            # scale_y_continuous(
            #   "mpg (US)",
            #   sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
            # )


            p2<- ggplot2::ggplot(data=baseGraph, ggplot2::aes(x= get(varNames[i]), group=1)) +
              ggplot2::geom_line(ggplot2::aes( y= porcentaje, group = 1),
                        color = "red",
                        size = 1) +
              ggplot2::theme_bw() +
              ggplot2::theme(panel.grid=ggplot2::element_blank()) +
              ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA))  +
              ggplot2::scale_y_continuous(limits=c(0,30)) +
              ggplot2::geom_text(ggplot2::aes(y=porcentaje, label = porcentaje_f), size = 4, position=ggplot2::position_dodge(width=0.9), vjust=-0.5)

            plot(double_axis_graph(p1,p2))
          }


        }

        else {

          baseGraph<- data.frame(ddply(attritionBase,
                                       .(get(varNames[i])),
                                       summarise,
                                       porcentaje = sum(get(varResponse))/sum(POBLACION) * 100,
                                       frecuencia = sum(POBLACION)
          ))

          baseGraph$porcentaje_f <- as.character(round(as.numeric(baseGraph$porcentaje),2))
          baseGraph$frecuencia_f <- prettyNum(baseGraph$frecuencia,big.mark=",",scientific=FALSE)

          colnames(baseGraph)[1] <- varNames[i]


          p1 <- ggplot2::ggplot(data=baseGraph, ggplot2::aes(x= get(varNames[i]))) +
            ggplot2::geom_bar(ggplot2::aes(y= frecuencia),
                     stat = "identity",
                     color = "black",
                     fill = "#66CCCC")  +
            ggplot2::xlab(varNames[i])+
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position="top") +
            ggplot2::geom_text(ggplot2::aes(y=frecuencia, label = frecuencia_f), size = 4, position= ggplot2::position_dodge(width=0.9), vjust=-0.5) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
          # geom_line(aes( y= porcentaje, group = 1),
          #           color = "red",
          #           ylim = 0.10) +
          # scale_y_continuous(
          #   "mpg (US)",
          #   sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
          # )


          p2<- ggplot2::ggplot(data=baseGraph, ggplot2::aes(x= get(varNames[i]), group=1)) +
            ggplot2::geom_line(ggplot2::aes( y= porcentaje, group = 1),
                      color = "red",
                      size = 1) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.grid=ggplot2::element_blank()) +
            ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA))  +
            ggplot2::scale_y_continuous(limits=c(0,30)) +
            ggplot2::geom_text(ggplot2::aes(y=porcentaje, label = porcentaje_f), size = 4, position=ggplot2::position_dodge(width=0.9), vjust=-0.5)

          plot (double_axis_graph(p1,p2))
        }

        dev.off()
      },
      error=function(cond) {

        dev.off()

      }
    )

  }

}
