#' Truncar variable Function
#'
#' @param dataset
#' @param nombre_variable
#' @param percentiles Default = c(0.01,0.09)
#' @param porc_tolerancia_negativos  Default= 0.01
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @importFrom dplyr pull
#' @importFrom stats quantile
#'
truncar_variable <- function(dataset,nombre_variable,percentiles = c(0.01,0.09), porc_tolerancia_negativos = 0.01){

  observaciones_validas <- dataset[!is.na(dataset[[nombre_variable]]), ] %>% pull(nombre_variable)
  # print("observaciones validas")
  # print(observaciones_validas)

  porc_bajo_cero <- if_else(observaciones_validas <0, 1,0) %>% mean()

  # print("porcentaje bajo cero")
  # print(porc_bajo_cero)

  if(porc_bajo_cero <= porc_tolerancia_negativos){

    cotas <- c(
      0,
      quantile(
        dataset[[nombre_variable]],
        probs = c(percentiles[2]),
        na.rm = TRUE
      )
    )

  }
  else{

    cotas <- quantile(
      dataset[[nombre_variable]],
      probs = c(percentiles[1], percentiles[2]),
      na.rm = TRUE
    )
  }

  resultado <- if_else(!is.na(dataset[[nombre_variable]]) & dataset[[nombre_variable]] <= cotas[1], cotas[1],dataset[[nombre_variable]] )
  resultado <- if_else(!is.na(resultado) & resultado >= cotas[2], cotas[2],resultado)

  return(resultado)

}
