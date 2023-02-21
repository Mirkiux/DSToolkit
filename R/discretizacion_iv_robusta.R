#' Discretizacion IV robusta Function
#'
#' @param vector_variable Original Variable/vector
#' @param num_levels Number of Levels
#' @param intentos_por_num_levels Default = 10
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr pull
#' @importFrom utils head
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom stats quantile
#' @importFrom rlang .data
#'
#' @return vector_discretizado
#'
#' @examples
discretizacion_iv_robusta <- function(vector_variable,num_levels, intentos_por_num_levels = 10){

  puntos_corte_candidatos <- quantile(vector_variable, probs = seq(0,1,1/num_levels),na.rm = TRUE)
  duplicados_encontrados <- puntos_corte_candidatos[duplicated(puntos_corte_candidatos)] %>% unique()
  duplicados_almacenados <- duplicados_encontrados

  conteo_intentos <- 0
  while(length(duplicados_encontrados)>0){

    if (conteo_intentos <= intentos_por_num_levels){

      puntos_corte_candidatos <- quantile(
        vector_variable[!(vector_variable %in% duplicados_almacenados)],
        probs = seq(0,1,1/num_levels)
      )
      duplicados_encontrados <- puntos_corte_candidatos[duplicated(puntos_corte_candidatos)] %>%
        unique()
      duplicados_almacenados <- c(
        duplicados_almacenados,
        duplicados_encontrados
      )
      conteo_intentos <- conteo_intentos + 1

    }
    else{
      print("reduciendo num_levels")
      num_levels <- num_levels - 1
      print(num_levels)
      puntos_corte_candidatos <- quantile(vector_variable, probs = seq(0,1,1/num_levels),na.rm = TRUE)
      duplicados_encontrados <- puntos_corte_candidatos[duplicated(puntos_corte_candidatos)] %>% unique()
      duplicados_almacenados <- duplicados_encontrados
      conteo_intentos <- 0


    }


  }

  puntos_corte <- c(head(puntos_corte_candidatos,-1),Inf)

  vector_discretizado <- if_else(
    vector_variable %in% duplicados_almacenados,
    paste("acumulacion",vector_variable),
    discretizar(
      vector_variable,
      puntos_corte,
      paste("grupo",c(1:(length(puntos_corte)-1)) %>% as.character())
    )
  )
  orden_etiquetas <- tibble(variable = vector_variable, discretizacion = vector_discretizado) %>%
    group_by(.data$discretizacion) %>%
    summarise(min = min(.data$variable, na.rm = TRUE)) %>%
    arrange(min) %>%
    pull(.data$discretizacion)

  vector_discretizado <- factor(vector_discretizado,levels = orden_etiquetas)

  return(vector_discretizado)

}
