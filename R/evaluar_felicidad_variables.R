#' Evaluar Felicidad Variables Function
#'
#' @param id_tibble ID
#' @param logit Logit model
#' @param especificacion_modelo  Model Formula
#' @param bd_madre Main DataBase
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#'
#' @return tibble_evaluacion
#' @export
#'
#' @examples
evaluar_felicidad_variables <- function(id_tibble, logit, especificacion_modelo, bd_madre){

  # No visible binding for global variable
  variables_data <- NULL


  resumen <- logit$modelo %>% summary()
  variables_logit <- resumen$terms %>% attr("term.labels")
  # print(variables_logit)
  variables_logit_crear <- variables_logit[str_detect(variables_logit,"[_]woe$")]
  # print(variables_logit_crear)
  variables_logit_existentes <- variables_logit[variables_logit %in% names(bd_madre)]
  variables_seleccion_bd_madre <- c(
    names(id_tibble),
    variables_logit_existentes,
    variables_logit_crear %>% str_remove("[_]woe$"),
    "ESTADO_INICIAL",
    "ESTADO_FINAL"
  ) %>%
    unique()
  #print(variables_seleccion_bd_madre)
  # print(variables_seleccion_bd_madre[variables_seleccion_bd_madre %in% names(bd_madre)])

  num_obs <- dim(id_tibble)[1]
  # print(variables_seleccion_bd_madre)
  # print(variables_seleccion_bd_madre[!(variables_seleccion_bd_madre %in% names(bd_madre))])
  base_trabajo <- left_join(id_tibble,bd_madre[,variables_seleccion_bd_madre])

  if(dim(base_trabajo)[1] != num_obs) stop("error al obtener datos de la base madre")

  base_trabajo <- left_join(
    base_trabajo,
    especificacion_modelo %>% select(.data$ESTADO_FINAL,.data$RESULTADO)
  ) %>%
    mutate(
      RESULTADO =if_else(is.na(.data$RESULTADO),1-especificacion_modelo$RESULTADO,.data$RESULTADO)
    )

  variables_finales_modelo <- c(variables_logit_existentes,variables_logit_crear)
  variables_finales_modelo <- variables_finales_modelo[variables_finales_modelo %in% logit$resumen$variable]

  tibble_evaluacion <- tibble(
    nombre_modelo = especificacion_modelo$NOMBRE_MODELO,
    variables_modelo = variables_finales_modelo,
    variables_data = str_remove(variables_finales_modelo,"[_]woe$")
  ) %>%
    mutate(
      variable_original = variables_data  %>% str_remove("[_]CORTADO[_].+$"),
      felicidad = sapply(variables_data, function(x){analizar_bivariados(base_trabajo,x,descriptivos = FALSE,plot=FALSE)$evaluacion_discriminacion$pendiente *  especificacion_modelo$FELICIDAD}),
      felicidad = if_else(.data$felicidad > 0, 1, 0)
    )

  print(especificacion_modelo$NOMBRE_MODELO)
  return(tibble_evaluacion)

}
