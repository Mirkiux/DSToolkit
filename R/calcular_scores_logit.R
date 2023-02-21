#' Calcular Scores Logit
#'
#' @param id_tibble ID
#' @param logit Model Result
#' @param especificacion_modelo Model Formula
#' @param bd_madre Main Dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom magrittr %>%
#' @importFrom stringr str_remove
#' @importFrom scorecard woebin
#' @importFrom stats predict
#' @importFrom grr matches
#' @importFrom rlang .data
#'
#' @return score
#' @export
#'
#' @examples
calcular_scores_logit <- function(id_tibble, logit, especificacion_modelo, bd_madre){

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

  #Creando woes

  #base_trabajo %>% names %>% print
  woe_binning <- woebin(
    dt = base_trabajo,
    y = "RESULTADO",
    x = variables_logit_crear %>% str_remove("[_]woe$"),
    var_skip = c("ID","ESTADO_INICIAL","ESTADO_FINAL"))

  woe_datos <- woebin_ply(
    base_trabajo[,c("ID",variables_logit_crear %>% str_remove("[_]woe$"))],
    woe_binning
  )
  # woe_datos %>% names() %>% print()

  base_trabajo <- left_join(base_trabajo,woe_datos)

  if(dim(base_trabajo)[1] != num_obs) stop("error al generar variables en woes")

  base_trabajo %>% select(-matches("^ID$")) %>% apply(2, function(x) {if_else(is.na(x),1,0) %>% mean}) %>% print()

  base_trabajo$SCORE <- predict(logit$modelo,type = "response", base_trabajo)

  score <- base_trabajo$SCORE

  return(score)

}
