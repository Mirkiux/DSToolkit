#' Resumir modelo logit Function
#'
#' @param logit
#' @param nombre_modelo
#'
#' @importFrom magrittr %>%
#' @importFrom tibble tribble
#' @importFrom dplyr mutate
#' @return
#' @export
#'
#' @examples
resumir_modelo_logit <- function(logit,nombre_modelo){
  tabla_resumen <- tribble(
    ~caracteristica, ~valor,
    "Num. Obs.", logit$modelo$y %>% length(),
    "Tasa de respuesta",round(100*(logit$modelo$y %>% mean()),2),
    "Gini", 2*logit$auc -1
  ) %>% mutate(
    `Nombre modelo` = nombre_modelo
  )


  return(tabla_resumen)

}
