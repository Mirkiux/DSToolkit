#' Resumir Variable Function
#'
#' @param dataset
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#'
resumir_variable <- function(dataset){

  nombre_variable <- names(dataset)

  tipo_variable <- dataset[[nombre_variable]] %>% class

  num_valores <- dataset[[nombre_variable]] %>%
    unique() %>%
    na.omit() %>%
    length()

  num_nas <- dataset[[nombre_variable]] %>%
    is.na() %>%
    if_else(1,0)

  porc_nas <- sum(num_nas)/length(num_nas)

  num_nas <- sum(num_nas)

  resultado <- tibble(
    variable = nombre_variable,
    tipo = tipo_variable,
    num_valores_unicos = num_valores,
    num_nas  = num_nas,
    porc_nas = porc_nas
  )

  return(resultado)
}
