#' Truncar outliers Function
#' @param dataset
#' @param percentiles Default = c(0.001,0.009)
#' @param porc_tolerancia_negativos Default = 0.01
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#'
#'
truncar_outliers <- function(dataset, percentiles = c(0.01,0.09), porc_tolerancia_negativos = 0.01){

  variables_a_truncar <- dataset %>% select_if(is.numeric) %>% names()

  for(variable in variables_a_truncar){

    dataset[[variable]] <- truncar_variable(dataset,variable)

  }

  return(dataset)

}
