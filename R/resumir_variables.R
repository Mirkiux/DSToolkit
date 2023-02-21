#' Resumir variables Function
#'
#' @param dataset
#' @param excepciones_remover Default = NULL
#' @param missing_threshold Default = 0.1
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom purrr reduce
#'
resumir_variables <- function(dataset,excepciones_remover = NULL,missing_threshold = 0.1){

  # no visible binding for global variable 'num_nas' 'porc_nas'
  num_nas <- porc_nas <- NULL

  lista_variables <- names(dataset)
  resumen_exante_variables <- lapply(lista_variables,function(x){select(dataset,x)}) %>%
    lapply(resumir_variable) %>%
    reduce(bind_rows) %>%
    mutate(
      accion = if_else(
        num_nas == 0,
        "completo",
        if_else(
          porc_nas <= missing_threshold,
          "imputar",
          "remover"
        )

      )
    )

  #modificando excepciones

  resumen_exante_variables$accion <- if_else(
    resumen_exante_variables$accion == "remover" & (resumen_exante_variables$accion %in% excepciones_remover),
    "imputar",
    resumen_exante_variables$accion
  )

  return(resumen_exante_variables)

}
