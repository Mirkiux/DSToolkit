#' Prediccion Arbol Function
#'
#' @param df
#' @param modelo_1
#' @param modelo_2
#' @param target
#' @param destination
#' @param var_skip
#' @param id_variables
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_remove
#' @importFrom rpart rpart
#' @importFrom rpart rpart.control
#' @importFrom rpart prune
#' @importFrom stats predict
#'
#' @examples
prediccion_arbol <- function(df,modelo_1, modelo_2 = NULL,target = "ESTADO_FINAL",destination = "FLAG_PREDICCION_ARBOL",var_skip = NULL,id_variables = c("ID")){

  vector_variables <- c(
    modelo_1$resumen$variable,
    modelo_2$resumen$variable
  ) %>% unique()
  vector_variables <- vector_variables[vector_variables != "(Intercept)" & !(vector_variables %in% var_skip)] %>%
    str_remove("[_]woe$") %>%
    str_remove("[_]CORTADO[_].+$") %>%
    unique()


  formula_arbol <- paste(
    target,
    paste(vector_variables,collapse = " + "),
    sep =" ~ "
  )

  arbol_inicial <- rpart(
    formula_arbol,
    data = df,
    method = "class",
    control = rpart.control(
      cp = 0,
      minsplit = 100,
      maxdepth = 5
    )
  )

  cp_optimo <- arbol_inicial$cptable[arbol_inicial$cptable[,4] == min(arbol_inicial$cptable[,4]),1]

  arbol_podado <- prune(arbol_inicial,cp = cp_optimo)

  df[[destination]] <- predict(arbol_podado,type = "class",newdata = df)

  return(df[,c(id_variables,destination)])

}
