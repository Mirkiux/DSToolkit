#' Nomalizar variable Function
#'
#' @param variable_vector
#' @param critical_value
#'
#' @return
#' @export
#'
#' @importFrom stats sd
#' @importFrom magrittr %>%
#'
#' @examples
nomalizar_variable <- function(variable_vector, critical_value = 1.96){

  vector_numeric <- try_to_numeric(variable_vector)

  if(vector_numeric[["exito"]]){

    variable_mean <- mean(vector_numeric[["datos"]], na.rm = TRUE)
    variable_sd <- sd(vector_numeric[["datos"]], na.rm = TRUE)

    z_score <- (variable_vector - variable_mean)/variable_sd
  }
  else{

    z_score <- variable_vector

  }

  vector_truncacion_sup <- variable_vector[z_score <= critical_value]

  if( length(vector_truncacion_sup) > 0){

    valor_truncacion_sup <- vector_truncacion_sup %>% max(na.rm = TRUE)
    variable_vector[z_score > critical_value] <- valor_truncacion_sup

  }

  vector_truncacion_inf <- variable_vector[z_score >= -critical_value] %>% min(na.rm = TRUE)

  if( length(vector_truncacion_inf) > 0){

    valor_truncacion_inf <- vector_truncacion_inf %>% min(na.rm = TRUE)
    variable_vector[z_score < -critical_value] <- valor_truncacion_inf

  }

  return(variable_vector)

}
