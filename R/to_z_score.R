#' to_z_score Function
#'
#' @param variable_vector
#'
#' @return
#' @export
#'
#' @importFrom stats sd
#'
#' @examples
to_z_score <- function(variable_vector){

  vector_numeric <- try_to_numeric(variable_vector)

  if(vector_numeric[["exito"]]){

    variable_mean <- mean(vector_numeric[["datos"]], na.rm = TRUE)
    variable_sd <- sd(vector_numeric[["datos"]], na.rm = TRUE)

    z_score <- (variable_vector - variable_mean)/variable_sd
  }
  else{

    z_score <- variable_vector

  }


  return(z_score)
}
