#' Quitar Function
#'
#' @param vector_original input vector
#' @param vector_remover vector to remove from input
#'
#' @importFrom magrittr %>%
#' @return resultado
#' @export
#'
#' @examples
#' quitar(c(1:10),c(1:5))
#' quitar(c("a","b","c"),"a")
quitar <- function(vector_original,vector_remover){
  resultado <- vector_original[!(vector_original %in% vector_remover)]
  return(resultado)
}
