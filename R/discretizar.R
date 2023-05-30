#' discretizar function
#'
#' @param vector_valores
#' @param puntos_corte
#' @param etiquetas
#'
#' @return
#' @export
#'
#' @examples
discretizar <- function(vector_valores,puntos_corte = c(-Inf,2,10,30,Inf), etiquetas = c("unpredictive","weak","medium","strong")){

  if (is.null(etiquetas)){

    etiquetas <- puntos_corte[-1]
  }

  vector_clasificacion <- character(length = length(vector_valores))


  #Iniciando clasificacion

  for (i in 2:length(puntos_corte)){
    limite_inferior <- puntos_corte[i-1]
    limite_superior <- puntos_corte[i]
    vector_clasificacion[which(vector_clasificacion=="" & !is.na(vector_valores))] <- ifelse(
      vector_valores[which(vector_clasificacion=="" & !is.na(vector_valores))] >= limite_inferior & vector_valores[which(vector_clasificacion=="" & !is.na(vector_valores))] < limite_superior,
      etiquetas[i-1],
      vector_clasificacion[which(vector_clasificacion=="" & !is.na(vector_valores))]
    )

  }
  return(vector_clasificacion)
}
