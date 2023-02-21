#' Extraer Categoricas Function
#'
#' @param lista_seleccion Globl list of variables
#' @param lista_variables List of variables
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom utils adist
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#'
#' @return categoricas_obtenidas
#' @export
#'
#' @examples
extraer_categoricas <- function(lista_seleccion,lista_variables){


  categoricas <- lista_seleccion[!(lista_seleccion %in% lista_variables)]
  if ( length(categoricas) > 0){
    matriz_distancias <- adist(categoricas,lista_variables) %>% as_tibble()
    #print(matriz_distancias)
    matriz_distancias <- cbind(categoricas,matriz_distancias) %>% as_tibble()
    names(matriz_distancias) <- c("variable_categorica", lista_variables )
    matriz_distancias$variable_categorica <- as.character(matriz_distancias$variable_categorica)
    #print(matriz_distancias)
    nombre_lista_variables <- as.name(lista_variables)
    matriz_distancias <- pivot_longer(matriz_distancias, cols = lista_variables ,names_to = "variable_principal",values_to = "distancia")
    matriz_distancias_minimas <- matriz_distancias %>% group_by(.data$variable_categorica) %>% summarise(distancia = min(.data$distancia))
    #print(matriz_distancias_minimas)
    matriz_selecciones <- matriz_distancias_minimas %>%
      left_join(matriz_distancias)
    #print(matriz_selecciones)
    matriz_selecciones <- matriz_selecciones %>%
      filter(str_detect(.data$variable_categorica,.data$variable_principal))
    #print(matriz_selecciones)
    categoricas_obtenidas <- matriz_selecciones$variable_principal %>% unique()
  }
  else{
    categoricas_obtenidas <- c()
  }
  return(categoricas_obtenidas)

}
