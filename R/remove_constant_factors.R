#' Remove constant factor Function
#'
#' @param dataset
#' @param default_dummy
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @return
#' @export
#'
#' @examples
remove_constant_factors <- function(dataset,default_dummy = 0){

  factor_columns <- dataset %>% select_if(is.factor) %>% names()

  for (i in factor_columns){


    num_levels <- unique(dataset[[i]]) %>% length()

    if(num_levels <= 1  ){
      column_name <- as.name(i)
      dataset <- dataset %>% dplyr::select(-!!enquo(column_name))
      paste("remover factores constantes:",i,"factor removida") %>% print()
    }

  }

  #Also removing dummy variables with unique value
  numeric_columns <- dataset %>% select_if(is.numeric) %>% names()

  for (i in numeric_columns){


    num_levels <- unique(dataset[[i]]) %>% length()

    if(num_levels <= 1  ){

      #trying to fix by replacing na's by its default value
      dataset[[i]] <- if_else(
        is.na(dataset[[i]]),
        default_dummy,
        dataset[[i]]
      )
      if(num_levels <= 1  ){
        column_name <- as.name(i)
        dataset <- dataset %>% dplyr::select(-!!enquo(column_name))
        paste("remover factores constantes:",i,"dummy removida") %>% print()
      }

    }

  }


  return(dataset)
}
