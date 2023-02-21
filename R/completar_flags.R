#' Completar flags
#'
#' @param dataset Dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#'
#' @return dataset
#' @export
#'
#' @examples
completar_flags <- function(dataset){

  flag_columns <- dataset %>% select(matches("FLA?G")) %>% names()
  for (i in flag_columns){

    if  (is.numeric(dataset[[i]])){
      dataset[[i]] <- dataset[[i]] %>% is.na() %>% if_else(0,dataset[[i]])
    }
    else{

      dataset[[i]] <- as.character(dataset[[i]]) %>% is.na() %>% if_else("SIN_DATO",as.character(dataset[[i]]))

    }




  }

  return(dataset)

}
