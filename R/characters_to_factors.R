#' Characters to factor Function
#'
#' @param dataset Dataset
#' @param not_factor Default = NULL
#'
#' @return dataset
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#'
#' @export
#'
#' @examples

characters_to_factor <- function(dataset, not_factor = NULL){

  character_columns <- dataset %>% select_if(is.character) %>% names()
  character_columns <- character_columns[!(character_columns %in% not_factor)]

  for ( i in character_columns){

    dataset[[i]] <- dataset[[i]] %>% factor

  }

  return(dataset)

}
