#' Completar character columns Function
#'
#' @param dataset Dataset
#' @param cutoff_nas Default = 0.8
#' @param not_complete Default = NULL
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#'
#' @return dataset
#' @export
#'
#' @examples
completar_character_columns <- function(dataset, cutoff_nas = 0.8, not_complete = NULL){

  character_columns <- dataset %>% select_if(is.character) %>% names()
  character_columns <- character_columns[!(character_columns %in% not_complete)]

  for ( i in character_columns){

    num_uniques <- dataset[[i]] %>% unique()
    num_uniques <- num_uniques[!is.na(num_uniques)] %>% length()

    porc_nas <- dataset[[i]] %>% is.na() %>% if_else(1,0) %>% sum()
    porc_nas <- porc_nas/length(dataset[[i]])

    if(porc_nas<=cutoff_nas & num_uniques > 1){

      dataset[[i]] <- dataset[[i]] %>% is.na %>% if_else("SIN_DATO",dataset[[i]])

    }

  }

  return(dataset)

}
