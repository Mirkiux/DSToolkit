#' Try character to numeric Function
#'
#' @param dataset
#' @param not_numeric
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#'
#' @return
#' @export
#'
#' @examples
try_character_to_date <- function(dataset, not_date = NULL){

  character_columns <- dataset %>% select_if(is.character) %>% names
  character_columns <- character_columns[!(character_columns %in% not_date)]


  for ( i in character_columns){

    convertido <- dataset[[i]] %>% try_to_date()

    if(convertido[["exito"]]){

      dataset[[i]] <- convertido[["datos"]]

    }

  }

  return(dataset)
}
