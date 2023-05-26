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
try_character_to_numeric <- function(dataset, not_numeric = NULL){

  character_columns <- dataset %>% select_if(is.character) %>% names
  character_columns <- character_columns[!(character_columns %in% not_numeric)]

  #adding treatment of integer variables
  character_columns <- c(
    character_columns,
    dataset %>% select_if(is.integer) %>% names
  )

  for ( i in character_columns){

    convertido <- dataset[[i]] %>% try_to_numeric()

    if(convertido[["exito"]]){

      dataset[[i]] <- convertido[["datos"]]

    }

  }

  return(dataset)
}
