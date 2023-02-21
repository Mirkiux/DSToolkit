#' Prepare dataset Function
#'
#' @param dataset
#' @param not_numeric
#' @param not_factor
#'
#' @export
#' @return
#'
#' @examples
prepare_dataset <- function(dataset, not_numeric = NULL, not_factor = NULL){

  dataset <- try_character_to_numeric(dataset, not_numeric)

  dataset <- characters_to_factor(dataset, not_factor)

  #removing factor variables with only one level

  dataset <- remove_constant_factors(dataset)

  dataset <- get_complete_variables(dataset)

  return(dataset)
}
