#' Title
#'
#' @param x Vector
#' @param not_numeric List of excepted variable
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @return
#'
#' @export
#'
#' @examples
try_to_numeric <- function(x, not_numeric = NULL){

  num_nas <- if_else(is.na(x),1,0) %>% sum()

  trial <- as.numeric(x)
  num_nas_x <- if_else(is.na(trial),1,0) %>% sum()

  result <- list()
  result[["exito"]] <- num_nas == num_nas_x

  if(result[["exito"]]){

    result[["datos"]] <- trial


  }
  else{

    result[["datos"]] <- x

  }

  return(result)
}
