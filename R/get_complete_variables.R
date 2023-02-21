#' Get complete variables Function
#'
#' @param dataset Dataset
#'
#' @return dataset
#'
#' @importFrom rlang enquo
#' @importFrom dplyr if_else
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#'
#' @examples
get_complete_variables <- function(dataset){

  for (i in names(dataset)){

    num_nas <- if_else(is.na(dataset[[i]]),1,0) %>% sum()

    if(num_nas > 0){

      column_name <- as.name(i)
      dataset <- dataset %>% dplyr::select(- !!enquo(column_name))
      paste(i,"descartada") %>% print()

    }

  }

  return(dataset)
}
