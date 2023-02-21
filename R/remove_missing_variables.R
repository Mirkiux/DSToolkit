#' Remove missing variables Function
#'
#' @param dataset
#' @param not_remove
#' @param threshold
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @return
#' @export
#'
#' @examples
remove_missing_variables <- function(dataset,not_remove = NULL ,threshold = 0.9){

  to_remove <- names(dataset)
  to_remove <- to_remove[!(to_remove %in% not_remove)]

  for (i in to_remove){

    num_nas <- if_else(is.na(dataset[[i]]),1,0)
    porc_nas <- sum(num_nas)/length(num_nas)

    if(porc_nas > threshold){

      column_name <- as.name(i)
      dataset <- dataset %>% dplyr::select(- !!enquo(column_name))
      paste("remocion variables:",i,"descartada") %>% print()

    }

  }

  return(dataset)
}
