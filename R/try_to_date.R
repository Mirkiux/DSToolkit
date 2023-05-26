#' Title
#'
#' @param x Vector
#' @param not_date List of excepted variable
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @return
#'
#' @export
#'
#' @examples
try_to_date <- function(x, not_date = NULL){

  num_nas <- if_else(is.na(x),1,0) %>% sum()

  result <- tryCatch(
    {
      list(
        exito = TRUE,
        datos = as.Date(x)
      )

    },
    error=function(cond) {

      return(
        list(
          exito = FALSE,
          datos = x
        )
      )
    }
  )


  return(result)
}
