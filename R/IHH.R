#' IHH Function
#'
#' @param x Vector
#' @param num_elem Set to true if there are Null values in vector. Default = NULL
#'
#' @importFrom magrittr %>%
#'
#' @return ihh
#' @export
#'
#' @examples
IHH <- function(x,num_elem = NULL){

  if(!is.null(num_elem)) {
    x <- sort(x,decreasing = TRUE)
    x <- x[1:min(num_elem,length(x))]
  }

  ihh <-( ( (x/sum(x,na.rm=TRUE))*100 )^2 ) %>% sum(na.rm = TRUE)
  return(ihh)


}
