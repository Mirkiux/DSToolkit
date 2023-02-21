#' To_woe Function
#'
#' @param dataset
#' @param variable
#' @param varobj
#' @param bin_num_limit
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyselect  all_of
#' @importFrom scorecard woebin
#' @importFrom scorecard woebin_ply
#'
#' @return
#' @export
#'
#' @examples
to_woe <- function(dataset,variable, varobj = "RESULTADO", bin_num_limit = 8){

  bd_woe <- dataset %>%
    select(
      all_of( c(variable,varobj) )
    )
  bin_info <- woebin(bd_woe,varobj,variable, bin_num_limit = bin_num_limit)
  binned_variable <- woebin_ply(bd_woe, bin_info)

  return(binned_variable[[variable %>% paste0("_woe") ]])
}
