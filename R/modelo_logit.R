#' Modelo Logit Function
#'
#' @param dataset
#' @param covariables
#' @param target_variable
#' @param woe
#' @param graficar
#' @param titulo
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#' @importFrom scorecard woebin
#' @importFrom scorecard woebin_ply
#' @importFrom tibble as_tibble
#' @importFrom pROC roc
#' @importFrom pROC auc
#' @importFrom pROC ggroc
#' @importFrom stats glm
#' @importFrom stats binomial
#'
#' @return
#' @export
#'
#' @examples
modelo_logit <- function(dataset,covariables,target_variable = "RESULTADO", woe = FALSE, graficar = TRUE, titulo = "modelo logit"){

  covariables_sin_interacciones <- covariables[!str_detect(covariables,"[*]")]
  dataset <- prepare_dataset(dataset[,c(target_variable,covariables_sin_interacciones)])

  if(woe){

    binning <- woebin(dataset,target_variable,covariables_sin_interacciones)
    dataset <- woebin_ply(dataset,binning)
    name_target <- as.name(target_variable)
    covariables <- dataset %>% select(-!!enquo(name_target)) %>% names

  }

  logit_formula <- paste(
    target_variable,
    paste(covariables,collapse = " + "),
    sep = " ~ "
  )

  logit_model <- glm(
    logit_formula,
    family=binomial(link='logit'),
    data = dataset
  )

  results <- list()
  results[["modelo"]] <- logit_model
  results[["resumen"]] <- summary(logit_model)$coefficients %>%
    as_tibble(rownames="variable")

  roc_model <- roc(
    logit_model$y,
    logit_model$fitted.values
  )

  results[["auc"]] <- auc(roc_model)

  if(graficar){



    results[["grafico"]] <- ggroc(roc_model) +
      ggplot2::ggtitle(
        paste(
          titulo,
          " \nAUC:",
          round(results[["auc"]],3),
          " GINI: ",
          round(results[["auc"]]*2-1,3),
          sep=""
        )
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 1,
          xend = 0,
          y = 0,
          yend = 1
        ),
        color="grey",
        linetype="dashed"
      )

  }
  return(results)
}
