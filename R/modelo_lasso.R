#' Modelo lasso
#'
#' @param dataset Dataset
#' @param woe Default = TRUE
#' @param graficar Default = TRUE
#' @param titulo Default = "modelo lasso"
#' @param ridge Default = FALSE
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#' @importFrom Rprofet BinProfet
#' @importFrom Rprofet WOEProfet
#' @importFrom tibble as_tibble
#' @importFrom stats model.matrix
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet glmnet
#' @importFrom pROC roc
#' @importFrom pROC auc
#' @importFrom pROC ggroc
#' @importFrom stats predict
#' @importFrom stats rbinom
#'
#' @return results
#' @export
#'
#' @examples
modelo_lasso <- function(dataset, woe = FALSE, graficar = TRUE, titulo ="modelo lasso", ridge = FALSE){
  ID <- NULL
  x <- prepare_dataset(dataset)

  if(woe){

    x$ID = seq(1:nrow(x)) ## make the ID variable
    binned <- BinProfet(x, id= "ID", target= "RESULTADO", num.bins = 10) ## Binning variables

    WOE_dat <- WOEProfet(binned, "ID","RESULTADO", c(3:(dim(binned)[2])))
    WOE_dat <- as_tibble(WOE_dat$WOE)
    x <- WOE_dat %>% dplyr::select(-ID)
    rm(WOE_dat)
    rm(binned)
  }

  y <- x$RESULTADO
  x <- model.matrix(RESULTADO ~ .,data = x)

  #splitting into train and test set
  train_selection <- if_else(rbinom(length(y),1,0.7) == 1,TRUE,FALSE)
  x.test <- x[!train_selection,]
  y.test <- y[!train_selection]
  x <- x[train_selection,]
  y <- y[train_selection]

  #performing feature selection
  cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial",type.measure = "auc")

  #running final model
  lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",
                        lambda = cv.lasso$lambda.1se, maxit = 1e+07)
  results <- list()
  results[["lasso.model"]]  <- lasso.model
  results[["cv.lasso"]]  <- cv.lasso
  results[["variables"]] <- x[,lasso.model$beta %>% attr("i")] %>% as_tibble() %>% names
  results[["score_train"]] <- lasso.model %>% predict(newx = x, lambda = cv.lasso$lambda.1se) %>% as.numeric()
  results[["score_test"]] <- lasso.model %>% predict(newx = x.test, lambda = cv.lasso$lambda.1se) %>% as.numeric()
  results[["train_selection"]] <- train_selection

  #Optionally performing ridge regression
  if(ridge){
    #performing feature selection
    cv.ridge <- cv.glmnet(x, y, alpha = 0, family = "binomial",type.measure = "auc")

    #running final model
    ridge.model <- glmnet(x, y, alpha = 0, family = "binomial",
                          lambda = cv.ridge$lambda.1se, maxit = 1e+06)

    results[["ridge.model"]] <- ridge.model
  }


  #Evaluating train set
  roc_train <- roc(y,results[["score_train"]])
  auc_train <- auc(roc_train)

  results[["roc_train"]] <- roc_train
  results[["auc_train"]] <- auc_train

  #evaluating test set
  roc_test <- roc(y.test,results[["score_test"]])
  auc_test <- auc(roc_test)

  results[["roc_test"]] <- roc_test
  results[["auc_test"]] <- auc_test

  if(graficar){



    results[["grafico_train"]] <- ggroc(roc_train) +
      ggplot2::ggtitle(
        paste(
          titulo,
          " - train set\nAUC:",
          round(results[["auc_train"]],3),
          " GINI: ",
          round(results[["auc_train"]]*2-1,3),
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


    results[["grafico_test"]] <- ggroc(roc_test) +
      ggplot2::ggtitle(
        paste(
          titulo,
          "Modelo benchmark - test set\nAUC:",
          round(results[["auc_test"]],3),
          " GINI: ",
          round(results[["auc_test"]]*2-1,3),
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
