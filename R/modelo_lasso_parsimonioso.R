#' Modelo lasso parsimonioso Function
#'
#' @param dataset
#' @param max_variables
#' @param woe
#' @param graficar
#' @param titulo
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
#'
#' @return
#' @export
#'
#' @examples
modelo_lasso_parsimonioso <- function(dataset,max_variables=20, woe = FALSE, graficar = TRUE, titulo = "modelo lasso parsimonioso"){
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

  x.selected <- x
  max_auc <- 0

  for ( i in c(1:50)){ #performs a maximum of 50 iterations

    #performing feature selection
    cv.lasso <- cv.glmnet(x.selected, y, alpha = 1, family = "binomial",type.measure = "auc")

    #running model
    lasso.model <- glmnet(x.selected, y, alpha = 1, family = "binomial",
                          lambda = cv.lasso$lambda.1se, maxit = 1e+06)


    score_train <- lasso.model %>% predict(newx = x.selected, lambda = cv.lasso$lambda.1se) %>% as.numeric()
    roc_train <- roc(y,score_train)
    auc_train <- auc(roc_train)
    lasso_variables <- x.selected[,lasso.model$beta %>% attr("i")] %>% as_tibble() %>% names

    if(auc_train >= max_auc ){

      selected_model <- lasso.model
      max_auc <- auc_train
      selected_variables <- lasso_variables
      roc_train_selected  <- roc_train
      auc_train_selected <- auc_train
      x.selected <- x.selected[,c("RESULTADO",lasso_variables)]

    }
    else{

      break

    }
    if(length(lasso_variables) <= max_variables) break


  }


  results <- list()
  results[["lasso.model"]]  <- selected_model
  results[["variables"]] <- selected_variables
  results[["score_train"]] <- selected_model %>%
    predict(newx = x.selected, lambda = selected_model$lambda.1se) %>%
    as.numeric()

  x.test <- x.test[,names(x.selected %>% as_tibble())]

  results[["score_test"]] <- selected_model %>%
    predict(newx = x.test, lambda = selected_model$lambda.1se) %>% as.numeric()
  results[["train_selection"]] <- train_selection

  results[["variables"]] <- selected_variables

  roc_test <- roc(y.test,results[["score_test"]])
  auc_test <- auc(roc_test)

  results[["roc_test"]] <- roc_test
  results[["auc_test"]] <- auc_test
  results[["roc_train"]] <- roc_train_selected
  results[["auc_train"]] <- auc_train_selected

  if(graficar){



    results[["grafico_train"]] <- ggroc(results[["roc_train"]]) +
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


    results[["grafico_test"]] <- ggroc(results[["roc_test"]]) +
      ggplot2::ggtitle(
        paste(
          titulo,
          " - test set\nAUC:",
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
