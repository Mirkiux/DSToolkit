#' Imputar Valores Function
#'
#' @param dataset Dataset
#' @param especificacion Especification DataSet
#' @param not_remove not_remove = NULL
#' @param guess_if_na guess_if_na = TRUE
#' @param verbose verbose = TRUE
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @importFrom stats runif
#' @importFrom rlang .data
#'
#' @return dataset
#' @export
#'
#' @examples
imputar_valores <-function(dataset,especificacion,not_remove = NULL, guess_if_na = TRUE, verbose = TRUE){

  print("#################################################################")
  print("################Iniciando imputacion#############################")
  not_remove <- c(
    not_remove,
    especificacion %>% dplyr::filter(.data$accion != "remover") %>% pull(.data$variable)
  ) %>%
    unique()
  tabla_resumen <- resumir_variables(dataset) %>%
    left_join(
      especificacion %>% select(.data$variable,.data$default)
    ) %>%
    mutate(

      accion = if_else(
        .data$variable %in%  not_remove,
        "imputar",
        as.character(.data$accion)

      )
    )



  #añadiendo valores de imputacion por defecto
  if(guess_if_na){

    tabla_resumen <- tabla_resumen %>%
      mutate(

        default = coalesce(
          as.character(.data$default),
          case_when(
            .data$tipo %in% c("numeric","integer") ~ "0",
            .data$tipo %in% c("character","text") ~ "SIN_DATO",
            TRUE ~ .data$default
          )
        )

      )

  }

  #print("subseting")
  tabla_imputar_final <- tabla_resumen %>% filter(accion == "imputar" & !is.na(default) )
  variables_imputar <- tabla_imputar_final %>%
    pull("variable")
  valores_imputar <- tabla_imputar_final %>%
    pull("default") %>%
    as.character()

  #print("subseted")

  for ( i in 1:length(variables_imputar)){

    if(verbose) paste(variables_imputar[i],"intentando imputacion") %>% print()

    intento_numerico <- valores_imputar[i] %>% as.numeric()

    if ( !is.na(intento_numerico)){

      dataset[,variables_imputar[i]] <- if_else(
        is.na(dataset[,variables_imputar[i]]),
        intento_numerico,
        as.numeric(dataset[,variables_imputar[i]])
      )

    }
    else{

      if(coalesce(tolower(valores_imputar[i]),"") == "dist"){

        nombre_variable <- variables_imputar[i]
        population <-  dataset[
          !is.na( dataset[,variables_imputar[i]]),
          variables_imputar[i]]

        num_nas <- if_else(is.na(population),1,0) %>% sum()

        if (num_nas > 0){

          population[which(is.na(population))] %>% print
          stop("na's in population")
        }

        index_nas <- which(
          is.na(
            dataset[,variables_imputar[i]]
          )
        )

        num_missing <- length(index_nas)
        #population %>% class() %>% print

        index_draws <- runif(
          n = num_missing,
          min = 0.55,
          max = length(population)+0.45
        ) %>%
          as.integer()
        #print(index_draws)

        draws <- population[index_draws]


        dataset[index_nas,variables_imputar[i]] <- draws

      }
      else{

        if (class(variables_imputar[i]) %in% c("Date")){

          dataset[,variables_imputar[i]] <- coalesce(
            as.Date(dataset[,variables_imputar[i]]),
            as.Date(valores_imputar[i])
          )

        }
        else {

          dataset[,variables_imputar[i]] <- if_else(
            is.na(dataset[,variables_imputar[i]]),
            valores_imputar[i],
            as.character(dataset[,variables_imputar[i]])
          )


        }



      }

    }

    if(verbose) paste(variables_imputar[i],"imputacion correcta!") %>% print()

  }

  print("#################################################################")
  print("###################Imputacion terminada##########################")
  return(dataset)

}
