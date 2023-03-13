#' Title
#'
#' @param dataset_original
#' @param dataset_adicional
#' @param cols_to_complete
#' @param id_cols
#' @param ordering_col
#'
#' @return
#' @export
#'
#' @examples
complementar_info_anterior <- function(
    dataset_original,
    dataset_adicional,
    cols_to_complete,
    id_cols = c("ctotal_idinstancia"),
    ordering_col = "id_current_stage"
){

  dataset_adicional_listo <- dataset_adicional %>%
    select(
      all_of(c(id_cols,cols_to_complete,ordering_col)) #selecionando las columnas relevantes
    ) %>%
    pivot_longer(
      cols = all_of(cols_to_complete), #expandiendo hacia abajo la tabla
      names_to = "name",
      values_to = "value"
    ) %>%
    filter(
      !is.na(value) #quitando los valores nulos
    )

  dataset_adicional_listo <- dataset_adicional_listo %>%
    group_by(
      across(
        all_of(c(id_cols,"name"))
      ) #agrupando por instancia y nombre de variable
    ) %>%
    arrange(
      desc(all_of(ordering_col)) #ordenando de acuerdo a la etapa
    ) %>%
    mutate(
      indice = row_number() #creando indice
    ) %>%
    filter(
      indice == 1 #sekleccionando info mas reciente
    ) %>%
    select(
      -all_of(ordering_col), #quitando columnas irrelevantes
      -indice
    )

  dataset_adicional_listo <- dataset_adicional_listo %>%
    pivot_wider(
      id_cols = id_cols #transformando a formato wide
    )

  dataset_adicional_listo <- dataset_original %>%
    left_join(dataset_adicional_listo, by = id_cols, suffix = c("",".y"))

  if (!("flag_auto_completado" %in% names(dataset_adicional_listo)) ){

    dataset_adicional_listo <- dataset_adicional_listo %>%
    mutate(
      flag_auto_completado = 0,
      lista_campos_autocompletados = ""
    )

  }



  for (col in cols_to_complete){

    col_adicional <- paste0(col,".y")


    if (length(dataset_adicional_listo[[col_adicional]]) > 0 ) {

      dataset_adicional_listo[["flag_auto_completado"]] <- ifelse(
        !(is.na(dataset_adicional_listo[[col_adicional]])) & is.na(dataset_adicional_listo[[col]]),
        1,
        dataset_adicional_listo[["flag_auto_completado"]]
      )

      dataset_adicional_listo[["lista_campos_autocompletados"]] <- ifelse(
        !(is.na(dataset_adicional_listo[[col_adicional]])) & is.na(dataset_adicional_listo[[col]]),
        paste(dataset_adicional_listo[["lista_campos_autocompletados"]],col_adicional,sep=","),
        dataset_adicional_listo[["lista_campos_autocompletados"]]
      )


    }

    dataset_adicional_listo[[col]] <- coalesce(
      dataset_adicional_listo[[col]],
      dataset_adicional_listo[[col_adicional]])

  }

  dataset_adicional_listo <- dataset_adicional_listo %>% select(-matches(".y$"))
  return(dataset_adicional_listo)
}
