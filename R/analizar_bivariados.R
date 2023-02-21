#' Analizar bivariados Function
#'
#' @param data Dataset selected
#' @param variable Variable to analize
#' @param var_obj RESULTADO Variable
#' @param default_levels Default = 4
#' @param descriptivos Default = TRUE
#' @param p_value_threshold Default = 0.1
#' @param max_levels Default = 10
#' @param robust Default = TRUE
#' @param plot Default = TRUE
#'
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @importFrom stats quantile
#'
#' @return results
#'
#' @export
#'
#'
#' @examples
analizar_bivariados <-function(data,variable, var_obj = "RESULTADO", default_levels = 4, descriptivos = TRUE, p_value_threshold = 0.1, max_levels = 10, robust = TRUE,plot=TRUE){

  # no visible binding for global variable
  ..count.. <-variable_discretizada <- RESULTADO <- exitos <- total <- porc_exitos <- grupo <- porc_exitos_label <- NULL

  data$RESULTADO <- data[[var_obj]]
  results <- list()
  tipo_variable <- class(data[[variable]])
  nombre_variable <- as.name(variable)

  if(tipo_variable %in% c("numeric","integer")){

    num_levels <- data[[variable]] %>% unique %>% length

    if (num_levels > max_levels){

      if ( robust){

        data <- data %>% mutate(
          variable_discretizada = discretizacion_iv_robusta(data[[variable]],default_levels)
        )

      }
      else{

        data <- data %>%
          mutate(
            variable_discretizada = discretizar(
              !!enquo(nombre_variable),
              c(quantile(!!enquo(nombre_variable),probs = seq(0, 1, 1/default_levels),na.rm = TRUE) %>% head(-1),Inf),
              c("primer_cuartil","segundo_cuartil","tercer_cuartil","cuarto_cuartil")
            ) %>%
              factor(
                levels = c("primer_cuartil","segundo_cuartil","tercer_cuartil","cuarto_cuartil")
              )
          )

      }


    }
    else {

      orden_variable <-  data[[variable]] %>% unique %>% sort
      data <- data %>%
        mutate(
          variable_discretizada = factor(!!enquo(nombre_variable), levels = orden_variable)
        )

    }

    if(descriptivos){
      results[["tabla_descriptivos"]] <- data %>%
        group_by(
          variable_discretizada
        ) %>%
        summarise(
          minimo = min(!!enquo(nombre_variable)),
          maximo = max(!!enquo(nombre_variable)),
          n = n()
          # sow_sbp_cc = 100*(sum(SBP_SALDO_CC_M0)/sum(SALDOTOTAL_CC_M0))

        )
    }


  }
  else{

    data <- data %>%
      mutate(
        variable_discretizada = factor(!!enquo(nombre_variable))
      )

    if(descriptivos){
      results[["tabla_descriptivos"]] <- data %>%
        group_by(
          variable_discretizada
        ) %>%
        summarise(
          n = n()
          # sow_sbp_cc = 100*(sum(SBP_SALDO_CC_M0)/sum(SALDOTOTAL_CC_M0))

        )
    }

  }

  resumen <- data %>%
    group_by(variable_discretizada, RESULTADO) %>%
    summarise( n = n()) %>%
    mutate(exitos = n*RESULTADO) %>%
    group_by(variable_discretizada) %>%
    summarise(
      total = sum(n),
      exitos = sum(exitos)
    ) %>%
    mutate(
      grupo = "porcentaje de exitos",
      porc_exitos = exitos/total,
      porc_exitos_label = round(porc_exitos*100,2) %>% paste("%")
    ) %>%
    select(
      grupo,
      variable_discretizada,
      porc_exitos,
      porc_exitos_label
    )


  #assessing discriminant power

  if(length(resumen$porc_exitos)> 1 ){

    tabla_evaluacion_pendiente <- tibble(eje_y = resumen$porc_exitos, eje_x = as.numeric(resumen$variable_discretizada))
    modelo <-   lm(eje_y ~ eje_x, data = tabla_evaluacion_pendiente)
    resumen_modelo <- modelo %>% summary()
    tabla_resumen <- resumen_modelo$coefficients %>% as_tibble(rownames = "variable")
    pendiente <- tabla_resumen %>% filter(variable == "eje_x") %>% pull(2)
    p_value <- tabla_resumen %>% filter(variable == "eje_x") %>% pull(5)

    #results[["evaluacion_discriminacion"]]

    if( pendiente != 0 & (p_value <= p_value_threshold | is.nan(p_value))){

      seleccion <- "ok"

    }
    else{

      seleccion <- "revisar"
    }

  }
  else{
    pendiente <- NA
    p_value <- NA
    seleccion <- "revisar"


  }

  results[["evaluacion_discriminacion"]] <- tibble(
    variable = variable,
    pendiente = pendiente,
    p_value = p_value,
    accion = seleccion)
  if(plot){
    plot <- ggplot2::ggplot() +
      ggplot2::geom_bar(
        data = data,
        ggplot2::aes(
          x = variable_discretizada,
          y=..count../sum(..count..),
          #y= stat(count)/sum(stat(count)),
          fill = factor(RESULTADO)
        )
      ) +
      ggplot2::xlab(
        variable
      ) +
      ggplot2::geom_line(
        data = resumen,
        ggplot2::aes(x = variable_discretizada, y = porc_exitos, group = grupo),
        stat="identity"
      ) +
      ggplot2::geom_text(
        data = resumen,
        ggplot2::aes(x = variable_discretizada, y = porc_exitos, label = porc_exitos_label)
      ) +
      ggplot2::geom_abline(slope = 0, intercept = mean(data$RESULTADO,na.rm =TRUE))


    results[["plot"]] <- plot

  }

  results[["variable_discretizada"]] <- data$variable_discretizada %>% unique

  #tabla de descriptivos


  return(results)

}
