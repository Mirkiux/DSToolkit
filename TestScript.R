library(usethis)
library(devtools)
library(readr)

# Documentacion y testeo inicial ------------------------------------------

# Create documentacion
devtools::document()

# Cargamos el paquete
devtools::load_all()

# Revisamos las funciones
devtools::check()

# Package metadata --------------------------------------------------------
# https://r-pkgs.org/description.html#dependencies (Solo se realiza una vez)
# https://r-pkgs.org/namespace.html#imports
# https://nathaneastwood.github.io/2019/08/18/no-visible-binding-for-global-variable/

# Default is "Imports"
# usethis::use_package("dplyr")
# √ Adding 'dplyr' to Imports field in DESCRIPTION
# * Refer to functions with `dplyr::fun()`

# usethis::use_package("ROCR")
# usethis::use_package("Rprofet")
# usethis::use_package("ggplot2")
# usethis::use_package("glmnet")
# usethis::use_package("pROC")
# usethis::use_package("plyr")
# usethis::use_package("purrr")
# usethis::use_package("rlang")
# usethis::use_package("rpart")
# usethis::use_package("scorecard")
# usethis::use_package("stringr")
# usethis::use_package("tibble")
# usethis::use_package("gtable")
# usethis::use_package("magrittr")
# usethis::use_package("tidyr")
# usethis::use_package("utils")
# usethis::use_package("grDevices")
# usethis::use_package("grr")
# usethis::use_package("tidyselect")
# usethis::use_package("stats")
# usethis::use_package("grid")

# Testeo de funciones -----------------------------------------------------

# Base x
Real_estate <- read_csv("Real estate.csv")
# Real_estate$RESULTADO = rnorm(414,200,10)
Real_estate$RESULTADO = c(rep(10,200),rep(20,200),rep(30,14))

# Testeo (Negativo)

analizar_bivariados(Real_estate,"X1_transaction_date")

discretizacion_iv_robusta(Real_estate[["X1_transaction_date"]],4)

modelo <- lm(Y_house_price_unit_area ~ X2_house_age + X4_number_convenience_stores,
                   data = Real_estate)

calculoGini(Real_estate,modelo) # La estructura de la base no ayuda

BASE6 <- FiltroMissing(Real_estate,p,nmissxvar) # El input p no se utiliza en la funcion

gbpct(Real_estate$X2_house_age,Real_estate$Y_house_price_unit_area) # No especifica el objeto a retornar

# two plots
p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
  ggplot2::geom_line(colour = "blue")
p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, drat)) +
  ggplot2::geom_line(colour = "red") +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA))

GRAFICO1 <- double_axis_graph(p1,p2) # Error en el final

# Testeo (Positivo)

BASE1 <- characters_to_factor(Real_estate)
BASE2 <- completar_character_columns(Real_estate,cutoff_nas = 1.0)
BASE3 <- completar_flags(Real_estate)

BASE4 <- extraer_categoricas() # Solicitar base para los insumos

BASE5 <- Filtro1obs(Real_estate)

BASE6 <- get_complete_variables(Real_estate)

BASE7 <- IHH(Real_estate$Y_house_price_unit_area)

BASE8 <- ImputacionVar(Real_estate$Y_house_price_unit_area)

BASE9 <- prepare_dataset(Real_estate)

BASE10 <- quitar(c(1:10),c(1:5))

BASE11 <- remove_constant_factors(Real_estate)

BASE12 <- remove_missing_variables(Real_estate)

BASE13 <- nomalizar_variable(Real_estate[["Y_house_price_unit_area"]])
hist(Real_estate[["Y_house_price_unit_area"]])
hist(BASE13)


BASE14 <- to_woe(Real_estate,"X1_transaction_date") #ok, el error es propio de la base

BASE15 <- to_z_score(Real_estate[["X1_transaction_date"]])
hist(Real_estate[["Y_house_price_unit_area"]])
hist(BASE15)

BASE16 <- truncar_outliers(Real_estate)

BASE17 <- truncar_variable(Real_estate,"Y_house_price_unit_area")

BASE18 <- try_character_to_numeric(Real_estate) #Devuelve character
# input es toda una base

BASE19 <- try_to_numeric(Real_estate[["character1"]])
# input un vector columna

# BASE20 <- WOExVar(Real_estate[["character1"]])
