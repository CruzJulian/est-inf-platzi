
# paquetes ----------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("magrittr")
library("purrr")


# Distribución normal -----------------------------------------------------

tamano_muestral <- 35
iteraciones <- 100

tibble(
  muestras = replicate(iteraciones, rnorm(tamano_muestral, 5, 3), simplify = FALSE),
  medias = map_dbl(muestras, mean),
  desv_est = map_dbl(muestras, sd)
) -> simulaciones

qplot(medias, desv_est, data = simulaciones) + 
  annotate("point", 5, 3, colour = "#dd5533", size = 5) +
  xlim(3, 7) +
  ylim(0, 5) +
  theme_minimal()


# Distribución uniforme ---------------------------------------------------


tamano_muestral <- 35
iteraciones <- 100

tibble(
  muestras = replicate(iteraciones, runif(tamano_muestral, 3, 8), simplify = FALSE),
  a = map_dbl(muestras, min),
  b = map_dbl(muestras, max)
) -> simulaciones

qplot(a, b, data = simulaciones) + 
  annotate("point", 3, 8, colour = "#dd5533", size = 5) +
  xlim(3, 4) +
  ylim(7, 8) +
  theme_minimal()


# regresión lineal --------------------------------------------------------

genera_y <- function(x){
-0.3*x + 1 + rnorm(length(x), 0, 0.5)
}

estima_betas <- function(x, y){
  coef(lm(y ~ x))
}

tibble(
  datos_x = replicate(iteraciones, seq(-3, 3, length.out = tamano_muestral), simplify = FALSE),
  datos_y = map(datos_x, genera_y),
  betas = map2(datos_x, datos_y, estima_betas),
  beta_0 = map_dbl(betas, extract, 1),
  beta_1 = map_dbl(betas, extract, 2),
  # plot = map2(datos_x, datos_y, plot)
) -> simulaciones

qplot(beta_0, beta_1, data = simulaciones) + 
  annotate("point", 1, -0.3, colour = "#dd5533", size = 5) +
  xlim(0.5, 1.5) +
  ylim(-0.7, 0.3) +
  theme_minimal()

