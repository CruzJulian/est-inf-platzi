
# Hacer la curva de convergencia de una muestra simulada. -----------------




# Paquetes ----------------------------------------------------------------

library("dplyr")
library("purrr")
library("ggplot2")
library("magrittr")


# Distribución normal -----------------------------------------------------


iteraciones <- 100

tibble(
  media = 5,
  desv = 3,
  tamano_muestral = seq(11, iteraciones + 10, 1),
  muestras = map(tamano_muestral, rnorm, media, desv),
  media_estimada = map_dbl(muestras, mean),
  desv_estimada = map_dbl(muestras, sd),
  dif_cuad_medias = (media - media_estimada)^2,
  dif_cuad_sd = (desv_estimada - desv)^2
) -> simulaciones

plot(media_estimada ~ tamano_muestral, data = simulaciones)
lines(media ~ tamano_muestral, data = simulaciones, col = 2)

plot(dif_cuad_medias ~ tamano_muestral, data = simulaciones, type = "l")


plot(desv_estimada ~ tamano_muestral, data = simulaciones)
lines(desv ~ tamano_muestral, data = simulaciones, col = 2)

plot(dif_cuad_sd ~ tamano_muestral, data = simulaciones, type = "l")


# Gráficos bonitos --------------------------------------------------------

qplot(tamano_muestral, media_estimada, data = simulaciones) +
  geom_hline(yintercept = simulaciones$media, colour = "red") +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_medias, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE) +
  theme_minimal()


qplot(tamano_muestral, desv_estimada, data = simulaciones) +
  geom_hline(yintercept = simulaciones$desv, colour = "red") +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_sd, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE) +
  theme_minimal()




# Distribución uniforme ---------------------------------------------------


iteraciones <- 1000

tibble(
  a = 3,
  b = 8,
  tamano_muestral = seq(11, iteraciones + 10, 1),
  muestras = map(tamano_muestral, runif, a, b),
  a_estimada = map_dbl(muestras, min),
  b_estimada = map_dbl(muestras, max),
  dif_cuad_a = (a - a_estimada)^2,
  dif_cuad_b = (b_estimada - b)^2
) -> simulaciones

plot(a_estimada ~ tamano_muestral, data = simulaciones)
lines(a ~ tamano_muestral, data = simulaciones, col = 2)

plot(dif_cuad_a ~ tamano_muestral, data = simulaciones, type = "l")


plot(b_estimada ~ tamano_muestral, data = simulaciones)
lines(b ~ tamano_muestral, data = simulaciones, col = 2)

plot(dif_cuad_b ~ tamano_muestral, data = simulaciones, type = "l")


# Gráficos bonitos --------------------------------------------------------

qplot(tamano_muestral, a_estimada, data = simulaciones) +
  geom_hline(yintercept = simulaciones$a, colour = "red") +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_a, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE) +
  theme_minimal()


qplot(tamano_muestral, b_estimada, data = simulaciones) +
  geom_hline(yintercept = simulaciones$b, colour = "red") +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_b, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE) +
  theme_minimal()



# Regresión lineal --------------------------------------------------------


iteraciones <- 1000

genera_x <- function(n) seq(-3, 3, length.out = n)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

estima_betas <- function(x, y){
  coef(lm(y ~ x))
}

tibble(
  beta_0 = 1,
  beta_1 = -0.3,
  tamano_muestral = seq(11, iteraciones + 10, 1),
  datos_x = map(tamano_muestral, genera_x),
  datos_y = map(datos_x, genera_y),
  betas = map2(datos_x, datos_y, estima_betas),
  beta_0_estimado = map_dbl(betas, extract, 1),
  beta_1_estimado = map_dbl(betas, extract, 2),
  dif_cuad_beta_0 = (beta_0_estimado - beta_0)^2,
  dif_cuad_beta_1 = (beta_1_estimado - beta_1)^2
  # plot = map2(datos_x, datos_y, plot)
) -> simulaciones


plot(beta_0_estimado ~ tamano_muestral, data = simulaciones)
lines(beta_0 ~ tamano_muestral, data = simulaciones, col = 2)

plot(dif_cuad_beta_0 ~ tamano_muestral, data = simulaciones, type = "l")


plot(beta_1_estimado ~ tamano_muestral, data = simulaciones)
lines(beta_1 ~ tamano_muestral, data = simulaciones, col = 2)

plot(dif_cuad_beta_1 ~ tamano_muestral, data = simulaciones, type = "l")


# Gráficos bonitos --------------------------------------------------------

qplot(tamano_muestral, beta_0_estimado, data = simulaciones) +
  geom_hline(yintercept = simulaciones$beta_0, colour = "red") +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_beta_0, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE) +
  theme_minimal()


qplot(tamano_muestral, beta_1_estimado, data = simulaciones) +
  geom_hline(yintercept = simulaciones$beta_1, colour = "red") +
  theme_minimal()

qplot(tamano_muestral, dif_cuad_beta_1, data = simulaciones) +
  geom_smooth(size = 1, se = FALSE) +
  theme_minimal()



