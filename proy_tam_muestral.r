
# Revisamos el tamaño muestral de nuestra red neuronal. -------------------


# Paquetes ----------------------------------------------------------------

library("dplyr")
library("saber")
library("nnet")
library("caret")
library("parallel")
library("purrr")
library("magrittr")

data("SB11_20112")


# función de pliegue ------------------------------------------------------

rmse_fold <- function(pliegue, X, Y, nn_size){
  pliegue_logic <- seq_len(nrow(X)) %in% pliegue
  entrena_X <- subset(X, !pliegue_logic)
  entrena_Y <- subset(Y, !pliegue_logic)
  modelo <- nnet(X, Y, size = nn_size, linout = TRUE, trace = FALSE)
  Y_pronosticado <- as.vector(predict(modelo, newdata = subset(X, pliegue_logic)))
  rmse <- sqrt(mean((unlist(subset(Y, pliegue_logic)) - Y_pronosticado)^2))
  rmse
}

# Red neuronal ------------------------------------------------------------


tamano_muestral <- 2000
n_pliegues <- 10
tamanos_muestrales <- seq(2000, 10000, by = 200)
n_pliegues <- 20
neuronas <- 4

c(
  "ECON_SN_TELEFONIA", 
  "ECON_SN_CELULAR",
  "ECON_SN_INTERNET",
  "ECON_SN_COMPUTADOR"
) -> nombres_X

"MATEMATICAS_PUNT" -> nombre_Y

SB11_20112 %>% 
  sample_n(tamano_muestral) %>% 
  extract(c(nombres_X, nombre_Y)) %>% 
  na.omit() -> muestra

X <- muestra[nombres_X]

Y <- muestra[nombre_Y]

Y_v <- unlist(Y)

neuronas <- 8

createFolds(Y_v, k = n_pliegues) -> pliegues

calcula_rmse_tam <- function(tamano_muestral){
  
  SB11_20112 %>% 
    sample_n(tamano_muestral) %>% 
    extract(c(nombres_X, nombre_Y)) %>% 
    na.omit() -> muestra
  
  X <- muestra[nombres_X]
  Y <- muestra[nombre_Y]
  Y_v <- unlist(Y)
  
  createFolds(Y_v, k = n_pliegues) -> pliegues
  
  pliegues %>% 
    map_dbl(rmse_fold, X, Y, nn_size = neuronas) %>% 
    mean 
  
}

mclapply(tamanos_muestrales, calcula_rmse_tam, mc.cores = floor(detectCores()*0.8)) -> rmse_por_tam

plot(tamanos_muestrales, rmse_por_tam)
