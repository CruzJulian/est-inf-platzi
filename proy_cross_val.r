
# Hacemos la validación cruzada de nuestra red neuronal. ------------------

# Paquetes ----------------------------------------------------------------

library("dplyr")
library("saber")
library("nnet")
library("caret")
library("purrr")

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

muestra <- sample_n(SB11_20112, tamano_muestral)

c(
  "ECON_SN_TELEFONIA", 
  "ECON_SN_CELULAR",
  "ECON_SN_INTERNET",
  "ECON_SN_COMPUTADOR"
) -> nombres_X

X <- muestra[nombres_X]

Y <- muestra["MATEMATICAS_PUNT"]

Y_v <- unlist(Y)

neuronas <- 10

red_neuronal <- nnet(X, Y, size = neuronas, linout = TRUE)

plot(Y_v ~ predict(red_neuronal))
lines(1:100, col = 2)

n_pliegues <- 10

createFolds(Y_v, k = n_pliegues) -> pliegues


pliegues %>% 
  map_dbl(rmse_fold, X, Y, nn_size = neuronas) %>% 
  mean


