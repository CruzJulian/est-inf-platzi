
# Hacemos la validación cruzada de nuestra red neuronal. ------------------



# Paquetes ----------------------------------------------------------------

library("caret")
library("dplyr")
library("nnet")
library("purrr")


# función de pliegue ------------------------------------------------------

rmse_fold <- function(pliegue, X, Y){
  entrena_X <- X[-pliegue]
  entrena_Y <- Y[-pliegue]
  modelo <- nnet(X, Y, size = 8, linout = TRUE, trace = FALSE)
  Y_pronosticado <- as.vector(predict(modelo, newdata = data.frame(X[pliegue])))
  rmse <- sqrt(mean((Y[pliegue] - Y_pronosticado)^2))
  rmse
}

# Red neuronal ------------------------------------------------------------


n_pliegues <- 10
tamano_muestral <- 23

genera_y <- function(x){
  cos(x) + rnorm(length(x), 0, 0.5)
  # beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}


rmse_modelo <- vector()


X <- seq(0, 3*pi, length.out = tamano_muestral)
Y <- genera_y(X)



createFolds(Y, k = n_pliegues) -> pliegues


pliegues %>% 
  map_dbl(rmse_fold, X, Y) %>% 
  mean 

