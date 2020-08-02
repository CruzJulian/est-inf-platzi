
# Hacemos la validación cruzada de nuestra red neuronal. ------------------



# Paquetes ----------------------------------------------------------------

library("caret")
library("nnet")
library("parallel")

# función de pliegue ------------------------------------------------------

rmse_por_pliegue <- function(pliegue, X, Y){
  entrena_X <- X[-pliegue]
  entrena_Y <- Y[-pliegue]
  modelo <- nnet(X, Y, size = 8, linout = TRUE, trace = FALSE)
  Y_pronosticado <- as.vector(predict(modelo, newdata = data.frame(X[pliegue])))
  rmse <- sqrt(mean((Y[pliegue] - Y_pronosticado)^2))
  rmse
}

# Red neuronal ------------------------------------------------------------


n_pliegues <- 5
tamano_muestral <- 3000

genera_y <- function(x){
  cos(x) + rnorm(length(x), 0, 0.5)
}


X <- seq(0, 3*pi, length.out = tamano_muestral)
Y <- genera_y(X)


createFolds(Y, k = n_pliegues) -> pliegues

mclapply(
  pliegues,
  rmse_por_pliegue,
  X,
  Y,
  mc.cores = floor(detectCores()*0.8)
  ) -> rmse_pliegues 

rmse_pliegues <- unlist(rmse_pliegues)

plot(rmse_pliegues, ylim = c(0, 1))

mean(rmse_pliegues)


# Tidy approach -----------------------------------------------------------

library("dplyr")
library("magrittr")

n_pliegues <- 5
tamano_muestral <- 3000

tibble(
  pliegues = createFolds(Y, k = n_pliegues),
  rmse_pliegues = mclapply(
    pliegues,
    rmse_por_pliegue,
    X,
    Y,
    mc.cores = floor(detectCores()*0.8)
  ) %>% unlist,
  nombres = names(pliegues)
) -> validacion

validacion %$% mean(rmse_pliegues)

ggplot(validacion) +
  geom_vline(aes(xintercept = 0), size = 1.5) +
  geom_segment(aes(x = 0, y = nombres, xend = rmse_pliegues, yend = nombres), colour = "grey75") +
  geom_point(aes(x = rmse_pliegues, y = nombres), size = 4) +
  theme_minimal()
