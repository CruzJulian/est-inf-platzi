
# Correr nuestro código de red neuronal con datos reales del paque --------



# Paquetes ----------------------------------------------------------------

library("dplyr")
library("saber")
library("nnet")

data("SB11_20112")


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


red_neuronal <- nnet(X, Y, size = 10, linout = TRUE)

plot(unlist(Y) ~ predict(red_neuronal))
lines(1:100, col = 2)

