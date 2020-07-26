
# Hacer bootstrapping sobre nuestra regresión lineal. ---------------------


# Paquetes ----------------------------------------------------------------


library("dplyr")
library("purrr")
library("tidyr")
library("ggplot2")

# Bootstrapping -----------------------------------------------------------


tamano_muestral <- 23
iteraciones <- 1000
beta_0 <- 1
beta_1 <- -0.3


genera_x <- function(n) seq(-3, 3, length.out = n)

genera_y <- function(x, beta_0, beta_1){
  beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

tibble(
  datos_x = genera_x(tamano_muestral),
  datos_y = genera_y(datos_x, beta_0, beta_1)
) -> datos_simulados


lm(datos_y ~ datos_x, data = datos_simulados) -> modelo
coefficients(modelo) -> coeficientes_muestrales
summary(modelo)
confint(modelo)


coeficientes <- list() #vamos a guardar aquí los coeficientes de una regresión

for(i in 1:iteraciones){
  muestra <- sample_n(datos_simulados, nrow(datos_simulados), replace = TRUE)
  lm(datos_y ~ datos_x, data = muestra) -> modelo
  coeficientes[[i]] <- coefficients(modelo)
}


coeficientes %>% 
  transpose %>% 
  lapply(unlist) %>% 
  as_tibble() %>% 
  gather(key = coeficiente, value = valor) %>% 
  group_by(coeficiente) %>% 
  summarise(
    LI = quantile(valor, 0.025),
    LS = quantile(valor, 0.975)
  )



plot(beta_0, beta_1, ylim = c(-0.5, 0), xlim = c(0.7, 1.6))

for(i in 1:iteraciones){
points(coeficientes[[i]][1], coeficientes[[i]][2])
}
points(coeficientes_muestrales[1], coeficientes_muestrales[2], pch = 20, col = 4, cex = 3)
points(beta_0, beta_1, pch = 20, col = 2, cex = 3)


# Gráfico bonito ----------------------------------------------------------


coeficientes %>% 
  transpose %>% 
  lapply(unlist) %>% 
  as_tibble() %>% 
  gather(key = coeficiente, value = valor) %>% 
  ggplot +
  aes(x = valor) + 
  geom_density(fill = "#78D92A", alpha = 0.5, colour = "#78D92A") +
  facet_wrap(~coeficiente, nrow = 4,  scales = "free") + theme_light()

coeficientes %>% 
  transpose %>% 
  lapply(unlist) %>% 
  as_tibble(.name_repair = "universal") %>% 
  # gather(key = coeficiente, value = valor) %>% 
  ggplot +
  aes(x = .Intercept., y = datos_x) + 
  geom_point() +
  annotate("point",x = beta_0, y = beta_1, colour = "#ED2B05", size = 5) +
  annotate("point",x = coeficientes_muestrales[1], y = coeficientes_muestrales[2], colour = "#058ECD", size = 5) +
  theme_light()




