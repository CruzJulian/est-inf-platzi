
# Simular la misma prueba de hipótesis con distintas condiciones e --------


# Paquetes ----------------------------------------------------------------


# wmw con dos gamas -------------------------------------------------------

# La media de una gamma es shape/rate, vamos a mover el shape -------------

tamano_muestral <- 10
iteraciones <- 200
dif_media_ini <- 0
dif_media_fin <- 3
media_x <- 1
n_pasos <- 20
umbral_significancia <- 0.05
dif_medias <- seq(dif_media_ini, dif_media_fin, length.out = n_pasos)



# prueba wmw --------------------------------------------------------------


potencia_wmw <- vector()

for(k in seq_along(dif_medias)){ 
  
  sim_shape <- dif_medias[k] + media_x
  
  p_valores <- vector()
  
  for(i in seq_len(iteraciones)){
    x <- rgamma(tamano_muestral, 1, 1)
    y <- rgamma(tamano_muestral, sim_shape, 1)
    p_valores[i] <- wilcox.test(x, y) %$% p.value
    
  }
  
  potencia_wmw[k] <- mean(p_valores < umbral_significancia)
  
}


# prueba t ----------------------------------------------------------------



potencia_t <- vector()

for(k in seq_along(dif_medias)){ 
  
  sim_shape <- dif_medias[k] + media_x
  
  p_valores <- vector()
  
  for(i in seq_len(iteraciones)){
    x <- rgamma(tamano_muestral, 1, 1)
    y <- rgamma(tamano_muestral, sim_shape, 1)
    p_valores[i] <- wilcox.test(x, y) %$% p.value
    
  }
  
  potencia_t[k] <- mean(p_valores < umbral_significancia)
  
}




plot(dif_medias, potencia_wmw, ylim = c(0, 1), col = 4, type = "l")
lines(dif_medias, potencia_t, col = 2)


# Tidy approach -----------------------------------------------------------



library("ggplot2")
library("dplyr")
library("magrittr")


tibble(
  dif_medias = rep(dif_medias, 2),
  potencia = c(potencia_t, potencia_wmw),
  prueba = c(rep("t-Student", n_pasos), rep("Wilcoxon-Mann-Whitney", n_pasos))
) %>% 
  ggplot +
  geom_line(aes(x = dif_medias, y = potencia, colour = prueba), size = 1) + 
  theme_minimal()
  
