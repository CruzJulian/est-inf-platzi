
# Correr nuestro código de intervalo de confianza con datos reales --------


# Paquetes ----------------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("nebulae-co/saber")
  
library("dplyr")
library("saber")

data("SB11_20112")


table(SB11_20112$ECON_SN_INTERNET)


# intervalos de confianza de la media -------------------------------------


tamano_muestral <- 3000

muestra <- sample_n(SB11_20112, tamano_muestral)

pobla_A <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 0]
muestra_A <- muestra$FISICA_PUNT[muestra$ECON_SN_INTERNET == 0]

pobla_B <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 1]
muestra_B <- muestra$FISICA_PUNT[muestra$ECON_SN_INTERNET == 1]


media_pob_A <- mean(pobla_A, na.rm = TRUE)
media_muestra_A <- mean(muestra_A, na.rm = TRUE)
min_A <- media_muestra_A - 10*sd(muestra_A, na.rm = TRUE)/sqrt(tamano_muestral)
max_A <- media_muestra_A + 10*sd(muestra_A, na.rm = TRUE)/sqrt(tamano_muestral)
t_test_A <- t.test(muestra_A)
intervalo_A <- t_test_A$conf.int
LI_A <- min(intervalo_A)
LS_A <- max(intervalo_A)

media_pob_B <- mean(pobla_B, na.rm = TRUE)
media_muestra_B <- mean(muestra_B, na.rm = TRUE)
min_B <- media_muestra_B - 10*sd(muestra_B, na.rm = TRUE)/sqrt(tamano_muestral)
max_B <- media_muestra_B + 10*sd(muestra_B, na.rm = TRUE)/sqrt(tamano_muestral)
t_test_B <- t.test(muestra_B, na.rm = TRUE)
intervalo_B <- t_test_B$conf.int
LI_B <- min(intervalo_B)
LS_B <- max(intervalo_B)

plot(media_pob_A, media_pob_B, xlim = c(min_A, max_A), ylim = c(min_B, max_B), col = 4, pch = 20)
points(media_muestra_A, media_muestra_B, col = 2, pch = 20)
abline(1,1)
rect(LI_A, LI_B, LS_A, LS_B)




iteraciones <- 100

  plot(media_pob_A, media_pob_B, xlim = c(min_A, max_A), ylim = c(min_B, max_B), col = 4, pch = 20)
  abline(1,1)

for(i in seq_len(iteraciones)){
  
  muestra <- sample_n(SB11_20112, tamano_muestral)
  
  muestra_A <- muestra$FISICA_PUNT[muestra$ECON_SN_INTERNET == 0]
  
  muestra_B <- muestra$FISICA_PUNT[muestra$ECON_SN_INTERNET == 1]
  
  
  media_muestra_A <- mean(muestra_A, na.rm = TRUE)
  t_test_A <- t.test(muestra_A)
  intervalo_A <- t_test_A$conf.int
  LI_A <- min(intervalo_A)
  LS_A <- max(intervalo_A)
  
  media_muestra_B <- mean(muestra_B, na.rm = TRUE)
  t_test_B <- t.test(muestra_B, na.rm = TRUE)
  intervalo_B <- t_test_B$conf.int
  LI_B <- min(intervalo_B)
  LS_B <- max(intervalo_B)
  
  points(media_muestra_A, media_muestra_B, col = 2, pch = 20)
  rect(LI_A, LI_B, LS_A, LS_B)
  
}


points(media_pob_A, media_pob_B, col = 4, pch = 20, cex = 3)
  

# Gráfico bonito ----------------------------------------------------------

library("purrr")
library("magrittr")
library("ggplot2")
library("LaCroixColoR")


colores <- lacroix_palette("Pamplemousse")

tibble(
  muestra = replicate(iteraciones, sample_n(SB11_20112, tamano_muestral), simplify = FALSE),
  muestra_A = map(muestra, filter, ECON_SN_INTERNET == 0) %>% map(extract2, "FISICA_PUNT"),
  t_test_A = map(muestra_A, t.test),
  intervalo_A = map(t_test_A, extract2, "conf.int"),
  LI_A = map_dbl(intervalo_A, min),
  LS_A = map_dbl(intervalo_A, max),
  muestra_B = map(muestra, filter, ECON_SN_INTERNET == 1) %>% map(extract2, "FISICA_PUNT"),
  t_test_B = map(muestra_B, t.test),
  intervalo_B = map(t_test_B, extract2, "conf.int"),
  LI_B = map_dbl(intervalo_B, min),
  LS_B = map_dbl(intervalo_B, max)
) -> analisis

media_pob_A <- mean(pobla_A, na.rm = TRUE)
min_A <- media_pob_A - 10*sd(pobla_A, na.rm = TRUE)/sqrt(tamano_muestral)
max_A <- media_muestra_A + 10*sd(pobla_A, na.rm = TRUE)/sqrt(tamano_muestral)

media_pob_B <- mean(pobla_B, na.rm = TRUE)
min_B <- media_pob_B - 10*sd(pobla_B, na.rm = TRUE)/sqrt(tamano_muestral)
max_B <- media_pob_B + 10*sd(pobla_B, na.rm = TRUE)/sqrt(tamano_muestral)


analisis %>% 
  ggplot +
  geom_rect(aes(xmin = LI_A, xmax = LS_A, ymin = LI_B, ymax = LS_B), alpha = 0.2, fill = colores[6]) +
  annotate("point", media_muestra_A, media_muestra_B, colour = colores[1], size = 5) +
  annotate("point", media_pob_A, media_pob_B, colour = colores[4], size = 5) +
  geom_abline(intercept = 0, slope = 1, colour = colores[4], size = 1) +
  xlim(min_A, max_A) +
  ylim(min_B, max_B) +
  theme_minimal()





