
# Paquetes ----------------------------------------------------------------

library("dplyr")
library("LaCroixColoR")


colores <- lacroix_palette("Pamplemousse")


# intervalos de confianza de la media -------------------------------------


tamano_muestral <- 35
iteraciones <- 100


tibble(
  muestra_A = replicate(iteraciones, rnorm(tamano_muestral, 5, 3), simplify = FALSE),
  t_test_A = map(muestra_A, t.test),
  intervalo_A = map(t_test_A, extract2, "conf.int"),
  LI_A = map_dbl(intervalo_A, min),
  LS_A = map_dbl(intervalo_A, max),
  muestra_B = replicate(iteraciones, rnorm(tamano_muestral, 2, 3), simplify = FALSE),
  t_test_B = map(muestra_B, t.test),
  intervalo_B = map(t_test_B, extract2, "conf.int"),
  LI_B = map_dbl(intervalo_B, min),
  LS_B = map_dbl(intervalo_B, max)
) -> simulaciones


simulaciones %>% 
  ggplot +
  geom_rect(aes(xmin = LI_A, xmax = LS_A, ymin = LI_B, ymax = LS_B), alpha = 0.2, fill = colores[6]) +
  annotate("point", 5, 2, colour = colores[1], size = 5) +
  geom_abline(intercept = 0, slope = 1, colour = colores[4], size = 1) +
  xlim(-2, 8) +
  ylim(-2, 8) +
  theme_minimal()

