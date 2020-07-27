
# Correr nuestro código de estimación puntual con datos reales del --------

# https://www.icfes.gov.co/nl/investigadores-y-estudiantes-posgrad --------

# https://github.com/nebulae-co/saber -------------------------------------




# Paquetes ----------------------------------------------------------------


# install.packages("devtools")
# devtools::install_github("nebulae-co/saber")

library("dplyr")
library("saber")


# carga de datos ----------------------------------------------------------

# data("SB11_20111") # 31707
data("SB11_20112")



# SB11_20112 %>% names()


tamano_muestral <- 27

mean(SB11_20112$MATEMATICAS_PUNT)
sd(SB11_20112$MATEMATICAS_PUNT)

mean(sample(SB11_20112$MATEMATICAS_PUNT, tamano_muestral))
sd(sample(SB11_20112$MATEMATICAS_PUNT, tamano_muestral))

plot(
mean(SB11_20112$MATEMATICAS_PUNT),
sd(SB11_20112$MATEMATICAS_PUNT),
pch = 20,
cex = 4,
col = "white"
)

iteraciones <- 38

for(i in seq_len(iteraciones)){
points(
  mean(sample(SB11_20112$MATEMATICAS_PUNT, tamano_muestral)),
  sd(sample(SB11_20112$MATEMATICAS_PUNT, tamano_muestral)),
  pch = 20
  
)
}

points(
  mean(SB11_20112$MATEMATICAS_PUNT),
  sd(SB11_20112$MATEMATICAS_PUNT),
  pch = 20,
  cex = 4,
  col = 2
)


# Gráfico bonito ----------------------------------------------------------

library("ggplot2")
library("purrr")

tibble(
  muestras = replicate(iteraciones, sample(SB11_20112[["MATEMATICAS_PUNT"]], tamano_muestral), simplify = FALSE),
  medias = map_dbl(muestras, mean),
  desv = map_dbl(muestras, sd),
) %>% ggplot + 
  geom_point(aes(x = medias, y = desv)) +
  annotate(
    geom = "point", 
    x = mean(SB11_20112[["MATEMATICAS_PUNT"]]), 
    y = sd(SB11_20112[["MATEMATICAS_PUNT"]]),
    size = 4,
    colour = "#ED2B05") +
  theme_minimal()



