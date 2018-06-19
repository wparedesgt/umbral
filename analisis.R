##Analisis de Datos
library(tidyverse)

load("rda/ExitoEscolar.rda")


### Edad Media inscritos hombres

DatosH <- ExitoEscolar %>% filter(Sexo == "Masculino", edad > 18) %>% .$edad

PromH <- mean(DatosH)
DSH <- sd(DatosH)
nH <- length(DatosH)
MediaH <- median(DatosH)

Edad_Inscritos_H <- rnorm(nH, PromH, DSH)


data.frame(Edad_Inscritos_H) %>% ggplot(aes(Edad_Inscritos_H)) + geom_histogram(color = "Orange", binwidth = 2) + geom_vline(xintercept=PromH, col = "blue") + geom_vline(xintercept=MediaH, col = "red", show.legend = "Media") + labs(x= "Edad Hombres", y = "Cantidad", title = "Edad Hombres Inscritos", subtitle = "Promedio linea Azul, Media Linea Roja")


ggsave("plots/histograma-edad-hombres.png")

###Edad Media inscritas mujeres

DatosM <- ExitoEscolar %>% filter(Sexo == "Femenino", edad > 18) %>% .$edad


PromM <- mean(DatosM)
DSM <- sd(DatosM)
nM <- length(DatosM)
MediaM <- median(DatosM)

Edad_Inscritos_M <- rnorm(nM, PromM, DSM)


data.frame(Edad_Inscritos_M) %>% ggplot(aes(Edad_Inscritos_M)) + geom_histogram(color = "Orange", binwidth = 2) + geom_vline(xintercept=PromH, col = "blue") + geom_vline(xintercept=MediaH, col = "red", show.legend = "Media") + labs(x= "Edad Mujeres", y = "Cantidad", title = "Edad Mujeres Inscritas", subtitle = "Promedio linea Azul, Media Linea Roja")


ggsave("plots/histograma-edad-Mujeres.png")


###Comparando en dos boxplot

DatosG <- ExitoEscolar %>% filter(edad > 18) %>% select(edad, Sexo)
CantRegistros <- nrow(DatosG)


DatosG %>% ggplot(aes(Sexo, edad)) + geom_boxplot()

ggsave("plots/boxplot-sexo-edad.png")

