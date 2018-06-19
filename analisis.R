##Analisis de Datos
library(tidyverse)
library(ggthemes)
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


DatosG %>% ggplot(aes(Sexo, edad, fill = Sexo)) + geom_boxplot() + theme_solarized() 

ggsave("plots/boxplot-sexo-edad.png")


### Total de Inscritos por Especialidad Grafico Variable Datos

InscritosEspecialidad <- Datos%>% ggplot(aes(Especialidad_Pre_Inscrita, fill = Especialidad_Pre_Inscrita)) + geom_bar() + theme(axis.text = element_text(angle = 45, hjust = 1)) + ylab("Cantidad Total Pre Inscritos") + xlab("Especialidad") 

InscritosEspecialidad

ggsave("plots/Barra-especialidad.png")


### Grafico Cantidad de Hombres Vrs Mujeres Inscritas

Genero <- Datos %>% ggplot(aes(Sexo, fill = Sexo)) + geom_bar()+ ylab("Cantidad") + xlab("Genero")

Genero

ggsave("plots/hombres-mujeres-inscritos.png")

### Grafico por Grupo Etnico

PlotGrupoEtnico <- Datos %>% ggplot(aes(Grupo_Etnico, fill = Grupo_Etnico)) + geom_bar() + ylab("Cantidad") + xlab("Grupo Etnico")

PlotGrupoEtnico

ggsave("plots/grupos-etnicos-totales.png")

