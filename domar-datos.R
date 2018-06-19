library(tidyverse)
library(readxl)
library(tm)
library(lubridate)

url <- "data/exitoescolar.xlsx"

hoy <- Sys.Date()



# Leyendo archivo XLSX

#DatosMaestros <- read.xlsx(url, sheetIndex = 1)

DatosMaestros <- read_excel(url, col_types = c("date", "numeric", "text", 
                                               "text", "text", "text", "text", "numeric", "date", "text", "numeric", "text", "text", "numeric", "text","numeric", "text", "numeric", "text", "text", "numeric", "text", "text", "text", "text", "text", "text"))


#Creo un data set filtrado

ExitoEscolar <- DatosMaestros

## Se eliminan las columnas que no son funcionales

ExitoEscolar$`ID Participante (Uso oficial)` <- NULL
ExitoEscolar$`1.01 Apellidos` <- NULL
ExitoEscolar$`1.02 Nombres` <- NULL
ExitoEscolar$"2.01 Nombre del centro educativo donde trabaja actualmente:" <- NULL
ExitoEscolar$`2.05` <- NULL

##Cambiando el Nombre a las columnas

colnames(ExitoEscolar)[1] <- "Fecha_Ingreso"
colnames(ExitoEscolar)[2] <- 'Especialidad_Pre_Inscrita'
colnames(ExitoEscolar)[3] <- "Sexo"
colnames(ExitoEscolar)[4] <- "Grupo_Etnico"
colnames(ExitoEscolar)[5] <- "Telefono"
colnames(ExitoEscolar)[6] <- "Fecha_Nacimiento"
colnames(ExitoEscolar)[7] <- "Formaciones"
colnames(ExitoEscolar)[8] <- "DPI"

##Transforma el DPI a un vector caracter para poder filtrar los duplicados por DPI

ExitoEscolar$DPI <- as.character(ExitoEscolar$DPI)

#Continuamos con el nombre de las columnas

colnames(ExitoEscolar)[9] <- "Departamento"
colnames(ExitoEscolar)[10] <- "Cod_Centro"
colnames(ExitoEscolar)[11] <- "Mod_Centro"
colnames(ExitoEscolar)[12] <- "Rol_Docente"
colnames(ExitoEscolar)[13] <- "Años_Servicio"
colnames(ExitoEscolar)[14] <- "Especialidad_Impartida"
colnames(ExitoEscolar)[15] <- "Otras_Especialidades"
colnames(ExitoEscolar)[16] <- "Años_Especialidad"
colnames(ExitoEscolar)[17] <- "Grados_Impartidos"

##Creo una columna nueva con los datos de las otras columnas

ExitoEscolar <- mutate(ExitoEscolar, municipio =  paste(ExitoEscolar$"2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:", ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__1`, ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__2`, ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__3`, ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__4`))


##Elimino las columnas sobrantes

ExitoEscolar$"2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:" <- NULL
ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__1` <- NULL
ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__2` <- NULL
ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__3` <- NULL
ExitoEscolar$`2.03 Municipio donde está ubicado el centro educativo en el que trabaja actualmente:__4` <- NULL

##Elimino los NA de la columna Municipio

ExitoEscolar$municipio <- removeWords(ExitoEscolar$municipio, "NA")

## Limpio la base de datos de los DPIs duplicados

ExitoEscolar <- ExitoEscolar[-c(which(duplicated(ExitoEscolar$DPI))), ]

##Quito los errores con las tildes por el formato por cada columna

ExitoEscolar$Especialidad_Pre_Inscrita <- iconv(ExitoEscolar$`Especialidad_Pre_Inscrita`, from = "UTF-8", to = "UTF-8")

ExitoEscolar$Formaciones <- iconv(ExitoEscolar$Formaciones, from = "UTF-8", to = "UTF-8")

ExitoEscolar$Departamento <- iconv(ExitoEscolar$Departamento, from = "UTF-8", to = "UTF-8")

ExitoEscolar$Mod_Centro <- iconv(ExitoEscolar$Mod_Centro, from = "UTF-8", to = "UTF-8")

ExitoEscolar$Especialidad_Impartida <- iconv(ExitoEscolar$Especialidad_Impartida, from = "UTF-8", to = "UTF-8")

ExitoEscolar$Otras_Especialidades <- iconv(ExitoEscolar$Otras_Especialidades, from = "UTF-8", to = "UTF-8")

ExitoEscolar$Grados_Impartidos <- iconv(ExitoEscolar$Grados_Impartidos, from = "UTF-8", to = "UTF-8")

ExitoEscolar$municipio <- iconv(ExitoEscolar$municipio, from = "UTF-8", to = "UTF-8")

##Fecha de Nacimiento class = date

ExitoEscolar$Fecha_Nacimiento <- as.Date(ExitoEscolar$Fecha_Nacimiento)

##Añadiendo la Edad por fecha de nacimiento

ExitoEscolar <- ExitoEscolar %>% mutate(edad = interval(start = ExitoEscolar$Fecha_Nacimiento, end = hoy)/duration(num = 1, units = "years"))


## Creando la variable Datos y Cantidad de Registros

Datos <- ExitoEscolar %>% filter(!is.na(Especialidad_Pre_Inscrita))
CantRegistros <- nrow(Datos)

save(Datos, file = "rda/Datos.rda")
save(ExitoEscolar, file = "rda/ExitoEscolar.rda")
save(DatosMaestros, file = "rda/DatosMaestros.rda")