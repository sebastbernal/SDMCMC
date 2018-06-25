# Cargar paquetes
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(magrittr)
library(rsoi)

# Configurar el directorio de trabajo
#Betty
setwd("X:/Dropbox/¡¡¡THESIS!!!/Datos/")
#BlackHawck
#setwd("E:/Dropbox/¡¡¡THESIS!!!/Datos/")

#Leer densidad y casos
densidad <- read.csv("Mosquitoes/Densidad mensual. Nuqui. Mar 98- Abr 05.csv")
casos <- read.csv("Cases/Monthly_Cases_and_Prevalence_Pf&Pv_Nuqui_January94_to_April05.csv")

#Arreglar fechas densidad y casos
densidad$Fecha <- dmy(paste(1, as.character(densidad$Fecha), sep = "-"))
densidad$mes <- month(densidad$Fecha, label = TRUE)
densidad$ano <- year(densidad$Fecha)

casos$Fecha <- dmy(paste(1, as.character(casos$Mes), sep = "-"))
casos$ano <- year(casos$Fecha)
casos$mes <- month(casos$Fecha, label = TRUE)

# Cargar las funciones magiquitas
source("IDEAM.R")
source("ONI.R")

# Leer datos IDEAM
bahia <- read_IDEAM("Climate/IDEAM/Bahia Solano/Bahia Solano. IDEAM. Todos")
amargal <- read_IDEAM("Climate/IDEAM/Nuqui Amargal/Todos diario, mensual por ano")

#Aplicar función al ONI
ONI <-
  download_oni() %>%
  mutate(ano = Year,
         mes = Month,
         oni = round(ONI,1),
         ENSO = factor(if_else(oni >= 0.5,
                               "Nino",
                               if_else(oni <= -0.5,
                                       "Nina",
                                       "Normal")),
                       levels = c("Nina", "Normal", "Nino"))) %>%
  select(ano:ENSO) %>%
  eventosENSO()

# Tablas con conteos de NA's
amargal.na <-
  amargal %>%
  group_by(ano, mes) %>%
  summarise(prec.na = sum(is.na(PRECIPITACION.TOTALES)),
            temp.max.na = sum(is.na(TEMPERATURA.MAXIMOS)),
            temp.mean.na = sum(is.na(TEMPERATURA.MEDIOS)),
            temp.min.na = sum(is.na(TEMPERATURA.MINIMOS))
            )

bahia.na <-
  bahia %>%
  group_by(ano, mes) %>%
  summarise(prec.na = sum(is.na(PRECIPITACION.TOTALES)),
            temp.max.na = sum(is.na(TEMPERATURA.MAXIMOS)),
            temp.mean.na = sum(is.na(TEMPERATURA.MEDIOS)),
            temp.min.na = sum(is.na(TEMPERATURA.MINIMOS))
            )

# Serie mensual
amargal.mes <-
  amargal %>%
  mutate(rango_temp = TEMPERATURA.MAXIMOS - TEMPERATURA.MINIMOS) %>%
  group_by(ano, mes) %>%
  summarise(precipitacion = sum(PRECIPITACION.TOTALES, na.rm = TRUE),
            temperatura = mean(TEMPERATURA.MEDIOS, na.rm = TRUE),
            rango_temp = mean(rango_temp, na.rm = TRUE)) %>%
  left_join(ONI)

bahia.mes <-
  bahia %>%
  mutate(rango_temp = TEMPERATURA.MAXIMOS - TEMPERATURA.MINIMOS) %>%
  group_by(ano, mes) %>%
  summarise(precipitacion = sum(PRECIPITACION.TOTALES, na.rm = TRUE),
            temperatura = mean(TEMPERATURA.MEDIOS, na.rm = TRUE),
            rango_temp = mean(rango_temp, na.rm = TRUE)) %>%
  left_join(ONI)

# Tope de NA's a admitir por mes
tope.na.prec <- 7
tope.na.temp <- 14

# Ciclo anual precipitacion ENSO
amargal.prec <-
  amargal.na %>%
  filter(prec.na < tope.na.prec) %>%
  left_join(amargal.mes) %>%
  select(ano, mes, prec.na, precipitacion, ENSO)

cicloA_prec_amargal <-
  amargal.prec %>%
  group_by(mes, ENSO) %>%
  summarise(n = n(),
            prec = mean(precipitacion, na.rm = TRUE))

bahia.prec <-
  bahia.na %>%
  filter(prec.na < tope.na.prec) %>%
  left_join(bahia.mes) %>%
  select(ano, mes, prec.na, precipitacion, ENSO)

cicloA_prec_bahia <-
  bahia.prec %>%
  group_by(mes, ENSO) %>%
  summarise(n = n(),
            prec = mean(precipitacion, na.rm = TRUE))

# Ciclo anual temperatura ENSO
amargal.temp <-
  amargal.na %>%
  filter(temp.mean.na < tope.na.temp) %>%
  left_join(amargal.mes) %>%
  select(ano, mes, temp.mean.na, temperatura, ENSO)

cicloA_temp_amargal <-
  amargal.temp %>%
  group_by(mes, ENSO) %>%
  summarise(n = n(),
            temp = mean(temperatura, na.rm = TRUE))

bahia.temp <-
  bahia.na %>%
  filter(temp.mean.na < tope.na.temp) %>%
  left_join(bahia.mes) %>%
  select(ano, mes, temp.mean.na, temperatura, ENSO)

cicloA_temp_bahia <-
  bahia.temp %>%
  group_by(mes, ENSO) %>%
  summarise(n = n(),
            temp = mean(temperatura, na.rm = TRUE))

# Ciclo anual rango temperatura ENSO
amargal.rango <-
  amargal.na %>%
  filter(temp.max.na < tope.na.temp,
         temp.min.na < tope.na.temp) %>%
  left_join(amargal.mes) %>%
  select(ano, mes, temp.max.na, temp.min.na, rango_temp, ENSO)

cicloA_rango_amargal <-
  amargal.rango %>%
  group_by(mes, ENSO) %>%
  summarise(n = n(),
            rango = mean(rango_temp, na.rm = TRUE))

bahia.rango <-
  bahia.na %>%
  filter(temp.max.na < tope.na.temp,
         temp.min.na < tope.na.temp) %>%
  left_join(bahia.mes) %>%
  select(ano, mes, temp.max.na, temp.min.na, rango_temp, ENSO)

cicloA_rango_bahia <-
  bahia.rango %>%
  group_by(mes, ENSO) %>%
  summarise(n = n(),
            rango = mean(rango_temp, na.rm = TRUE))

# Serie mensual dummy
serie_dummy <- data.frame(ano = rep(1994:2005, each = 12),
                          mes = factor(rep(month.abb, 12),
                                       levels = month.abb))

# Nuqui
nuqui <-
  serie_dummy %>%
  left_join(ONI) %>%
  left_join(select(amargal.prec,
                   ano, mes, precipitacion),
            by = c("ano", "mes")) %>%
  left_join(select(bahia.prec,
                   ano, mes, precipitacion),
            by = c("ano", "mes")) %>%
  left_join(select(amargal.temp,
                   ano, mes, temperatura),
            by = c("ano", "mes")) %>%
  left_join(select(bahia.temp,
                   ano, mes, temperatura),
            by = c("ano", "mes")) %>%
  left_join(select(amargal.rango,
                   ano, mes, rango_temp),
            by = c("ano", "mes")) %>%
  left_join(select(bahia.rango,
                   ano, mes, rango_temp),
            by = c("ano", "mes")) %>%
  left_join(select(cicloA_prec_bahia, -n),
            by = c("mes", "ENSO")) %>%
  left_join(select(cicloA_temp_bahia, -n),
            by = c("mes", "ENSO")) %>%
  left_join(select(cicloA_rango_bahia, -n),
            by = c("mes", "ENSO")) %>%
  left_join(casos %>%
              select(ano, mes, casos.pf = Casos.falciparum, casos.pv = Casos.vivax)) %>%
  left_join(densidad %>%
              select(ano, mes, mosquitos = mosquitos)) %>%
  mutate(precipitacion = if_else(is.na(precipitacion.x), #Acá completa los vacios con los registros de Bahia
                                 precipitacion.y, precipitacion.x),
         temperatura = if_else(is.na(temperatura.x),
                                 temperatura.y, temperatura.x),
         rango_temp = if_else(is.na(rango_temp.x),
                              rango_temp.y, rango_temp.x),
         precipitacion = if_else(is.na(precipitacion), #Acá completa los NA's con el ciclo Anual de Bahia
                                 prec, precipitacion),
         temperatura = if_else(is.na(temperatura),
                               temp, temperatura),
         rango_temp = if_else(is.na(rango_temp),
                              rango, rango_temp),
         fecha = ymd(paste(ano, mes, 1, sep = "-"))) %>%
  select(fecha, ano:ENSO, casos.pf:rango_temp)
View(nuqui)

#Escribir CSV
write.csv(nuqui, file = "nuqui.csv")

# Restringir a periodo de tiempo
nuqui90 <-
  nuqui %>%
  filter(ano >= 1994, ano <= 1998)

nuqui00 <-
  nuqui %>%
  filter(ano >= 1999, ano <= 2005)

nuqui98 <-
  nuqui %>%
  filter(ano >= 1999, ano <= 2005)


#Ciclo Anual Nuqui ENSO
cicloA_nuqui_enso <-
  nuqui %>%
  group_by(mes, ENSO) %>%
  summarise(precipitacion = mean(precipitacion, na.rm = TRUE),
            temperatura = mean(temperatura, na.rm = TRUE),
            rango = mean(rango_temp, na.rm = TRUE),
            casos.pf = mean(casos.pf, na.rm = TRUE),
            casos.pv = mean(casos.pv, na.rm = TRUE),
            mosquitos = mean(mosquitos, na.rm = TRUE)
  )

#Ciclo Anual Nuqui98 ENSO
cicloA_nuqui98_enso <-
  nuqui98 %>%
  group_by(mes, ENSO) %>%
  summarise(precipitacion = mean(precipitacion, na.rm = TRUE),
            temperatura = mean(temperatura, na.rm = TRUE),
            rango = mean(rango_temp, na.rm = TRUE),
            casos.pf = mean(casos.pf, na.rm = TRUE),
            casos.pv = mean(casos.pv, na.rm = TRUE),
            mosquitos = mean(mosquitos, na.rm = TRUE)
  )

#Ciclo Anual Nuqui
cicloA_nuqui <-
  nuqui %>%
  group_by(mes) %>%
  summarise(precipitacion = mean(precipitacion, na.rm = TRUE),
            temperatura = mean(temperatura, na.rm = TRUE),
            rango = mean(rango_temp, na.rm = TRUE),
            casos.pf = mean(casos.pf, na.rm = TRUE),
            casos.pv = mean(casos.pv, na.rm = TRUE),
            mosquitos = mean(mosquitos, na.rm = TRUE)
  )