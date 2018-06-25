# Cargar paquetes
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggthemes)
library(PerformanceAnalytics)

# Configurar el directorio de trabajo
#Betty
#setwd("X:/Dropbox/¡¡¡THESIS!!!/Datos/")
#BlackHawck
setwd("E:/Dropbox/¡¡¡THESIS!!!/Datos/")

# Cargar las funciones magiquitas
source("IDEAM.R")

# Leer datos IDEAM
bahia <- read_IDEAM("Climate/IDEAM/Bahia Solano/Bahia Solano. IDEAM. Todos")
amargal <- read_IDEAM("Climate/IDEAM/Nuqui Amargal/Todos diario, mensual por ano")

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
            rango_temp = mean(rango_temp, na.rm = TRUE))

bahia.mes <-
  bahia %>%
  mutate(rango_temp = TEMPERATURA.MAXIMOS - TEMPERATURA.MINIMOS) %>%
  group_by(ano, mes) %>%
  summarise(precipitacion = sum(PRECIPITACION.TOTALES, na.rm = TRUE),
            temperatura = mean(TEMPERATURA.MEDIOS, na.rm = TRUE),
            rango_temp = mean(rango_temp, na.rm = TRUE))

# Tope de NA's a admitir por mes
tope.na.prec <- 7
tope.na.temp <- 14

# Ciclo anual precipitacion
amargal.prec <-
  amargal.na %>%
  filter(prec.na < tope.na.prec) %>%
  left_join(amargal.mes) %>%
  select(ano, mes, prec.na, precipitacion)

cicloA_prec_amargal <-
  amargal.prec %>%
  group_by(mes) %>%
  summarise(n = n(),
            prec = mean(precipitacion, na.rm = TRUE))

bahia.prec <-
  bahia.na %>%
  filter(prec.na < tope.na.prec) %>%
  left_join(bahia.mes) %>%
  select(ano, mes, prec.na, precipitacion)

cicloA_prec_bahia <-
  bahia.prec %>%
  group_by(mes) %>%
  summarise(n = n(),
            prec = mean(precipitacion, na.rm = TRUE))

# Grafica ciclo anual precipitacion amargal
ggplot(cicloA_prec_amargal,
  aes(x = mes,
      y = prec,
      group = 1)) + 
  geom_line(color="dodgerblue3") +
  geom_point(color="dodgerblue3") +
  labs(title = "Annual Cycle of Precipitation at Amargal",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "November 1997 - April 2010") +
  theme_classic()


# Graficas ciclo anual precipitacion Bahía Solano
ggplot(cicloA_prec_bahia,
       aes(x = mes,
           y = prec,
           group = 1)) +
  geom_line(color="steelblue1") +
  geom_point(color="steelblue1") +
  scale_color_manual("ENSO", values = c("blue", "black", "red")) +
  labs(title = "Annual Cycle of Precipitation at Panamericana",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1963 - August 2016") +
  theme_classic()


# Ciclo anual restringido a periodo de tiempo
amargal.9705 <-
  amargal.prec %>%
  filter(ano >= 1997, ano <= 2005)

cicloA_prec_amargal_9705 <-
  amargal.9705 %>%
  group_by(mes) %>%
  summarise(n = n(),
            prec = mean(precipitacion, na.rm = TRUE))

bahia.9405 <-
  bahia.prec %>%
  filter(ano >= 1994, ano <= 2005)

cicloA_prec_bahia_9405 <-
  bahia.9405 %>%
  group_by(mes) %>%
  summarise(n = n(),
            prec = mean(precipitacion, na.rm = TRUE))

# Grafica ciclo anual precipitacion amargal 1997-2005
ggplot(cicloA_prec_amargal_9705,
       aes(x = mes,
           y = prec,
           group = 1)) + 
  geom_line(color="dodgerblue3") +
  geom_point(color="dodgerblue3") +
  labs(title = "Annual Cycle of Precipitation at Amargal",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "November 1997 - December 2005") +
  theme_classic()


# Graficas ciclo anual precipitacion Bahía Solano 1994-2005
ggplot(cicloA_prec_bahia_9405,
       aes(x = mes,
           y = prec,
           group = 1)) +
  geom_line(color="steelblue1") +
  geom_point(color="steelblue1") +
  scale_color_manual("ENSO", values = c("blue", "black", "red")) +
  labs(title = "Annual Cycle of Precipitation at Panamericana",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

