# Cargar paquetes
library(plyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(Hmisc)
require(reshape2)
require(lubridate)

# Configurar el directorio de trabajo
# Betty
setwd("X:/Dropbox/¡¡¡THESIS!!!/Datos/Climate")
# Blackhawck
setwd("E:/Dropbox/¡¡¡THESIS!!!/Datos/Climate")

# Leer datos
datos<-read.csv2("Nuqui_TRMM_CHIRPS.csv", header=T)
datos$date<-as.Date(datos$Fecha, format = "%d/%m/%Y")

# Calculate month from date & create factor:
datos$month <- with(datos, factor(as.POSIXlt(date)$mon, label=month.abb))

# Calculate year from date:
datos$year <- with(datos, 1900 + as.POSIXlt(date)$year)
datos$mes <- month(datos$Fecha)

# Serie mensual
datos.mes <-
  datos %>%
  group_by(year, mes) %>%
  summarise(Nuquí.Sur.TRMM = sum(Nuquí.sur.TRMM),
            Nuquí.Sur.CHIRPS = sum(Nuquí.sur.CHIRPS),
            Nuquí.Centro.TRMM = sum(Nuquí.centro.TRMM),
            Nuquí.Centro.CHIRPS= sum(Nuquí.centro.CHIRPS),
            Nuquí.Norte.TRMM = sum(Nuquí.norte.TRMM),
            Nuquí.Norte.CHIRPS = sum(Nuquí.norte.CHIRPS),
            Bahía.Solano.Sur.TRMM = sum(Bahía.Solano.sur.TRMM),
            Bahía.Solano.Sur.CHIRPS = sum(Bahía.Solano.sur.CHIRPS),
            Bahía.Solano.Centro.TRMM = sum(Bahía.Solano.centro.TRMM),
            Bahía.Solano.Centro.CHIRPS = sum(Bahía.Solano.centro.CHIRPS),
            Bahía.Solano.Norte.TRMM = sum(Bahía.Solano.norte.TRMM),
            Bahía.Solano.Norte.CHIRPS = sum(Bahía.Solano.norte.CHIRPS)
           )

# Serie mensual 1994-2005
datos.mes.9405 <-
  datos.mes %>%
  filter(year >= 1994, year <= 2005)

# Ciclo anual 
CicloAnual <-
  datos.mes %>%
  group_by(mes) %>%
  summarise(n = n(),
            Nuquí.South.TRMM = mean(Nuquí.Sur.TRMM, na.rm = TRUE),
            Nuquí.South.CHIRPS = mean(Nuquí.Sur.CHIRPS, na.rm = TRUE),
            Nuquí.Center.TRMM = mean(Nuquí.Centro.TRMM, na.rm = TRUE),
            Nuquí.Center.CHIRPS= mean(Nuquí.Centro.CHIRPS, na.rm = TRUE),
            Nuquí.North.TRMM = mean(Nuquí.Norte.TRMM, na.rm = TRUE),
            Nuquí.North.CHIRPS = mean(Nuquí.Norte.CHIRPS, na.rm = TRUE),
            Bahía.Solano.South.TRMM = mean(Bahía.Solano.Sur.TRMM, na.rm = TRUE),
            Bahía.Solano.South.CHIRPS = mean(Bahía.Solano.Sur.CHIRPS, na.rm = TRUE),
            Bahía.Solano.Center.TRMM = mean(Bahía.Solano.Centro.TRMM, na.rm = TRUE),
            Bahía.Solano.Center.CHIRPS = mean(Bahía.Solano.Centro.CHIRPS, na.rm = TRUE),
            Bahía.Solano.North.TRMM = mean(Bahía.Solano.Norte.TRMM, na.rm = TRUE),
            Bahía.Solano.North.CHIRPS = mean(Bahía.Solano.Norte.CHIRPS, na.rm = TRUE)
            )

# Ciclo anual 1994-2005
CicloAnual9405 <-
  datos.mes.9405 %>%
  group_by(mes) %>%
  summarise(n = n(),
            Nuquí.South.TRMM = mean(Nuquí.Sur.TRMM, na.rm = TRUE),
            Nuquí.South.CHIRPS = mean(Nuquí.Sur.CHIRPS, na.rm = TRUE),
            Nuquí.Center.TRMM = mean(Nuquí.Centro.TRMM, na.rm = TRUE),
            Nuquí.Center.CHIRPS= mean(Nuquí.Centro.CHIRPS, na.rm = TRUE),
            Nuquí.North.TRMM = mean(Nuquí.Norte.TRMM, na.rm = TRUE),
            Nuquí.North.CHIRPS = mean(Nuquí.Norte.CHIRPS, na.rm = TRUE),
            Bahía.Solano.South.TRMM = mean(Bahía.Solano.Sur.TRMM, na.rm = TRUE),
            Bahía.Solano.South.CHIRPS = mean(Bahía.Solano.Sur.CHIRPS, na.rm = TRUE),
            Bahía.Solano.Center.TRMM = mean(Bahía.Solano.Centro.TRMM, na.rm = TRUE),
            Bahía.Solano.Center.CHIRPS = mean(Bahía.Solano.Centro.CHIRPS, na.rm = TRUE),
            Bahía.Solano.North.TRMM = mean(Bahía.Solano.Norte.TRMM, na.rm = TRUE),
            Bahía.Solano.North.CHIRPS = mean(Bahía.Solano.Norte.CHIRPS, na.rm = TRUE)
            )
###################   Nuqui  All Data  ###################
# Grafica ciclo anual precipitacion Nuquí Sur TRMM
ggplot(CicloAnual,
       aes(x = mes,
           y = Nuquí.South.TRMM,
           group = 1)) + 
  geom_line(color="deepskyblue1") +
  geom_point(color="deepskyblue1") +
  labs(title = "Annual Cycle of Precipitation at Nuquí South TRMM",
     x = "Month",
     y = "Total Precipitation [mm]",
     subtitle = "January 1998 - December 2015") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Sur CHIRPS
ggplot(CicloAnual,
       aes(x = mes,
           y = Nuquí.South.CHIRPS,
           group = 1)) + 
  geom_line(color="dodgerblue1") +
  geom_point(color="dodgerblue1") +
  labs(title = "Annual Cycle of Precipitation at Nuquí South CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1981 - December 2016") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Centro TRMM
ggplot(CicloAnual,
       aes(x = mes,
           y = Nuquí.Center.TRMM,
           group = 1)) + 
  geom_line(color="deepskyblue3") +
  geom_point(color="deepskyblue3") +
  labs(title = "Annual Cycle of Precipitation at Nuquí Center TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2015") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Centro CHIRPS
ggplot(CicloAnual,
       aes(x = mes,
           y = Nuquí.Center.CHIRPS,
           group = 1)) + 
  geom_line(color="dodgerblue3") +
  geom_point(color="dodgerblue3") +
  labs(title = "Annual Cycle of Precipitation at Nuquí Center CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1981 - December 2016") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Norte TRMM
ggplot(CicloAnual,
       aes(x = mes,
           y = Nuquí.North.TRMM,
           group = 1)) + 
  geom_line(color="deepskyblue4") +
  geom_point(color="deepskyblue4") +
  labs(title = "Annual Cycle of Precipitation at Nuquí North TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2015") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Norte CHIRPS
ggplot(CicloAnual,
       aes(x = mes,
           y = Nuquí.North.CHIRPS,
           group = 1)) + 
  geom_line(color="dodgerblue4") +
  geom_point(color="dodgerblue4") +
  labs(title = "Annual Cycle of Precipitation at Nuquí North CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1981 - December 2016") +
  theme_classic()

###################   Bahia All data    ###################
# Grafica ciclo anual precipitacion Bahía Solano Sur TRMM 
ggplot(CicloAnual,
       aes(x = mes,
           y = Bahía.Solano.South.TRMM,
           group = 1)) + 
  geom_line(color="skyblue1") +
  geom_point(color="skyblue1") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano South TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2015") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Sur CHIRPS
ggplot(CicloAnual,
       aes(x = mes,
           y = Bahía.Solano.South.CHIRPS,
           group = 1)) + 
  geom_line(color="steelblue1") +
  geom_point(color="steelblue1") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano South CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1981 - December 2016") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Centro TRMM
ggplot(CicloAnual,
       aes(x = mes,
           y = Bahía.Solano.Center.TRMM,
           group = 1)) + 
  geom_line(color="skyblue3") +
  geom_point(color="skyblue3") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano Center TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2015") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Centro CHIRPS
ggplot(CicloAnual,
       aes(x = mes,
           y = Bahía.Solano.Center.CHIRPS,
           group = 1)) + 
  geom_line(color="steelblue3") +
  geom_point(color="steelblue3") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano Center CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1981 - December 2016") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Norte TRMM
ggplot(CicloAnual,
       aes(x = mes,
           y = Bahía.Solano.North.TRMM,
           group = 1)) + 
  geom_line(color="skyblue4") +
  geom_point(color="skyblue4") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano North TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2015") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Norte CHIRPS
ggplot(CicloAnual,
       aes(x = mes,
           y = Bahía.Solano.North.CHIRPS,
           group = 1)) + 
  geom_line(color="steelblue4") +
  geom_point(color="steelblue4") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano North CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1981 - December 2016") +
  theme_classic()

###############   Nuqui  Simulation Period  #################
# Grafica ciclo anual precipitacion Nuquí Sur TRMM
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Nuquí.South.TRMM,
           group = 1)) + 
  geom_line(color="deepskyblue1") +
  geom_point(color="deepskyblue1") +
  labs(title = "Annual Cycle of Precipitation at Nuquí South TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Sur CHIRPS
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Nuquí.South.CHIRPS,
           group = 1)) + 
  geom_line(color="dodgerblue1") +
  geom_point(color="dodgerblue1") +
  labs(title = "Annual Cycle of Precipitation at Nuquí South CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Centro TRMM
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Nuquí.Center.TRMM,
           group = 1)) + 
  geom_line(color="deepskyblue3") +
  geom_point(color="deepskyblue3") +
  labs(title = "Annual Cycle of Precipitation at Nuquí Center TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Centro CHIRPS
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Nuquí.Center.CHIRPS,
           group = 1)) + 
  geom_line(color="dodgerblue3") +
  geom_point(color="dodgerblue3") +
  labs(title = "Annual Cycle of Precipitation at Nuquí Center CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Norte TRMM
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Nuquí.North.TRMM,
           group = 1)) + 
  geom_line(color="deepskyblue4") +
  geom_point(color="deepskyblue4") +
  labs(title = "Annual Cycle of Precipitation at Nuquí North TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Nuquí Norte CHIRPS
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Nuquí.North.CHIRPS,
           group = 1)) + 
  geom_line(color="dodgerblue4") +
  geom_point(color="dodgerblue4") +
  labs(title = "Annual Cycle of Precipitation at Nuquí North CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

###############   Bahia Simulation period    ################
# Grafica ciclo anual precipitacion Bahía Solano Sur TRMM 
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Bahía.Solano.South.TRMM,
           group = 1)) + 
  geom_line(color="skyblue1") +
  geom_point(color="skyblue1") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano South TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Sur CHIRPS
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Bahía.Solano.South.CHIRPS,
           group = 1)) + 
  geom_line(color="steelblue1") +
  geom_point(color="steelblue1") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano South CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Centro TRMM
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Bahía.Solano.Center.TRMM,
           group = 1)) + 
  geom_line(color="skyblue3") +
  geom_point(color="skyblue3") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano Center TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Centro CHIRPS
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Bahía.Solano.Center.CHIRPS,
           group = 1)) + 
  geom_line(color="steelblue3") +
  geom_point(color="steelblue3") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano Center CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Norte TRMM
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Bahía.Solano.North.TRMM,
           group = 1)) + 
  geom_line(color="skyblue4") +
  geom_point(color="skyblue4") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano North TRMM",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2005") +
  theme_classic()

# Grafica ciclo anual precipitacion Bahía Solano Norte CHIRPS
ggplot(CicloAnual9405,
       aes(x = mes,
           y = Bahía.Solano.North.CHIRPS,
           group = 1)) + 
  geom_line(color="steelblue4") +
  geom_point(color="steelblue4") +
  labs(title = "Annual Cycle of Precipitation at Bahía Solano North CHIRPS",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()
