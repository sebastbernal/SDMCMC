#Cargar paquetes
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(magrittr)
library(rsoi)

library(ggplot2)
library(scales)
library(gridExtra)
library(ggthemes)
library(PerformanceAnalytics)

pf <- expression(paste("Monthly ", italic("P. falciparum"), " cases"))
pf2 <- expression(paste(italic("P. falciparum"), " cases"))
pv <- expression(paste("Monthly ", italic("P. vivax"), " cases"))
pv2 <- expression(paste(italic("P. vivax"), " cases"))

# Cargar Nuqui RData
load("Analysis IDEAM 4_Julio_23.RData")

################# Time Series #####################
#Faltan lineas verticales de guía: +geom_vline() ¿Para que? Don't remember

#Precipitation Time Series
ggplot(nuqui, 
       aes(fecha,
           precipitacion)) +
  geom_line(color="blue") +
  labs(title = "Total Monthly Precipitation at Nuqui",
       x = "Time",
       y = "Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic() +
  scale_x_date(breaks = date_breaks("1 year"),
               labels = date_format("%y"))

# Temperature Time Series
ggplot(nuqui,
       aes(fecha,
           temperatura)) + 
  geom_line(color="red")+
  labs(title = "Mean Monthly Air Temperature at Nuqui",
       x = "Time",
       y = "Temperature [ºC]",
       subtitle = "January 1994 - December 2005")+
  theme_classic() +
  scale_x_date(breaks = date_breaks("1 year"),
               labels = date_format("%y"))

# DTR Time Series
ggplot(nuqui,
       aes(fecha,
           rango_temp)) + 
  geom_line(color="darkorange2")+
  labs(title = "Daily Temperature Range at Nuqui",
       x = "Time",
       y = "Temperature [ºC]",
       subtitle = "January 1994 - December 2005")+
  theme_classic() +
  scale_x_date(breaks = date_breaks("1 year"),
               labels = date_format("%y"))

# Pf Cases Time Series
ggplot(nuqui,
       aes(fecha,
           casos.pf)) + 
  geom_line(color="darkorchid1")+
  labs(title = pf,
       x = "Time",
       y = "Number of Cases",
       subtitle = "January 1994 - April 2005")+
  theme_classic() +
  scale_x_date(breaks = date_breaks("1 year"),
               labels = date_format("%y"))

# # Pv Cases Time Series
# ggplot(nuqui,
#        aes(fecha,
#            casos.pv)) + 
#   geom_line(color="deeppink")+
#   labs(title = pv,
#        x = "Time",
#        y = "Number of Cases",
#        subtitle = "January 1994 - April 2005")+
#   theme_classic() +
#   scale_x_date(breaks = date_breaks("1 year"),
#                labels = date_format("%y"))

# Mosquitoes Time Series
ggplot(densidad,
       aes(Fecha,
           mosquitos)) + 
  geom_line(color="forestgreen")+
  labs(title = "Mosquitoes at Nuqui",
       x = "Time",
       y = "Number of Mosquitoes",
       subtitle = "March 1998 - April 2005")+
  theme_classic() +
  scale_x_date(breaks = date_breaks("1 year"),
               labels = date_format("%y"))

###################### Ciclo Anual ##############################
# 
# # Ciclo anual precipitacion amargal
# ggplot(cicloA_prec_amargal,
#        aes(x = mes,
#            y = prec,
#            group = ENSO,
#            color = ENSO)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual("ENSO", values = c("blue", "black", "red")) +
#   labs(title = "Annual Cycle of Precipitation at Amargal",
#        x = "Month",
#        y = "Total Precipitation [mm]",
#        subtitle = "November 1997 - April 2010") +
#   theme_classic()
# 
# # Ciclo anual temperatura amargal
# ggplot(cicloA_temp_amargal,
#        aes(x = mes,
#            y = temp,
#            group = ENSO,
#            color = ENSO)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual("ENSO", values = c("blue", "black", "red")) +
#   labs(title = "Annual Cycle of Mean Temperature at Amargal",
#        x = "Month",
#        y = "Mean Temperature [ºC]",
#        subtitle = "November 1997 - April 2010") +
#   theme_classic()
# 
# # Ciclo anual rango amargal
# ggplot(cicloA_rango_amargal,
#        aes(x = mes,
#            y = rango,
#            group = ENSO,
#            color = ENSO)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual("ENSO", values = c("blue", "black", "red")) +
#   labs(title = "Annual Cycle of Diurnal Temperature Range at Amargal",
#        x = "Month",
#        y = "Diurnal Temperature Range [ºC]",
#        subtitle = "November 1997 - April 2010") +
#   theme_classic()
# 
# # Ciclo anual precipitacion Bahía Solano
# ggplot(cicloA_prec_bahia,
#        aes(x = mes,
#            y = prec,
#            group = ENSO,
#            color = ENSO)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual("ENSO", values = c("blue", "black", "red")) +
#   labs(title = "Annual Cycle of Precipitation at Bahia Solano",
#        x = "Month",
#        y = "Total Precipitation [mm]",
#        subtitle = "January 1963 - August 2016") +
#   theme_classic()
# 
# # Ciclo anual temperatura Bahia
# ggplot(cicloA_temp_bahia,
#        aes(x = mes,
#            y = temp,
#            group = ENSO,
#            color = ENSO)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual("ENSO", values = c("blue", "black", "red")) +
#   labs(title = "Annual Cycle of Mean Temperature at Bahia Solano",
#        x = "Month",
#        y = "Mean Temperature [ºC]",
#        subtitle = "January 1978 - August 2016") +
#   theme_classic()
# 
# # Ciclo anual rango Bahia
# ggplot(cicloA_rango_bahia,
#        aes(x = mes,
#            y = rango,
#            group = ENSO,
#            color = ENSO)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual("ENSO", values = c("blue", "black", "red")) +
#   labs(title = "Annual Cycle of Diurnal Temperature Range at Bahia Solano",
#        x = "Month",
#        y = "Diurnal Temperature Range [ºC]",
#        subtitle = "January 1978 - August 2016") +
#   theme_classic()

nuqui %>%
  # glimpse
  # Filtrar por aÃ±os para ver una porciÃ³n de la serie
  filter(ano >= 1995,
         ano <= 2005) %>%
  ggplot(aes(x = fecha,
             y = precipitacion)) +
  geom_line() +
  geom_point()

# Ciclo anual precipitacion Nuqui ENSO
ggplot(cicloA_nuqui_enso,
       aes(x = mes,
           y = precipitacion,
           group = ENSO,
           color = ENSO)) +
  geom_line() +
  geom_point() +
  scale_color_manual("ENSO", values = c("red", "blue", "black")) +
  labs(title = "Annual Cycle of Precipitation at Nuqui",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Ciclo anual precipitacion Nuqui98 ENSO
ggplot(cicloA_nuqui98_enso,
       aes(x = mes,
           y = precipitacion,
           group = ENSO,
           color = ENSO)) +
  geom_line() +
  geom_point() +
  scale_color_manual("ENSO", values = c("red", "blue", "black")) +
  labs(title = "Annual Cycle of Precipitation at Nuqui",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1998 - December 2005") +
  theme_classic()

# Ciclo anual precipitacion Nuqui
ggplot(cicloA_nuqui,
       aes(x = mes,
           y = precipitacion,
           group = 1)) +
  geom_line(color="blue") +
  geom_point(color="blue") +
  labs(title = "Annual Cycle of Precipitation at Nuqui",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Ciclo anual temperatura Nuqui ENSO
ggplot(cicloA_nuqui_enso,
       aes(x = mes,
           y = temperatura,
           group = ENSO,
           color = ENSO)) +
  geom_line() +
  geom_point() +
  scale_color_manual("ENSO", values = c("red", "blue", "black")) +
  labs(title = "Annual Cycle of Mean Temperature at Nuqui",
       x = "Month",
       y = "Mean Temperature [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()
  

# Ciclo anual temperatura Nuqui
ggplot(cicloA_nuqui,
       aes(x = mes,
           y = temperatura,
           group = 1)) +
  geom_line(color="red") +
  geom_point(color="red") +
   labs(title = "Annual Cycle of Mean Temperature at Nuqui",
       x = "Month",
       y = "Mean Temperature [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Ciclo Anual Rango Nuqui ENSO
ggplot(cicloA_nuqui_enso,
       aes(x = mes,
           y = rango,
           group = ENSO,
           color = ENSO)) +
  geom_line() +
  geom_point() +
  scale_color_manual("ENSO", values = c("red", "blue", "black")) +
  labs(title = "Annual Cycle of Diurnal Temperature Range at Nuqui",
       x = "Month",
       y = "Diurnal Temperature Range [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()

# Ciclo Anual Rango Nuqui
ggplot(cicloA_nuqui,
       aes(x = mes,
           y = rango,
           group = 1)) +
  geom_line(color="darkorange2") +
  geom_point(color="darkorange2") +
  labs(title = "Annual Cycle of Diurnal Temperature Range at Nuqui",
       x = "Month",
       y = "Diurnal Temperature Range [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()


# Ciclo Anual Casos Pf ENSO
ggplot(cicloA_nuqui_enso,
       aes(x = mes,
           y = casos.pf,
           group = ENSO,
           color = ENSO)) +
  geom_line() +
  geom_point() +
  scale_color_manual("ENSO", values = c("red", "blue", "black")) +
  labs(title = "Annual Cycle of P. falciparum cases at Nuqui",
       x = "Month",
       y = "Cases",
       subtitle = "January 1994 - April 2005") +
  theme_classic() #+ 
  #ylim(c(76,300)) #El Niño
  #ylim(c(0,75)) #Normal y La Niña

# Ciclo Anual Casos Pf
ggplot(cicloA_nuqui,
       aes(x = mes,
           y = casos.pf,
           group = 1)) +
  geom_line(color="darkorchid1") +
  geom_point(color="darkorchid1") +
  labs(title = "Annual Cycle of P. falciparum cases at Nuqui",
       x = "Month",
       y = "Cases",
       subtitle = "January 1994 - April 2005") +
  theme_classic()

#Anna Stewart Seasonality

# ggplot(nuqui,
#        aes(mes,
#            casos.pf))+
#   geom_boxplot()+
#   geom_point(aes(colour=factor(mes)))+
#   geom_smooth(aes(group=1))+
#   theme_bw()+
#   labs(x="Month",y="Monthly RSV Cases", title="A1.") +
#   theme(legend.position="none",
#         legend.title=element_blank(),
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         legend.text = element_text(size = 12),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         plot.title = element_text(size = 22))

# # Ciclo Anual Casos Pv ENSO
# ggplot(cicloA_nuqui_enso,
#        aes(x = mes,
#            y = casos.pv,
#            group = ENSO,
#            color = ENSO)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual("ENSO", values = c("blue", "black", "red")) +
#   labs(title = "Annual Cycle of P. vivax cases at Nuqui",
#        x = "Month",
#        y = "Cases",
#        subtitle = "January 1994 - April 2005") +
#   theme_classic()
# 
# # Ciclo Anual Casos Pv
# ggplot(cicloA_nuqui,
#        aes(x = mes,
#            y = casos.pv,
#            group = 1)) +
#   geom_line(color="deeppink") +
#   geom_point(color="deeppink") +
#   labs(title = "Annual Cycle of P. vivax cases at Nuqui",
#        x = "Month",
#        y = "Cases",
#        subtitle = "January 1994 - April 2005") +
#   theme_classic()

# Ciclo Anual Mosquitoes ENSO
ggplot(cicloA_nuqui_enso,
       aes(x = mes,
           y = mosquitos,
           group = ENSO,
           color = ENSO)) +
  geom_line() +
  geom_point() +
  scale_color_manual("ENSO", values = c("red", "blue", "black")) +
  labs(title = "Annual Cycle of Mosquitoes at Nuqui",
       x = "Month",
       y = "Mosquitoes",
       subtitle = "March 1998 - April 2005") +
  theme_classic()

# Ciclo Anual Mosquitoes
ggplot(cicloA_nuqui,
       aes(x = mes,
           y = mosquitos,
           group = 1)) +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") +
  labs(title = "Annual Cycle of Mosquitoes at Nuqui",
       x = "Month",
       y = "Mosquitoes",
       subtitle = "March 1998 - April 2005") +
  theme_classic()

################### Box Plot ############################

# Boxplot de todas las variables de nuqui
# nuqui %>%
#   gather(var, val, casos:rango_temp) %>%
#   ggplot(aes(x = mes,
#              y = val)) +
#   geom_boxplot() +
#   facet_grid(var ~ .,
#              scales = "free") 

# Boxplot Precipitacion 1994-2005
bpprec <- ggplot(nuqui,
       aes(x = mes,
           y = precipitacion)) +
  geom_boxplot(fill = "royalblue4")+
  labs(title = "Precipitation at Nuqui",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))
bpprec

# Boxplot Precipitacion 1994-2005 ENSO
ggplot(nuqui,
       aes(x = mes,
           y = precipitacion)) +
  labs(title = "Precipitation at Nuqui",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 2005") +
  theme_classic() +
  geom_boxplot(aes(fill=ENSO)) +
  scale_fill_manual("ENSO", values = c("brown1", "deepskyblue1", "azure3")) +
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Precipitacion 1994-1998
ggplot(nuqui90,
       aes(x = mes,
           y = precipitacion)) +
  geom_boxplot(fill = "royalblue4")+
  labs(title = "Precipitation at Nuqui",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1994 - December 1998") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Precipitacion 1999-2005
ggplot(nuqui00,
       aes(x = mes,
           y = precipitacion)) +
  geom_boxplot(fill = "royalblue4")+
  labs(title = "Precipitation at Nuqui",
       x = "Month",
       y = "Total Precipitation [mm]",
       subtitle = "January 1999 - December 2005") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Temperatura 1994-2005
bptemp <- ggplot(nuqui,
       aes(x = mes,
           y = temperatura)) +
  geom_boxplot(fill="red4")+
  labs(title = "Air Temperature at Nuqui",
       x = "Month",
       y = "Temperature [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

bptemp

# Boxplot Temperatura 1994-2005 ENSO
ggplot(nuqui,
       aes(x = mes,
           y = temperatura)) +
  labs(title = "Air Temperature at Nuqui",
       x = "Month",
       y = "Temperature [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic() +
  geom_boxplot(aes(fill=ENSO)) +
  scale_fill_manual("ENSO", values = c("brown1", "deepskyblue1", "azure3")) +
    theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Temperatura 1994-1998
ggplot(nuqui90,
       aes(x = mes,
           y = temperatura)) +
  geom_boxplot(fill="red4")+
  labs(title = "Air Temperature at Nuqui",
       x = "Month",
       y = "Temperature [ºC]",
       subtitle = "January 1994 - December 1998") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Temperatura 1999-2005
ggplot(nuqui00,
       aes(x = mes,
           y = temperatura)) +
  geom_boxplot(fill="red4")+
  labs(title = "Air Temperature at Nuqui",
       x = "Month",
       y = "Temperature [ºC]",
       subtitle = "January 1999 - December 2005") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Rango 1994-2005
bprango <- ggplot(nuqui,
       aes(x = mes,
           y = rango_temp)) +
  geom_boxplot(fill="darkorange2")+
  labs(title = "Diurnal Temperature Range at Nuqui",
       x = "Month",
       y = "Diurnal Temperature Range [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))
bprango

# Boxplot Rango 1994-2005 ENSO
ggplot(nuqui,
       aes(x = mes,
           y = rango_temp)) +
  labs(title = "Diurnal Temperature Range at Nuqui",
       x = "Month",
       y = "Diurnal Temperature Range [ºC]",
       subtitle = "January 1994 - December 2005") +
  theme_classic()+
  geom_boxplot(aes(fill=ENSO)) +
  scale_fill_manual("ENSO", values = c("brown1", "deepskyblue1", "azure3")) +
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Rango 1994-1998
ggplot(nuqui90,
       aes(x = mes,
           y = rango_temp)) +
  geom_boxplot(fill="darkorange2")+
  labs(title = "Diurnal Temperature Range at Nuqui",
       x = "Month",
       y = "Diurnal Temperature Range [ºC]",
       subtitle = "January 1994 - December 1998") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Rango 1999-2005
ggplot(nuqui00,
       aes(x = mes,
           y = rango_temp)) +
  geom_boxplot(fill="darkorange2")+
  labs(title = "Diurnal Temperature Range at Nuqui",
       x = "Month",
       y = "Diurnal Temperature Range [ºC]",
       subtitle = "January 1999 - December 2005") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))+
  ylim(c(4,8))

# Boxplot Casos.pf 1994-2005
bpcases.pf <- ggplot(nuqui,
       aes(x = mes,
           y = casos.pf)) +
  geom_boxplot(fill="darkorchid1")+
  labs(title = pf2,
       x = "Month",
       y = "Number of cases",
       subtitle = "January 1994 - December 2005") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))
bpcases.pf

# Boxplot Casos.pf 1994-2005 ENSO
ggplot(nuqui,
       aes(x = mes,
           y = casos.pf)) +
   labs(title = pf2,
       x = "Month",
       y = "Number of cases",
       subtitle = "January 1994 - December 2005") +
  theme_classic()+
  geom_boxplot(aes(fill=ENSO)) +
  scale_fill_manual("ENSO", values = c("brown1", "deepskyblue1", "azure3")) +
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Casos.pf 1994-1998
ggplot(nuqui90,
       aes(x = mes,
           y = casos.pf)) +
  geom_boxplot(fill="darkorchid1")+
  labs(title = pf2,
       x = "Month",
       y = "Number of cases",
       subtitle = "January 1994 - December 1998") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Casos.pf 1999-2005
ggplot(nuqui00,
       aes(x = mes,
           y = casos.pf)) +
  geom_boxplot(fill="darkorchid1")+
  labs(title = pf2,
       x = "Month",
       y = "Number of cases",
       subtitle = "January 1999 - December 2005") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))

# # Boxplot Casos.pv 1994-2005
# bpcases.pv <- ggplot(nuqui,
#                      aes(x = mes,
#                          y = casos.pv)) +
#   geom_boxplot(fill="deeppink")+
#   labs(title = pv2,
#        x = "Month",
#        y = "Number of cases",
#        subtitle = "January 1994 - December 2005") +
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0, size =16), 
#         axis.title.y=element_text(vjust=1, size = 14),
#         axis.title.x=element_text(vjust=0, size = 14))
# 
# bpcases.pv
# 
# # Boxplot Casos.pv 1994-2005 ENSO
# ggplot(nuqui,
#        aes(x = mes,
#            y = casos.pv)) +
#   labs(title = pv2,
#        x = "Month",
#        y = "Number of cases",
#        subtitle = "January 1994 - December 2005") +
#   theme_classic()+
#   geom_boxplot(aes(fill=ENSO)) +
#   scale_fill_manual("ENSO", values = c("deepskyblue1", "azure3", "brown1")) +
#   theme(plot.title = element_text(hjust = 0, size =16), 
#         axis.title.y=element_text(vjust=1, size = 14),
#         axis.title.x=element_text(vjust=0, size = 14))
# 
# # Boxplot Casos.pv 1994-1998
# ggplot(nuqui90,
#        aes(x = mes,
#            y = casos.pf)) +
#   geom_boxplot(fill="deeppink")+
#   labs(title = pv2,
#        x = "Month",
#        y = "Number of cases",
#        subtitle = "January 1994 - December 1998") +
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0, size =16), 
#         axis.title.y=element_text(vjust=1, size = 14),
#         axis.title.x=element_text(vjust=0, size = 14))
# 
# # Boxplot Casos.pv 1999-2005
# ggplot(nuqui00,
#        aes(x = mes,
#            y = casos.pv)) +
#   geom_boxplot(fill="deeppink")+
#   labs(title = pv2,
#        x = "Month",
#        y = "Number of cases",
#        subtitle = "January 1999 - December 2005") +
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0, size =16), 
#         axis.title.y=element_text(vjust=1, size = 14),
#         axis.title.x=element_text(vjust=0, size = 14))

# Boxplot Mosquitos 1998-2005
bpmosqui <- ggplot(nuqui,
       aes(x = mes,
           y =mosquitos)) +
  geom_boxplot(fill="forestgreen")+
  labs(title = "Mosquitoes",
       x = "Month",
       y = "Number of mosquitoes",
       subtitle = "March 1998 - April 2005") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))
bpmosqui

# Boxplot Mosquitos 1998-2005 ENSO
ggplot(nuqui,
       aes(x = mes,
           y =mosquitos)) +
  labs(title = "Mosquitoes",
       x = "Month",
       y = "Number of mosquitoes",
       subtitle = "March 1998 - April 2005") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, size =16), 
        axis.title.y=element_text(vjust=1, size = 14),
        axis.title.x=element_text(vjust=0, size = 14))+
  geom_boxplot(aes(fill=ENSO)) +
  scale_fill_manual("ENSO", values = c("brown1", "deepskyblue1", "azure3"))
  

#Dos boxplot por grafico
# grid.arrange(bpprec, bpmosqui, ncol=2)
# grid.arrange(bptemp, bpmosqui, ncol=2)
# grid.arrange(bprango, bpmosqui, ncol=2)
# 
# grid.arrange(bpprec, bpcases.pf, ncol=2)
# grid.arrange(bptemp, bpcases.pf, ncol=2)
# grid.arrange(bprango, bpcases.pf, ncol=2)
# 
# grid.arrange(bpprec, bpcases.pv, ncol=2)
# grid.arrange(bptemp, bpcases.pv, ncol=2)
# grid.arrange(bprango, bpcases.pv, ncol=2)

############### Dos Ejes Y ###########################

#Casos vs temperatura 1994-1995
op1 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1994, ano <= 1995),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1994, ano <= 1995),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("top", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op1)

#Casos vs temperatura 1996-1997
op2 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1996, ano <= 1997),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1996, ano <= 1997),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topleft", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op2)

#Casos vs temperatura 1998-1999
op3 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1998, ano <= 1999),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1998, ano <= 1999),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op3)

#Casos vs temperatura 2000-2001
op4 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2000, ano <= 2001),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2000, ano <= 2001),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("top", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op4)

#Casos vs temperatura 2002-2003
op5 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2002, ano <= 2003),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2002, ano <= 2003),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op5)

#Casos vs temperatura 2004-2005
op6 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2004, ano <= 2005),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2004, ano <= 2005),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op6)

#Casos vs temperatura 1994-1996
op17 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1994, ano <= 1996),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1994, ano <= 1996),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op17)

#Casos vs temperatura 1997-1998
op18 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1997, ano <= 1998),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1997, ano <= 1998),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op18)

#Casos vs temperatura 1999-2000
op19 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1999, ano <= 2000),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1999, ano <= 2000),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op19)

#Casos vs temperatura 2001-2003
op20 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2001, ano <= 2003),
     plot(fecha, temperatura, type = "l", col = "red",
          xlab = "Time", ylab = "Temperature"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2001, ano <= 2003),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("red", "darkorchid1"), lty = 1, legend = c("Temperature", "Cases"))
par(op20)


#Casos vs precipitacion 1994-1995
op7 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1994, ano <= 1995),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1994, ano <= 1995),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("top", col = c("blue", "darkorchid1"), lty = 1, legend = c("Precipitation", "Cases"))
par(op7)

#Casos vs precipitacion 1996-1997
op8 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1996, ano <= 1997),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1996, ano <= 1997),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topleft", col = c("blue", "darkorchid1"), lty = 1, legend = c("Precipitation", "Cases"))
par(op8)

#Casos vs precipitacion 1998-1999
op9 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1998, ano <= 1999),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1998, ano <= 1999),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("top", col = c("blue", "darkorchid1"), lty = 1, legend = c("Precipitation", "Cases"))
par(op9)

#Casos vs precipitacion 2000-2001
op10 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2000, ano <= 2001),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2000, ano <= 2001),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("top", col = c("blue", "darkorchid1"), lty = 1, legend = c("Precipitation", "Cases"))
par(op10)

#Casos vs precipitacion 2002-2003
op11 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2002, ano <= 2003),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2002, ano <= 2003),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topright", col = c("blue", "darkorchid1"), lty = 1, legend = c("Precipitation", "Cases"))
par(op11)

#Casos vs precipitacion 2004-2005
op12 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2004, ano <= 2005),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2004, ano <= 2005),
     plot(fecha, casos.pf, type = "l", col = "darkorchid1",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Cases", side = 4, line = 3)
legend("topleft", col = c("blue", "darkorchid1"), lty = 1, legend = c("Precipitation", "Cases"))
par(op12)

#Mosquitos vs preciptacion 1998-1999
op13 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 1998, ano <= 1999),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 1998, ano <= 1999),
     plot(fecha, mosquitos, type = "l", col = "green",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Mosquitoes", side = 4, line = 3)
legend("topleft", col = c("blue", "green"), lty = 1, legend = c("Precipitation", "Mosquitoes"))
par(op13)

#Mosquitos vs preciptacion 2000-2001
op14 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2000, ano <= 2001),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2000, ano <= 2001),
     plot(fecha, mosquitos, type = "l", col = "green",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Mosquitoes", side = 4, line = 3)
legend("topleft", col = c("blue", "green"), lty = 1, legend = c("Precipitation", "Mosquitoes"))
par(op14)

#Mosquitos vs preciptacion 2002-2003
op15 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2002, ano <= 2003),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2002, ano <= 2003),
     plot(fecha, mosquitos, type = "l", col = "green",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Mosquitoes", side = 4, line = 3)
legend("topleft", col = c("blue", "green"), lty = 1, legend = c("Precipitation", "Mosquitoes"))
par(op15)

#Mosquitos vs preciptacion 2004-2005
op16 <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 5) + .1)
with(filter(nuqui,
            ano >= 2004, ano <= 2005),
     plot(fecha, precipitacion, type = "l", col = "blue",
          xlab = "Time", ylab = "Precipitation"))
par(new = TRUE)
with(filter(nuqui,
            ano >= 2004, ano <= 2005),
     plot(fecha, mosquitos, type = "l", col = "green",
          xaxt = "n", yaxt = "n", xlab = "", ylab=""))
axis(4)
mtext("Mosquitoes", side = 4, line = 3)
legend("top", col = c("blue", "green"), lty = 1, legend = c("Precipitation", "Mosquitoes"))
par(op16)

################# Correlations ##########################

#Cross-lagged correlations
correl1 <- with(filter(nuqui,
                      ano >= 1994, ano <= 2005),
               ccf(temperatura, casos.pf, lag.max = 8, na.action = na.pass))

correl2 <- with(filter(nuqui,
                       ano >= 1994, ano <= 2005),
                ccf(rango_temp, casos.pf, lag.max = 8, na.action = na.pass))

correl3 <- with(filter(nuqui,
                       ano >= 1994, ano <= 2005),
                ccf(precipitacion, casos.pf, lag.max = 8, na.action = na.pass))

correl4 <- with(filter(nuqui,
                       ano >= 1998, ano <= 2005),
                ccf(temperatura, mosquitos, lag.max =8, na.action = na.pass))

correl5 <- with(filter(nuqui,
                       ano >= 1998, ano <= 2005),
                ccf(precipitacion, mosquitos, lag.max = 8, na.action = na.pass))

correl6 <- with(filter(nuqui,
                       ano >= 1998, ano <= 2005),
                ccf(rango_temp, mosquitos, lag.max = 8, na.action = na.pass))

correl7 <- with(filter(nuqui,
                       ano >= 1998, ano <= 2005),
                ccf(mosquitos, casos.pf, lag.max = 8, na.action = na.pass))

correl8 <- with(filter(nuqui,
                       ano >= 1994, ano <= 2005),
                ccf(oni, casos.pf, lag.max = 8, na.action = na.pass))

correl9 <- with(filter(nuqui,
                       ano >= 1994, ano <= 2005),
                ccf(oni, mosquitos, lag.max = 8, na.action = na.pass))

correl10 <- with(filter(nuqui,
                       ano >= 1994, ano <= 2005),
                ccf(oni, temperatura, lag.max = 8, na.action = na.pass))

correl11 <- with(filter(nuqui,
                       ano >= 1994, ano <= 2005),
                ccf(oni, precipitacion, lag.max = 8, na.action = na.pass))

correl12 <- with(filter(nuqui,
                        ano >= 1994, ano <= 2005),
                 ccf(oni, rango_temp, lag.max = 8, na.action = na.pass))

correl13 <- with(filter(nuqui,
                        ano >= 1994, ano <= 2005),
                 ccf(temperatura, rango_temp, lag.max = 8, na.action = na.pass))

correl14 <- with(filter(nuqui,
                        ano >= 1994, ano <= 2005),
                 ccf(precipitacion, rango_temp, lag.max = 8, na.action = na.pass))

correlations <- data.frame(correl1$lag, correl1$acf, correl2$acf, 
                           correl3$acf, correl7$acf, correl8$acf,
                           correl4$acf, correl6$acf, correl5$acf, 
                           correl9$acf)

write.csv(correlations, file = "Correlations.csv")
