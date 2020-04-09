#Original File: POMP_mosq_v4_Search1_Nov17.R

# Clear objects -------
rm(list = ls())
graphics.off() 

#setwd("E:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2") #blackhawck
setwd("X:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2 New/Sep96_Dic03") #betty

#Load POMP object
source("pomp_object_VS2EI2_Nov22_Sep96_Dic03.R")

#Output ---------------------------------------------------------------
output <- read.csv("mifOutput_S1_VS2EI2_Nov24_Sep96_Dic03.csv")
output <- arrange(output, -loglik)
#plot(output)

param <- as.numeric(output[1,2:21]) ## param from the best likelihood
names(param) <- colnames(output)[2:21]

#Simular------------------------------------------------------------------------------
simT <- NULL
for(j in 1:1000){ #milsimulaciones con los mismos parametros cambiando el ruido.
  sim.test <- simulate(po,params=param,nsim=1) ## pomp function
  simT <- cbind(simT, data.frame(sim.test)[,'cases']) #junta los resultados de las simulaciones
}

#Plot ------------------------------------------------------------------------------------------------
dat$mean <- apply(simT, 1, function(x){mean(x, na.rm = T)}) #la funcion apply lee por fila y me calcula mean, mediam, quanitle ...
dat$median <- apply(simT, 1, function(x){median(x, na.rm = T)})
dat$qtLow <- apply(simT, 1, function(x){quantile(x,probs = 0.1, na.rm = T)}) #quantil 10 para estimar CI
dat$qtHig <- apply(simT, 1, function(x){quantile(x,probs = 0.9, na.rm = T)}) #quantil 90 para estimar CI

require(ggplot2)
ggplot() +   
  geom_line(data = dat, aes(x = time, y = cases)) + 
  geom_line(data = dat, aes(x = time, y = median), colour="red")  +
  geom_ribbon(data = dat, aes(x = time, ymax=qtHig, ymin=qtLow, group = 1), alpha =0.35) +
  labs(x = "Month",
       y = "Cases [human/month]") +
  theme_classic()
