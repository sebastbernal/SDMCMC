# Clear objects -------
rm(list = ls())
graphics.off() 

#Load packages---------
library (pomp)
library (plyr)

# setwd("E:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2") #blackhawck
# setwd("X:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2") #betty

#Load POMP object
source("pomp_object_VS2EI2_Nov24_Sep96_Dic03.R")

# Cluster input --------------------------------------
indexCluster <- as.numeric(1:100) #as.numeric(Sys.getenv("PBS_ARRAYID"))
seqP <- seq(from = 1, to = 10000, by = 100)
now.num <- seqP[indexCluster]

#Define params for POMP ---------------------
y <- read.csv("mifOutput_S1_VS2EI2_Nov24_Sep96_Dic03.csv")
params <- as.numeric(y[1,]) #1 se cambia por now.num cuando se manda al cluster.
param.names <- colnames(y)
names(params) <- param.names

#MIF -------------------------------------------------------------------------------------------------------
#Read grid. cont es el contador de la linea.
#5 cambiar por le tamano de filas que tengo generado por el LatinHC
for(cont in now.num:(now.num+99)){ #for(cont in now.num:(now.num+99)) cuando se manda al cluster.
  for(i in 1:3){ #for(i in 1:3) cuando se manda al cluster.
    param <- as.numeric(y[cont,])
    names(param) <- param.names
    cat(cont, i, "\n") ## print look id
    
    #iterated filtering
    tryCatch(mif2(
      po,
      Nmif = 150, #Change between 50 to 150
      start = param,
      rw.sd = rw.sd(sigOBS=0.03, sigPRO=0.03, 
                    muEI1=0.003, muI1I2=0.01, muI1S1=0.01, muS2S1=0.03,
                    rho=0.03, bT=0.03, q=0.03, c=0.03, 
                    S1_0=0.03, S2_0=0.03, E_0=0.03, I1_0=0.03, I2_0=0.03, K_0=0.03, F_0=0.03),
      Np = 10000, #change between 1000 to 15000,
      cooling.type = "hyperbolic",
      cooling.fraction.50 = 0.5,
      transform=TRUE), error = function(e) e) -> mifout
    
    #IF para seguir los loops si MIF no cae en el error. Si es cero hay error.
    if(length(coef(mifout)) > 0){
      loglik.mif <- replicate(n=5,logLik(pfilter(po,
                    params=coef(mifout),Np=1000,max.fail=500)))
      bl <- logmeanexp(loglik.mif,se=TRUE)
      loglik.mif.est <- bl[1]
      loglik.mif.se <- bl[2]
      cat(cont, loglik.mif.est, loglik.mif.se, "\n") #print
    }
    
    #Save Output -------
    if(is.finite(loglik.mif.est)){ #if cuando hay valores que son numeros imprimir MIF
      par.out <- coef(mifout) #funcion que me da la informacion de los parametros
      names(par.out) <- param.names
      if(file.exists("mifOutput_S2_VS2EI2_Nov24_Sep96_Dic03.csv")){
        write.table(t(as.matrix(c(cont,par.out,loglik.mif.est,loglik.mif.se))), 
                    "mifOutput_S2_VS2EI2_Nov24_Sep96_Dic03.csv", append = T, 
                    col.names=F, row.names=F, sep = ",")
      } else{ 
        write.table(t(as.matrix(c(cont,par.out,loglik.mif.est,loglik.mif.se))), 
                    "mifOutput_S2_VS2EI2_Nov24_Sep96_Dic03.csv", append = T, 
                    col.names=c("run", param.names, "loglik", "loglik.se"), row.names=F, sep = ",") 
      }
    }
  }
}
