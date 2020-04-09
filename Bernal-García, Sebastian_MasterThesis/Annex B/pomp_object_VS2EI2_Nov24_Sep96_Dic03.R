rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows

######### Load packages ################
library(pomp)
library(magrittr)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
library(foreach)
library(tgp)
stopifnot(packageVersion("pomp") >= "1.7")

############ Read Data ########
setwd("E:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2 NEW/Sep96_Dic03") #blackhawck
#setwd("X:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2 NEW/Sep96_Dic03") #betty

dat <- read.csv("E:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2 NEW/Sep96_Dic03/covariates3.csv",sep=",") #blackhawck
#dat <- read.csv("x:/Dropbox/¡¡¡THESIS!!!/POMP/VS2EI2 NEW/Sep96_Dic03/covariates3.csv",sep=",") #betty
#dat <- read.csv("covariates3.csv",sep=",") #cluster
#dat[-(73:136),] ->dat #Simulate from January 1994 to December 1999. Exclude from 2000 to 2005
dat <- dat[33:120,] #Simulate from September 1996 to December 2003.

## Convert Date column (in the data) to R internal Date format ########
dates <- as.Date(dat$Date,format='%d/%m/%Y')
dat <- mutate(dat, year = as.numeric(format(dates, format = "%Y")),
              month = as.numeric(format(dates, format = "%m")),
              day = as.numeric(format(dates, format = "%d")),
              time = year+month/12)

## normalization of the temperature covariate
dat$temperaturen <-  (dat$temperature - mean(dat$temperature)) / sd(dat$temperature)

###### Smooth the popsize to get population size and its derivative, dpopdt. ?I don't get this! #######
# x <- na.omit(dat2) 
# spl <- smooth.spline(dat2$time,dat2$population) 
# pop=predict(spl,deriv=0,x=dat$time)$y
# dpopdt=predict(spl,deriv=1,x=dat$time)$y
# 
# dat2 <- mutate(dat, pop = pop, dpopdt = dpopdt)

############ Define the POMP components ############
# Define the process model. ---------------------------------------------
simul <- "
// compute transmission rate 
double betaIN = exp(bT*temperaturen); //Waiting for Mercedes advice.

// gamma white noise
double dW = rgammawn(sigPRO,dt);

// force of infection
double foi = (((I1+q*I2)/population)*betaIN)*dW/dt;

// compute transition numbers
double dBS1 = (delta*population+dpop)*dt; //I don't understand this Eq.
double dS1E = F*S1*dt;
double dEI1 = muEI1*E*dt;
double dI1S1 = muI1S1*I1*dt;
double dI1I2 = muI1I2*I1*dt;
double dI2S2 = muI2S2*I2*dt;
double dS2I2 = c*F*S2*dt;
double dS2S1 = muS2S1*S2*dt;

double dS1D = delta*S1*dt;
double dED = delta*E*dt;
double dI1D = delta*I1*dt;
double dI2D = delta*I2*dt;
double dS2D = delta*S2*dt;

double dKK = ((foi-K)/(tau/2.0))*dt;
double dFF = ((K-F)/(tau/2.0))*dt;

// compute equations
S1 += dBS1 + dS2S1 + dI1S1- dS1E - dS1D;
E += dS1E - dEI1 - dED;
I1 += dEI1 - dI1S1 - dI1I2 - dI1D;
S2 += dI2S2 - dS2I2 - dS2D;
I2 += dI1I2 + dS2I2 - dI2S2 - dI2D;
K += dKK;
F += dFF;
H += rho*dEI1;
W += (dW-dt)/sigPRO;

// track errors
if (S1 < 0.0) { err -= S1; S1=0.0; }
if (E < 0.0) { err -= E; E=0.0; }
if (I1 < 0.0) { err -= I1; I1=0.0; }
if (I2 < 0.0) { err -= I2; I2=0.0; }
if (S2 < 0.0) { err -= S2; S2=0.0; }
if (K < 0.0) { err -= K; K=0.0; }
if (F < 0.0) { err -= F; F=0.0; }
"

# Define the rmeasure. ----------
rmeas <- "
double size = 1.0/sigOBS/sigOBS;
cases = rnbinom_mu(size,H);
"

# Define the dmeasure. ----------------
dmeas <- "
double size = 1.0/sigOBS/sigOBS;
static double tol = 1.0e-18;
lik = dnbinom_mu(cases,size,H+tol,0)+tol;
if (give_log) lik = log(lik);
"

# Initialize ---------------------------------------------------------
initlz <- Csnippet("
                   double m = population/(S1_0+E_0+I1_0+I2_0+S2_0); //I don't understand this Eq.
                   S1 = nearbyint(m*S1_0); //I don't understand this Eq.
                   E = nearbyint(m*E_0);
                   I1 = nearbyint(m*I1_0);
                   I2 = nearbyint(m*I2_0);
                   S2 = nearbyint(m*S2_0);
                   K = K_0;
                   F = F_0;
                   H = 0;
                   W = 0;
                   err = 0;
                   ")

# Transmform parameters. -------------------------
fromEst <- Csnippet("
                    TsigOBS = exp(sigOBS);
                    TsigPRO = exp(sigPRO);
                    TmuEI1 = exp(muEI1);
                    TmuI1S1 = exp(muI1S1);
                    TmuI1I2 = exp(muI1I2);
                    TmuI2S2 = exp(muI2S2);
                    TmuS2S1 = exp(muS2S1);
                    Trho = expit(rho);
                    Tq = expit(q);
                    Tc = expit(c);
                    Tdelta = exp(delta);
                    TS1_0 = exp(S1_0);
                    TS2_0 = exp(S2_0);
                    TE_0 = exp(E_0);
                    TI1_0 = exp(I1_0);
                    TI2_0 = exp(I2_0);
                    TK_0 = exp(K_0);
                    TF_0 = exp(F_0);
                    double sum = TS1_0+TE_0+TI1_0+TI2_0+TS2_0; //I don't understand this Eq.
                    TS1_0 /= sum; //I don't understand this Eq.
                    TE_0 /= sum;
                    TI1_0 /= sum;
                    TI2_0 /= sum;
                    TS2_0 /= sum;
                    ")

# Untransmform parameters. ---------------------------
toEst <- Csnippet("
                  TsigOBS = log(sigOBS);
                  TsigPRO = log(sigPRO);
                  TmuEI1 = log(muEI1);
                  TmuI1S1 = log(muI1S1);
                  TmuI1I2 = log(muI1I2);
                  TmuI2S2 = log(muI2S2);
                  TmuS2S1 = log(muS2S1);
                  TbT = log(bT);
                  Trho = logit(rho);
                  Tq = logit(q);
                  Tc = logit(c);
                  Tdelta = log(delta);
                  TS1_0 = log(S1_0);
                  TS2_0 = log(S2_0);
                  TE_0 = log(E_0);
                  TI1_0 = log(I1_0);
                  TI2_0 = log(I2_0);
                  TK_0 = log(K_0);
                  TF_0 = log(F_0);
                  double sum = S1_0+E_0+I1_0+I2_0+S2_0; //I don't understand this Eq.
                  TS1_0 = log(S1_0/sum); //I don't understand this Eq.
                  TE_0 = log(E_0/sum);
                  TI1_0 = log(I1_0/sum);
                  TI2_0 = log(I2_0/sum);
                  TS2_0 = log(S2_0/sum);
                  ")

#Define params for POMP ---------------------------------
y <- read.csv("mifInput_S1_VS2EI2_Nov20.csv")
params <- as.numeric(y[1,])
names(params) <- colnames(y)

# Define the POMP object. ---------------------------------------------------------------------
po <- pomp(
  data=subset(dat,select=c('time','cases')),     
  times='time',
  t0=2*dat$time[1]-dat$time[2],
  covar=subset(dat,select=c('time','population','dpop','tau','temperaturen')),
  rprocess=euler.sim(Csnippet(simul),delta.t=2/365),
  rmeasure = Csnippet(rmeas),
  dmeasure = Csnippet(dmeas),
  toEstimationScale=toEst,
  fromEstimationScale=fromEst,
  initializer=initlz,
  tcovar = 'time',
  zeronames = c('H','W','err'),
  statenames = c('S1','E','I1','S2','I2','K','F','H','err','W'),
  params = params,
  paramnames = names(params)
  #cdir="/home/sbernal/malariaColombia/"
)
