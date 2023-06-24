# INC2 code by Michelle and Caitlin for 2023 April

library(tidyverse)
library(SoilR)
library(FME)

# Read in data
Cinits <- c(1199.218125, 1198.987771, 1202.890795, 1208.769544, 1280.327308, 869.183259, 871.99067, 907.076619, 880.866037, 540.873971, 535.095807) # these numbers reflect if I average C per treatment, Information from INC3 -> CombinedIRMS -> Treatment_Calculations
treatment_names <- c('DASE_C', 'DASE_O', 'AD_S', 'POET_S', 'NREL_S', 'AD_N', 'POET_N', 'NREL_N', 'CS_N', 'GWC16', 'GWC20')
inputs_frame = 0

CO2flux_0 <- read.csv("INC2data_mod.csv", header=TRUE) 

i=1 # treatment
n=2 # saving number

# DASE O/C combined, so run this code and don't run it in a loop just run the #1 treatment
i = 1 # 
Cinits[1] <- (1199.218125+1198.987771)/2 # just averaged DASE together
CO2flux_0 <- read.csv("DASEcomb_INC2data_mod.csv", header=TRUE) # in Excel, I averaged DASE_C and DASE_O together and then just deleted Num = 2, calling the average Num = 1 so 2 is missing now

#Sample key as follows:
# C/O means closed/open valve
#'DASE_C' = 1 standard dosage
#'DASE_O' = 2 standard dosage
#'AD_S' = 3 standard dosage
#'POET_S' = 4 standard dosage
#'NREL_S' = 5 standard dosage
#'AD_N' = 6 new ie. halved dosage
#'POET_N' = 7 new dosage
#'NREL_N' = 8 new dosage 
#'CS_N' = 9 new dosage
#'GWC16' = 10 PALOUSE SOIL CONTROL 1
#'GWC20' = 11 PALOUSE SOIL CONTROL 2

# init saving stuffs

# AICc
num_treatments = 11
AICc_1p_tot <- numeric(length=num_treatments)
AICc_2ps_tot <- numeric(length=num_treatments)
AICc_2pp_tot <- numeric(length=num_treatments)
AICc_3pp_tot <- numeric(length=num_treatments)
#AICc_3pp_fixed_tot <- numeric(length=num_treatments)

# R
R_1p_tot <-  numeric(length=num_treatments)
R_2ps_tot <-  numeric(length=num_treatments)
R_2pp_tot <-  numeric(length=num_treatments)
R_3pp_tot <-  numeric(length=num_treatments)
#R_3pp_fixed_tot <-  numeric(length=num_treatments)

# parameters
onep_par <- list(length = num_treatments)
twops_par <- list(length = num_treatments)
twopp_par <- list(length = num_treatments)
threepp_par <- list(length = num_treatments)
#threepp_fixed_par <- list(length = num_treatments)

# short term projections w/in incubation, to graph
days=seq(0,135) #Incubation days
short_totalfitCumm <- as.data.frame(matrix(nrow = length(days), ncol = num_treatments*3+1))
short_totalfitCumm[, 1] <- days
colnames(short_totalfitCumm)[1] <- 'days'

# longterm projections
proj_days = seq(1,to= 36500, by = 365/5)
totalfitCumm <- as.data.frame(matrix(nrow = length(proj_days), ncol = num_treatments*3+1))
totalfitCumm[, 1] <- proj_days 
colnames(totalfitCumm)[1] <- 'days'

# Inputs every end of year, 99 inputs in dataframe, this only works for inputs w/ time steps of 365/5 days
# inputs_vals <- 1000*c(0.664092011,  # from CombinedIRMS -> Treatment Calculations in INC2fka3
#                   0.664224585,
#                   0.668063965,
#                   0.674809499,
#                   0.746066085,
#                   0.333617312,
#                   0.337195283,
#                   0.372710457,
#                   0.345811145, 
#                   0,
#                   0)	 # these numbers reflect if I average the residues in each treatment, Information from INC2 -> IRMS -> "IRMS_summary" -> IRMS_Pre  
# 
# 
# inputs_mainframe <- data.frame(proj_days, matrix(0, length(proj_days), length(treatment_names)))
# colnames(inputs_mainframe) <- c('days', 'DASE_C', 'DASE_O', 'AD_S', 'POET_S', 'NREL_S', 'AD_N', 'POET_N', 'NREL_N', 'CS_N', 'GWC16', 'GWC20')
# a = 2 # column counter
# b = 1 # inputs_vals counter
# while (a < 2+length(treatment_names)) {
#   inputs_mainframe[seq(from = 6, to = length(proj_days), by = 5), a] <-  inputs_vals[b] 
#   a = a + 1
#   b = b + 1
# }
# write.csv(inputs_mainframe, file = 'inputframe.csv') 

#while (i < num_treatments+1) { 
  
# begin looping
CO2flux <- CO2flux_0 %>%
  filter(Num == i) %>%    # loop through treatment
  select(time, cummCO2) 

plot(x=CO2flux$time, y=CO2flux$cummCO2)

Ctotal= Cinits[i]

# graphing
theme_C <- theme_light() +
  theme(panel.grid.minor = element_blank(),
        #text = element_text(size = 30), #for facetwrapped plots
        strip.background = element_rect(color="black", fill="#93C5FF", size=1.5, linetype="solid"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# One pool model
eCO2func = function(pars) {
  mod=OnepModel(
    t=days,
    k = pars[1], # GUESSES K1
    C0 = Ctotal,
    In = inputs_frame,
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=days,cummCO2=rowSums(AccR)))
}

#cost function
eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=CO2flux[,1:2]))
}

inipars=c(k=.0001)  # for Palouse soil control should ~= .0006

# fit model to data
eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf),lower=c(0))

onep_par[[i]] <- eCO2fit$par 

# rerun model w/ best parameter set for short term
fitmod=OnepModel(t=days, k=eCO2fit$par,
                 In = inputs_frame,
                 C0=Ctotal)

fitCumm=getAccumulatedRelease(fitmod)

short_totalfitCumm[, n] <- rowSums(fitCumm)
colnames(short_totalfitCumm)[n] <- '1P'


# plot short-term incubation v. model 
fitCumm1 <- rowSums(fitCumm)
fitframe <- data.frame(days, fitCumm1)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe, aes(x = days, y = fitCumm1)) +  # model data
  xlim(0, 135) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '1 Pool Model') +
  theme_C
plot1

# save AICc and npars
npars=length(eCO2fit$par)
AIC_1p=(2*npars)-2*log(eCO2fit$ms)
AICc_1p=AIC_1p+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

#pseudo r-squared
fitmod=OnepModel(t=CO2flux$time, k=eCO2fit$par,
                 In = inputs_frame,
                 C0=Ctotal)

CO2flux$fitCumm1p<-rowSums(getAccumulatedRelease(fitmod))

plot(CO2flux$cummCO2, CO2flux$fitCumm1p)+abline(coef = c(0,1))
test<-summary(lm(cummCO2~fitCumm1p, data=CO2flux))
R_1p<-test$r.squared

# RERUN FOR LONG TERM
fitmod=OnepModel(t=proj_days, k=eCO2fit$par,
                 In = inputs_frame,
                 C0=Ctotal)

fitCumm=getAccumulatedRelease(fitmod)

totalfitCumm[, n] <- rowSums(fitCumm)
colnames(totalfitCumm)[n] <- '1P'

# LONG TERM
fitCumm2 <- rowSums(fitCumm)
fitframe2 <- data.frame(proj_days, fitCumm2)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe2, aes(x = proj_days, y = fitCumm2)) +  # model data
  xlim(0, 36500) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '1 Pool Model') +
  theme_C
plot1

n <- n + 1

#two pool series
eCO2func=function(pars){
  mod=TwopSeriesModel(
    t=days,
    ks=pars[1:2],
    a21=pars[3]*pars[1],
    C0=Ctotal*c(pars[4],1-pars[4]),
    In=0,
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=days,cummCO2=rowSums(AccR)))
}

#cost function
eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=CO2flux[,1:2]))
}

inipars=c(k1=0.5,k2=0.05,alpha21=0.5,gamma=0.5)

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,Inf,1,1),lower=c(0,0,0,0))
options(scipen = 999)
twops_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
fitmod=TwopSeriesModel(t=days, ks=eCO2fit$par[1:2],
                       a21=eCO2fit$par[3]*eCO2fit$par[1],
                       C0=Ctotal*c(eCO2fit$par[4],1-eCO2fit$par[4]),
                       In=0)
fitCumm=getAccumulatedRelease(fitmod)

short_totalfitCumm[, n] <- rowSums(fitCumm)
colnames(short_totalfitCumm)[n] <- '2PS'

#Plot the results
fitCumm1 <- rowSums(fitCumm)
fitframe <- data.frame(days, fitCumm1)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe, aes(x = days, y = fitCumm1)) +  # model data
  xlim(0, 135) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Series Model') +
  theme_C
plot1

npars=length(eCO2fit$par)
AIC_2ps=(2*npars)-2*log(eCO2fit$ms)
AICc_2ps=AIC_2ps+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

#pseudo r-squared
fitmod=TwopSeriesModel(t=CO2flux$time, ks=eCO2fit$par[1:2],
                       a21=eCO2fit$par[3]*eCO2fit$par[1],
                       C0=Ctotal*c(eCO2fit$par[4],1-eCO2fit$par[4]),
                       In=0)

CO2flux$fitCumm2ps=rowSums(getAccumulatedRelease(fitmod))

plot(CO2flux$cummCO2, CO2flux$fitCumm2ps)+abline(coef = c(0,1))
test<-summary(lm(cummCO2~fitCumm2ps, data=CO2flux))
R_2ps<-test$r.squared

# RERUN FOR LONG TERM
fitmod=TwopSeriesModel(t=proj_days, ks=eCO2fit$par[1:2],
                       a21=eCO2fit$par[3]*eCO2fit$par[1],
                       C0=Ctotal*c(eCO2fit$par[4],1-eCO2fit$par[4]),
                       In=0)
fitCumm=getAccumulatedRelease(fitmod)

totalfitCumm[, n] <- rowSums(fitCumm)
colnames(totalfitCumm)[n] <- '2PS'

# LONG TERM
fitCumm2 <- rowSums(fitCumm)
fitframe2 <- data.frame(proj_days, fitCumm2)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe2, aes(x = proj_days, y = fitCumm2)) +  # model data
  xlim(0, 36500) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Series Model') +
  theme_C
plot1

n <- n + 1

#two pool parallel model
eCO2func=function(pars){
  mod=TwopParallelModel(
    t=days,
    ks=pars[1:2],
    gam=pars[3],
    C0=Ctotal*c(pars[3],1-pars[3]), 
    In=0,
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=days,cummCO2=rowSums(AccR)))
}

eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=CO2flux[,1:2]))
}

inipars=c(k1=0.05,k2=0.000000005,gamma=0.08) #for deeper depths, need different starting values

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,Inf,1),lower=c(0,0,0))

twopp_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
fitmod=TwopParallelModel(t=days, ks=eCO2fit$par[1:2], 
                         gam=eCO2fit$par[3],
                         C0=Ctotal*c(eCO2fit$par[3],1-eCO2fit$par[3]), 
                         In=0)
fitCumm=getAccumulatedRelease(fitmod)

short_totalfitCumm[, n] <- rowSums(fitCumm)
colnames(short_totalfitCumm)[n] <- '2PP'

#Plot the results
fitCumm1 <- rowSums(fitCumm)
fitframe <- data.frame(days, fitCumm1)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe, aes(x = days, y = fitCumm1)) +  # model data
  xlim(0, 135) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Parallel Model') +
  theme_C
plot1

npars=length(eCO2fit$par)
AIC_2pp=(2*npars)-2*log(eCO2fit$ms)
AICc_2pp=AIC_2pp+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

#pseudo r-squared
fitmod=TwopParallelModel(t=CO2flux$time, ks=eCO2fit$par[1:2], 
                         gam=eCO2fit$par[3],
                         C0=Ctotal*c(eCO2fit$par[3],1-eCO2fit$par[3]), 
                         In=0)

CO2flux$fitCumm2pp=rowSums(getAccumulatedRelease(fitmod))

plot(CO2flux$cummCO2, CO2flux$fitCumm2pp)+abline(coef = c(0,1))
test<-summary(lm(cummCO2~fitCumm2pp, data=CO2flux))
R_2pp<-test$r.squared

# LONG TERM: RERUN MODEL TO PREDICT LONG TERM
fitmod=TwopParallelModel(t=proj_days, ks=eCO2fit$par[1:2], 
                         gam=eCO2fit$par[3],
                         C0=Ctotal*c(eCO2fit$par[3],1-eCO2fit$par[3]), 
                         In=0)
fitCumm=getAccumulatedRelease(fitmod)

totalfitCumm[, n] <- rowSums(fitCumm)
colnames(totalfitCumm)[n] <- '2PP'

# LONG TERM: PLOT
fitCumm2 <- rowSums(fitCumm)
fitframe2 <- data.frame(proj_days, fitCumm2)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe2, aes(x = proj_days, y = fitCumm2)) +  # model data
  xlim(0, 36500) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Parallel Model') +
  theme_C
plot1

n <- n + 1

#three pool parallel
eCO2func=function(pars){
  mod=ThreepParallelModel(
    t=days,
    ks=pars[1:3],
    gam1=pars[4],
    gam2=pars[5],
    C0=Ctotal*c(pars[4],pars[5],1-pars[4]-pars[5]), 
    In=0,
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=days,cummCO2=rowSums(AccR)))
}

eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=CO2flux[,1:2]))
}

inipars=c(k1=0.005,k2=0.00005,k3=0.000000005,gam1=0.01, gam2=0.1) #for deeper depths, need different starting values

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,Inf,Inf,1,1),lower=c(0,0,0,0,0))

threepp_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
fitmod=ThreepParallelModel(t=days, ks=eCO2fit$par[1:3], 
                           gam1=eCO2fit$par[4],
                           gam2=eCO2fit$par[5],
                           C0=Ctotal*c(eCO2fit$par[4],eCO2fit$par[5],1-eCO2fit$par[4]-eCO2fit$par[5]), 
                           In=0)
fitCumm=getAccumulatedRelease(fitmod)

short_totalfitCumm[, n] <- rowSums(fitCumm)
colnames(short_totalfitCumm)[n] <- '3PP'

#Plot the results
plot(CO2flux[,1:2],type="p",xlab="Days",
     ylab="Cummulative respiration (mg C g-1 soil)")
lines(rowSums(fitCumm))

fitCumm1 <- rowSums(fitCumm)
fitframe <- data.frame(days, fitCumm1)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe, aes(x = days, y = fitCumm1)) +  # model data
  xlim(0, 135) +
  #ylim(0, 40) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '3 Pool Model') +
  theme_C
plot1

npars=length(eCO2fit$par)
AIC_3pp=(2*npars)-2*log(eCO2fit$ms)
AICc_3pp=AIC_3pp+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

#pseudo r-squared
fitmod=ThreepParallelModel(t=CO2flux$time, ks=eCO2fit$par[1:3], 
                           gam1=eCO2fit$par[4],
                           gam2=eCO2fit$par[5],
                           C0=Ctotal*c(eCO2fit$par[4],eCO2fit$par[5],1-eCO2fit$par[4]-eCO2fit$par[5]), 
                           In=0)

CO2flux$fitCumm3pp=rowSums(getAccumulatedRelease(fitmod))

plot(CO2flux$cummCO2, CO2flux$fitCumm3pp)+abline(coef = c(0,1))
test<-summary(lm(cummCO2~fitCumm3pp, data=CO2flux))
R_3pp<-test$r.squared

# LONG TERM: Run the model again with best parameter set
fitmod=ThreepParallelModel(t=proj_days, ks=eCO2fit$par[1:3], 
                           gam1=eCO2fit$par[4],
                           gam2=eCO2fit$par[5],
                           C0=Ctotal*c(eCO2fit$par[4],eCO2fit$par[5],1-eCO2fit$par[4]-eCO2fit$par[5]), 
                           In=0)
fitCumm=getAccumulatedRelease(fitmod)

totalfitCumm[, n] <- rowSums(fitCumm)
colnames(totalfitCumm)[n] <- '3PP'

# LONG TERM: PLOT
fitCumm2 <- rowSums(fitCumm)
fitframe2 <- data.frame(proj_days, fitCumm2)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe2, aes(x = proj_days, y = fitCumm2)) +  # model data
  xlim(0, 36500) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '3 Pool Model') +
  theme_C
plot1

n <- n + 1

# ## THREE POOL PARALLLEL FIXED
# 
# # ks from 2pp for treatment = 10
# k_soil1=0.00844858563460
# k_soil2=0.00000000368253
# 
# # output from 2pp modelling of treatment = 10, soil 
# #               k1               k2            gamma 
# #  0.00844858563460 0.00000000368253 0.10493249963063 
#   
# #three pool parallel
# eCO2func=function(pars){
#   mod=ThreepParallelModel(
#     t=days,
#     #ks=pars[1:3],
#     ks=c(k_soil1, k_soil2, pars[1]),
#     gam1=pars[2], # if you specify this won't you force the model guess to be what your prev. model says, # you can't define gamma
#     gam2=pars[3],
#     C0=Ctotal*c(pars[2],pars[3],1-pars[2]-pars[3]), 
#     In=0,
#     pass=TRUE
#   )
#   AccR=getAccumulatedRelease(mod)
#   return(data.frame(time=days,cummCO2=rowSums(AccR)))
# }
# 
# eCO2cost=function(pars){
#   modelOutput=eCO2func(pars)
#   return(modCost(model=modelOutput, obs=CO2flux[,1:2]))
# }
# 
# inipars=c(k3=0.000000005,gam1=0.01, gam2=0.1) #for deeper depths, need different starting values
# 
# eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
#                upper=c(Inf,1,1),lower=c(0,0,0))
# 
# threepp_fixed_par[[i]] <- eCO2fit$par
# 
# #Run the model again with best parameter set
# fitmod=ThreepParallelModel(t=days, ks=c(k_soil1, k_soil2, eCO2fit$par[1]), 
#                            gam1=eCO2fit$par[2],
#                            gam2=eCO2fit$par[3],
#                            C0=Ctotal*c(eCO2fit$par[2],eCO2fit$par[3],1-eCO2fit$par[2]-eCO2fit$par[3]), 
#                            In=0)
# fitCumm=getAccumulatedRelease(fitmod)
# 
# short_totalfitCumm[, n] <- rowSums(fitCumm)
# colnames(short_totalfitCumm)[n] <- '3PP_fixed'
# 
# #Plot the results
# plot(CO2flux[,1:2],type="p",xlab="Days",
#      ylab="Cummulative respiration (mg C g-1 soil)")
# lines(rowSums(fitCumm))
# 
# fitCumm1 <- rowSums(fitCumm)
# fitframe <- data.frame(days, fitCumm1)
# 
# plot1 <- ggplot() +
#   geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
#   geom_line(data = fitframe, aes(x = days, y = fitCumm1)) +  # model data
#   xlim(0, 135) +
#   #ylim(0, 40) +
#   labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '3 Pool Model FIXED') +
#   theme_C
# plot1
# 
# npars=length(eCO2fit$par)
# AIC_3pp_fixed=(2*npars)-2*log(eCO2fit$ms)
# AICc_3pp_fixed=AIC_3pp_fixed+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))
# 
# #pseudo r-squared
# fitmod=ThreepParallelModel(t=CO2flux$time, ks=c(k_soil1, k_soil2, eCO2fit$par[1]), 
#                            gam1=eCO2fit$par[2],
#                            gam2=eCO2fit$par[3],
#                            C0=Ctotal*c(eCO2fit$par[2],eCO2fit$par[3],1-eCO2fit$par[2]-eCO2fit$par[3]), 
#                            In=0)
# 
# CO2flux$fitCumm3pp_fixed=rowSums(getAccumulatedRelease(fitmod))
# 
# plot(CO2flux$cummCO2, CO2flux$fitCumm3pp_fixed)+abline(coef = c(0,1))
# test<-summary(lm(cummCO2~fitCumm3pp_fixed, data=CO2flux))
# R_3pp_fixed<-test$r.squared
# 
# #LONG TERM: Run the model again with best parameter set
# fitmod=ThreepParallelModel(t=proj_days, ks=c(k_soil1, k_soil2, eCO2fit$par[1]), 
#                            gam1=eCO2fit$par[2],
#                            gam2=eCO2fit$par[3],
#                            C0=Ctotal*c(eCO2fit$par[2],eCO2fit$par[3],1-eCO2fit$par[2]-eCO2fit$par[3]), 
#                            In=0)
# fitCumm=getAccumulatedRelease(fitmod)
# 
# totalfitCumm[, n] <- rowSums(fitCumm)
# colnames(totalfitCumm)[n] <- '3PP_fixed'
# 
# # LONG TERM: PLOT
# fitCumm2 <- rowSums(fitCumm)
# fitframe2 <- data.frame(proj_days, fitCumm2)
# 
# plot1 <- ggplot() +
#   geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
#   geom_line(data = fitframe2, aes(x = proj_days, y = fitCumm2)) +  # model data
#   xlim(0, 36500) +
#   #ylim(0, 100) +
#   labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '3 Pool Model FIXED') +
#   theme_C
# plot1
# 
# n <- n + 1

# Save outputs INSIDE of loop
AICc_1p_tot[i] <- AICc_1p
AICc_2ps_tot[i] <- AICc_2ps
AICc_2pp_tot[i] <- AICc_2pp
AICc_3pp_tot[i] <- AICc_3pp
#AICc_3pp_fixed_tot[i] <- AICc_3pp_fixed

R_1p_tot[i] <- R_1p
R_2ps_tot[i] <- R_2ps
R_2pp_tot[i] <- R_2pp
R_3pp_tot[i] <- R_3pp
#R_3pp_fixed_tot[i] <- R_3pp_fixed

i = i+1
print(i)
print(n)

}


# Save outputs OUTSIDE of loop
AICc_tot <- data.frame(abs(AICc_1p_tot), abs(AICc_2ps_tot), abs(AICc_2pp_tot), abs(AICc_3pp_tot))
rownames(AICc_tot) <- treatment_names
colnames(AICc_tot) <- c('1P', '2PS', '2PP', '3PP')

R_tot <- data.frame(abs(R_1p_tot), abs(R_2ps_tot), abs(R_2pp_tot), abs(R_3pp_tot))
rownames(R_tot) <- treatment_names
colnames(R_tot) <- c('1P', '2PS', '2PP', '3PP')

write.csv(AICc_tot, file = 'DASEavg_INC2_365by5_AICc_tot.csv') 
write.csv(R_tot, file = 'DASEavg_INC2_365by5_R_tot.csv')

# Export Parameters
write.csv(onep_par, file = 'DASEavg_365by5_onep_par.csv') 
write.csv(twops_par, file = 'DASEavg_365by5_twops_par.csv') 
write.csv(twopp_par, file = 'DASEavg_365by5_twopp_par.csv') 
write.csv(threepp_par, file = 'DASEavg_365by5_threepp_par.csv') 

# Export the cummCO2
write.csv(totalfitCumm, file = 'DASEavg_365by5_INC2_multmodels_projectedcummCO2.csv') 
write.csv(short_totalfitCumm, file = 'DASEavg_short_365by5_INC2_multmodels_projectedcummCO2.csv') 
