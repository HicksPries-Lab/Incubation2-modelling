#### Soil R Models that work for Incubation 2, gitHub version
# Michelle Wang | JAN 2023

# Load packages + functions
library(tidyverse)
library(SoilR)
library(FME)


# Intializations
i = 2 # CHANGE IF INTERESTED, treatment type 
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
n = 2 # start this at 2 for things to save correctly, column index, save to right column in fitFrames

# Initial Soil + Residue input into treatment jars [mg C]
#Cinits <- 1000*c(1.199218125, 1.198987771, 1.202890795, 1.208769544, 1.280327308, 0.869183259, 0.87199067, 0.907076619, 0.880866037, 0.540873971, 0.535095807) # these numbers reflect if I average C per treatment, Information from INC3 -> CombinedIRMS -> Treatment_Calculations
#Cinits <- c(514.4336596, 1145.656643, 1059.347782, 1151.299211, 1188.126723) # these numbers reflect if I average C per treatment, Information from INC2 -> IRMS -> "IRMS_summary" -> IRMS_Pre
Cinits <- c(1199.218125, 1198.987771, 1202.890795, 1208.769544, 1280.327308, 869.183259, 871.99067, 907.076619, 880.866037, 540.873971, 535.095807) # these numbers reflect if I average C per treatment, Information from INC3 -> CombinedIRMS -> Treatment_Calculations

num_treatments = 11
AICc_1p_tot <- numeric(length=num_treatments)
AICc_2pf_tot <- numeric(length=num_treatments)
AICc_2ps_tot <- numeric(length=num_treatments)
AICc_2pp_tot <- numeric(length=num_treatments)

onep_par <- list(length = num_treatments)
twopf_par <- list(length = num_treatments)
twopp_par <- list(length = num_treatments)
twops_par <- list(length = num_treatments)

k_soil = 0.000631349

# gamma_soil <- c(1,	0.446666266,	0.490614741,	0.44600461,	0.437380269)

gamma_soil <- c(0.446229175
                ,0.44601221
                ,0.444617943
                ,0.4417385
                ,0.417284877
                ,0.616171494
                ,0.613304024
                ,0.589108076
                ,0.607419142
                ,1
                ,1) # Information from INC3 -> CombinedIRMS -> Treatment_Calculations

# TIME VECTOR
#days <- 0:3650
end_day <- 365*10  ### CHANGE TIME VECTOR [days] ### 
days <- seq(from = 1, to = end_day, by = 1)
# days <- seq(from = 1, to = end_day, by = 365/5)
#days <- seq(0, round(last(CO2flux$time)))  # this days vector is just the length of your data

## INPUTS
# OPTION 1: 0 inputs throughout year after first one
inputs_frame = 0

# CUMM. CO2 PRODUCED VECTOR, INITIALIZE dataframe
totalfitCumm <- as.data.frame(matrix(nrow = length(days), ncol = num_treatments*3+1))
totalfitCumm[, 1] <- days 

# Graphing theme
theme_C <- theme_light() +
  theme(panel.grid.minor = element_blank(),
        #text = element_text(size = 30), #for facetwrapped plots
        strip.background = element_rect(color="black", fill="#93C5FF", size=1.5, linetype="solid"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Read in data
CO2flux_0 <- read.csv("INC2data_mod.csv", header=TRUE) 

# LOOP THROUGH ALL MODELS
#while (i < num_treatments+1) {    # COMMENT IN/OUT TO CHECK FOR ONE TREATMENT, loop through 5 treatments

CO2flux <- CO2flux_0 %>%
  filter(Num == i) %>%    # loop through treatment
  select(time, cummCO2) 

Ctotal= Cinits[i]        ### initial total C from IRMS (mg C in treatment) ###
#inputs_frame <- inputs_mainframe[, c(1, i+1)] ### CHANGE THIS BY COMMENTING  IN/OUT DEPENDING ON INPUTS OR NOT ### subset main dataframe so it's just the days and relevant treatment

#cost function
eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=CO2flux[,1:2]))#CO2flux[c(1,3,5,7,9,11,12:29),1:2]))#CO2flux[,1:2]))
}

###one pool model####
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

inipars=c(k=.0001)  # for Palouse soil control should ~= .0006

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf),lower=c(0))
options(scipen = 999)
onep_par[[i]] <- eCO2fit$par

fitmod=OnepModel(t=days, k=eCO2fit$par[1],
                 In = inputs_frame,
                 C0=Ctotal)


fitCumm=getAccumulatedRelease(fitmod)
a <- rowSums(fitCumm)

fitCumm1 <- rowSums(fitCumm)
totalfitCumm[, n] <- fitCumm1
n <- n + 1

fitframe1 <- data.frame(days, fitCumm1)

npars=length(eCO2fit$par)
AIC_1p=(2*npars)-2*log(eCO2fit$ms)
AICc_1p=AIC_1p+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

# TEST PLOT
plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe1, aes(x = days, y = fitCumm1)) +  # model data
  xlim(0, 500) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '1 Pool Model') +
  theme_C
plot1

# ###two pool feedback model####
# eCO2func=function(pars){
#   mod=TwopFeedbackModel(
#     t=days,
#     ks=pars[1:2],
#     a21=pars[3]*pars[1],
#     a12=pars[4]*pars[2],
#     C0=Ctotal*c(pars[5],1-pars[5]),
#     In = inputs_frame,
#     pass=TRUE
#   )
#   AccR=getAccumulatedRelease(mod)
#   return(data.frame(time=days,cummCO2=rowSums(AccR)))
# }
#
# inipars=c(k1=0.5,k2=0.05,alpha21=0.5,alpha12=0.1,gamma=0.5)
#
# eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
#                upper=c(Inf,Inf,1,1,1),lower=c(0,0,0,0,0))
# options(scipen = 999)
# twopf_par[[i]] <- eCO2fit$par
#
# #Run the model again with best parameter set
# fitmod=TwopFeedbackModel(t=days, ks=eCO2fit$par[1:2],
#                          a21=eCO2fit$par[3]*eCO2fit$par[1],
#                          a12=eCO2fit$par[4]*eCO2fit$par[2],
#                          C0=Ctotal*c(eCO2fit$par[5],1-eCO2fit$par[5]),
#                          In = inputs_frame,)
# fitCumm=getAccumulatedRelease(fitmod)
#
# #Use AIC to evaluate which model is the best fit (should be lowest AIC)
# npars=length(eCO2fit$par)
# AIC_2pf=(2*npars)-2*log(eCO2fit$ms)
# AICc_2pf =AIC_2pf+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))
#
# # #Plot the results
# # plot(CO2flux[,1:2],type="p",xlab="Days",
# #      ylab="Cummulative respiration (mg C g-1 soil)")
# # lines(rowSums(fitCumm))
#
# fitCumm2 <- rowSums(fitCumm)
# totalfitCumm[, n] <- fitCumm2
# n <- n + 1
# fitframe2 <- data.frame(days, fitCumm2)
#
# ## SO SLOW W THIS ON
# # plot2pf <- ggplot() +
# #   geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
# #   geom_line(data = fitframe2, aes(x = days, y = fitCumm2)) +
# #   labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Feedback Model') +
# #   theme_C
# # plot2pf
# 

#### FOLLOWING 2 CHUNKS OF CODE (2PS, 2PP) GUESS PARAMETERS ASSUMING K1 AND GAMMA ARE FIXED 
###two pool series model####
eCO2func=function(pars){
  mod=TwopSeriesModel(
    t=CO2flux$time, # run this only for the incubation data, this was "days" and it worked prev. till 1/18 caitlin's comments
    ks=c(k_soil,pars[1]),
    a21=pars[2]*k_soil,
    C0=Ctotal*c(gamma_soil[i],1-gamma_soil[i]), # I get this number from my IRMS data. gamma_soil = initial C from soil/(initial C from soil + initial C from residue)
    In = 0, # run this for 0 since that's just the incubation
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=CO2flux$time,cummCO2=rowSums(AccR)))
}

inipars=c(k2=.002, alpha21=0.9) # approx. what INC1 values were if I fix k1


eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,1),lower=c(0,0))
options(scipen = 999)
twops_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
fitmod=TwopSeriesModel(t=days, ks=c(k_soil,eCO2fit$par[1]),
                       a21=eCO2fit$par[2]*k_soil,
                       C0=Ctotal*c(gamma_soil[i],1-gamma_soil[i]),
                       In = inputs_frame)
fitCumm=getAccumulatedRelease(fitmod)

npars=length(eCO2fit$par)
AIC_2ps=(2*npars)-2*log(eCO2fit$ms)
AICc_2ps=AIC_2ps+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

fitCumm3 <- rowSums(fitCumm)
totalfitCumm[, n] <- fitCumm3
n <- n + 1
fitframe3 <- data.frame(days, fitCumm3)

## Cumulative CO2 Released
plot2ps <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
  geom_line(data = fitframe3, aes(x = days, y = fitCumm3)) +
  xlim(0, 1.5*max(CO2flux$time)) +  # to only see relevant model data
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Series Model') +
  theme_C
plot2ps

##two pool parallel model####
eCO2func=function(pars){
  mod=TwopParallelModel(
    t=CO2flux$time, # was days
    ks=c(k_soil, pars[1]), #ONCE YOU HAVE K1
    #ks=c(pars[1],pars[2]),
    gam= gamma_soil[i],
    #C0=Ctotal*c(gamma_soil[i],1-gamma_soil[i]),
    C0=Ctotal*c(gamma_soil[i],1-gamma_soil[i]),
    In = 0, # was inputs_frame
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=CO2flux$time,cummCO2=rowSums(AccR)))
}

#inipars=c(k1=1, k2=0.8, gam = .4) #for deeper depths, need different starting values
inipars=c(k2=0.002) #ONCE YOU HAVE K1 for deeper depths, need different starting values
eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf),lower=c(0))

twopp_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
#fitmod=TwopParallelModel(t=days, ks=c(eCO2fit$par[1],eCO2fit$par[2]),
fitmod=TwopParallelModel(t=days, ks=c(k_soil,eCO2fit$par[1]), # ONCE YOU HAVE K1
                         gam=gamma_soil[i],
                         C0=Ctotal*c(gamma_soil[i],1-gamma_soil[i]), # at some point this was in the code but unclear if it ever worked: C0=Ctotal*c(eCO2fit$par[4],1-eCO2fit$par[4]
                         In = inputs_frame)
fitCumm=getAccumulatedRelease(fitmod)

npars=length(eCO2fit$par)
AIC_2pp=(2*npars)-2*log(eCO2fit$ms)
AICc_2pp=AIC_2pp+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

fitCumm4 <- rowSums(fitCumm)
totalfitCumm[, n] <- fitCumm4
n <- n + 1
fitframe4 <- data.frame(days, fitCumm4)

## TEST PLOT
plot2pp <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe4, aes(x = days, y = fitCumm4)) +  # model data
  xlim(0, 1.5*max(CO2flux$time)) +  # to only see relevant model data
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Parallel Model') +
  theme_C
plot2pp

### FOLLOWING 2 CHUNKS OF CODE (2PS, 2PP)  GUESS ALL PARAMETERS AND DO NOT FIX ANYTHING #########
###two pool series model####
eCO2func=function(pars){
  mod=TwopSeriesModel(
    t=CO2flux$time, # run this only for the incubation data, this was "days" and it worked prev. till 1/18 caitlin's comments
    ks=pars[1:2],
    a21=pars[3]*pars[1],
    C0=Ctotal*c(pars[4],1-pars[4]), 
    In = 0, # run this for 0 since that's the incubation
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=CO2flux$time,cummCO2=rowSums(AccR)))
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
                       In = 30,)
fitCumm=getAccumulatedRelease(fitmod)

# # Try getC to get carbon stocks: ??? why is there a bump in the beginning, is it bc of inputs?
# Ct1=getC(fitmod)
# plot(days, Ct1[,2], type="l", ylab="Carbon stocks (mg)", xlab="Time (days)") 
# lines(days, Ct1[,1], col = 'green')

npars=length(eCO2fit$par)
AIC_2ps=(2*npars)-2*log(eCO2fit$ms) 
AICc_2ps=AIC_2ps+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1)) 

fitCumm3 <- rowSums(fitCumm)
totalfitCumm[, n] <- fitCumm3
n <- n + 1
fitframe3 <- data.frame(days, fitCumm3)

## SO SLOW W THIS ON
plot2ps <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
  geom_line(data = fitframe3, aes(x = days, y = fitCumm3)) +
  xlim(0, 200) +
  ylim(0, 1000) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Series Model') +
  theme_C
plot2ps

###two pool parallel model####
eCO2func=function(pars){
  mod=TwopParallelModel(
    t=CO2flux$time, # was days
    ks=pars[1:2],
    gam=pars[3],
    C0=Ctotal*c(pars[3],1-pars[3]), 
    In = 0, # was inputs_frame
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=CO2flux$time,cummCO2=rowSums(AccR)))
}

inipars=c(k1=0.005,k2=0.000000005,gamma=0.08) #for deeper depths, need different starting values

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,Inf,1),lower=c(0,0,0))

# Caitlin's extra code that checks parameters
# var2 = eCO2fit$var_ms_unweighted    # saves parameters
# fitmcmcF1 = modMCMC(f=eCO2cost, p=eCO2fit$par, niter=500, ntrydr = 5,     # adjusts parameters
#                     updatecov = 50, var0=var2, upper=c(Inf,Inf,1),lower=c(0,0,0))#upper=c(3,rep(1,5)), lower=rep(0,6))
# 
# fitmod=TwopParallelModel(t=days, ks=fitmcmcF1$par[1,1:2],   # fits model w new parameters
#                          gam=fitmcmcF1$par[1,3],
#                          C0=Ctotal*c(fitmcmcF1$par[1,3],1-fitmcmcF1$par[1,3]),
#                          In = inputs_frame,)
# fitCumm=getAccumulatedRelease(fitmod)   # outputs what you're interested in

twopp_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
fitmod=TwopParallelModel(t=days, ks=eCO2fit$par[1:2],
                         gam=eCO2fit$par[3],
                         C0=Ctotal*c(eCO2fit$par[3],1-eCO2fit$par[3]),
                         In = inputs_frame,)
fitCumm=getAccumulatedRelease(fitmod)

npars=length(eCO2fit$par)
AIC_2pp=(2*npars)-2*log(eCO2fit$ms) 
AICc_2pp=AIC_2pp+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1)) 

#Plot the results
# plot(CO2flux[,1:2],type="p",xlab="Days",
#      ylab="Cummulative respiration (mg C g-1 soil)")
# lines(rowSums(fitCumm))

fitCumm4 <- rowSums(fitCumm)
totalfitCumm[, n] <- fitCumm4
n <- n + 1
fitframe4 <- data.frame(days, fitCumm4)

## SO SLOW W THIS ON
plot2pp <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
  geom_line(data = fitframe4, aes(x = days, y = fitCumm4)) +
  xlim(0, 200) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Parallel Model') +
  theme_C
plot2pp
### END GUESSING CODE #########

####### SAVE DATA
## AIC 
AICc_1p_tot[i] <- AICc_1p
# AICc_2pf_tot[i] <- AICc_2pf
AICc_2ps_tot[i] <- AICc_2ps
AICc_2pp_tot[i] <- AICc_2pp

#totalfitCumm <- cbind(days, fitCumm1, fitCumm2, fitCumm3, fitCumm4) 

##### LOOPING LOGISTICS
i <- i+1
print(i)
print(n)
}

# Export AICc
#AICc_tot <- data.frame(abs(AICc_1p_tot))
# AICc_tot <- data.frame(abs(AICc_2pf_tot), abs(AICc_2pp_tot), abs(AICc_2ps_tot))
AICc_tot <- data.frame(abs(AICc_1p_tot), abs(AICc_2pp_tot), abs(AICc_2ps_tot))
rownames(AICc_tot) <- c('DASE_C', 'DASE_O', 'AD_S', 'POET_S', 'NREL_S', 'AD_N', 'POET_N', 'NREL_N', 'CS_N', 'GWC16', 'GWC20')

# colnames(AICc_tot) <- c('2PF', '2PP', '2PS')
colnames(AICc_tot) <- c('1P', '2PP', '2PS')
write.csv(AICc_tot, file = 'INC2_AICc_tot.csv') ### CHANGE V/P ###

# Export Parameters
write.csv(onep_par, file = 'onep_INC2.csv') ### CHANGE V/P ###
# write.csv(twopf_par, file = 'twopf_parV.csv') ### CHANGE V/P ###
write.csv(twopp_par, file = 'INC2_twopp_fixedkgam.csv') ### CHANGE V/P ###
write.csv(twops_par, file = 'INC2_twops_fixedkgam.csv') ### CHANGE V/P ###

# Export the cummCO2
write.csv(totalfitCumm, file = 'INC2_FIXEDKGAME_projectedcummCO2.csv') ### CHANGE V/P ###





#three pool
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
  return(modCost(model=modelOutput, obs=CO2flux))
}

inipars=c(k1=0.005,k2=0.00005,k3=0.000000005,gam1=0.01, gam2=0.1) #for deeper depths, need different starting values

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,Inf,Inf,1,1),lower=c(0,0,0,0,0))

eCO2fit$par

#Run the model again with best parameter set
fitmod=ThreepParallelModel(t=days, ks=eCO2fit$par[1:3], 
                           gam1=eCO2fit$par[4],
                           gam2=eCO2fit$par[5],
                           C0=Ctotal*c(eCO2fit$par[4],eCO2fit$par[5],1-eCO2fit$par[4]-eCO2fit$par[5]), 
                           In=0)
fitCumm=getAccumulatedRelease(fitmod)

#Plot the results
plot(CO2flux[,1:2],type="p",xlab="Days",
     ylab="Cummulative respiration (mg C g-1 soil)")
lines(rowSums(fitCumm))

fitCumm1 <- rowSums(fitCumm)
fitframe <- data.frame(days, fitCumm1)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe, aes(x = days, y = fitCumm1)) +  # model data
  xlim(0, 100) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '3 Pool Model') +
  theme_C
plot1

# for treatment 1 what this says is that it only releases 150 out of 1200??? like how is that ok
# like yea it fits data but also it's not guessing soil
# general question does it matter then, fixing the data?
# bc i could just have it guess all parameters all the time
# 1/k is turnover time
# k1 = fast, k2 = faster, k3 = slowest