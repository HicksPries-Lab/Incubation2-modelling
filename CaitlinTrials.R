#Trying out for Michelle

i=10

# Read in data
#CO2flux_0 <- read.csv("INC2data_mod.csv", header=TRUE) 

CO2flux <- CO2flux_0 %>%
  filter(Num == i) %>%    # loop through treatment
  select(time, cummCO2) 

plot(x=CO2flux$time, y=CO2flux$cummCO2)

Ctotal= Cinits[i]

days=seq(0,135) #Incubation days

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
  return(modCost(model=modelOutput, obs=CO2flux))
}

inipars=c(k=.0001)  # for Palouse soil control should ~= .0006

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf),lower=c(0))

eCO2fit$par

fitmod=OnepModel(t=days, k=eCO2fit$par,
                 In = inputs_frame,
                 C0=Ctotal)

fitCumm=getAccumulatedRelease(fitmod)

fitframe <- data.frame(days, fitCumm)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe, aes(x = days, y = fitCumm)) +  # model data
  xlim(0, 135) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '1 Pool Model') +
  theme_C
plot1

npars=length(eCO2fit$par)
AIC_1p=(2*npars)-2*log(eCO2fit$ms)
AICc_1p=AIC_1p+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

#pseudo r-squared
fitmod=OnepModel(t=CO2flux$time, k=eCO2fit$par,
                 In = inputs_frame,
                 C0=Ctotal)

CO2flux$fitCumm<-rowSums(getAccumulatedRelease(fitmod))

plot(CO2flux$cummCO2, CO2flux$fitCumm)+abline(coef = c(0,1))
test<-summary(lm(cummCO2~fitCumm, data=CO2flux))
R_1p<-test$r.squared


# #two pool series
# eCO2func=function(pars){
#   mod=TwopSeriesModel(
#     t=days,
#     ks=pars[1:2],
#     a21=pars[3]*pars[1],
#     C0=Ctotal*c(pars[4],1-pars[4]), 
#     In=0,
#     pass=TRUE
#   )
#   AccR=getAccumulatedRelease(mod)
#   return(data.frame(time=days,cummCO2=rowSums(AccR)))
# }
# 
# eCO2cost=function(pars){
#   modelOutput=eCO2func(pars)
#   return(modCost(model=modelOutput, obs=CO2flux))
# }
# 
# inipars=c(k1=0.5,k2=0.05,alpha21=0.5,gamma=0.5)
# 
# eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
#                upper=c(Inf,Inf,1,1),lower=c(0,0,0,0))
# options(scipen = 999)
# eCO2fit$par
# 
# #Run the model again with best parameter set
# fitmod=TwopSeriesModel(t=days, ks=eCO2fit$par[1:2], 
#                        a21=eCO2fit$par[3]*eCO2fit$par[1],
#                        C0=Ctotal*c(eCO2fit$par[4],1-eCO2fit$par[4]), 
#                        In=0)
# fitCumm=getAccumulatedRelease(fitmod)
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
#   #ylim(0, 100) +
#   labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Series Model') +
#   theme_C
# plot1
# 
# npars=length(eCO2fit$par)
# AIC_2ps=(2*npars)-2*log(eCO2fit$ms)
# AICc_2ps=AIC_2ps+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1))

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

eCO2fit$par

#Run the model again with best parameter set
fitmod=TwopParallelModel(t=days, ks=eCO2fit$par[1:2], 
                         gam=eCO2fit$par[3],
                         C0=Ctotal*c(eCO2fit$par[3],1-eCO2fit$par[3]), 
                         In=0)
fitCumm=getAccumulatedRelease(fitmod)

#Plot the results

fitCumm1 <- rowSums(fitCumm)
fitframe <- data.frame(days, fitCumm1)

plot1 <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) + # INC data
  geom_line(data = fitframe, aes(x = days, y = fitCumm1)) +  # model data
  xlim(0, 135) +
  #ylim(0, 100) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool parallel Model') +
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

eCO2fit$par

days=seq(0,3600) #Incubation days

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
