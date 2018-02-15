### Scipts to execute analyses in the paper
### "Suppression of native tree recruitment 
### in northeastern North American deciduous 
### forests by an invasive understory shrub, 
### Berberis thunbergii

### Authors:
### Link AF, Turnblacer T, Snyder CK, Daugherty SE, Utz RM

### Published in 
### Invasive Plant Science and Management [2/8/2018]

### Data used and available:
### Dryad.org/Github, Arthur F. Link -  [It-s-like---it-s-like-a-yeti]
### Current script runs through Github

#---------------------------------------------------------------------------------------#

#Load all required packages
library(ggplot2);library(Hmisc);library(dplyr);library(coda);library(tidyr);library(lattice);library(rjags);library(R2jags);library(cowplot);library(dunn.test);library(gridExtra);library(Rmisc)
#make sure all packages and dependancies work, especially with RJAGS
#RJAGS download for computer: http://www.sourceforge.net/projects/mcmc-jags/files

#Load data
Finished=read.delim("https://github.com/arthurlink/It-s-like---it-s-like-a-yeti/raw/master/Seedling.total_Link et al. 2018.txt")
Finished.s=read.delim("https://github.com/arthurlink/It-s-like---it-s-like-a-yeti/raw/master/Seedling.species.type_Link et al. 2018.txt")

#---------------------Predictive drymass data---------------------
# 45 stem sections that were 10 cm long were collected. They had various diameters ranging from 1-25, one was previously elimited (outlier)
# Equation creation:
D=read.delim("https://github.com/arthurlink/It-s-like---it-s-like-a-yeti/raw/master/Berberis.drymass.lm_Link et al. 2018.txt")
plot(D)#transformation required of d.mass and diameter
mod.lm=lm(log(D$Dry.mass)~log(D$Diameter))
par(mfrow=c(2,2))
plot(mod.lm)#assumptions met
graphics.off()
summary(mod.lm)#significant, (+)relation between dry.mass and diameter, (r^2=.94)
plot(log(D$Dry.mass)~log(D$Diameter));abline(-2.02043,1.65725)#outcome from summary
#Equation created provided a predictive relationship between stem diameter and plant drymass

#--------------------Graph of stem predictability-------------------------------------

p.1=ggplot(data=D, aes(x=log(Diameter),y=log(Dry.mass)))+
  geom_point(lwd=1.2)+
  geom_abline(intercept = -2.02043,slope = 1.65725)+
  labs(x='ln - Stem diameter (mm)',y=expression('ln - Whole-plant dry mass (g)'))+
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3))+
  annotate("text", x = 2, y = -1.75,
           label = "paste(italic(R) ^ 2, \" = 0.93\")", parse = T)+
  annotate("text",x=2,y=-1.35,
           label= "Dry mass = \n exp (-2.0+1.7*ln (Diameter)),")+
  theme(text = element_text(size=16))+
  theme_classic()

#x11(5,5)
p.1
#ggsave("Final.PRed..jpeg",dpi = 2000)

#--------------------Frequentist analysis Data on plot being true "control" or "invaded"------------#
par(mfrow=c(1,2))
hist(Finished$Stem.N);hist(Finished$Stem.D) # both dep.var. are positively skewed...non-parametric or transformation
graphics.off()
# Decided to do non-parametric (Mann-Whitney-Wilcoxon test)
# Null: distributions of both factors are equal
wilcox.test(Stem.N~P.T, data=Finished) #significance p<.001, reject null.
dunn.test(Finished$Stem.N,Finished$P.T,method = "bonferroni")

wilcox.test(Stem.D~P.T, data=Finished) #significance p<.001, reject null.
dunn.test(Finished$Stem.D,Finished$P.T,method = "bonferroni")

### Berberis density plots (not included in journal publication)
### Predicted drymass, from lm equation
y1=ggplot(data=Finished, aes(x=P.T,y=Stem.D,fill=P.T))+
  geom_boxplot(lwd=0.8,notch = T)+
  scale_y_continuous(limits=c(1,1000),breaks=c(0,250,500,750,1000))+
  scale_fill_manual(values=c('gray70','black'))+
  labs(x='Plot Type',y=expression('Barberry dry mass (g) per m'^2))+
  scale_x_discrete(labels=c('Control','Invaded'))+
  theme(text = element_text(size=16))+
  theme_classic()+
  guides(fill=F)
y1
plot(Finished$P.T,Finished$Stem.D)#four outliers on T >1000, removed

## Stem counts per m^2
y2=ggplot(data=Finished, aes(x=P.T,y=Stem.N,fill=P.T))+
  geom_boxplot(lwd=0.8,notch = T)+
  scale_y_continuous(limits=c(0,35),breaks=c(0,10,20,30))+
  scale_fill_manual(values=c('gray70','black'))+
  labs(x='',y=expression('Barberry stem counts per m'^2))+
  scale_x_discrete(labels=c('Control','Invaded'))+
  theme(text = element_text(size=16))+
  theme_classic()+
  guides(fill=F)
y2
sinrow=plot_grid(y2,y1,
                 align='v',
                 nrow=2)

Final=plot_grid(sinrow,ncol=1,rel_heights = c(1,.1))

#x11(5,5)
Final
#ggsave("Final.Seedling.jpeg",dpi = 300)

#---------------------------First model:C/T seedlings------------------------------------
# Model was ran as a normal distribution
# Data did not follow assumptions,
# All other distribution types/transformations would have limited or greatly altered the data
# I.E. gamma cannot work due to zeros. This really did not fit an exponential from the hypothesis:
# Research question: Seedling predictions of C vs. T

CT.mod <- function() {
  for(i in 1:N){
    Seedling.D[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta1 * CTn[i]
  }
  alpha ~ dnorm(0.1, 0.1)
  beta1 ~ dunif(-100, 100)
  tau ~ dgamma(.01, .01)
}

Seedling.D <- Finished$Seedling.D
CTn <- Finished$CTn
N <- nrow(Finished)

sim.dat.jags <- as.list(Finished[,c(3,6)])
sim.dat.jags$N <- nrow(Finished)

bayes.mod.params <- c("alpha", "beta1")

bayes.mod.inits <- function(){
  list("alpha" = rnorm(1), "beta1" = rnorm(1))
}

set.seed(123)
bayes.mod.fit <- jags(data = sim.dat.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000, model.file = CT.mod)

print(bayes.mod.fit)
bayes.mod.fit.mcmc <- as.mcmc(bayes.mod.fit)
summary(bayes.mod.fit.mcmc)
xyplot(bayes.mod.fit.mcmc)
densityplot(bayes.mod.fit.mcmc)

#---------------------------------------Plot for Seedlings~Plots-----------------------------------

# Base-r
Finished$P.T=ifelse(Finished$P.T=="C","Control","Invaded")
Finished$P.T=as.factor(Finished$P.T)

y1=group.CI(Seedling.D~P.T,Finished)
y1      
go=data.frame(y1)

colnames(go)=c('Type','Upper','Mean','Lower')

colnames(Finished)[2]=c('Type')
g=ggplot(data=go,aes(x=Type,y=Mean,fill=Type))+
  geom_jitter(data=Finished,aes(x=Type,y=Seedling.D,color=Type),width = 0.25)+
  geom_errorbar(data=go,aes(ymin=Lower,ymax=Upper),width=.2,size=1)+
  geom_point(size=6,shape=21,stroke=1)+
  scale_fill_manual(values=c('gray','black'))+
  scale_color_manual(values=c('gray','black'))+
  ylim(-.1, 12)+
  labs(y='',x='')+
  theme_classic()

g
#x11()
#ggsave('topo2.jpeg')

#---------------------------------------Second Model: Stem.Seedling.Model---------------------------
# Also ran this one as if the data were normal because of the issue with producing viable data, given much of the data being lost
# Utilized a JAGS code for exponential-like model
Stem.N<- Finished$Stem.N

BCn.mod<- function() {
  # Likelihood
  for(i in 1:N){
    Seedling.D[i] ~ dnorm(mu[i],tau)  
    mu[i] <- alpha*exp(-beta1*Stem.N[i])
  }
  # Prior distributions
  alpha ~ dunif(0,10)
  beta1 ~ dunif(0,15)
  tau~dgamma(.01,.01)
}

sim.dat.jags <- as.list(Finished[,c(4,3)])
sim.dat.jags$N <- nrow(Finished)
bayes.mod.inits <- function(){
  list("alpha" = rexp(1), "beta1" = rexp(1))}

bayes.mod.params <- c("alpha", "beta1")

set.seed(123)

bayes.mod.fit <- jags(sim.dat.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000, model.file = BCn.mod)

print(bayes.mod.fit)

bayes.mod.fit.mcmc <- as.mcmc(bayes.mod.fit)
summary(bayes.mod.fit.mcmc)

xyplot(bayes.mod.fit.mcmc)
densityplot(bayes.mod.fit.mcmc)
densityplot(bayes.mod.fit.mcmc, layout=c(2,2), aspect="fill")

jags.mcmc=as.mcmc(bayes.mod.fit)
plot(jags.mcmc)
summary(jags.mcmc)
HPDinterval(jags.mcmc)


#---------------------------------------Third Model: Density.Seedling.Model----------------

BD.mod<- function() {
  # Likelihood
  for(i in 1:N){
    Seedling.D[i] ~ dnorm(mu[i],tau)  
    mu[i] <- alpha*exp(-beta1*Stem.D[i])
  }#prior distributions
  alpha ~ dunif(-10,10)
  beta1 ~ dunif(-15,15)
  tau~dgamma(.01,.01)
}

sim.dat.jags <- as.list(Finished[,c(5,3)])
sim.dat.jags$N <- nrow(Finished)
bayes.mod.inits <- function(){
  list("alpha" = rexp(1), "beta1" = rexp(1))}

bayes.mod.params <- c("alpha", "beta1")

Seedling.D<- Finished$Seedling.D
Stem.D <- Finished$Stem.D
N <- nrow(Finished)

bayes.mod.inits <- function(){
  list("alpha" = dexp(1), "beta1" = dexp(1))
}

set.seed(123)

bayes.mod.fit <- jags(data = sim.dat.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000, model.file = BD.mod)

print(bayes.mod.fit)

#---------------------------------------Plot for Seedling~Stem.D------------------------

#x11(5,7)
par(mfrow=c(2,1),mar=c(5,5,1,1))
plot(Seedling.D~Stem.N,Finished, pch=16, las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression('Barberry stem count per m'^2),
     ylab=expression('Seedling density per m'^2),
     col=c('grey','black')[as.numeric(Type)])
curve(5.970*exp(-(2.404)*x),0,90,col='gray37',lwd=1,add = T)
curve(3.534*exp(-(.149)*x),1,90,col='gray37',add = T)
curve(4.73*exp(-(.627)*x),0,90,col='black',lwd=2,add = T)
legend(58,20, c("Control","Invaded"),col=c('grey','black'),pch=16)
legend(58,14.5, c("Mean","97.5% C.I."), lty=c(1,1), lwd=c(2,1),col=c("black","gray37"))

plot(Seedling.D~Stem.D,Finished, pch=16, las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression('Barberry dry mass (g) per m'^2),
     ylab=expression(''),
     col=c('grey','black')[as.numeric(Type)])
curve(5.952*exp(-(9.933)*x),0,3215,col='gray37',lwd=1,add = T)
curve(3.438*exp(-(.011)*x),4,3215,col='gray37',add = T)
curve(4.669*exp(-(1.133)*x),0,3215,col='black',lwd=2,add = T)
#legend(2000,20, c("Control","Treatment"),col=c('grey','black'),pch=16)
#legend(2000,18, c("Pred. Mean","97.5% C.I."), lty=c(1,1), lwd=c(2,1),col=c("black","gray37"))
graphics.off()

#----------------------------------------Final Bayesian, Species pred.-----------------
# Models outcomes were used to create the table in publication, the figures produced were not inlcuded in publication

ACRU=filter(Finished.s,Seed.T=="ACRU")
hist(ACRU$Seedling.D)

Stem.N<- ACRU$Stem.N
Seedling.D <- ACRU$Seedling.D
N <- nrow(ACRU)

BCn.mod<- function() {
  # Likelihood
  for(i in 1:N){
    Seedling.D[i] ~ dnorm(mu[i],tau)  
    mu[i] <- alpha*exp(-beta1*Stem.N[i])
  }
  # Prior distributions
  alpha ~ dunif(0,10)
  beta1 ~ dunif(0,15)
  tau~dgamma(.01,.01)
}

sim.dat.jags <- as.list(ACRU[,c(5,4)])
sim.dat.jags$N <- nrow(ACRU)
bayes.mod.inits <- function(){
  list("alpha" = rexp(1), "beta1" = rexp(1))}

bayes.mod.params <- c("alpha", "beta1")

set.seed(123)

bayes.mod.fit <- jags(sim.dat.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000, model.file = BCn.mod)

print(bayes.mod.fit)

#Output for Stem.N for ACRU species-------------------------------------------------
#mu.vect sd.vect    2.5%     25%     50%     75%   97.5%  Rhat n.eff
#alpha      3.327   0.673   2.013   2.887   3.318   3.759   4.684 1.001  3000
#beta1      1.975   3.036   0.008   0.215   0.687   2.163  11.956 1.001  3000
#deviance 236.301   3.949 230.401 233.220 235.613 239.058 244.436 1.001  2400

#----------------------------------ACRU w/stem.d-------------------------------------

BD.mod<- function() {
  # Likelihood
  for(i in 1:N){
    Seedling.D[i] ~ dnorm(mu[i],tau)  
    mu[i] <- alpha*exp(-beta1*Stem.D[i])
  }#prior distributions
  alpha ~ dunif(-10,10)
  beta1 ~ dunif(-15,15)
  tau~dgamma(.01,.01)
}

sim.dat.jags <- as.list(ACRU[,c(6,4)])
sim.dat.jags$N <- nrow(ACRU)
bayes.mod.inits <- function(){
  list("alpha" = rexp(1), "beta1" = rexp(1))}

bayes.mod.params <- c("alpha", "beta1")

Seedling.D<- ACRU$Seedling.D
Stem.D <- ACRU$Stem.D
N <- nrow(ACRU)

bayes.mod.inits <- function(){
  list("alpha" = dexp(1), "beta1" = dexp(1))
}

set.seed(123)

bayes.mod.fit <- jags(data = sim.dat.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000, model.file = BD.mod)

print(bayes.mod.fit)

#Outcomes for ACRUw/stem.d------------------------------------------------------
#mu.vect sd.vect    2.5%     25%     50%     75%   97.5%  Rhat n.eff
#alpha      3.269   0.720   1.882   2.769   3.270   3.744   4.672 1.001  3000
#beta1      4.777   4.719   0.002   0.321   3.253   8.729  14.316 1.001  3000
#deviance 239.432   3.684 231.399 237.513 240.063 241.523 245.979 1.001  2200


#-----------------------------------Plot for Stem.N---------------------------------
plot(Seedling.D~Stem.N,ACRU, col=193,
     xlab=expression("Barberry Stem Count"),
     ylab=expression("ACRU Seedling Density"~~'(1'~m^2~'Plots)'),
     pch=c(16,1)[as.numeric(P.T)])
curve(3.327*exp(-(1.975)*x),0,52,col='black',lwd=2,add = T)
legend(30,15, c("Pred. Mean","97.5% C.I."), lty=c(1,1), lwd=c(2,1),col=c("black","gray37"))
legend(17,15, c("Control","Treatment"),pch=c(16,1))

#-----------------------------------Plot for Stem.D---------------------------------
plot(Seedling.D~Stem.D,ACRU, col=193,
     xlab=expression("Barberry Stem Dry Mass"),
     ylab=expression("ACRU Seedling Density"~~'(1'~m^2~'Plots)'),
     pch=c(16,1)[as.numeric(P.T)])
curve(3.269*exp(-(4.777)*x),0,1350,col='black',lwd=2,add = T)

#SAAL Stem.N&Stem.D-----------------------------------------------------------------
SAAL=filter(Finished.s,Seed.T=="SAAL")
hist(SAAL$Seedling.D)

Seedling.D<- SAAL$Seedling.D
Stem.D <- SAAL$Stem.D
Stem.N <- SAAL$Stem.N
N <- nrow(SAAL)


BCn.mod<- function() {
  # Likelihood
  for(i in 1:N){
    Seedling.D[i] ~ dnorm(mu[i],tau)  
    mu[i] <- alpha*exp(-beta1*Stem.N[i])
  }
  # Prior distributions
  alpha ~ dunif(0,10)
  beta1 ~ dunif(0,15)
  tau~dgamma(.01,.01)
}

sim.dat.jags <- as.list(SAAL[,c(5,4)])
sim.dat.jags$N <- nrow(SAAL)
bayes.mod.inits <- function(){
  list("alpha" = rexp(1), "beta1" = rexp(1))}

bayes.mod.params <- c("alpha", "beta1")

set.seed(123)

bayes.mod.fit <- jags(sim.dat.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000, model.file = BCn.mod)

print(bayes.mod.fit)

##Output for Stem.N for ACRU species-------------------------------------------------
#         mu.vect sd.vect   2.5%    25%    50%     75%   97.5%  Rhat n.eff
#alpha      1.220   0.143  0.952  1.124  1.215   1.308   1.517 1.001  3000
#beta1      0.027   0.083  0.000  0.003  0.008   0.016   0.243 1.001  3000
#deviance 101.128   4.053 96.911 98.298 99.924 102.527 112.563 1.001  3000

#----------------------------------SAAL w/stem.d-------------------------------------

BD.mod<- function() {
  # Likelihood
  for(i in 1:N){
    Seedling.D[i] ~ dnorm(mu[i],tau)  
    mu[i] <- alpha*exp(-beta1*Stem.D[i])
  }#prior distributions
  alpha ~ dunif(0,10)
  beta1 ~ dunif(0,15)
  tau~dgamma(.01,.01)
}

sim.dat.jags <- as.list(SAAL[,c(6,4)])
sim.dat.jags$N <- nrow(SAAL)
bayes.mod.inits <- function(){
  list("alpha" = rexp(1), "beta1" = rexp(1))}

bayes.mod.params <- c("alpha", "beta1")

bayes.mod.inits <- function(){
  list("alpha" = rexp(1), "beta1" = rexp(1))
}

set.seed(123)

bayes.mod.fit <- jags(data = sim.dat.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000, model.file = BD.mod)

print(bayes.mod.fit)

##outcomes
#          mu.vect sd.vect   2.5%     25%     50%     75%   97.5%  Rhat n.eff
#alpha      1.288   0.185  0.948   1.166   1.277   1.397   1.673 1.001  3000
#beta1      0.128   0.288  0.000   0.001   0.020   0.142   0.821 1.010   210
#deviance 107.639   6.602 97.163 101.319 109.351 112.190 119.977 1.010   200


#Plots for both stem.n and stem.d------------------------------------------------------
###Bring all together
#x11(5,5)
par(xpd=T,mfrow=c(3,2),mar=c(5,6,2,2)+.1)

plot(Seedling.D~Stem.N,Finished, pch=16, las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression(""),
     ylab=expression("Total native seedling density"~~'(1'~m^2~')'),
     col=c('grey','black')[as.numeric(Type)])
curve(5.970*exp(-(2.404)*x),0,90,col='gray37',lwd=1,add = T)
curve(3.534*exp(-(.149)*x),1,90,col='gray37',add = T)
curve(4.73*exp(-(.627)*x),0,90,col='black',lwd=2,add = T)
#legend(32,14, c("Control","Treatment"),col=c('grey','black'),pch=16)
#legend(55,20, c("Pred. Mean","97.5% C.I."), lty=c(1,1), lwd=c(2,1),col=c("black","gray37"))

plot(Seedling.D~Stem.D,Finished, pch=16,las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression(""),
     ylab=expression(""),
     col=c('grey','black')[as.numeric(Type)])
curve(5.952*exp(-(9.933)*x),0,3215,col='gray37',lwd=1,add = T)
curve(3.438*exp(-(.011)*x),4,3215,col='gray37',add = T)
curve(4.669*exp(-(1.133)*x),0,3215,col='black',lwd=2,add = T)

plot(Seedling.D~Stem.N,ACRU, pch=16,las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression(""),
     ylab=expression("ACRU seedling density"~~'(1'~m^2~')'),
     col=c('grey','black')[as.numeric(P.T)])
curve(3.327*exp(-(1.975)*x),0,52,col='black',lwd=2,add = T)

plot(Seedling.D~Stem.D,ACRU, pch=16,las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression(""),
     ylab=expression(""),
     col=c('grey','black')[as.numeric(P.T)])
curve(3.269*exp(-(4.777)*x),0,1350,col='black',lwd=2,add = T)

plot(Seedling.D~Stem.N,SAAL, pch=16,las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression("Barberry stem count"),
     ylab=expression("SAAL seedling density"~~'(1'~m^2~')'),
     ylim=c(0,3.5),
     col=c('grey','black')[as.numeric(P.T)])
curve(1.220*exp(-(0.027)*x),0,74,col='black',lwd=2,add = T)

plot(Seedling.D~Stem.D,SAAL, pch=16,las=1,col.axis='gray25', cex.lab=1.5,
     xlab=expression("Barberry stem dry mass (g)"),
     ylab='',
     ylim=c(0,3.5),
     col=c('grey','black')[as.numeric(P.T)])
curve(1.288*exp(-(.128)*x),0,2250,col='black',lwd=2,add = T)

# [End]