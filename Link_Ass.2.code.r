# Art Link

soc=read.csv("https://github.com/arthurlink/It-s-like---it-s-like-a-yeti/raw/master/66_207_141_24139470%20(1)%20-%20Copy.csv")
chi=read.csv("https://github.com/arthurlink/It-s-like---it-s-like-a-yeti/raw/master/66_207_141_24139470%20(1).csv")

ts.plot(soc$Sockeye)
ts.plot(chi$ChinookAdult)

plot(log(soc$Sockeye)~soc$Year)
abline(lm(log(soc$Sockeye)~soc$Year))

#Initial data review
mod.lm=lm(log(soc$Sockeye)~soc$Year)
par(mfrow=c(2,2))
plot(mod.lm)#assumptions met
graphics.off()
summary(mod.lm)#not significant
coef(mod.lm)

library(ggplot2)
G=ggplot(data=soc,
         aes(x=Year,y=log(Sockeye)))+
           geom_point(lwd=1.4)+
           #geom_abline(intercept = -0.028872,slope = 0.005701)+
           labs(x='Years',y=expression(paste(italic('ln'))~-~Sockeye~(N)))+
           theme(text = element_text(size=16))+
           theme_classic()
         
G

plot(log(chi$ChinookAdult)~chi$Year)
abline(lm(log(chi$ChinookAdult)~chi$Year))

mod.lm1=lm(log(chi$ChinookAdult )~chi$Year)
par(mfrow=c(2,2))
plot(mod.lm1)#assumptions met, sort of breaking some, but not bad
graphics.off()
summary(mod.lm1)#significant, (+)relation, (r^2=.17)
coef(mod.lm1)

G2=ggplot(data=chi,
         aes(x=Year,y=log(ChinookAdult)))+
  geom_point(lwd=1.4)+
  geom_abline(intercept = -2.370972198,slope = 0.007700015)+
  labs(x='Years',y=expression(paste(italic('ln'))~-~Chinook~(N)))+
  theme(text = element_text(size=16))+
  theme_classic()

G2

library(cowplot)
sinrow=plot_grid(G,G2,
                 align='vh',
                 nrow=2)

Final=plot_grid(sinrow,ncol=1,rel_heights = c(1,.1))

#x11(5,7)
Final
#ggsave('plots.jpeg',dpi = 200)

###########################
###########################
#First option of manageme

trajectory.random = function(lambda,lambda.sd,N0,generations){
  N=data.frame(N0)
  for (i in 2:length(generations)){
    N[i,1]=N[i-1,1]*rnorm(1,lambda,lambda.sd)
    N[i,1]=ifelse(N[i,1]<2,0,N[i,1]) #Because if the population shrinks to <2, 
    #it no longer exists.
  }
  N
}

#general outcome of model, without iterations
population=trajectory.random(1.03,0.3,100,1:100)
population[,2]=1:nrow(population)
ggplot(population,aes(x=population[,2],y=population[,1]))+
  geom_line(size=1)+
  theme_bw()+
  labs(y='Population size',x='Time')



#############################

Simulated.populations=seq(from=100,to=200,by=5) #These are our theoretical carrying capacities.

simulations=data.frame(numeric(0)) #Creates an empty data frame to populate in the for-loop below.

for (j in 1:length(Simulated.populations)){
  for (k in 1:75){
    simulations[k,j]=trajectory.random(1.03,0.3,Simulated.populations[j],1:100)[100,1] #Records the final 
    #population size in the simulation.
  }
  colnames(simulations)[j]=paste('k.',Simulated.populations[j],sep='') #Renames the column of the output
  #to correspond with the carrying capacity size.
}

y1=colSums(simulations==0) #Counts the number of times the population went extinct in 50 simulations.

plot(seq(from=100,to=200,by=5),colSums(simulations==0)) #Creates a basic plot of the findings.

### Second option
###########################

Simulated.lambda=seq(from=.3,to=.2,by=-.005) #Theoretical lambda.

simulations=data.frame(numeric(0)) #Creates an empty data frame to populate in the for-loop below.

for (j in 1:length(Simulated.lambda)){
  for (k in 1:75){
    simulations[k,j]=trajectory.random(1.03,Simulated.lambda[j],100,1:100)[100,1] #Records the final 
    #population size in the simulation.
  }
  colnames(simulations)[j]=paste('k.',Simulated.lambda[j],sep='') #Renames the column of the output
  #to correspond with the lambda.
}

y2=colSums(simulations==0) #Counts the number of times the population went extinct in 50 simulations.

plot(seq(from=.3,to=.2,by=-.005),colSums(simulations==0)) #Creates a basic plot of the findings.

###
#prep for plots

x1=seq(from=100,to=200,by=5)
x2=seq(from=.3,to=.2,by=-.005)
yo=data.frame(x1,y1,x2,y2)

#### Analysis

#First option
mod.lm3=lm(log(yo$y1)~yo$x1)
par(mfrow=c(2,2))
plot(mod.lm3)#assumptions met
graphics.off()
summary(mod.lm3)#significant
coef(mod.lm3)

#Second option
mod.lm4=lm(log(yo$y2)~yo$x2)
par(mfrow=c(2,2))
plot(mod.lm4)#assumptions met
graphics.off()
summary(mod.lm4)#significant
coef(mod.lm4)

### Plots

G3=ggplot(data=yo,
         aes(x=x1,y=log(y1)))+
  geom_point(lwd=1.4)+
  geom_abline(intercept =3.967,slope = -.0047)+
  scale_y_continuous(lim=c(2.9,3.65))+
  labs(x='Carrying Capacity (females)',y=expression(paste(italic('ln'))~-~Armadillo~(N)))+
  theme(text = element_text(size=16))+
  theme_classic()

G3

G4=ggplot(data=yo,
          aes(x=x2,y=log(y2)))+
  geom_point(lwd=1.4)+
  geom_abline(intercept = -5.909308,slope = -32.337022)+
  scale_x_reverse(lim=c(.3,.2))+
  scale_y_continuous(lim=c(0,4))+
  labs(x=expression(paste(italic((lambda)))),y='Growth Rate')+
  theme(text = element_text(size=16))+
  theme_classic()

G4

sinrow=plot_grid(G3,G4,
                 align='vh',
                 nrow=1)

Final=plot_grid(sinrow,ncol=1,rel_heights = c(1,.1))

#x11(5,7)
Final
#ggsave('plots1.jpeg',dpi = 200)

#End!
