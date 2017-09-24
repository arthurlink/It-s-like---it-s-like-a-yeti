### Link.Assignment_1
###

########## Load stuff ###############
library(ggplot2);library(dplyr)

y=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Ass.csv") 


############### Quest.1_Data viz ############


#Still need to do an analysis for this to get line-of-fit and r value
mod.1=lm(log(y$Above.ground.carbon)~log(y$Species.diversity))
summary(mod.1)#not signficant
par(mfrow=c(2,2));plot(mod.1)#follows assumptions
graphics.off()


G.1=ggplot(y,
          aes(y=log(Above.ground.carbon),
              x=log(Species.diversity)))+
  scale_color_continuous()+
  geom_point(shape=19,na.rm=T,size=3)+
  scale_x_continuous(limits = c(-2,2))+
  scale_y_continuous(limits = c(-2.5,1.5))+
  labs(y=expression(paste(italic('ln'))~-~Aboveground~Carbon~Storage~(Mg~ha^-1)),
       x=expression(paste(italic('ln'))~-~Species~Diversity~(H[s])))+
  theme_classic()
G.1

mod.2=lm(log(y$Above.ground.carbon)~log(y$DBH.6.div))
summary(mod.2)#Significant
par(mfrow=c(2,2));plot(mod.2)#issue with leverage
graphics.off()

plot(log(y$Above.ground.carbon)~log(y$DBH.6.div))
G.2=ggplot(y,
           aes(y=log(Above.ground.carbon),
               x=log(DBH.6.div)))+
  scale_color_continuous()+
  geom_abline(intercept = 0.00008572,slope=.685)+
  geom_point(shape=19,na.rm=T,size=3)+
  scale_x_continuous(limits = c(-1.5,1.25))+
  scale_y_continuous(limits = c(-2.5,1.5))+
  labs(y=expression(ln~-~Aboveground~Carbon~Storage~(Mg~ha^-1)),
       x=expression(paste(italic('ln'))~-~Height~Diversity~(H[d]~'8 cm class')))+
  theme_classic()
G.2


################## Question 2 Analysis: #############################


mod1=lm(log(y$Above.ground.carbon)~log(y$Species.diversity)*log(y$Height.3.diversity))
summary(mod1)
par(mfrow=c(2,2));plot(mod1)#follows assumptions
graphics.off()

#Trying it the way given by centering
y$Sd.c=y$Species.diversity-mean(y$Species.diversity)
y$Hd.c=y$Height.3.diversity-mean(y$Height.3.diversity)
y$PSH.ci=y$Sd.c*y$Hd.c
hist(log(y$PSH.ci))
mod1.1=lm(log(y$Above.ground.carbon)~log(y$Species.diversity)+log(y$Height.3.diversity)+log(y$PSH.ci))
summary(mod1.1)#significance of height.diversity
par(mfrow=c(2,2));plot(mod1.1)#follows assumptions, a little rough at ends (possibly limit outliers).

mod2=lm(log(y$Above.ground.carbon)~log(y$Height.3.diversity))
summary(mod2)
par(mfrow=c(2,2));plot(mod2)#follows assumptions
graphics.off()

G1=ggplot(y,
         aes(y=log(Above.ground.carbon),
             x=log(Height.3.diversity)))+
  scale_color_continuous()+
  geom_abline(intercept = 0.0002,slope=.49622)+
  geom_point(shape=19,na.rm=T,size=3)+
  scale_x_continuous(limits = c(-.75,1))+
  scale_y_continuous(limits = c(-2.5,1.5))+
  labs(y=expression(ln~-~Aboveground~Carbon~Storage~Mg~(h^-1)),
       x=expression(paste(italic('ln'))~-~Height~Diversity~(H[h]~'2m class')))+
  theme_classic()
G1

library(cowplot)
sinrow=plot_grid(G.1,G.2,G1,
                 align='vh',
                 nrow=1)

Final=plot_grid(sinrow,ncol=1,rel_heights = c(1,.1))

x11(5,7)
Final
ggsave('plots.jpeg',dpi = 200)

### get rid of x replications and marks. Correct Y axis and add comma to y axis

