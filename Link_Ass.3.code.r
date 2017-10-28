# Art Link. Ass.3
########### Data
library(Rmisc);library(ggplot2);library(cowplot);library(MASS);library(dplyr)
y=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Dataset_ExtinctionDebt_Gonzalez-Varo_et_al_JAPPLECOL.csv")


#Q1
########### 
# 1.A
#Patch dist - log transformed
y1=group.CI(log(patch_size.ha.)~myrtus_presence,y)
y1
y1$myrtus_presence
colnames(y1)=c('myrtus_presence','upper','mean','lower')

dd=8
d1=position_dodge(width = 1)

GG1=ggplot(y1,
           aes(x=myrtus_presence,y=mean,fill=as.factor(myrtus_presence)))+
  geom_errorbar(aes(ymax=upper, ymin=lower), 
                width=0.2, size=0.5, color="black",position = d1)+
  geom_point( size=3, shape=21,position=d1)+
  scale_fill_manual(values=c('white','black'))+
  #scale_y_continuous(limits = c(-1,8),breaks = c(1,3,5,7))+
  labs(y=paste(italic('ln'))~-~Patch~size~(ha),x='')+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+
  guides(fill=F)
GG1

### analysis
t.test(log(patch_size.ha.)~as.factor(myrtus_presence),y) #Significant
plot(log(patch_size.ha.)~as.factor(myrtus_presence),y)
by(log(y$patch_size.ha.),as.factor(y$myrtus_presence),sd)
#1.B
###########
#disturbances

y2=group.CI(disturbance.proportion_of_nitrophilous_ruderal_spp.~myrtus_presence,y)
y2

GG2=ggplot(y2,
           aes(x=myrtus_presence,y=disturbance.proportion_of_nitrophilous_ruderal_spp..mean,fill=as.factor(myrtus_presence)))+
  geom_errorbar(aes(ymax=disturbance.proportion_of_nitrophilous_ruderal_spp..upper, ymin=disturbance.proportion_of_nitrophilous_ruderal_spp..lower), 
                width=0.2, size=0.5, color="black",position = d1)+
  geom_point( size=3, shape=21,position=d1)+
  scale_fill_manual(values=c('white','black'))+
  #scale_y_continuous(limits = c(-1,8),breaks = c(1,3,5,7))+
  labs(y='Patch distrubances (prop. nitrophilous and ruderal spp)',x='',size=dd)+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+
  guides(fill=F)
GG2

#analysis, data was fine as is!
t.test(disturbance.proportion_of_nitrophilous_ruderal_spp.~as.factor(myrtus_presence),y)
plot(disturbance.proportion_of_nitrophilous_ruderal_spp.~as.factor(myrtus_presence),y)
by(y$disturbance.proportion_of_nitrophilous_ruderal_spp.,as.factor(y$myrtus_presence),sd)


###1.C
### scatter
y$R2_2002= log(y$R2_2002)
y$R2_1956=log(y$R2_1956)#remove -inf

y=y[!is.infinite(rowSums(y)),]
y$patch_size.ha.=log(y$patch_size.ha.)

GG3=ggplot(y)+
  geom_jitter(aes(x=R2_1956,y=patch_size.ha.),shape=21,size=3,color='black',fill='white')+
  geom_smooth(aes(x=R2_1956,y=patch_size.ha.),method='lm',color='white',fill='black')+
  geom_jitter(aes(x=R2_2002,y=patch_size.ha.),shape=21,size=3,color='grey',fill='black')+
  geom_smooth(aes(x=R2_2002,y=patch_size.ha.),method='lm',color='black')+
  xlim(-6,0)+ylim(-1,6.5)+
  theme_classic()

#x11(5,7)  
GG3
#ggsave('yooodle.jpeg')


#### analysis
mod=lm(patch_size.ha.~R2_1956,y)
summary(mod)
coef(mod)
anova(mod)

mod1=lm(patch_size.ha.~R2_2002,y)
summary(mod1)
coef(mod1)
anova(mod1)
#Combining the plots


sinrow=plot_grid(GG1,
                 GG2,
                 #GG3,
                 align='ho',
                 nrow=2)

Final=plot_grid(sinrow,ncol=1,rel_heights = c(.5,.5))

#x11(5,7)
Final
#yepp
#ggsave('yogi.jpeg')


####
#Q2
###########
mod=glm(as.factor(myrtus_presence)~patch_size.ha.+
        disturbance.proportion_of_nitrophilous_ruderal_spp.+
        R2_1956,data=y,
        family='binomial')
summary(mod)
par(mfrow=c(2,2))
plot(mod)

mod3=glm(as.factor(myrtus_presence)~patch_size.ha.,data=y,
        family='binomial')
coef(mod3)
summary(mod3)
summary(y$patch_size.ha.)
sim.dattaaa=data.frame(patch_size.ha.=-1.5:8,Presence=predict(mod3,(list(patch_size.ha.=-1.5:8)),type='response'))
GG4=ggplot(y, aes(x=patch_size.ha., y=myrtus_presence))+
  geom_jitter(alpha=1,width = .1,height=.4)+
  geom_line(data=sim.dattaaa, aes(x = patch_size.ha., y = Presence))+
  labs(x = 'ln-patch',y='M.pres')+
  theme_classic()

#x11(5,6)
GG4
#ggsave('oh.jpeg')

###
mod4=lm(myrtus_presence~patch_size.ha.+
          disturbance.proportion_of_nitrophilous_ruderal_spp.+
          R1_2002+
          R2_2002+
          R5_2002+
          R1_1956+
          R2_1956+
          R5_1956,y)
summary(mod4)

step=stepAIC(mod4,direction = 'both')
step$anova

