# Art Link. Ass.4
########### Data upload

si=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Site%20details.csv",skip=1)
bi=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Bird%20detection%20rates.csv",skip=1)
ba=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Bat%20capture%20rates.csv",skip=1)
ma=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Large%20mammal%20detection%20rates.csv",skip=2)

########### Packages

library(vegan);library(ggplot2);library(Rmisc)

#Does bats (ba) matter in sense of logging
ba1=cbind(substr(as.character(ba$Type),1,1),ba) #Removed different CA and CB
colnames(ba1)[1]=c('Type1')
ba1=ba[,-c(1,2,3)]
head(ba1)

#richness
sp=specnumber(ba1)
sp

#wiener d.
ba.sh=diversity(ba1)
ba.sh

#combine frames
ba2=cbind(ba1,ba.sh,substr(as.character(ba$Type),1,1))
colnames(ba2)[51]=c('Type')
ci=group.CI(ba.sh~as.factor(Type),data=ba2)
colnames(ci)=c('Type','upper','mean','lower')

#analysis
hist(ba.sh)#fits assumptions, no transformation
mod1=aov(ba.sh~as.factor(Type),data=ba2)
summary(mod1)#no significance

#ploooooter
g=ggplot(ci,aes(x=Type,y=mean,fill=Type))+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.1,size=.8)+
  geom_point(size=6,color='black',shape=21,stroke=1)+
  scale_fill_manual(values=c('grey50','white','black'))+
  labs(y='Shannon-Weiner diversity',x='')+
  theme_classic()
#x11(3,3)
g
#ggsave('yo.jpeg')
########

#birds and effect, also site information

lu=bi[-c(35:40),]#remove controls
lu1=lu[,-c(1,2)]#remove site and type

#wiener d.
lu.sh=diversity(lu1)

#This is too simple, so we should complicated things!!! Add to site


#combine frames
si2=si[-c(35:40),]
si3=cbind(si2,lu.sh)


#two-way anova
mod2=aov(lu.sh~Logging*as.factor(Time.since.logging_months),data=si3)#do not account for survey, skid trail, logging intensity.
summary(mod2)#significant
TukeyHSD(mod2)#less in 12 compared to 16
#plot(mod2)# assumptions met

ci2=group.CI(lu.sh~Logging,data=si3)
colnames(ci2)=c('Logging','upper','mean','lower')
#graphic
g2=ggplot(ci2,aes(x=Logging,y=mean,fill=Logging))+
  geom_line(data=si3,aes(x=Logging,y=lu.sh,color=as.factor(Time.since.logging_months)),size=4)+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.1,size=.8)+
  geom_point(size=6,color='black',shape=21,stroke=1)+
  scale_color_manual(values=c('grey85','grey50','grey25'))+
  scale_fill_manual(values=c('white','black'))+
  labs(y='Shannon-Weiner diversity',x='')+
  theme_classic()
#x11(3,3)
g2
#ggsave('yo1.jpeg')
#subtract differences, this is dumb, there has to be other ways of doing this
y=diff(si3$lu.sh,1,1)
#y=y[c(T,F)]want to include the dummy dATA
y
effect=append(y,0,0) #Cool thing
si4=cbind(si3,effect)
si4$effect=ifelse(si4$Logging=='Unlogged',0,si4$effect)
si4[si4==0]=NA
si4=na.omit(si4) #do not want to transform, will lose too much

wilcox.test(si4$effect~si4$Time.since.logging_months)

ci2=group.CI(effect~as.factor(Time.since.logging_months),data=si4)
colnames(ci2)=c('Logging','upper','mean','lower')
#graphic
g3=ggplot(ci2,aes(x=Logging,y=mean,fill=Logging))+
  #geom_line(data=si4,aes(x=Logging,y=effect),size=4)+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.1,size=.8)+
  geom_point(size=6,color='black',shape=21,stroke=1)+
  geom_hline(yintercept=0,linetype=3)+
  #scale_color_manual(values=c('grey85','grey50','grey25'))+
  scale_fill_manual(values=c('grey50','grey25'))+
  labs(y='Shannon-Weiner diversity',x='')+
  theme_classic()
#x11(3,3)
g3
#ggsave('yo3.jpeg')
