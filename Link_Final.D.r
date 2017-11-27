### Link.Final_Worms
###
########## Load stuff ###############
library(ggplot2);library(dplyr);library(vegan);library(Rmisc);library(stringr);library(tidyr);library(reshape2)

site=read.csv('https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Coordinates%20and%20conditions.csv')
spec=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/Worms%20Mary.csv") 

############### Data work ############
# Poor data quality!!! So many edits required!!! Wow, finally cleared it all, I think.
summary(spec$Family)
summary(spec$Genus)
summary(spec$Species)#whatever

#split between ec and temp
site=cbind(site,str_split_fixed(site$X3...Conductivity,"/",2)[,c(1,2)])
site=cbind(site,str_split_fixed(site$X6...Conductivity,"/",2)[,c(1,2)])
colnames(site)[c(9,10,11,12)]=c('E3','T3','E6','T6')

#split between different site and site types
spec=cbind(spec,str_split_fixed(spec$Site,'-',3))
colnames(spec)[c(7,8,9)]=c('S','P','T')
spec$T=str_sub(spec$T,-1)

site=cbind(site,str_split_fixed(site$Site.ID,'-',3))
colnames(site)[c(13,14,15)]=c('S','P','T')
summary(site$S)

#combine family and genus
spec=filter(spec,Count>0)
spec=filter(spec,Family!='Unknown')
spec$y=paste(spec$Family,spec$Genus,sep = '.')

sp=spec[,c(1,10,6)]

yoodle=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/to.csv")
yo=dcast(yoodle,Site~Gen.Fam)
yo1=yo[,-c(1)]

# diversity analysis, check out ecoindR
#richness
sp=specnumber(yo1)
sp

#wiener d.
sh=diversity(yo1)
sh

yo1=cbind(yo1,sp,sh)
yo1=cbind(yo1,str_split_fixed(yo$Site,'-',3))
colnames(yo1)[c(15,16,17)]=c('Sr','Pr','Tr')
yo1$Tr=str_sub(yo1$Tr,-1)
#richness
hist(log(yo1$sp))
plot(log(yo1$sp)~as.factor(yo1$Tr))
t.test(log(yo1$sp)~as.factor(yo1$Tr))
#Sh.winer
hist(yo1$sh)#pull
plot(yo1$sh~as.factor(yo1$Tr))
wilcox.test(yo1$sh~as.factor(yo1$Tr))#no differenece

############## Basic measurements for Sites
#rough data, just do non-para
wilcox.test(site$pH~site$Control.or.Invaded)#sig
plot(site$pH~site$Control.or.Invaded)

#top-surface temp.
wilcox.test(site$Temperature~site$Control.or.Invaded)#no sig

hist(as.numeric(site$T3))
hist(as.numeric(site$T6))#data okay

t.test(as.numeric(site$T3)~site$Control.or.Invaded)
t.test(as.numeric(site$T6)~site$Control.or.Invaded)


hist(as.numeric(site$E3))
hist(as.numeric(site$E6))#data okay

wilcox.test(as.numeric(site$E3)~site$Control.or.Invaded)
wilcox.test(as.numeric(site$E6)~site$Control.or.Invaded)

##### sites and species
# I chose to combine LT and IR because of the similarity of site history (forest)
levels(spec$S)[levels(spec$S)=='LT']='IR'

hist(spec$Count)
#poisson, do NBR!!!! https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
library(foreign);library(MASS)
#Initial observation of data
mod=glm(Count~T+S+Stage,spec, family = 'poisson')##### significant in stages and site
summary(mod)#Sig. of site and stage, not treatments
#plot(mod)
mean(spec$Count);var(spec$Count) #2vs.6, sort of off

y1=group.CI(Count~S+T,spec)
colnames(y1)=c('Site','Type','upper','mean','lower')

d1=position_dodge(width = 0.8)
dd=8
#plot
g=ggplot(y1,aes(x=Site,y=mean,fill=Type))+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.1,size=.8,position=d1)+
  geom_point(size=6,color='black',shape=21,stroke=1,position=d1)+
  scale_fill_manual(values=c('white','black'))+
  labs(y='Worm count per sample',x='Site')+
  theme_classic()
#x11(3,3)
g

#Good basic graph, but lets get better
#BNR, loses significance
modo=glm.nb(Count~S,spec)
summary(modo)
pp=expand.grid(S=c(levels(spec$S)))
pred=predict(modo,newdata = pp,se.fit = T)
cumlut=2
pred=data.frame(S=pp,
                fit=pred$fit,
                ci.lower=pred$fit+cumlut*pred$se.fit,
                ci.upper=pred$fit-cumlut*pred$se.fit)
ggplot(pred,aes(S,fit))+
  geom_point(stat='identity',color='black',fill='grey')+
  geom_errorbar(aes(ymax=ci.upper,ymin=ci.lower),width=.4)+
  labs(y='Mean response',x='Site Location')+
  theme_classic()

###nmds
levels(yo1$Sr)[levels(yo1$Sr)=='LT']='IR'
library(indicspecies)
mod2=multipatt(yo1[,1:12],yo1$Sr,func='IndVal')
summary(mod2) #Lumbricidae.Octolasion



mod3=metaMDS(yo1[1:12],dist='manhattan')
yo1[,18:19]=mod3$points
colnames(yo1)[18:19]=c('NMDS1','NMDS2')
stressplot(mod3)

### trying to compare others and lumbriccidae
totals1=data.frame(colSums(yo1[yo1$Sr=='EHC',1:12]))
colnames(totals1)[1]='count'
totals1$spp=rownames(totals1); totals1$spp=sub('[.]',' ',totals1$spp)
#totals1[nrow(totals1)+1,]=NA
#totals1$count[13]=sum(totals1$count[grep('Lumbricidae',totals1$spp)])
totals1$type='EHC'
#totals1=filter(totals1,count>0)

totals2=data.frame(colSums(yo1[yo1$Sr=='IR',1:12]))
colnames(totals2)[1]='count'
totals2$spp=rownames(totals2); totals2$spp=sub('[.]',' ',totals2$spp)
totals2$type='IR'

totals=rbind(totals1,totals2)
#totals[6,2]='Total'



ggplot(data=totals,aes(x=spp,y=count,fill=type))+
  geom_bar(stat="identity", position=position_dodge(),color='black')+
  scale_fill_manual(values=c('gray50','gray70'))+
  scale_y_continuous(expand=c(0,0))+
  labs(x='',y='Total count per sample type',fill='')+
  theme(legend.position = c(0.9, 0.9))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8,face='italic'))


#NMDS site plot
ggplot(yo1,aes(NMDS1,NMDS2,fill=Sr))+
  geom_point(size=4,shape=21)+
  labs(x='NMDS axis 1',y='NMDS axis 2')+
  scale_fill_manual(values=c('white','black','grey'))+
  theme(legend.key = element_blank(), legend.position = c(0.9, 0.2),legend.title.align=0.5,
        legend.background = element_rect(color = "gray60",size = .5, linetype = "solid"))+
  guides(fill=guide_legend(title='Plot type')) +
  theme_classic()

#####

adonis(yo1[,1:12]~yo1$Sr)
