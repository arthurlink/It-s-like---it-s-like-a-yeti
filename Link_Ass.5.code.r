# Art Link. Ass.5
########### Data upload

d=read.csv("https://raw.githubusercontent.com/arthurlink/It-s-like---it-s-like-a-yeti/master/MBSS%20data.csv")

########### Packages

library(ggplot2);library(MASS)

###########

#Cannot really change data to show comparisons

G1=ggplot(d)+
  geom_smooth(aes(x=Agriculture,y=BIBI),method='lm',color='black')+
  geom_smooth(aes(x=Urban,y=BIBI),method='lm',color='white',fill='black')+
    #xlim(-6,0)+ylim(-1,6.5)+
  labs(x='',y='B-IBI')+
  theme_classic()
G1

G2=ggplot(d)+
  geom_smooth(aes(x=Agriculture,y=Nitrate),method='lm',color='black')+
  geom_smooth(aes(x=Urban,y=Nitrate),method='lm',color='white',fill='black')+
  #xlim(-6,0)+ylim(-1,6.5)+
  labs(x='',y='Nitrate (mg/L)')+
  theme_classic()
G2

G3=ggplot(d)+
  geom_smooth(aes(x=Agriculture,y=Phosphate),method='lm',color='black')+
  geom_smooth(aes(x=Urban,y=Phosphate),method='lm',fill='black',color='white')+
  labs(x='Cover (%)',y='Phosphate (mg/L)')+
  theme_classic()
G3

library(cowplot)
sinrow=plot_grid(G1,
                 G2,
                 G3,
                 align='h',
                 nrow=3)

Final=plot_grid(sinrow,ncol=1,rel_heights = c(1,.1))

#x11(5,7)
Final
#ggsave('plots.jpeg',dpi = 200)

########## MLR
mod1=lm(BIBI~Urban*Agriculture,d)
summary(mod1)
step=stepAIC(mod1, direction="both")
step$anova#no interaction

mod1.1=lm(Nitrate~Urban*Agriculture,d)
summary(mod1.1)
step=stepAIC(mod1.1, direction="both")
step$anova#no interaction

mod1.2=lm(Nitrate~Urban*Agriculture,d)
summary(mod1.2)
step2=stepAIC(mod1.2,direction = 'both')
step2$anova#no interaction

#############

d$AU=d$Agriculture*d$Urban

pairs(d)
cor(d)#no need to subtract means

regU1=lm(BIBI~Urban,d)
regU2=lm(Nitrate~Urban,d)
regU3=lm(Phosphate~Urban,d)

regA1=lm(BIBI~Agriculture,d)
regA2=lm(Nitrate~Agriculture,d)
regA3=lm(Phosphate~Agriculture,d)

#Retrieved by: http://www.medicine.mcgill.ca/epidemiology/Joseph/courses/EPIB-621/multiple.regression.with.ci.txt
multiple.regression.with.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from an lm                   #
  #  (linear model) command in R and provides not                #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for intercept and slope.          #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  t.quantile <- qt(1-(1-level)/2, df=regress.out$df)
  number.vars <- length(regress.out$coefficients)
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * t.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci)
  return(output)
}

multiple.regression.with.ci(regU1)
multiple.regression.with.ci(regU2)
multiple.regression.with.ci(regU3)
#All for Urban have strong effects

multiple.regression.with.ci(regA1)
multiple.regression.with.ci(regA2)
multiple.regression.with.ci(regA3)
#BiBi effects are inconclusive.

reg4.1=lm(Nitrate~Urban+Agriculture,d)
reg4.2=lm(Phosphate~Agriculture,d)
reg4.3=lm(BIBI~Urban+Agriculture,d)


multiple.regression.with.ci(reg4.1)#okay
multiple.regression.with.ci(reg4.2)#Urban not significant,phosphate
multiple.regression.with.ci(reg4.3)#okay

#remove phosphate, Urban analysis, same for initial mlr
reg4.2=lm(Phosphate~Agriculture,d)
multiple.regression.with.ci(reg4.2)#cooh

#Add interactions
reg5.1=lm(BIBI~Urban+Agriculture+AU,d)
reg5.2=lm(Nitrate~Urban+Agriculture+AU,d)
reg5.3=lm(Phosphate~Agriculture,d)#same as before

multiple.regression.with.ci(reg5.1)
multiple.regression.with.ci(reg5.2)

#Without interactions: reg4.1-4.3
multiple.regression.with.ci(reg4.1)#okay
multiple.regression.with.ci(reg4.2)#Urban not significant,phosphate
multiple.regression.with.ci(reg4.3)#okay


#Urban.BIBI
sqrt(var(d$BIBI))*-0.0198
sqrt(var(d$BIBI))*c(-0.02231537,-0.01729019)
#Agri.BIBI
sqrt(var(d$BIBI))*-0.0042347
sqrt(var(d$BIBI))*c(-0.00611129,-0.00235815)
#Urban.Nit
sqrt(var(d$Nitrate))*0.017849
sqrt(var(d$Nitrate))*c(0.01390149,0.02179687)
#Agri.Nit
sqrt(var(d$Nitrate))*0.048082
sqrt(var(d$Nitrate))*c(0.04513366,0.05103046)
#Agri.Pho
sqrt(var(d$Phosphate))*0.0001392
sqrt(var(d$Phosphate))*c(0.0009617287,0.0031068000)


#### plot


#No nitrate with urban
yo=read.csv("H:/Graduate School/Applied Eco/Assignment 5/yoo.csv")

g3=ggplot(yo,aes(x=as.factor(Treat),y=Mean,fill=as.factor(Cover)))+
  #geom_line(data=si4,aes(x=Logging,y=effect),size=4)+
  geom_errorbar(aes(ymin=Lower,ymax=Higher),width=.2,size=.8)+
  geom_point(size=6,color='black',shape=21,stroke=1)+
  geom_hline(yintercept=0,linetype=3)+
  #scale_color_manual(values=c('grey85','grey50','grey25'))+
  scale_fill_manual(values=c('black','white'))+
  labs(y='Interaction Effect',x='')+
  theme_classic()
x11(3,3)
g3
ggsave('yodfsd.jpeg')
