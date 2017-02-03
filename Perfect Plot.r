library(ggplot2)
Perf.Plot=theme(panel.background=element_rect(fill = 'white', colour = 'white'),
                   axis.title.x = element_text(size=16,margin=margin(15,0,0,0)),
                   axis.title.y = element_text(size=16,margin=margin(0,15,0,0)),
                   axis.line.x= element_line(colour = "black",size = 1, linetype = "solid"),
                   axis.line.y= element_line(colour = "black",size = 1, linetype = "solid"),
                   axis.text.x = element_text(size=14,colour = "black"),
                   axis.text.y = element_text(size=14,colour = "black"),
                   axis.line=element_line(size=2,color='black'),
                   plot.margin=unit(c(0.6,0.6,0.6,0.6),'cm'))

Art.Plot=theme(panel.background=element_blank(),
               axis.title=element_text(size=14),
               axis.line=element_line(size=1),
               axis.ticks=element_line(size=1),
               axis.text=element_text(size=12, colour='black'),
               plot.margin =unit(c(.5,.5,.5,.5),'cm'))



Awf.Plot=
  theme(plot.background = element_rect(size = 5, color = "blue"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(angle= 135,colour = "purple", hjust=8),
        axis.text.x = element_text(colour = "red", hjust = 4),
        axis.line.x = element_line(size=8, colour="green"),
        axis.line.y = element_line(size=2, colour="darkgoldenrod4"),
        panel.background = element_rect(color='purple',fill = "yellow1",size=4),
        strip.background = element_rect(fill = "chartreuse1", size=35),
        axis.ticks.x = element_line(color='blue', size = 5),
        axis.ticks.y = element_line(color='chartreuse1', size = 3),
        axis.ticks.length =unit(1.2, 'inch'),
        panel.grid.major = element_line(size=2, colour='burlywood'),
        panel.grid.minor = element_line(size=3, colour='red2'))

