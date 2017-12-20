##This correspond to the Bird egg data from Stoddard et al 2017

library(scales)

BirdEgg<-read.csv("BirdEggData/BirdEggData.csv",header = T,stringsAsFactors = F)

#measuring volume of the egg

BirdEgg$T_value<-1/(BirdEgg$Ellipticity+1)

BirdEgg$Lambda<-BirdEgg$Asymmetry+1

step_1<-sapply(list(-0.75,-0.5,-0.25,0,0.25,0.5,0.75),
               function(x){((BirdEgg$T_value*(1+x)^(1/(1+BirdEgg$Lambda)))*
                           ((1-x)^(BirdEgg$Lambda/(1+BirdEgg$Lambda))))^2})
          step_1[,1]<-step_1[,1]*4
          step_1[,2]<-step_1[,2]*2
          step_1[,3]<-step_1[,3]*4
          step_1[,4]<-step_1[,4]*2
          step_1[,5]<-step_1[,5]*4
          step_1[,6]<-step_1[,6]*2
          step_1[,7]<-step_1[,7]*4

BirdEgg$Volume<-(pi*(BirdEgg$AvgLength..cm.^3)*rowSums(step_1))/96

BirdEgg%>%
  filter(Volume<100)%>%
  ggplot()+
  aes(x=AvgLength..cm.,y=Volume)+
  geom_point(size=1,shape=1)

BirdEgg[order(BirdEgg$Volume,decreasing = T),c(4,12)]

################
###FIGURE 1b ###
################

#This correspond to the density plot with bird egg volume in figure 1b

BirdEgg%>%
  ggplot()+
  aes(Volume)+
  geom_density(fill=I("blue"),
               col=I("black"), 
               alpha=I(.2))+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  labs(x= expression("Egg Volume cm"^{3}),y ="Probability density")+
  ggtitle(label="Variation in Egg size among Bird species")+
  theme(
    plot.title = element_text(family="serif", size=27, face="bold.italic"),
    axis.text=element_text(size=20), 
    axis.title = element_text(size=24))


