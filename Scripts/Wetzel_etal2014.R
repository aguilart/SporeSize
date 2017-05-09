###Wetzel_etal2014

library(ggplot2)
library(tidyr)
library(dplyr)

#loading the data and tiding up

Wetzel2014<-read.csv("Wetzel_etal2014.csv",header = TRUE,stringsAsFactors = FALSE)

Wetzel2014$AMF_species<-sub("\\s","_",Wetzel2014$AMF_species)

Wetzel2014<-Wetzel2014[-grep("\\d|sp\\.$",Wetzel2014$AMF_species),]

Wetzel2014$AMF_species[which(Wetzel2014$AMF_species=="Ambispora_sp<comma> resembling Am. Fennica")]<-
  "Ambispora_fennica"

Wetzel2014$AMF_species[which(Wetzel2014$AMF_species=="Funneliformis_caledonius")]<-
  "Funneliformis_caledonium"

Wetzel2014$AMF_species[which(Wetzel2014$AMF_species=="Funneliformis_geosporus")]<-
  "Funneliformis_geosporum"

Wetzel2014$AMF_species[which(Wetzel2014$AMF_species=="Archaeospora_myriocarpa")]<-
  "Acaulospora_myriocarpa"



#1. Correct the names

Wetzel2014[!Wetzel2014[,1]%in%AMF_Taxonomy[,1],1]

Wetzel2014<-secondTrial(AMF_Taxonomy,Wetzel2014)

#2. Check what new spores data need to be entried

AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],
                                Wetzel2014[,1]))),
             c(1,13)]

#3. Change the format of the dataframe to do the desired ggplot

Wetzel2014_df<-gather(Wetzel2014,key=sites,
                      value=abundance,NoTill:Till)

names(Wetzel2014_df)[1]<-"good.names"


#4. Adding trait data and order the factor levels from no disturbance to more disturbance

Wetzel2014_df<-left_join(Wetzel2014_df,AMF_All_Copy[,c(1,13)])

Wetzel2014_df$sites<-factor(Wetzel2014_df$sites,
                          levels = c("NoTill","Till"))

#5. Performing the ggplot for all the data
Wetzel2014_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("Wetzel2014 abundance")

#Measuring braycurtis distances among treatments, community weigthed means of spore size and IQR

transposed<-t(Wetzel2014[,-1]);
transposed<-transposed[match(levels(Wetzel2014_df$sites),row.names(transposed)),]

dists <- as.matrix(vegdist(transposed, 
                           method='bray'))[, 1];

dists <- data.frame(habitat=names(dists), bray.dist=dists);

row.names(dists)<-NULL

# Wetzel2014_df$Abund_Area<-Wetzel2014_df$abundance*Wetzel2014_df$SporeArea
# Wetzel2014_df$Abund_Area[Wetzel2014_df$Abund_Area==0]<-NA

tapply(Wetzel2014_df$abundance,
       Wetzel2014_df$sites,sum,na.rm=TRUE)

Wetzel2014_df$Abund_Area<-
          Wetzel2014_df$SporeArea*
          ave(Wetzel2014_df$abundance,Wetzel2014_df$sites,FUN = function(x){x/sum(x)})
Wetzel2014_df$Abund_Area[Wetzel2014_df$Abund_Area==0]<-NA

CommTraits<-cbind(
  data.frame(CWMean=
               tapply(Wetzel2014_df$Abund_Area,
                      Wetzel2014_df$sites,sum,na.rm=TRUE)),
  
  data.frame(IQR=
               tapply(Wetzel2014_df$Abund_Area,
                      Wetzel2014_df$sites,IQR,na.rm=TRUE)),
  
  data.frame(IDR=
               tapply(Wetzel2014_df$Abund_Area,
                      Wetzel2014_df$sites,function(x){diff(quantile(x,c(0.1,0.9),na.rm = TRUE,names = FALSE))})
  ))


CommTraits$habitat<-row.names(CommTraits)
row.names(CommTraits)<-NULL
CommTraits<-CommTraits[,c(4,1,2,3)]
CommTraits<-merge(dists,CommTraits,by="habitat")
CommTraits$habitat<-factor(CommTraits$habitat,
                           levels = levels(Wetzel2014_df$sites))

plot(CommTraits$bray.dist,CommTraits$CWMean)
CommTraits%>%
  ggplot(aes(x=bray.dist,y=CWMean,size=IQR,col=habitat))+
  geom_point(alpha=0.5)#+scale_y_log10()+
  theme(axis.text.x = element_text(size = 5))

CommTraitsWetzel<-CommTraits
CommTraitsWetzel$Study<-"Wetzel2014"
CommTraitsWetzel
  
rm(CommTraits,dists,transposed,Wetzel2014,Wetzel2014_df)      
