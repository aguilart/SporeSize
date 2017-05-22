library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)

#load file and tiding up

Oehl2003<-read.csv("Oehl_etal2003.csv",header = TRUE,stringsAsFactors = FALSE)

Oehl2003$Species<-sub("\\s","_",Oehl2003$Species)
Oehl2003<-Oehl2003[-grep("BR\\d$",Oehl2003$Species),]
Oehl2003$Species[which(Oehl2003$Species=="Acaulospora_scrobiculta")]<-
  "Acaulospora_scrobiculata" 

Oehl2003$Species[which(Oehl2003$Species=="Scutellopora_pellucida")]<-
  "Scutellospora_pellucida"

Oehl2003$Species[which(Oehl2003$Species=="Archeospora_leptoticha")]<-
  "Archaeospora_leptoticha" 


#1. Correct the names

Oehl2003[!Oehl2003[,1]%in%AMF_Taxonomy[,1],1]

Oehl2003<-secondTrial(AMF_Taxonomy,Oehl2003)

#2. Check what new spores data need to be entried

AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],
                                Oehl2003[,1]))),
             c(1,13)]

#3. Change the format of the dataframe to do the desired ggplot

Oehl2003_df<-gather(Oehl2003,key=sites,
                      value=abundance,W:R)

names(Oehl2003_df)[1]<-"good.names"

#4. Adding trait data

Oehl2003_df<-left_join(Oehl2003_df,AMF_All_Copy[,c(1,13)])

Oehl2003_df$sites<-factor(Oehl2003_df$sites,
                          levels = c("W","V","G","O","L","F","S","R"))

#5. Performing the ggplot for all the data
Oehl2003_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("Oehl2003 abundance with singletons")


#Measuring braycurtis distances among treatments, community weigthed means of spore size and IQR

transposed<-t(Oehl2003[,-1]);
transposed<-transposed[match(levels(Oehl2003_df$sites),row.names(transposed)),]

dists <- as.matrix(vegdist(transposed, 
                           method='bray'))[, 1];

dists <- data.frame(habitat=names(dists), bray.dist=dists);

row.names(dists)<-NULL

Oehl2003_df$Abund_Area<-
  Oehl2003_df$SporeArea*
  ave(Oehl2003_df$abundance,Oehl2003_df$sites,FUN = function(x){x/sum(x)})
Oehl2003_df$Abund_Area[Oehl2003_df$Abund_Area==0]<-NA

CommTraits<-cbind(
  data.frame(CWMean=
               tapply(Oehl2003_df$Abund_Area,
                      Oehl2003_df$sites,sum,na.rm=TRUE)),
  
  data.frame(IQR=
               tapply(Oehl2003_df$Abund_Area,
                      Oehl2003_df$sites,IQR,na.rm=TRUE)),
  
  data.frame(IDR=
               tapply(Oehl2003_df$Abund_Area,
                      Oehl2003_df$sites,function(x){diff(quantile(x,c(0.1,0.9),na.rm = TRUE,names = FALSE))})
  ))


CommTraits$habitat<-row.names(CommTraits)
row.names(CommTraits)<-NULL
CommTraits<-CommTraits[,c(4,1,2,3)]
CommTraits<-merge(dists,CommTraits,by="habitat")
CommTraits$habitat<-factor(CommTraits$habitat,
                           levels = levels(Oehl2003_df$sites))

#plot(CommTraits$bray.dist,CommTraits$CWMean)

CommTraitsOehl2003<-CommTraits
CommTraitsOehl2003$Study<-"Oehl2003"
CommTraitsOehl2003

CommTraitsOehl2003%>%
  ggplot(aes(x=bray.dist,y=CWMean,size=IQR,col=habitat))+
  geom_point(alpha=0.5)#+scale_y_log10()+
theme(axis.text.x = element_text(size = 5))

rm(CommTraits,dists,transposed,Oehl2003_df)      
