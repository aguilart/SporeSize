############Bonfin et al 2013 Analysis############################

library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)
library(doBy)
# install.packages("devtools")
# devtools::install_github("richfitz/remake")
# devtools::install_github("richfitz/storr")
# 
# remake::make()

#Function to replace the given names of the study by accepeted names
#according to Schlusser website

secondTrial<-
  function(p1,p2){
    temporal<-p1[match(p2[,1],p1[,2]),1]
    
    correctOnes<-temporal[which(!is.na(p1[match(p2[,1],p1[,2]),1]))]
    
    p2[p2[,1]%in%p1[,2],1]<-correctOnes
    
    return(p2)
    
  }

#loading file

Bonfin2013<-read.csv("Bonfin_etal2013.csv",
                     header = TRUE,stringsAsFactors = FALSE)

#Cleaning up the dataframe
Bonfin2013$site[1:4]<-sub("^","Rainy_",Bonfin2013$site[1:4])
Bonfin2013$site[5:8]<-sub("^","Dry_",Bonfin2013$site[5:8])

Bonfin2013$Season<-NULL
Bonfin2013$SiteType<-NULL
Bonfin2013$SubSample.Actual.amount.where.spores.were.extracted.<-NULL
Bonfin2013$units<-NULL


nombres<-Bonfin2013[,1]
Bonfin2013<-t(Bonfin2013[,-1])
colnames(Bonfin2013)<-nombres
Bonfin2013<-data.frame(Bonfin2013)
Bonfin2013$AMF.species<-rownames(Bonfin2013)
rownames(Bonfin2013)<-NULL
Bonfin2013<-Bonfin2013[c(9,1:8)]
rm(nombres)

Bonfin2013$AMF.species<-sub("^A\\.","Acaulospora_",Bonfin2013$AMF.species)
Bonfin2013$AMF.species<-sub("^Gi\\.","Gigaspora_",Bonfin2013$AMF.species)
Bonfin2013$AMF.species<-sub("_\\.","_",Bonfin2013$AMF.species)
Bonfin2013$AMF.species<-sub("\\.","_",Bonfin2013$AMF.species)
Bonfin2013$AMF.species[6]<-"Acaulospora_spinosa"
Bonfin2013$AMF.species[14]<-"Scutellospora_pellucida"
Bonfin2013$AMF.species[9]<-"Gigaspora_rosea"
Bonfin2013<-Bonfin2013[-c(11,12,17:21),]

#Correcting names

Bonfin2013<-secondTrial(AMF_Taxonomy,Bonfin2013)

Bonfin2013[!Bonfin2013[,1]%in%AMF_Taxonomy[,1],1]



#Changing the format of the the dataframe

Bonfin2013_df<-gather(Bonfin2013,key=sites,
                       value=abundance,Rainy_NT:Dry_R05)

names(Bonfin2013_df)[1]<-"good.names"

Bonfin2013_df$sites<-factor(Bonfin2013_df$sites,
                            levels = unique(Bonfin2013_df$sites))


#Adding trait data

Bonfin2013_df<-left_join(Bonfin2013_df,AMF_All_Copy[,c(1,13)])


#Checking which spore data is missing
AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],
                                Bonfin2013_df[,1]))),
             c(1,13)]

#NOTE!!!! REMEMBER TO UPDATE THE TRAIT DATA IF NECESSARY AND RE-ASSINING TO THE DATA OF INTEREST

#Performing the graphic (plus extra things particular to this dataset)

#Creating a new column for presence/presence absence

Bonfin2013_df$presence<-Bonfin2013_df$abundance
Bonfin2013_df$presence[Bonfin2013_df$presence>1] <-1

#Performing the ggplot for all the data
Bonfin2013_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("Bonfin2013 abundance with singletons")


#Measuring braycurtis distances among treatments, community weigthed means of spore size and IQR

mat<-t(
  cbind(
    Bonfin2013[,grep("NT$",names(Bonfin2013))][1]+
      Bonfin2013[,grep("NT$",names(Bonfin2013))][2],
    
    Bonfin2013[,grep("R20$",names(Bonfin2013))][1]+
      Bonfin2013[,grep("R20$",names(Bonfin2013))][2],
    
    Bonfin2013[,grep("R10$",names(Bonfin2013))][1]+
      Bonfin2013[,grep("R10$",names(Bonfin2013))][2],
    
    Bonfin2013[,grep("R05$",names(Bonfin2013))][1]+
      Bonfin2013[,grep("R05$",names(Bonfin2013))][2]));
colnames(mat)<-Bonfin2013[[1]]


dists <- as.matrix(vegdist(mat, method='bray'))[, 1]
names(dists) <- sub("^Rainy_","", names(dists))
dists <- data.frame(habitat=names(dists), bray.dist=dists)
rownames(dists)<-NULL

#having just one dataframe that combines the abundance during the rainy and dry season
Bonfin2013_df2<-cbind(Bonfin2013_df[1:60,c(1:2)],
                      data.frame(abundance=Bonfin2013_df$abundance[1:60]+Bonfin2013_df$abundance[61:120]),
                      SporeArea=Bonfin2013_df[1:60,4])

Bonfin2013_df2<-droplevels(Bonfin2013_df2)

Bonfin2013_df2$Abund_Area<-
  Bonfin2013_df2$SporeArea*
  ave(Bonfin2013_df2$abundance,Bonfin2013_df2$sites,FUN = function(x){x/sum(x)})
Bonfin2013_df2$Abund_Area[Bonfin2013_df2$Abund_Area==0]<-NA

CommTraits<-cbind(
  data.frame(CWMean=
               tapply(Bonfin2013_df2$Abund_Area,
                      Bonfin2013_df2$sites,sum,na.rm=TRUE)),
  
  data.frame(IQR=
               tapply(Bonfin2013_df2$Abund_Area,
                      Bonfin2013_df2$sites,IQR,na.rm=TRUE)),
  
  data.frame(IDR=
               tapply(Bonfin2013_df2$Abund_Area,
                      Bonfin2013_df2$sites,function(x){diff(quantile(x,c(0.1,0.9),na.rm = TRUE,names = FALSE))})))


CommTraits$habitat<-row.names(CommTraits)
row.names(CommTraits)<-NULL
CommTraits<-CommTraits[,c(4,1,2,3)]
CommTraits$habitat<-sub("Rainy_","",CommTraits$habitat)

CommTraits<-merge(dists,CommTraits,by="habitat")


CommTraits%>%
  ggplot(aes(x=bray.dist,y=CWMean,size=IQR,col=habitat))+
  geom_point(alpha=0.5)+scale_y_log10()+
  theme(axis.text.x = element_text(size = 5))

CommTraitsBonfin2013<-CommTraits
CommTraitsBonfin2013$Study<-"Bonfin2013"
CommTraitsBonfin2013$Reference<-"NaturalForest"
CommTraitsBonfin2013<-CommTraitsBonfin2013[,c(1:5,7,6)]

rm(Bonfin2013,Bonfin2013_df,Bonfin2013_df2,CommTraits,dists,mat)


######Extra-code from Will

# Bonfin2013_df2 %>%
#   group_by(sites) %>%
#   filter(AbundArea>0) %>%
#   summarise(CWMean=mean(AbundArea,na.rm=TRUE), IQR=IQR(AbundArea,na.rm=TRUE)) -> CommTraitsdplr
