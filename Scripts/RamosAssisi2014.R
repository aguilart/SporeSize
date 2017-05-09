###RamosAssisi etal 2014 Analysis#####

#load data and tide up
Ramos2014<-read.csv("RamosAssisi_etal2014.csv",
                    header = TRUE,stringsAsFactors = FALSE)

Ramos2014$AMF_species<-sub("\\s","_",Ramos2014$AMF_species)
Ramos2014<-Ramos2014[-grep("sp$",Ramos2014$AMF_species),]
Ramos2014$AMF_species[15]<-"Rhizophagus_diaphanus"
Ramos2014$AMF_species[
  which(Ramos2014$AMF_species=="Funneliformis_geosporus")]<-"Funneliformis_geosporum"
Ramos2014$AMF_species[
  which(Ramos2014$AMF_species=="Glomus_clavispora")]<-"Glomus_clavisporum"

Ramos2014$AMF_species[
  which(Ramos2014$AMF_species=="Scutellosopora_pellucida")]<-"Scutellospora_pellucida"

#1. Correct the names

Ramos2014[!Ramos2014[,1]%in%AMF_Taxonomy[,1],1]

Ramos2014<-secondTrial(AMF_Taxonomy,Ramos2014)

#2. Check what new spores data need to be entried

AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],
                                Ramos2014[,1]))),
             c(1,13)]

#3. Change the format of the dataframe to do the desired ggplot

Ramos2014_df<-gather(Ramos2014,key=sites,
                      value=abundance,X7years:EM)

names(Ramos2014_df)[1]<-"good.names"


#Adding trait data

Ramos2014_df<-left_join(Ramos2014_df,AMF_All_Copy[,c(1,13)])

#Ordering the data from no disturbance to more disturbance
Ramos2014_df$sites<-
  factor(Ramos2014_df$sites,
                           levels=c("TM","EM","X7years","X11years","X14years"))

#Performing the ggplot for all the data
Ramos2014_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("Ramos2014 abundance with singletons")


#Measuring braycurtis distances among treatments, community weigthed means of spore size and IQR

transposed<-t(Ramos2014[,-1]);
transposed<-transposed[match(levels(Ramos2014_df$sites),row.names(transposed)),]

dists <- as.matrix(vegdist(transposed, 
                           method='bray'))[, 1];

dists <- data.frame(habitat=names(dists), bray.dist=dists);

row.names(dists)<-NULL

Ramos2014_df$Abund_Area<-
                        Ramos2014_df$SporeArea*
                        ave(Ramos2014_df$abundance,Ramos2014_df$sites,
                            FUN = function(x){x/sum(x)});
              Ramos2014_df$Abund_Area[Ramos2014_df$Abund_Area==0]<-NA


CommTraits<-cbind(
  data.frame(CWMean=
               tapply(Ramos2014_df$Abund_Area,
                      Ramos2014_df$sites,sum,na.rm=TRUE)),
  
  data.frame(IQR=
               tapply(Ramos2014_df$Abund_Area,
                      Ramos2014_df$sites,IQR,na.rm=TRUE)),
  
  data.frame(IDR=
               tapply(Ramos2014_df$Abund_Area,
                      Ramos2014_df$sites,function(x){diff(quantile(x,c(0.1,0.9),na.rm = TRUE,names = FALSE))})
  ))


CommTraits$habitat<-row.names(CommTraits)
row.names(CommTraits)<-NULL
CommTraits<-CommTraits[,c(4,1,2,3)]
CommTraits<-merge(dists,CommTraits,by="habitat")
CommTraits$habitat<-factor(CommTraits$habitat,
                           levels = levels(Ramos2014_df$sites))


CommTraits%>%
  ggplot(aes(x=bray.dist,y=CWMean,size=IQR,col=habitat))+
  geom_point(alpha=0.5)+#scale_y_log10()+
  theme(axis.text.x = element_text(size = 5))

CommTraitsRamos2014<-CommTraits
CommTraitsRamos2014$Study<-"Ramos2014"
CommTraitsRamos2014

rm(CommTraits,dists,transposed,Ramos2014,Ramos2014_df)      
