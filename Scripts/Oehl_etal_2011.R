##############Oehl 2011 community trait analysis#############

library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)

#Function to replace the given names of the study by accepeted names
#according to Schlusser website

secondTrial<-
  function(p1,p2){
    temporal<-p1[match(p2[,1],p1[,2]),1]
    
    correctOnes<-temporal[which(!is.na(p1[match(p2[,1],p1[,2]),1]))]
    
    p2[p2[,1]%in%p1[,2],1]<-correctOnes
    
    return(p2)
    
  }

#Function 

calc.distancesOehl2011_1<-function(x){
  datos<-as.matrix(vegdist(as.matrix(x[,-c(1:4)],method="bray")))
  datos<-data.frame(bray.dist=(datos[,grep("Grassland",colnames(datos))]))
  datos$Reference<-rownames(datos)[datos$bray.dist==0]
  datos$habitat<-rownames(datos)
  return(datos)
}

calc.distancesOehl2011_2<-function(x){
  datos<-as.matrix(vegdist(as.matrix(x[,-c(1:4)],method="bray")))
  datos1<-data.frame(bray.dist=(datos[,grep("Grassland",colnames(datos))][,1]))
  datos2<-data.frame(bray.dist=(datos[,grep("Grassland",colnames(datos))][,2]))
  datos1$Reference<-rownames(datos1)[datos1$bray.dist==0]
  datos1$habitat<-rownames(datos1)
  datos2$Reference<-rownames(datos2)[datos2$bray.dist==0]
  datos2$habitat<-rownames(datos2)
  datos<-rbind(datos1,datos2)
  
  return(datos)
}

##
#######################LOADING FILE AND TYDING UP##################################################

Oehl2010SporeAbundanceMatrix<-read.csv("Oehl2010SporeAbundanceMatrix.csv",header = TRUE,stringsAsFactors = FALSE)

#transposing it so it has the same format as in the other cases
  nombres<-Oehl2010SporeAbundanceMatrix[,1]
  Oehl2010SporeAbundanceMatrix<-t(Oehl2010SporeAbundanceMatrix[,-1])
  colnames(Oehl2010SporeAbundanceMatrix)<-nombres
  Oehl2010SporeAbundanceMatrix<-data.frame(Oehl2010SporeAbundanceMatrix)
  Oehl2010SporeAbundanceMatrix$AMF.species<-rownames(Oehl2010SporeAbundanceMatrix)
  rownames(Oehl2010SporeAbundanceMatrix)<-NULL
  Oehl2010SporeAbundanceMatrix<-Oehl2010SporeAbundanceMatrix[c(17,1:16)]
  rm(nombres)


  
#Replacing the points in the names by underscore
  Oehl2010SporeAbundanceMatrix[,1]<-sub("\\.","_",Oehl2010SporeAbundanceMatrix[,1])
  
#Identifying which names do not match the taxonomic references
  Oehl2010SporeAbundanceMatrix[!Oehl2010SporeAbundanceMatrix[,1]%in%AMF_Taxonomy[,1],1]
  
  #Changing by hand some typos, extra characters and related
    Oehl2010SporeAbundanceMatrix$
    AMF.species[which(Oehl2010SporeAbundanceMatrix$
                        AMF.species=="Glomus_occultum.group..occultum.and.albidum.")]<-
                                        "Glomus_occultum"
  
    Oehl2010SporeAbundanceMatrix$
      AMF.species[which(Oehl2010SporeAbundanceMatrix$
                          AMF.species=="Gloms_sp.BR24.resemb..Melanosporum")]<-
      "Glomus_melanosporum"
  
    Oehl2010SporeAbundanceMatrix$
      AMF.species[which(Oehl2010SporeAbundanceMatrix$
                          AMF.species=="Glomus_sp.BR.12.resemb..Epigaeum")]<-
      "Glomus_epigaeum"
    
    Oehl2010SporeAbundanceMatrix$
      AMF.species[which(Oehl2010SporeAbundanceMatrix$
                          AMF.species=="Glomus_sp.BR.18.resemb..Tortuosum")]<-
      "Glomus_tortuosum"
    
    Oehl2010SporeAbundanceMatrix$
      AMF.species[which(Oehl2010SporeAbundanceMatrix$
                          AMF.species=="Ambispora_sp.BR10.resemb..Appendicula")]<-
      "Ambispora_appendicula"
    
    Oehl2010SporeAbundanceMatrix$
      AMF.species[which(Oehl2010SporeAbundanceMatrix$
                          AMF.species=="Acaulospora_sp.BR.20.resemb..Rehmii")]<-
      "Acaulospora_rehmii"
    
    Oehl2010SporeAbundanceMatrix$
      AMF.species[which(Oehl2010SporeAbundanceMatrix$
                          AMF.species=="Glomus_.hoi.")]<-
      "Glomus_hoi"
    
    Oehl2010SporeAbundanceMatrix$
      AMF.species[which(Oehl2010SporeAbundanceMatrix$
                          AMF.species=="Archaeospora_trapppei")]<-
      "Archaeospora_trappei"
    
#Correcting names according to AMF_Taxonomy using secondTrial
  Oehl2010SporeAbundanceMatrix<-secondTrial(AMF_Taxonomy,Oehl2010SporeAbundanceMatrix)

#Identifying which names do not match the taxonomic references
  Oehl2010SporeAbundanceMatrix[!Oehl2010SporeAbundanceMatrix[,1]%in%AMF_Taxonomy[,1],1]

  
######  STEP I: COMPARING SPORE RANGES ACROSS PERTURBATION GRADIENT    #########

  
#Changing the format of the dataframe (in the previous versions I use stack base function)
  Oehlspecies_df<-gather(Oehl2010SporeAbundanceMatrix,key=sites,
                     value=abundance,Grassland.PShg:Arable.JLa)
  
  names(Oehlspecies_df)[1]<-"good.names"
  
  
#Adding trait information
  Oehlspecies_df<-left_join(Oehlspecies_df,AMF_All_Copy[,c(1,13)])
  
#Making the sites as factor column
  Oehlspecies_df$sites<-
  factor(Oehlspecies_df$sites,levels=c("Grassland.PShg","Grassland.TSg" ,"Grassland.DGg1", "Grassland.JLhg","Grassland.JLg",
                                       "Grassland.PSg","Grassland.GRhg","Grassland.DGg2","Grassland.AGg",
                                       "Arable.JLha","Arable.JLa",
                                       "Arable.PSa","Arable.TSa","Arable.DGa2",
                                       "Arable.DGa1","Arable.AGa"))


#Creating a new column for land use type according to the criteria of Oehl etal 2010
  Oehlspecies_df$LandUSe<-NA
  Oehlspecies_df$LandUSe[which(Oehlspecies_df$sites=="Grassland.PShg"|
                                Oehlspecies_df$sites=="Grassland.TSg"|
                                Oehlspecies_df$sites=="Grassland.DGg1"|
                                Oehlspecies_df$sites=="Grassland.JLhg"|
                                Oehlspecies_df$sites=="Grassland.JLg")]<-"very_low"
  Oehlspecies_df$LandUSe[which(Oehlspecies_df$sites=="Grassland.PSg"|
                                 Oehlspecies_df$sites=="Grassland.GRhg"|
                                 Oehlspecies_df$sites=="Grassland.DGg2"|
                                 Oehlspecies_df$sites=="Grassland.AGg")]<-"low"
  Oehlspecies_df$LandUSe[which(Oehlspecies_df$sites=="Arable.JLha"|
                                 Oehlspecies_df$sites=="Arable.JLa")]<-"moderate"
  Oehlspecies_df$LandUSe[which(Oehlspecies_df$sites=="Arable.PSa"|
                                 Oehlspecies_df$sites=="Arable.TSa"|
                                 Oehlspecies_df$sites=="Arable.DGa2")]<-"high"
  Oehlspecies_df$LandUSe[which(Oehlspecies_df$sites=="Arable.DGa1"|
                                 Oehlspecies_df$sites=="Arable.AGa")]<-"very_high"
  
  Oehlspecies_df$LandUSe<-
    factor(Oehlspecies_df$LandUSe,
           levels = c("very_low","low","moderate","high","very_high"))

#Creating a new column for presence/presence absence
  
  Oehlspecies_df$presence<-Oehlspecies_df$abundance
    Oehlspecies_df$presence[Oehlspecies_df$presence>1] <-1
  

Oehlspecies_df$Soil_Type<-
 c(rep("PS",length(grep(".PS",Oehlspecies_df$site))),
    rep("TS",length(grep(".TS",Oehlspecies_df$site))),
    rep("GR",length(grep(".GR",Oehlspecies_df$site))),
    rep("DG",length(grep(".DG",Oehlspecies_df$site))),
    rep("AG",length(grep(".AG",Oehlspecies_df$site))),
    rep("JL",length(grep(".JL",Oehlspecies_df$site)))
    )
    
#Checking which names have not spore data assign in AMF_All_Copy

#AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],Oehlspecies_df[,1]))),c(1,13)]
  

#Performing the ggplot for all the data
    Oehlspecies_df %>%
    filter(abundance!=0) %>%
    ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
    geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
    theme(axis.text.x = element_text(size = 5))+
    ggtitle("Oehl2010 abundance with singletons")
    
#Performing a ggplot for frequency data per land use type
    
    Oehlspecies_df %>%
      group_by(LandUSe,good.names)%>%
      summarise(sp_frequency=sum(presence==1)/length(presence),SporeArea=mean(SporeArea))%>%
      filter(sp_frequency>0)%>%
      ggplot(aes(x=LandUSe,y=SporeArea,size=sp_frequency,col=good.names))+
      geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
      theme(axis.text.x = element_text(size = 10))+
      ggtitle("Oehl2010 frequency in land use with singletons")

###  STEP II: COMPARING BRAYCURTIS DISTANCES FROM EACH DISTURBANCE SITE TO CONTROL REFERENCE  ####
    
    
#Measuring braycurtis distances among treatments, community weigthed means of spore size and IQR
    
transposed<-data.frame(t(Oehl2010SporeAbundanceMatrix[,-1]))
transposed$site<-row.names(transposed)
transposed<-transposed[,c(52,1:51)]

transposed$Soil_Type<-
  c(rep("PS",length(grep(".PS",transposed$site))),
    rep("TS",length(grep(".TS",transposed$site))),
    rep("GR",length(grep(".GR",transposed$site))),
    rep("DG",length(grep(".DG",transposed$site))),
    rep("AG",length(grep(".AG",transposed$site))),
    rep("JL",length(grep(".JL",transposed$site)))
  )    

transposed$LandUSe<-NA
transposed$LandUSe[which(transposed$site=="Grassland.PShg"|
                               transposed$site=="Grassland.TSg"|
                               transposed$site=="Grassland.DGg1"|
                               transposed$site=="Grassland.JLhg"|
                               transposed$site=="Grassland.JLg")]<-"very_low"
transposed$LandUSe[which(transposed$site=="Grassland.PSg"|
                               transposed$site=="Grassland.GRhg"|
                               transposed$site=="Grassland.DGg2"|
                               transposed$site=="Grassland.AGg")]<-"low"
transposed$LandUSe[which(transposed$site=="Arable.JLha"|
                               transposed$site=="Arable.JLa")]<-"moderate"
transposed$LandUSe[which(transposed$site=="Arable.PSa"|
                               transposed$site=="Arable.TSa"|
                               transposed$site=="Arable.DGa2")]<-"high"
transposed$LandUSe[which(transposed$site=="Arable.DGa1"|
                               transposed$site=="Arable.AGa")]<-"very_high"


transposed$Site_Type<-NA
transposed$Site_Type[grep("Grassland",transposed$site)]<-"Grassland"
transposed$Site_Type[grep("Arable",transposed$site)]<-"Arable"
transposed$Site_Type<-factor(transposed$Site_Type,
                             levels = c("Grassland","Arable"))

transposed<-transposed[order(transposed$Site_Type,decreasing = FALSE),]
transposed<-transposed[,c(1,53,54,55,2:52)]


transposed_splitted<-split(transposed,transposed$Soil_Type)

dists<-rbind(
  calc.distancesOehl2011_1(transposed_splitted$AG),
  calc.distancesOehl2011_2(transposed_splitted$DG),
  calc.distancesOehl2011_2(transposed_splitted$JL),
  calc.distancesOehl2011_2(transposed_splitted$PS),
  calc.distancesOehl2011_1(transposed_splitted$TS),
  #data.frame(bray.dist=0.0000000,Reference="Grassland.GRhg",habitat="Grassland.GRhg"),
  make.row.names=FALSE)


#Calculation of community weigthed means, IQR and IDR

Oehlspecies_df$Abund_Area<-
  Oehlspecies_df$SporeArea*
  ave(Oehlspecies_df$abundance,Oehlspecies_df$sites,
      FUN = function(x){x/sum(x)});
Oehlspecies_df$Abund_Area[Oehlspecies_df$Abund_Area==0]<-NA


CommTraits<-cbind(
        data.frame(CWMean=
                     tapply(Oehlspecies_df$Abund_Area,
                            Oehlspecies_df$sites,sum,na.rm=TRUE)),
        
        data.frame(IQR=
                     tapply(Oehlspecies_df$Abund_Area,
                            Oehlspecies_df$sites,IQR,na.rm=TRUE)),
        
        data.frame(IDR=
                     tapply(Oehlspecies_df$Abund_Area,
                            Oehlspecies_df$sites,function(x){diff(quantile(x,c(0.1,0.9),na.rm = TRUE,names = FALSE))})
      ))

        
        CommTraits$habitat<-row.names(CommTraits)
        row.names(CommTraits)<-NULL
        CommTraits<-CommTraits[,c(4,1,2,3)]
        
        CommTraits<-left_join(CommTraits,dists)
        
        CommTraits<-rbind(
        CommTraits[grep("Grassland",CommTraits$habitat),][
          CommTraits[grep("Grassland",CommTraits$habitat),]$bray.dist==0,],
        CommTraits[grep("Arable",CommTraits$habitat),])
        #CommTraits<-merge(dists,CommTraits,by="habitat")
        CommTraits<-
          CommTraits[c(1,5,2,3,4,6)]
    
        
        CommTraits$habitat<-factor(CommTraits$habitat,
                                   levels = levels(Oehlspecies_df$sites))
        

  CommTraitsOehl2011<-CommTraits    
  CommTraitsOehl2011$Study<-"Oehl2011"
  CommTraitsOehl2011
  CommTraitsOehl2011$habitatType<-
  
rm(CommTraits,dists,Oehl2010SporeAbundanceMatrix,Oehlspecies_df,transposed)      

  CommTraitsOehl2011$habitatType<-c(rep("Grassland",9),rep("ArableLand",12))
  
#Plotting only comparisons between adjacent land use types    

PlotOehl<-
CommTraitsOehl2011[c(1:6,8,9,10,13,14,15,18,19,21),]%>%
  ggplot(aes(x=bray.dist,y=CWMean,size=IQR,col=Reference))+
  geom_point(alpha=0.5)+#scale_y_log10()+
  geom_text(aes(label=habitat,size=500),hjust=-0.2,vjust=-0.5)+
  theme(axis.text.x = element_text(size = 5))+
  scale_size_continuous(guide = FALSE)+
  scale_colour_discrete(guide = FALSE)+
  ggtitle(paste("Temperate grasslands vs","\n","arable land"))