############STURMER TRAIT DISPERSION ANALYSIS########################################################

#Loading packages and functions

library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)
library(doBy)

#Function to replace the given names of the study by accepeted names
#according to Schlusser website

secondTrial<-
  function(p1,p2){
    temporal<-p1[match(p2[,1],p1[,2]),1]
    
    correctOnes<-temporal[which(!is.na(p1[match(p2[,1],p1[,2]),1]))]
    
    p2[p2[,1]%in%p1[,2],1]<-correctOnes
    
    return(p2)
    
  }

#1. Loading the file and tyding up
  SturmerAllDataCopy<-read.csv("SturmerAllData.csv",header = TRUE,stringsAsFactors = FALSE)
  SturmerAllDataCopy[!SturmerAllDataCopy[,1]%in%AMF_Taxonomy[,1],1]#Just checking if there is  need to change names. 
  #For the this dataset it should not be the case, executing this should return 0


#2. Changing the dataframe of SturmerAllDataCopy, so it could be used for ggplot and adding trait data

  St_All<-cbind(stack(SturmerAllDataCopy[,-1]),
                rep(SturmerAllDataCopy[,1],98))
        names(St_All)<-c("abundance","site","good.names")
  
        St_All<-left_join(St_All,AMF_All_Copy[,c(1,13)]) #Adding the spore area information.

        St_All$site<- #Changing the column "site" as factor and ordering levels along the disturbance gradient
                      #suggested by the author (less to more)

                  factor(St_All$site, levels =c("F_1_10","F_1_14","F_1_15","F_1_16",   
                  "F_1_3","F_1_4","F_1_5","F_1_6","F_1_7","F_1_8","F_1_9","F_3_39",   
                  "F_3_40","F_3_41","F_4_57","F_4_61","F_4_62","OSF_2_23","OSF_3_37","OSF_4_60", 
                  "OSF_5_70b", "OSF_5_77","OSF_6_81","OSF_6_81b","OSF_6_88","OSF_6_88b", "OSF_6_90",
                  "YSF_1_1","YSF_1_2","YSF_2_29","YSF_2_30","YSF_2_31","YSF_3_34","YSF_3_35",
                  "YSF_3_36","YSF_3_38","YSF_3_42","YSF_4_52","YSF_4_53","YSF_4_54",  "YSF_4_56",
                  "YSF_4_63","YSF_4_64","YSF_5_65","YSF_5_66b","YSF_5_68","YSF_5_68b",
                  "YSF_5_69","YSF_5_70","YSF_5_71","YSF_5_71b","YSF_5_73","YSF_5_74","YSF_5_75","YSF_5_76", 
                  "YSF_5_79","YSF_5_80","AF_2_17","AF_2_17b","AF_2_20","AF_2_22","AF_2_24","AF_2_24b","AF_2_25",
                  "AF_5_66","AF_5_67","AF_5_67b","P_6_82","P_6_83","P_6_84","P_6_85","P_6_86","P_6_87","P_6_89",
                  "P_6_91","P_6_92","P_6_93","P_6_94","P_6_95","P_6_96","C_2_18","C_2_19","C_2_19b","C_2_21",
                  "C_2_26","C_2_27","C_2_28","C_2_32","C_2_44","C_3_33","C_4_49","C_4_50","C_4_51","C_4_55",   
                  "C_4_58","C_4_59","C_5_72","C_5_78"))
  
        St_All$Site_Type<-#Creating a new column "Site_Type"
          c(rep("Agroforestry",length(grep("AF_",St_All$site))),
            rep("Crop",length(grep("C_",St_All$site))),
            rep("Old_Sec_Forest",length(grep("OSF_",St_All$site))),
            rep("Pasture",length(grep("P_",St_All$site))),
            rep("Pristine_Forest",length(grep("^F_",St_All$site))),
            rep("Young_Sec_Forest",length(grep("^YSF_",St_All$site))))
        
        St_All$Site_Type<-
          factor(St_All$Site_Type,
                 levels=c("Pristine_Forest","Old_Sec_Forest","Young_Sec_Forest", "Agroforestry","Pasture", "Crop"))

  
#3. Finally performing the ggplot at each location

  St_All%>%
    filter(abundance!=0) %>%
    ggplot(aes(x=site,y=SporeArea,size=abundance,col=good.names))+
    geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
    theme(axis.text.x = element_text(size = 5,angle = 45))+
    ggtitle("Sturmer2001All")
  
  
#Performing ggplot, this time with frequency data (number of times AMF occur at a given land use type)

  St_All %>%
    group_by(Site_Type,good.names) %>%
    summarise(sp_frequency=sum(abundance==1)/length(abundance),SporeArea=mean(SporeArea)) %>%
    filter(sp_frequency>0) %>%
    ggplot(aes(x=Site_Type,y=SporeArea,size=sp_frequency,col=good.names))+
    geom_point(alpha=0.5)+
    scale_y_log10()+
    theme(legend.position = "none")+
    ggtitle("Sturmer2001frequency")

#4. Measuring braycurtis distances among treatments, community weigthed means (CWR) of spore size 
#and IQR. This in order to plot CWM, IQR and IDR against bray. distances.
  
  #Calcuation of community weighted means
  
  #The following code is a two step process to calcuated CWM.NOTE: in this dataset CWM are calculated as the summation
  #of the product: (frequency of occurrence species "i" in site type "s") x (spore size of species i).
  #The same logic follows for calculating IQR and IDR

  IDR=function(x){diff(quantile(x,c(0.1,0.9),na.rm = TRUE,names = FALSE))}#Create a function to calculate IDR
   
CommTraits<-
  St_All%>%
    group_by(Site_Type,good.names) %>%
    summarise(sp_frequency=sum(abundance==1)/length(abundance),SporeArea=mean(SporeArea),
              Abund_Area=sp_frequency*SporeArea) %>%
    filter(sp_frequency>0) %>%# Until here frequency of each species has been multiplied by the Spore size
    group_by(Site_Type) %>%
    summarise(CWMean=sum(Abund_Area),IQR=IQR(Abund_Area),IDR=IDR(Abund_Area));#Here is the summation and calc. of IDR and IQR
    CommTraits<-data.frame(CommTraits);
    names(CommTraits)[1]<-"habitat"

  #Calculating Bray curtis distances. Note that in this study, bray distances 
  #were calculated using frequency data.

    prueba<- #this is step 1 in order to reformat the table for using the function vegdist
      St_All%>%
      group_by(Site_Type,good.names) %>%
      summarise(sp_frequency=sum(abundance==1)/length(abundance));
      prueba<-data.frame(prueba)
    
    transposed<- #this is step 2
        do.call(rbind,
        lapply(split(prueba[,3],prueba$Site_Type),t)
        );rownames(transposed)<-levels(prueba$Site_Type)

  #Finally bray. dist are calculated using vegdist
  dists <- as.matrix(vegdist(transposed,method='bray'))[, 1];
  dists <- data.frame(habitat=names(dists), bray.dist=dists);
  row.names(dists)<-NULL

  
  #Combining the data of CWMean and bray distances
  CommTraits<-merge(dists,CommTraits,by="habitat")
  CommTraits$habitat<-factor(CommTraits$habitat,
                            levels = levels(prueba$Site_Type))
  

  #plotting
  CommTraits%>%
    ggplot(aes(x=bray.dist,y=CWMean,size=IQR,col=habitat,label=habitat))+
    geom_point(alpha=0.5)+#+scale_y_log10()+
    geom_text(aes(label=habitat,size=0.5),hjust=1.5,vjust=1.5)+
    theme(axis.text.x = element_text(size = 5))


  #Saving the info from this study
  
  CommTraitsSturmer2001<-CommTraits
  CommTraitsSturmer2001$Reference<-"Pristine_Forest"
  CommTraitsSturmer2001$Study<-"Sturmer2001"
  
  rm(SturmerAllDataCopy,St_All,transposed,prueba,dists,CommTraits)