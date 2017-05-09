################ Spore community analysis Castillo et al 2006##############################

#Note, this study is a presence absence study, no data on abundances or frequencies


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

#1. loading the data and tidyng up, correcting names

Castillo2006<-read.csv("Castillo_etal2006.csv",header = TRUE,stringsAsFactors = FALSE)
              Castillo2006$AMF_species<-sub("\\s","_",Castillo2006$AMF_species)
              Castillo2006<-Castillo2006[-grep("spA$|spB$",Castillo2006$AMF_species),]
              Castillo2006$AMF_species[which(Castillo2006$AMF_species=="Acaulospor_laevis")]<-"Acaulospora_laevis"
              Castillo2006$AMF_species[which(Castillo2006$AMF_species=="Acaulospora_longlula")]<-"Acaulospora_longula"
              Castillo2006$AMF_species[which(Castillo2006$AMF_species=="Scutellospora_dipurpurascens")]<-"Scutellospora_dipurpurescens"
              Castillo2006$AMF_species[which(Castillo2006$AMF_species=="Scutellospora_auroglobosa")]<-"Scutellospora_aurigloba"
              
              #Here you see the names that need to be corrected
              Castillo2006[!Castillo2006[,1]%in%AMF_Taxonomy[,1],1]
              
              #Here you replace those names
              Castillo2006<-secondTrial(AMF_Taxonomy,Castillo2006)


#2. Change the format of the dataframe for ggplot, add trait data

Castillo2006_df<-gather(Castillo2006,key=sites,
                      value=abundance,EF:GR);
                      names(Castillo2006_df)[1]<-"good.names"

                #Adding trait data 
                Castillo2006_df<-left_join(Castillo2006_df,AMF_All_Copy[,c(1,13)])
                
                #Order the factor levels from no disturbance to more disturbance
                Castillo2006_df$sites<-factor(Castillo2006_df$sites,
                                          levels = c("EF","DF","GR"))

#3. Performing the ggplot for all the data
Castillo2006_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("Castillo2006 presence")


rm(Castillo2006,Castillo2006_df)