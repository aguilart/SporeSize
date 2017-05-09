###########################da Silva et al 2012 Spore analysis############################################

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

#1. loading the data, tidyng up and replacing names

daSilva2012<-read.csv("DaSilva_etal2012.csv",header = TRUE,stringsAsFactors = FALSE)
            daSilva2012$AMF_species<-sub("\\s","_",daSilva2012$AMF_species)
            daSilva2012$AMF_species[which(daSilva2012$AMF_species=="Funneliformis_geosporus")]<-
              "Funneliformis_geosporum"
            daSilva2012$AMF_species[which(daSilva2012$AMF_species=="Funneliformis_halonatus")]<-
              "Glomus_halonatum"
            daSilva2012$AMF_species[which(daSilva2012$AMF_species=="Orbispora_permambucana")]<-
              "Orbispora_pernambucana"
            daSilva2012$AMF_species[which(daSilva2012$AMF_species=="Glomus_glomeratum")]<-
              "Glomus_glomerulatum"

            #Here you see what names need to be replaced
            daSilva2012[!daSilva2012[,1]%in%AMF_Taxonomy[,1],1]
  
            #Here you replace those names
            daSilva2012<-secondTrial(AMF_Taxonomy,daSilva2012)

#3. Change the format of the dataframe for ggplot; adding trait data

daSilva2012_df<-gather(daSilva2012,key=sites,
                      value=abundance,NRF:SSR)
                names(daSilva2012_df)[1]<-"good.names"

                #Adding trait data 
                daSilva2012_df<-left_join(daSilva2012_df,AMF_All_Copy[,c(1,13)])
                
                #order the factor levels from no disturbance to more disturbance
                daSilva2012_df$sites<-factor(daSilva2012_df$sites,
                                          levels = c("NRF","R20","R8","SSR"))

daSilva2012_df$abundance<-daSilva2012_df$abundance/18

#5. Performing the ggplot for all the data
daSilva2012_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("daSilva2012 frequency")


rm(daSilva2012)