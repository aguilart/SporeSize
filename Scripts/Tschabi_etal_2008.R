# ##Merging all the data from Tschabi etal 2008
# Sudan<-read.csv("TchabiSudanSavana.csv", header = TRUE,stringsAsFactors = FALSE)
# NGuinea<-read.csv("TchabiNortherGuinea.csv", header = TRUE,stringsAsFactors = FALSE)
# SGuinea<-read.csv("TchabiSouthernGuinea.csv", header = TRUE,stringsAsFactors = FALSE)
# TchabiAllData<-Reduce(function(...)merge(...,by="AMF_species",all=TRUE),
#                      list(Sudan,NGuinea,SGuinea))
# 
# #Changing the NA for 0 in the abundance data
# TchabiAllData[,-1][(is.na(as.matrix(TchabiAllData[,-1])))]<-0
# #Replacing empty blanks in the names by an underscore
# TchabiAllData[,1]<-sub(" ","_",TchabiAllData[,1])
# 
# #Checking the names according to the taxonomic reference (AMF_Taxonomy)
# 
# TchabiAllData[!TchabiAllData[,1]%in%AMF_Taxonomy[,1],1]
# 
# #Correcting typos
# which(TchabiAllDataCopy$AMF_species=="Scutellospora_callospora")
# which(TchabiAllDataCopy$AMF_species=="Glomus_hyderabadense")
# which(TchabiAllDataCopy$AMF_species=="Glomus_brohulti")
# which(TchabiAllDataCopy$AMF_species=="Glomus_pachycauilis")
# 
# TchabiAllDataCopy[38,1]<-"Glomus_hyderabadensis"
# TchabiAllDataCopy[c(20,25),1]<-"Glomus_brohultii"
# TchabiAllDataCopy[46,1]<-"Sclerocystis_pachycaulis"
# TchabiAllDataCopy[59,1]<-"Scutellospora_calospora"

# #Checking the remaining names that do not match correct taxonomy, deleting entries as "sp"
#TchabiAllDataCopy[!TchabiAllDataCopy[,1]%in%AMF_Taxonomy[,1],1]
# 
# TchabiAllDataCopy<-TchabiAllDataCopy[-c(13,51,64,65),]
 
# write.csv(TchabiAllData,"TchabiAllData_etal2008.csv")#This is the dataframe that would need to be read to re-do the analysis

################################TCHABI DATA ANALYSIS#######################################

library(ggplot2)
library(tidyr)
library(dplyr)

TchabiAllDataCopy<-read.csv("TchabiAllData_etal2008.csv",header = TRUE,stringsAsFactors = FALSE)
#Correcting names according to AMF_Taxonomy using secondTrial

TchabiAllDataCopy<-secondTrial(AMF_Taxonomy,TchabiAllDataCopy)

# #Checking the remaining names that do not match correct taxonomy, deleting entries as "sp"
TchabiAllDataCopy[!TchabiAllDataCopy[,1]%in%AMF_Taxonomy[,1],1]
# 
# TchabiAllDataCopy<-TchabiAllDataCopy[-c(13,51,64,65),]

names(TchabiAllDataCopy)[2:9]<-sub(".x$","_Sudan",names(TchabiAllDataCopy)[2:9])
names(TchabiAllDataCopy)[10:17]<- sub(".y$","_NGuinea",names(TchabiAllDataCopy)[10:17])
names(TchabiAllDataCopy)[18:25]<-c("NaturalForest1_SGuinea","NaturalForest2_SGuinea","NaturalForest3_SGuinea",
                                   "Yam_SGuinea","MixedCropping_SGuinea","Peanuts_SGuinea","Cotton_SGuinea",
                                   "Fallow_SGuinea")


#Changing the format of the dataframe for the ggplot of spore sizes
TchabiData<-cbind(stack(TchabiAllDataCopy[,-1]),
              rep(TchabiAllDataCopy[,1],24))

names(TchabiData)<-c("abundance","site","good.names")
head(TchabiData)
rm(TchabiAllDataCopy)

#Adding the spore trait information

TchabiData<-left_join(TchabiData,AMF_All_Copy[,c(1,13)])

# #Checking which names have not spore data assign in AMF_All_Copy
# 
AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],TchabiData[,3]))),c(1,13)]

#Organizing the column factor so it follows the pattern less to more disturbance
TchabiData$site<-factor(TchabiData$site,
                        levels=c(
                          levels(TchabiData$site)[grep("NaturalForest",levels(TchabiData$site))],
                          levels(TchabiData$site)[grep("Mixed",levels(TchabiData$site))],
                          levels(TchabiData$site)[grep("Yam",levels(TchabiData$site))],
                          levels(TchabiData$site)[grep("Peanuts",levels(TchabiData$site))],
                          levels(TchabiData$site)[grep("Fallow",levels(TchabiData$site))],
                          levels(TchabiData$site)[grep("Cotton",levels(TchabiData$site))]))

#Creating the column Site_Type
TchabiData$Site_Type<-TchabiData$site
TchabiData$Site_Type<-sub("\\d_Sudan","Sudan",TchabiData$Site_Type)
TchabiData$Site_Type<-sub("\\d_NGuinea","NGuinea",TchabiData$Site_Type)
TchabiData$Site_Type<-sub("\\d_SGuinea","SGuinea",TchabiData$Site_Type)

TchabiData$Site_Type<-
  factor(TchabiData$Site_Type,
         levels=c("NaturalForestSudan","NaturalForestNGuinea","NaturalForestSGuinea",
                  "MixedCropping_Sudan","MixedCropping_NGuinea","MixedCropping_SGuinea",
                  "Yam_Sudan","Yam_NGuinea","Yam_SGuinea",
                  "Peanuts_Sudan","Peanuts_NGuinea","Peanuts_SGuinea",
                  "Fallow_Sudan","Fallow_NGuinea","Fallow_SGuinea",
                  "Cotton_Sudan","Cotton_NGuinea","Cotton_SGuinea"
         ))



#Doing ggplot to have a view of all sites

ggplot(filter(TchabiData,abundance!=0),
       aes(x=site,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5,angle=45))+
  ggtitle("Tchabi2008All")

#Doing ggplot, this time grouping by the type of site



#Creating the graphic
TchabiData %>%
  group_by(Site_Type,good.names) %>%
  summarise(sp_frequency=sum(abundance==1)/length(abundance),SporeArea=mean(SporeArea)) %>%
  filter(sp_frequency>0) %>%
  ggplot(aes(x=Site_Type,y=SporeArea,size=sp_frequency,col=good.names))+
  geom_point(alpha=0.5)+
  scale_y_log10()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 7,angle=45))+
  ggtitle("Tchabietal2008Frequency")

#removing the file from the workspace
rm(TchabiData)
