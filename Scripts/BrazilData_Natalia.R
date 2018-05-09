###################################################################################
################Natalia´s dataset #################################################
###################################################################################

#loading packages

library(tidyverse)


#loading files

brazildata<- 
        #The following is the most updated file from Natalia´s data (as for 16th August 2017)
        #it contains 31 studies. Abiotic variables come from other data sets such as world clim
        #and some vegatation maps from Brazil
        
        read.csv("NataliaData/CheckList_Natalia_Official.csv",header = T,
                 stringsAsFactors = F,dec=".")
      
        brazildata<-brazildata[,c(3,2,1,4:67)]
        brazildata[,1]<-sub("\\s","_",brazildata[,1])
        brazildata[,1]<-sub("\\s","",brazildata[,1])
        brazildata[,1]<-sub("\\s","",brazildata[,1])
        brazildata[,1]<-sub("\\s","",brazildata[,1])
        
        brazildata$AMF_changeNamesNotes<-NA
        brazildata$AMF_changeNamesNotes[grep("aff.",brazildata$Species)]<-"original names recorded as aff."
        brazildata[,1]<-sub("aff.","",brazildata[,1])
        
        brazildata[480,1]<-"Glomus_halonatum"
        brazildata[483,1]<-"Glomus_invermaium"
        brazildata[c(330,344,490),1]<-"Scutellospora_dipurpurencens"
        brazildata[
        which(brazildata[,1]=="Gigaspora_Albid"),1]<-"Gigaspora_albida"
        brazildata[
          which(brazildata[,1]=="S._Fulgida"),1]<-"Scutellospora_fulgida"
        brazildata[
          which(brazildata[,1]=="S._verrucosa"),1]<-"Scutellospora_verrucosa"
        brazildata[
          which(brazildata[,1]=="S._persica"),1]<-"Scutellospora_persica"
        brazildata[
          which(brazildata[,1]=="Acaulospora_Koskei"),1]<-"Acaulospora_koskei"
        brazildata[
          which(brazildata[,1]=="glomus_heterosporum"),1]<-"Glomus_heterosporum"
        brazildata[
          which(brazildata[,1]=="Glomusetunicatum"),1]<-"Glomus_etunicatum"
        brazildata[
          which(brazildata[,1]=="Glomus_sinuosu"),1]<-"Glomus_sinuosum"
        brazildata[
          which(brazildata[,1]=="A._scrobiculata"),1]<-"Acaulospora_scrobiculata"
        brazildata[
          which(brazildata[,1]=="A._excavata"),1]<-"Acaulospora_excavata"
        brazildata[
          which(brazildata[,1]=="G._etunicatum"),1]<-"Glomus_etunicatum"
        brazildata[
          which(brazildata[,1]=="Gigaspora_Gigantea"),1]<-"Gigaspora_gigantea"
        brazildata[
          which(brazildata[,1]=="Glomus_microcarpum"),1]<-"Gl._Microcarpum"
        brazildata[
          which(brazildata[,1]=="Gigaspora_Glomerulatum"),1]<-"Gigaspora_glomerulatum"
        brazildata[
          which(brazildata[,1]=="Acaulospora_Longula"),1]<-"Acaulospora_longula"
        brazildata[
          which(brazildata[,1]=="Scutellospora_Coremioides"),1]<-"Scutellospora_Coremioides"
        brazildata[
          which(brazildata[,1]=="Acaulospora_Morrowiae"),1]<-"Acaulospora_morrowiae"
        brazildata[
          which(brazildata[,1]=="Gigaspora_Gigantea"),1]<-"Gigaspora_gigantea"
        brazildata[
          which(brazildata[,1]=="Gl._Microcarpum"),1]<-"Glomus_microcarpum"
        
        #Function to replace the given names of the study by accepeted names
        #according to Schlusser website
        brazildata<-secondTrial(AMF_Taxonomy,brazildata)
        
        brazildata[!brazildata[,1]%in%AMF_Taxonomy[,1],1]
        
        #Funneliformis_halonatus match problem, closest Glomus halonatus/halonatum
        #Intrornatospora_intraornata match problem, closest Racocetra intraornata
        #Paradentiscutata_maritima needs to be added
        
        #Maybe approriate matches?:
        #"Paraglomus_pernambucanum"<-"Scutellospora_pernambucana"
        #"Cetraspora_gregaria"<-"Racocetra gregaria"
        #"Scutellospora_alterata"<-"Cetraspora_alterata"
        #"Ambispora_guedermania"<-"Ambispora_gerdemannii"
        #
        
       
        
        #Species that are mentioned in Natalia´s data but are not mentioned at all in Schussler website:
        #"Acaulospora_foveoreticulata", Racocetra_novaum, Acaulospora_endographis,
        
        #Typos to correct in Brazil data
        #"Acaulospora_gerdermanii"<-"Acaulospora_gerdemannii"
        #"Cetraspora_gilmore"<-"Cetraspora_gilmorei"
        #"Quatunica_erythropa"<-"Quatunica_erythropus"
        #"Scutellospora_dipurpurencens"<-"Scutellospora_dipurpurescens"
        #"Scutellospora_erytropa"<-"Scutellospora erythropus"
        #"Racocetra_intrornata"<-"Racocetra_intraornata"
        #"Glomus_agnicaule"<-"Glomus_magnicaule"
        #"Glomus_allidum"<-"Glomus_pallidum"
        #"Glomus_roliferum"<-"Glomus_proliferum"
        #"Glomus_enue"<-"Glomus_tenue"
        #"Glomus_ersiforme"<-"Glomus_versiforme"
        #"Glomus_fascibulatum"<-"Glomus_fasciculatum"
        #"Scutellospora_astanea"<-"Scutellospora_castanea"
        #"Scutellospora_dipurpurencens"<-"Scutellospora_dipurpurescens"
        #"Paradentiscutata_baiana"<-"Paradentiscutata_bahiana"
        #"Rhizoglomus_intraradices"<-"Sclerocystis_coremioides"
        #"Sclerocystis_sinuosum"<-"Sclerocystis_sinuosa"
        #"Sclerocystis_coremioide"<-"Sclerocystis_coremioides"
        #"Sclerocystis_clavisporum"<-"Sclerocystis_clavispora"
        #"Funnelifomis_mosseae"<-"Funneliformis_mosseae"
"

      
        names(brazildata)[1]<-"good.names"
        brazildata<-left_join(brazildata,AMF_All_Copy[,c(1,13)])
        
        brazildata[!brazildata[,1]%in%AMF_Taxonomy[,1],1][
        grep("Acaulospora",brazildata[!brazildata[,1]%in%AMF_Taxonomy[,1],1])]      
                

#exploration with some realistic variables: State, Name_Vegetation, 
        library(scales)

StateHistograms<-        

  StateHistograms2<-
  brazildata%>%
  ggplot()+
  aes(x=SporeArea,fill=
        #State)+
        NAME_Vegetation)+
        #Type)+
        #DESC_Type)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  geom_histogram(col=I("black"))+
  facet_grid(~NAME_Vegetation)+theme_bw()+
  coord_flip()+
  theme(legend.text=element_text(size=5))
  

StateBoxplots<-
brazildata%>%
  ggplot()+
  aes(x=Type,y=SporeArea,fill=
        #State)+
        NAME_Vegetation)+
        #Type)+
  #DESC_Type)+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  geom_boxplot()+facet_grid(~State)+theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Type referes to biome (as defined in brazil and it should be only 5, although we have 7),
# DESC_Type is description type, better not use this description because it has several 
# incongruencies. Natalia suggest to use NAME vegetaion as the main categorical variable
# this is based out of vegetaion maps from IBGE (brazillian institute)
# in the website there is literature that explain the definition of the these categories.
# If anything Type>Name_Vegetaion all comes from IGBE.

#to change Shrubland = Caatinga = Estepe
#savannna = Cerrado = tropical grassland

#4 forest catagories: Floresta atlantica ombrofila aberta, Floresta atlantica ombrofila densa
# Mata (Floresta) caducifolia estacional decidual, Mata (floresta) caducifolia estaciona semidecidual
# Floresta atlantica types are in the north of Brazil
# Mata caducifolia types are in the south of brazil 


brazildata[grep(" ",brazildata$Type),c(1,2,7)]
unique(brazildata$DESC_Type)

length(unique(brazildata$good.names))

names(brazildata)

brazildata%>%
  ggplot()+
  aes(x=latitude,y=SporeArea)+
  geom_point()

class(brazildata$latitude)
