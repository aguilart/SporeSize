###########Creating a complete database based on Schlusser website######################

#loading packages
library(tidyverse)

#loading functions

#Creating the function "restructure" (Will Cornwell)
restructure<-function(old){
  good.names<-NA
  Synonyms<-NA
  j<-1
  for (i in 1:length(old$Synonyms)){
    if (grepl(" ",old$Synonyms[i])) {
      sp_vec<-unlist(strsplit(old$Synonyms[i]," "))
      for (k in 1:length(sp_vec)){
        Synonyms[j]<-sp_vec[k]
        good.names[j]<-old$good.names[i]
        j<-j+1
      }
      next
    }
    good.names[j]<-old$good.names[i]
    Synonyms[j]<-old$Synonyms[i]
    j<-j+1
    print(i)
  }
  return(data.frame(good.names=good.names,Synonyms=Synonyms,stringsAsFactors = FALSE))
}

##Loading file and tidyig up
AMF_All<-read.csv("AMF_NamesTraitDataComplete.csv",header = TRUE,stringsAsFactors = FALSE)
          AMF_All$SporeArea<-#UDATING THE SPORE AREA COLUMN
            ((AMF_All$dim1.min+AMF_All$dim1.max)/4)*
            ((AMF_All$dim2.min+AMF_All$dim2.max)/4)*
            pi

AMF_All_Copy<-AMF_All#CREATING A COPY
              rm(AMF_All)
              AMF_All_Copy[,1]<-sub("\\s","_",AMF_All_Copy[,1])
              AMF_All_Copy[67,3]<-"Endogone_heterogama Gigaspora_heterogama Scutellospora_heterogama Fuscutata_heterogama"

              AMF_All_Copy<-AMF_All_Copy[grep("_",AMF_All_Copy$good.names),]
              AMF_All_Copy<-AMF_All_Copy[-c(163,164),]
              
              rownames(AMF_All_Copy)<-NULL
              
              
#Updating and cleaning the dataset (September 2017)

              
schlusserUpdate<-read.csv("SchlusserWebSiteUpdate2017.csv",header = T, stringsAsFactors = F)  

                schlusserUpdate$good.names<-sub(" ","_",schlusserUpdate$good.names)
                schlusserUpdate$good.names<-sub("\\s","_",schlusserUpdate$good.names)
                schlusserUpdate<-schlusserUpdate[grep("_",schlusserUpdate$good.names),]
                
                names(schlusserUpdate)[3]<-"Basionyms_Synonyms_Comments"
                names(schlusserUpdate)[4]<-"Authorities"
                names(schlusserUpdate)[5]<-"Family"
                names(schlusserUpdate)[6]<-"Order"
                
                schlusserUpdate[80,3]<-"Basionym: Glomus compressum Sieverd., Oehl, Palenz., Sánchez-Castro & G.A. Silva"
                rownames(schlusserUpdate)<-NULL

                schlusserUpdate[215,1]<-"Racocetra_alborosea"
                schlusserUpdate[207,1]<-"Racocetra_castanea"

#Updating dataset using the updtade of Schlusser website from 2017

old_names<-AMF_All_Copy[!AMF_All_Copy$good.names%in%schlusserUpdate$good.names,1]

AMF_All_Copy[!AMF_All_Copy$good.names%in%schlusserUpdate$good.names,3]<-
  paste(old_names,AMF_All_Copy[!AMF_All_Copy$good.names%in%schlusserUpdate$good.names,3])

AMF_All_Copy[!AMF_All_Copy$good.names%in%schlusserUpdate$good.names,3]<-
  sub(" NA$","",AMF_All_Copy[!AMF_All_Copy$good.names%in%schlusserUpdate$good.names,3])

#changing some "old" to "new" accepted names according to the website update 2017
AMF_All_Copy[!AMF_All_Copy$good.names%in%schlusserUpdate$good.names,c(1:3)]

AMF_All_Copy[94,1]<-"Rhizophagus_aggregatus"
AMF_All_Copy[99,1]<-"Diversispora_arenaria"
AMF_All_Copy[101,1]<-"Dominikia_aurea"
AMF_All_Copy[111,1]<-"Claroideoglomus_candidum"
AMF_All_Copy[116,1]<-"Corymbiglomus_corymbiforme"
AMF_All_Copy[129,1]<-"Corymbiglomus_globiferum"
AMF_All_Copy[136,1]<-"Dominikia_indica"
AMF_All_Copy[138,1]<-"Rhizophagus_invermaium"
AMF_All_Copy[169,1]<-"Corymbiglomus_tortuosum"

#Adding new species that have been introduced to in Schlusser Website

new_species<-
            schlusserUpdate[!schlusserUpdate$good.names%in%AMF_All_Copy$good.names,]
            rownames(new_species)<-NULL
          
                        rownames(
                        new_species[
                        grep("nym",new_species$Basionyms_Synonyms_Comments),])

            new_species$Synonyms<-NA
            
            new_species$Synonyms[c(11,15,24,25,28,30,33,36,37,38,39,40,41)]<-
              c(NA,"Glomus_compressum","Glomus_bistratum","Glomus_perpusillum",
              NA,NA,"Racocetra_intraornata","Glomus_macrocarpum","Glomus_macrocarpum",
             "Glomus_macrocarpum",
             "Glomus_fulvum","Glomus_fulvum",
             "Glomus_fasciculatum")


AMF_All_Copy<-merge(AMF_All_Copy,new_species[,c(1,3)],all = T)


#deleting duplicated names

AMF_All_Copy[grep("deserticola",AMF_All_Copy$good.names),c(1,2,13)]
AMF_All_Copy[157,c(1,2)]<-AMF_All_Copy[290,c(1,2)]
AMF_All_Copy[grep("heterogama",AMF_All_Copy$good.names),c(1,2,13)]
AMF_All_Copy$Comments[79]<-"INVAM data; others: original description (Endogone heterogama) 150-202; Oehl version (Fuscutata heterogama): 165-280"
AMF_All_Copy[grep("viscosum",AMF_All_Copy$good.names),c(1,2,13)]
AMF_All_Copy[AMF_All_Copy$good.names=="Septoglomus_viscosum",2]<-"Glomus_viscosum Viscospora_viscosa"
AMF_All_Copy[grep("punctata",AMF_All_Copy$good.names),c(1,2,13)]
AMF_All_Copy<-AMF_All_Copy[-c(34,125,200,290),]
rownames(AMF_All_Copy)<-NULL


#Adding extra information from the website
AMF_All_Copy<-left_join(AMF_All_Copy,schlusserUpdate,by ="good.names")
              AMF_All_Copy<-AMF_All_Copy[c(1,2,28,31,32,4:27,30,3,29)]
              
rm(old_names,new_species,schlusserUpdate)


#Creating a new dataframe just for AMF taxonomy, where each correct names gets repeated 
#acccording the number of synonyms it has. In order to do this, Will created the function
# "restructure" (see above)

AMF_Taxonomy<-restructure(AMF_All_Copy)


##Plotting some histograms for describing data variation

qplot(log10(AMF_All_Copy$SporeArea),
      #AMF_All_Copy$SporeArea,
      geom="histogram",
      #binwidth = 0.5,  
      main = "Spore size (spore projected area) mycorrhizal fungi", 
      xlab = "log10 Spore size",
      ylab = "Number of species",
      fill=I("blue"), 
      col=I("black"), 
      alpha=I(.2)#,xlim=c(20,50)
      
      )

#another form to make histograms

HistogramAMF<-
AMF_All_Copy[!AMF_All_Copy$good.names== "Glomus_tenue",]%>%
  ggplot()+
  aes(SporeArea)+
  geom_histogram(fill=I("blue"),
                 col=I("black"), 
                 alpha=I(.2))+
  labs(x= expression("Spore size "*mu*"m"^{2}),y="Number of species")+
  ggtitle(label="Variation in spore size among AMF species")+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  theme(
    plot.title = element_text(family="serif", size=27, face="bold.italic"),
    axis.text=element_text(size=20), 
    axis.title = element_text(size=24))

#knowing how many species we have data for

length(unique(AMF_All_Copy$good.names))

min(AMF_All_Copy[!AMF_All_Copy$good.names== "Glomus_tenue",]$SporeArea,na.rm = T)

AMF_All_Copy[
order(AMF_All_Copy$SporeArea,decreasing = F),
c(1,13)][1:6,]

temporal<-
AMF_All_Copy[
  order(AMF_All_Copy$SporeArea,decreasing = F),
  c(1,13)]
rownames(temporal)<-NULL

#These histogram and boxplot comparisons come from the data of conidia size of other soil ascomycete
# fungi. They come from the compendium of soil fungi

ConidiaDataAscos<-read.csv("ConidiaDataCompendiumAscos/ConidiaFromCompendium.csv",
                           header = T, stringsAsFactors = F)
names(ConidiaDataAscos)[17]<-"good.names"

#Boxplot
ComparisonAMF_Ascos<-
rbind(    AMF_All_Copy%>%
          select(good.names,SporeArea)%>%
          filter(good.names!="Glomus_tenue")%>%
          mutate(Phylum="Glomeromycota"),
        ConidiaDataAscos[grep("conidia$",ConidiaDataAscos$SporeName),]%>%
          select(good.names,SporeArea)%>%
          mutate(Phylum="Ascomycota")
        )%>%
  ggplot()+
  aes(Phylum,log10(SporeArea),fill=Phylum)+
  geom_boxplot()+
  ggtitle(label = "a) Comparison of spore area between Glomeromycota and Soil Ascomycota")+
  theme(legend.position = "none",
        plot.title = element_text(family="serif", size=12))


ggplot()+
  geom_histogram(data = AMF_All_Copy[!AMF_All_Copy$good.names== "Glomus_tenue",],
                        aes(log10(SporeArea),fill=I("blue")),#fill=I("blue"),
                        col=I("black"), 
                        alpha=I(.2))+
  geom_histogram(data=ConidiaDataAscos[grep("conidia$",ConidiaDataAscos$SporeName),],
                 aes(log10(SporeArea),fill=I("red")),#fill=I("red"),
                 col=I("black"), 
                 alpha=I(.2))+
  labs(x= expression("Spore size "*mu*"m"^{2}),y="Number of species")+
  theme(
    plot.title = element_text(family="serif", size=27, face="bold.italic"),
    axis.text=element_text(size=20,angle = 90), 
    axis.title = element_text(size=24))+
  scale_fill_manual(name="Fungal group",
    values = c("blue", "red"),
    labels=c("Glomeromycetes","Soil ascomycetes"))+
  #scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous(labels = c("1","10","100","1000","10000","100000","1000000"))




#NOTE: I need to update the entry of Sclerocystis clavispora, the size of the sporocarp
# is given instead of the spore: AMF_All_Copy[236,1]

names(AMF_All_Copy)

#The number of AMF species I am processing is 243
#(from which I got AMF spore data at this moment). This can be obtained by:
length(which(!is.na(AMF_All_Copy$SporeArea)))
