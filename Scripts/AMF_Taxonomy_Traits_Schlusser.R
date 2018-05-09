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


####################################################################################
#From this point on, AMF_All_Copy correspond to the file name "AMF_Spore_Database_Volume"
#This new file includes all the changes done up to December 2017 in the code. The reason
#for creating a new file instead of running again the whole script is because I entry data from 
#fitty species for fifty species by hand, than trying to do do it in R

#loading file and updating entries

AMF_All_Copy<-read.csv("AMF_Spore_Database_Volume.csv",header = T, stringsAsFactors = F)


#UDATING THE SPORE AREA COLUMN given the previous updates in data entry
AMF_All_Copy$SporeArea<-
  ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/4)*
  ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/4)*
  pi

#Caluculating Volume 
#For this it is assumed that fungal spores are prolate spheroids, which means
#two things. First, that the smallest diameter will be consider the "equatorial axis" and the largest
#is the "polar axis"; second, that the there are two equatorial axis of the same length. This is 
#important because the formula to calculate volume make distinction between the two type of axis.
#In this way, AMF spore can be only of two types, perfect spheres (in which case equatorial and 
#polar axis are of the same lenght), corrsponding to "globose" spores. And, spheroids similar to an
#american football.  A morphology like a "flying soucer" is not possible following this rules.


AMF_All_Copy$EquatorialAxis<-NaN
          AMF_All_Copy$EquatorialAxis[
            which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
              ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
                  which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
                          ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                  ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                    ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                      which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                              ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                 ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]

AMF_All_Copy$PolarAxis<-NaN
          AMF_All_Copy$PolarAxis[
            which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                      ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
                        which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                                ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
          AMF_All_Copy$PolarAxis[
            which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
                   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                    ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                      which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
                              ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
          AMF_All_Copy$PolarAxis[
            which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                    ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                      ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                        which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                                ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]

              which(AMF_All_Copy$EquatorialAxis>AMF_All_Copy$PolarAxis)#This should always be zero (0)
              
            #Now calculating volume
            AMF_All_Copy$SporeVolume<-(pi*(AMF_All_Copy$EquatorialAxis^2)*(AMF_All_Copy$PolarAxis))/6
            
            #Removing the "Endogone" entries. All the species of this old genera are synonimized to other
            #names. These synonims are the ones that have the actual spore data and, according to the 
            #Shussler website, they are the valid names. Apparently, the reason why Schussler kept them
            #is because they are synonimized based only in morphological characters (no DNA available)
            
            AMF_All_Copy<-AMF_All_Copy[-c(112:116),]
            rownames(AMF_All_Copy)<-NULL

            #Fixing some typos in the synonyms category
            # AMF_All_Copy$Synonyms[
            # grep("[a-z] [a-z]",AMF_All_Copy$Synonyms)]
            
            AMF_All_Copy$Synonyms[which(
              AMF_All_Copy$Synonyms=="Endogone_macrocarpa_var_geospora Glomus macrocarpum_var_geosporum Glomus_geosporum"
            )]<-"Endogone_macrocarpa_var_geospora Glomus_macrocarpum_var_geosporum Glomus_geosporum"

            AMF_All_Copy$Synonyms[which(
              AMF_All_Copy$Synonyms=="Endogone_macrocarpa  Paurocotylis fulva Endogone pampaloniana Endogone_nuda"
            )]<-"Endogone_macrocarpa Paurocotylis_fulva Endogone_pampaloniana Endogone_nuda"
            
            
            AMF_All_Copy$Synonyms[which(
              AMF_All_Copy$Synonyms=="Endogone_macrocarpa_var_ caledonia Glomus_caledonium"
            )]<-"Endogone_macrocarpa_var_caledonia Glomus_caledonium"
            
            AMF_All_Copy$Synonyms[grep("intraradices",AMF_All_Copy$Synonyms)]<-"Glomus_intraradices Rhizoglomus_intraradices"
            AMF_All_Copy$Synonyms[grep("fasciculat",AMF_All_Copy$Synonyms)]<-"Endogone_fasciculata Glomus_fasciculatum Endogone_arenacea Rhizoglomus_fasciculatum"
            AMF_All_Copy$Synonyms[grep("microaggr",AMF_All_Copy$good.names)]<-"Rhizoglomus_microaggregatum"
            AMF_All_Copy$Synonyms[grep("claru",AMF_All_Copy$Synonyms)]<-"Glomus_clarum Rhizoglomus_clarum"
            AMF_All_Copy$Synonyms[grep("natal",AMF_All_Copy$good.names)]<-"Rhizoglomus_natalensis"
            
#Creating a new dataframe just for AMF taxonomy, where each correct names gets repeated 
#acccording the number of synonyms it has. In order to do this, Will created the function
# "restructure" (see above)

AMF_Taxonomy<-restructure(AMF_All_Copy)




#####################################################################################################
####Descrition of spore size and variation within the Glomeromycota and comparison to Ascomycota#####
#####################################################################################################

##Plotting some histograms for describing data variation. NOTE: Glomus tenue is removed from all figure
# because, according to Arthur Schlusser, this fungus does not share any trait with known AMF species
# he actually says it most be a mistake.

library(scales)

#HistogramAMF<-
AMF_All_Copy[!AMF_All_Copy$good.names== "Glomus_tenue",]%>%
  ggplot()+
  aes(SporeVolume)+
  geom_histogram(fill=I("blue"),
                 col=I("black"), 
                 alpha=I(.2))+
  labs(x= expression("Spore volume "*mu*"m"^{3}),y="Number of species")+
  ggtitle(label="Variation in spore size among AMF species")+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  theme(
    plot.title = element_text(family="serif", size=27, face="bold.italic"),
    axis.text=element_text(size=20), 
    axis.title = element_text(size=24))

#############
##FIGURE 1b##
#############

# As density plot- This one correspond to figure 1b in the manuscript
AMF_All_Copy[!AMF_All_Copy$good.names== "Glomus_tenue",]%>%
  ggplot()+
  aes(SporeVolume)+
  geom_density(fill=I("blue"),
                 col=I("black"), 
                 alpha=I(.2))+
  labs(x= expression("Spore volume "*mu*"m"^{3}),y="Probability density")+
  ggtitle(label="Variation in spore size among AMF species")+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  theme(
    plot.title = element_text(family="serif", size=27, face="bold.italic"),
    axis.text=element_text(size=20), 
    axis.title = element_text(size=24))


#Comparison between the AMF spores and the conidia size of soil Ascomycota (around same number of fungi)
#They come from the compendium of soil fungi

ConidiaDataAscos<-read.csv("ConidiaDataCompendiumAscos/ConidiaFromCompendium.csv",
                           header = T, stringsAsFactors = F)
                  names(ConidiaDataAscos)[17]<-"good.names"

                  #Calculating spore (conidia) volume
                  ConidiaDataAscos$EquatorialAxis<-NaN
                  ConidiaDataAscos$EquatorialAxis[
                    which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)<
                            ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]<-
                    ((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)[
                      which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)<
                              ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]
                  ConidiaDataAscos$EquatorialAxis[which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)>
                                                          ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]<-
                    ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2)[
                      which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)>
                              ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]
                  ConidiaDataAscos$EquatorialAxis[which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)==
                                                          ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]<-
                    ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2)[
                      which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)==
                              ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]
                  
                  
                  ConidiaDataAscos$PolarAxis<-NaN
                  ConidiaDataAscos$PolarAxis[
                    which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)>
                            ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]<-
                    ((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)[
                      which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)>
                              ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]
                  ConidiaDataAscos$PolarAxis[
                    which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)<
                            ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]<-
                    ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2)[
                      which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)<
                              ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]
                  ConidiaDataAscos$PolarAxis[
                    which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)==
                            ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]<-
                    ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2)[
                      which(((ConidiaDataAscos$SporeDim1_min+ConidiaDataAscos$SporeDim1_max)/2)==
                              ((ConidiaDataAscos$SporeDim2_min+ConidiaDataAscos$SporeDim2_max)/2))]
                  
                  which(ConidiaDataAscos$EquatorialAxis>ConidiaDataAscos$PolarAxis)#This should always be zero (0)


                  ConidiaDataAscos$SporeVolume<-(pi*(ConidiaDataAscos$EquatorialAxis^2)*(ConidiaDataAscos$PolarAxis))/6


###############
###Figure 1a###
###############

#This figure correspond to Fig 1a in the manucript
Fungi_violin<-
    rbind(
    AMF_All_Copy%>%
            select(good.names,SporeVolume)%>%
            rename(offsrpingSize=SporeVolume)%>%
            filter(good.names!="Glomus_tenue")%>%
            mutate(Taxa="Glomeromycotina")%>%
            #mutate(offsrpingSize=offsrpingSize/((10000)^3))%>%
            mutate(Organism="Microorganism"),
                  
          ConidiaDataAscos[grep("conidia$",ConidiaDataAscos$SporeName),]%>%
            select(good.names,SporeVolume)%>%
            rename(offsrpingSize=SporeVolume)%>%
            mutate(Taxa="Ascomycota")%>%
            #mutate(offsrpingSize=offsrpingSize/((10000)^3))%>%
            mutate(Organism="Microorganism"))%>%      
                    
                    ggplot()+
                    aes(Taxa,offsrpingSize,fill=Taxa)+
                    geom_jitter(size=0.5, width = 0.3,alpha=1)+
                    geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
                    #facet_grid(. ~ Organism, scales = "free")+
                    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
                    labs(y=expression("Spore size ("*mu*"m³)"))+
                    theme(title = element_text(size = 25),
                          axis.title.x=element_blank(),
                          axis.text.x = element_text(size = 20),
                          axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size = 20),
                          legend.position = "none")
          
          

plants_violin<-          
          kewData%>%
            select(species,value)%>%
            rename(good.names=species, offsrpingSize=value)%>%
            mutate(Taxa="Angiosperms")%>%
            mutate(Organism="Macroorganism")%>%
            
            ggplot()+
            aes(Taxa,offsrpingSize,fill=Taxa)+
            geom_jitter(size=0.5, width = 0.3,alpha=0.1)+
            geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75),fill="purple")+
            #facet_grid(. ~ Organism, scales = "free")+
            scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
            labs(x="Taxa",y=expression("Seed size (mg)"))+
            theme(title = element_text(size = 25),
                  axis.title.x=element_blank(),
                  axis.text.x = element_text(size = 20),
                  axis.text.y = element_text(size = 20),
                  strip.text.x = element_text(size = 20),
                  legend.position = "none")
          

Birds_violin<-          
          BirdEgg%>%
            select(Species,Volume)%>%
            rename(good.names=Species,offsrpingSize=Volume)%>%
            mutate(Taxa="Birds")%>%
            mutate(Organism="Macroorganism")%>%
              ggplot()+
              aes(Taxa,offsrpingSize,fill=Taxa)+
              geom_jitter(size=0.5, width = 0.3,alpha=0.8)+
              geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75),fill="dark green")+
              #facet_grid(. ~ Organism, scales = "free")+
                    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
                    labs(x="Taxa",y=expression("Egg size (cm³)"))+
                    theme(title = element_text(size = 25),
                          axis.title.x=element_blank(),
                          axis.text.x = element_text(size = 20),
                          axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size = 20),
                          legend.position = "none")

grid.newpage()
grid.draw(cbind(ggplotGrob(Fungi_violin),
  ggplotGrob(plants_violin),
  ggplotGrob(Birds_violin),
  size = "last"))
              
#Alternatively as histograms
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
                  

###################################################################################################
##############Phylogenetic conservatism throughout the entire Glomeromycota########################
###################################################################################################

#Loading the tree (provided by Stefan)
AMF_Tree<-read.tree("tree.for.carlos.tre")

#We subset spore data for species present in the tree.
#(The phylogenetic tree includes half of the entire spore database).

AMF_SubsetInPhyloy<-AMF_All_Copy[
  AMF_All_Copy$good.names%in%AMF_Tree$tip.label,];
rownames(AMF_SubsetInPhyloy)<-NULL
AMF_SubsetInPhyloy[                    
  grep("Scutellospora",AMF_SubsetInPhyloy$good.names),4]<-"Gigasporaceae"
AMF_SubsetInPhyloy[                    
  grep("Cetraspora",AMF_SubsetInPhyloy$good.names),4]<-"Gigasporaceae"
AMF_SubsetInPhyloy[                    
  grep("Racocetra",AMF_SubsetInPhyloy$good.names),4]<-"Gigasporaceae"
AMF_SubsetInPhyloy[                    
  grep("Acaulospora",AMF_SubsetInPhyloy$good.names),4]<-"Acaulosporaceae"
AMF_SubsetInPhyloy[                    
  grep("Glomus",AMF_SubsetInPhyloy$good.names),4]<-"Glomeraceae"
AMF_SubsetInPhyloy[                    
  grep("Rhizophagus",AMF_SubsetInPhyloy$good.names),4]<-"Glomeraceae"

AMF_SubsetInPhyloy$Family<-as.character(AMF_SubsetInPhyloy$Family)
AMF_SubsetInPhyloy$Family<-factor(AMF_SubsetInPhyloy$Family,
                                  levels = c("Archaeosporaceae",
                                             "Geosiphonaceae",
                                             "Ambisporaceae",
                                             "Claroideoglomeraceae",
                                             "Glomeraceae",
                                             "Pacisporaceae",
                                             "Gigasporaceae",
                                             "Diversisporaceae",
                                             "Acaulosporaceae",
                                             "Paraglomeraceae",
                                             "",
                                             "sequences cluster inDiversisporaceae",
                                             "Entrophosporaceae"))

write.csv(
AMF_SubsetInPhyloy[-grep("glomoid",AMF_SubsetInPhyloy$Comments),c(1,35)],
"SporeVolume_Ambisporaceae_acaulosporoid.csv",row.names = F)

write.csv(
  AMF_SubsetInPhyloy[-grep("acaulosporoid",AMF_SubsetInPhyloy$Comments),c(1,35)],
  "SporeVolume_Ambisporaceae_glomoid.csv",row.names = F)

names(AMF_SubsetInPhyloy)
#Testing for phylogenetic conservatism                
phylosig(AMF_Tree,SporeVolumeAll,method = "lambda",test = T)

#This is the boxplot accompanying the phylogenetic tree in Figure 2 in the manuscript
# AMF_SubsetInPhyloy%>%
#   ggplot()+
#   aes(log10(SporeVolume))+
#   geom_histogram(fill=I("blue"),
#                  col=I("black"),
#                  alpha=I(.2))

#############
##Figure 2b##
#############
#[-c(45,74,75,83,99),]

AMF_SubsetInPhyloy%>%
  filter(Family!="")%>%
  filter(Family!="Entrophosporaceae")%>%
  filter(Family!="sequences cluster inDiversisporaceae")%>%
  ggplot()+
  aes(x=Family,y=SporeVolume,fill=Family)+
  geom_jitter(alpha=0.5)+
  geom_violin(alpha=0.5)+
  labs(x= "Family", y=expression("Spore size"~"("*mu*m^3*")"))+
  scale_y_log10(breaks=c(10^4,10^5,10^6,10^7),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+  
  theme(title = element_text(size = 25),
    axis.text.x = element_text(angle=0,vjust = 0.7,size=20),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(color = "black"),
        #strip.text.x = element_text(size = 20),
        legend.position = "none"#,panel.background = element_blank()
    )+
        coord_flip()
  
unique(AMF_SubsetInPhyloy$Family)



  #ggtitle(label = "a) Comparison of Offspring size (volume) between Glomeromycota and other taxa")+
  labs(x="Taxa",y=expression("Offspring size (cm³)"))+
  theme(title = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = "none")

############################### LEFTOVERS #####################################################

# 
# 
# AMF_SubsetInPhyloy%>%
#   select(good.names,Family,SporeVolume)%>%
#   arrange(desc(SporeVolume))
# 
# 
# 
# AMF_SubsetInPhyloy%>%
#   filter(Family==
#          #"Acaulosporaceae"
#          #"Ambisporaceae"
#          # "Archaeosporaceae"                    
#          "Claroideoglomeraceae"
#          # "Diversisporaceae"                    
#           #"Glomeraceae"
#          # "Entrophosporaceae"                   
#          # "Geosiphonaceae"                      
#          #"Gigasporaceae"
#          # "Pacisporaceae"                       
#          # "Paraglomeraceae"
#                           )%>%
#   ggplot()+
#   aes(log10(SporeVolume))+
#   geom_histogram(fill=I("blue"),
#                  col=I("black"),
#                  alpha=I(.2))
# 
#   
# 
# AMF_SubsetInPhyloy%>%
#   filter(Family==
#            #"Acaulosporaceae"
#            #"Ambisporaceae"
#            # "Archaeosporaceae"                    
#            # "Claroideoglomeraceae"
#            # "Diversisporaceae"                    
#            #"Glomeraceae"
#            # "Entrophosporaceae"                   
#            # "Geosiphonaceae"                      
#            "Gigasporaceae"
#          # "Pacisporaceae"                       
#          # "Paraglomeraceae"
# )%>%
#   summarise(top=max(SporeVolume,na.rm=T),
#             bot=min(SporeVolume,na.rm=T))
# 
# 
# SporeVolumeAll<-
#   AMF_All_Copy[
#     AMF_All_Copy$good.names%in%AMF_Tree$tip.label,35]
# names(SporeVolumeAll)<-AMF_All_Copy[
#   AMF_All_Copy$good.names%in%AMF_Tree$tip.label,1]
# SporeVolumeAll<-SporeVolumeAll[-c(7,27)]
# 
# 
# 
# 
# 
# AMF_All_Copy[order(AMF_All_Copy$SporeVolume,decreasing = T),
#              c(1,7,8,11,12,35)]
# 
# 
# 
# plot(AMF_Tree)
# 
# AMF_SubsetInPhyloy%>%
#   select(good.names,Family,SporeVolume)%>%
#   arrange(desc(SporeVolume))
# 
# do.call(rbind,
# #data.frame(
# tapply(log10(AMF_SubsetInPhyloy$SporeVolume),
#         AMF_SubsetInPhyloy$Family,function(x){
#         data.frame(max=range(x)[2],min=range(x)[1],
#                    diff=range(x)[2]-range(x)[1],
#                    no_species=length(x))}))
# 
# data.frame(number_species=
# tapply(AMF_SubsetInPhyloy$SporeVolume,
#        AMF_SubsetInPhyloy$Family,length))
# 
# length(AMF_SubsetInPhyloy$SporeVolume)

# #knowing how many species we have data for
# 
# length(unique(AMF_All_Copy$good.names))
# 
# min(AMF_All_Copy[!AMF_All_Copy$good.names== "Glomus_tenue",]$SporeArea,na.rm = T)
# 
# 
# AMF_All_Copy[
#   order(AMF_All_Copy$SporeArea,decreasing = T),
#   c(1,15,35)][1:6,]
# 
# temporal<-
#   AMF_All_Copy[
#     order(AMF_All_Copy$SporeArea,decreasing = F),
#     c(1,13)]
# rownames(temporal)<-NULL
  
#AMF_All_Copy[grep("Ambispora",AMF_All_Copy$good.names),c(1,6:13,30)]  
  
  
      rbind(AMF_All_Copy%>%
            select(good.names,SporeVolume)%>%
            rename(offsrpingSize=SporeVolume)%>%
            filter(good.names!="Glomus_tenue")%>%
            mutate(Taxa="Glomeromycotina")%>%
            mutate(offsrpingSize=offsrpingSize/((10000)^3))%>%
            mutate(Organism="Microorganism"),

          ConidiaDataAscos[grep("conidia$",ConidiaDataAscos$SporeName),]%>%
            select(good.names,SporeVolume)%>%
            rename(offsrpingSize=SporeVolume)%>%
            mutate(Taxa="Ascomycota")%>%
            mutate(offsrpingSize=offsrpingSize/((10000)^3))%>%
            mutate(Organism="Microorganism"),
          kewData%>%
            select(species,value)%>%
            rename(good.names=species, offsrpingSize=value)%>%
            mutate(Taxa="Angiosperms")%>%
            mutate(Organism="Macroorganism"),
          BirdEgg%>%
            select(Species,Volume)%>%
            rename(good.names=Species,offsrpingSize=Volume)%>%
            mutate(Taxa="Birds")%>%
            mutate(Organism="Macroorganism"))%>%
    
    ggplot()+
    aes(Taxa,offsrpingSize,fill=Taxa)+
    geom_jitter(size=0.5, width = 0.3,alpha=0.2)+
    geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
    facet_grid(. ~ Organism, scales = "free")+
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    labs(x="Taxa",y=expression("Offspring size"))+
    theme(title = element_text(size = 25),
          axis.title.x=element_blank(),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 20),
          strip.text.x = element_text(size = 20),
          legend.position = "none")
 
