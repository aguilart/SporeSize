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
ComparisonAMF_Ascos<-
    rbind(AMF_All_Copy%>%
            select(good.names,SporeVolume)%>%
            filter(good.names!="Glomus_tenue")%>%
            mutate(Phylum="Glomeromycota"),
          ConidiaDataAscos[grep("conidia$",ConidiaDataAscos$SporeName),]%>%
            select(good.names,SporeVolume)%>%
            mutate(Phylum="Ascomycota"))%>%
              ggplot()+
              aes(Phylum,log10(SporeVolume),fill=Phylum)+
              geom_boxplot()+
              # to change to a violin plot with points, comment out the 'geom_boxplot()+' line above and remove comments on the two lines below
              # try modifying the arguments to make the plot look the way that you like them
              # geom_violin(alpha=0.5, draw_quantiles=0.5)+
              # geom_jitter(width=0.1, size=0.5)+
              ggtitle(label = "a) Comparison of spore volume between Glomeromycota and Soil Ascomycota")+
              labs(x="Phyllum",y=expression("log10 Spore volume "*mu*"m"^{3}))+
              theme(legend.position = "none",
                    plot.title = element_text(family="serif", size=12))


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


#Testing for phylogenetic conservatism                
phylosig(AMF_Tree,SporeVolumeAll,method = "lambda",test = T)

#This is the boxplot accompanying the phylogenetic tree in Figure 2 in the manuscript
AMF_SubsetInPhyloy%>%
  ggplot()+
  aes(log10(SporeVolume))+
  geom_histogram(fill=I("blue"),
                 col=I("black"),
                 alpha=I(.2))


############################### LEFTOVERS #####################################################

# AMF_SubsetInPhyloy[-c(45,74,75,83,99),]%>%
#   ggplot()+
#   aes(x=Family,y=log10(SporeVolume))+
#   geom_boxplot()+
#   theme(axis.text.x = element_text(angle=90,vjust = 0.7,size=15),
#         axis.text.y = element_text(size = 20))+
#   coord_flip()
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
