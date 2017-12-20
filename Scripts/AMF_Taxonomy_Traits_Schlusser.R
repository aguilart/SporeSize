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
#This new file includes all the changes done up to this ponint in the code. The reason
#for creating a new file instead of running again the whole script is because it makes
#easier the data entry of new data on spore sizes, than trying to do do it in R

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

#write.csv(AMF_All_Copy,"AMF_Spore_Database_Volume.csv")

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
library(scales)

HistogramAMF<-
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


###

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



#knowing how many species we have data for

length(unique(AMF_All_Copy$good.names))

min(AMF_All_Copy[!AMF_All_Copy$good.names== "Glomus_tenue",]$SporeArea,na.rm = T)


AMF_All_Copy[
order(AMF_All_Copy$SporeArea,decreasing = T),
c(1,15,35)][1:6,]

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
          select(good.names,SporeVolume)%>%
          filter(good.names!="Glomus_tenue")%>%
          mutate(Phylum="Glomeromycota"),
        ConidiaDataAscos[grep("conidia$",ConidiaDataAscos$SporeName),]%>%
          select(good.names,SporeVolume)%>%
          mutate(Phylum="Ascomycota")
        )%>%
  ggplot()+
  aes(Phylum,log10(SporeVolume),fill=Phylum)+
  geom_boxplot()+
  ggtitle(label = "a) Comparison of spore volume between Glomeromycota and Soil Ascomycota")+
  labs(x="Phyllum",y=expression("log10 Spore volume "*mu*"m"^{3}))+
  theme(legend.position = "none",
        plot.title = element_text(family="serif", size=12))

###

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
# is given instead of the spore: AMF_All_Copy[253,1]



names(AMF_All_Copy)

#The number of AMF species I am processing is 243
#(from which I got AMF spore data at this moment). This can be obtained by:
length(which(!is.na(AMF_All_Copy$SporeArea)))


##

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

#Now calculating volume
ConidiaDataAscos$SporeVolume<-(pi*(ConidiaDataAscos$EquatorialAxis^2)*(ConidiaDataAscos$PolarAxis))/6