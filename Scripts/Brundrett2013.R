############Brundrett 2013 Analysis############################

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


#1. load files and tyding up

Brundrett2013<-read.csv("Brundrett_KakaduRegion.csv",header = TRUE,stringsAsFactors = FALSE)
Brundrett2013$MINE28<-NULL
Brundrett2013Traits<-read.csv("Brundrett_KakaduRegionTraits.csv",header = TRUE,stringsAsFactors = FALSE)

names(Brundrett2013)[1]<-"Genus"


#2. Change the format of the dataframe and adding trait data

Brundrett2013_df<-gather(Brundrett2013,key=sites,
                    value=abundance,RF22:MINE31)

    Brundrett2013_df<-left_join(Brundrett2013_df,Brundrett2013Traits)
    # create column with midpoint spore size
    Brundrett2013_df<-data.frame(Brundrett2013_df,
      do.call(rbind, strsplit(as.character(Brundrett2013_df$Size..µm.),'—')));
      names(Brundrett2013_df)[c(7,8)]<-c('minSize_um', 'maxSize_um')
    
    Brundrett2013_df$minSize_um<-as.numeric(as.character(Brundrett2013_df$minSize_um))
    Brundrett2013_df$maxSize_um<-as.numeric(as.character(Brundrett2013_df$maxSize_um))
    
    Brundrett2013_df$medSize_um<-
      rowMeans(Brundrett2013_df[, c('minSize_um', 'maxSize_um')])
    
    Brundrett2013_df$SporeArea<-pi*((Brundrett2013_df$medSize_um/2)^2)
    
    Brundrett2013_df$sites<-factor(Brundrett2013_df$sites,
                              levels = 
                                c("WOOD3",  "WOOD6" , "WOOD10", "WOOD11", "WOOD1" ,
                                  "WOOD2",  "WOOD16", "WOOD17", "WOOD20", "WOOD23" ,"WOOD24", "WOOD15",
                                  "RF22",   "WET5"  , "WET14",  "WET19",
                                  "ROCK8" , "ROCK9" ,
                                  "ROCK12" ,"ROCK13" ,"ROCK18" ,"ROCK21" ,"DIST7" , "DIST4"  ,"MINE25" ,"MINE26" ,"MINE27",
                                  "MINE29", "MINE30", "MINE31")
                                    )
#3. Performing the ggplot to compare spore size and range along an "author
    #defined" gradint of disturbance (less to more disturbed site)

Brundrett2013_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=Genus))+
  geom_point(alpha=0.5)+scale_y_log10()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("Brundrett2013 abundance")


#4. Measuring braycurtis distances among treatments, community weigthed means (CWR) of spore size 
#and IQR. This in order to plot CWM, IQR and IDR against bray. distances.

#Calcuation of community weighted means

#The following code is a two step process to calcuated CWM.CWM are calculated as the summation
#of the product: (relative abundance of species "i" in site "s") x (spore size of species i).
#The same logic follows for calculating IQR and IDR


Brundrett2013_df$Abund_Area<-
  #This calculates the relative abundance of each species per site
  #and multiplies it by the spore size of the particular species
  Brundrett2013_df$SporeArea*
  ave(Brundrett2013_df$abundance,Brundrett2013_df$sites,FUN = function(x){x/sum(x)})
Brundrett2013_df$Abund_Area[Brundrett2013_df$Abund_Area==0]<-NA

#And here the summation per site takes place
CommTraits<-cbind(
  data.frame(CWMean=
               tapply(Brundrett2013_df$Abund_Area,
                      Brundrett2013_df$sites,sum,na.rm=TRUE)),
  data.frame(IQR=
               tapply(Brundrett2013_df$Abund_Area,
                      Brundrett2013_df$sites,IQR,na.rm=TRUE)),
  data.frame(IDR=
               tapply(Brundrett2013_df$Abund_Area,
                      Brundrett2013_df$sites,function(x){diff(quantile(x,c(0.1,0.9),na.rm = TRUE,names = FALSE))})
  ))
    CommTraits$habitat<-row.names(CommTraits)
    row.names(CommTraits)<-NULL
    CommTraits<-CommTraits[,c(4,1,2,3)]


#Case 1) braydistances represent comparison between an arbitrary woodland
    #considered as no disturbance against all other site types.

transposed<-t(Brundrett2013[,-1]);
transposed<-transposed[match(levels(Brundrett2013_df$sites),row.names(transposed)),]

dists <- as.matrix(vegdist(transposed,method='bray'))[, 1];
dists <- data.frame(habitat=names(dists), bray.dist=dists);
row.names(dists)<-NULL

CommTraits1<-merge(dists,CommTraits,by="habitat")
CommTraits1$habitat<-factor(CommTraits1$habitat,
                           levels = levels(Brundrett2013_df$sites))

#plotting
CommTraits1%>%
  ggplot(aes(x=bray.dist,y=CWMean,size=IQR,col=habitat,label=habitat))+
  geom_point(alpha=0.5)+#+scale_y_log10()+
  geom_text(aes(label=habitat,size=0.5),hjust=1.5,vjust=1.5)+
  theme(axis.text.x = element_text(size = 5))


#Case 2) braydistances represent:
#             a.  comparison among all woodlands against all mine sites
#             b.  comparison among all rocky hills against all mine sites
#We did this because according to the description of the paper, both woodland and rocky hills
#represent the "natural" habitats and some mine sites share conditions similar to either of them

dists2<-
  rbind(#Woodlands sites vs Mine sites
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 1]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 1])[c(1,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 2]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 2])[c(2,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 3]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 3])[c(3,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 4]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 4])[c(4,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 5]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 5])[c(5,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 6]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 6])[c(6,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 7]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 7])[c(7,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 8]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 8])[c(8,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 9]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 9])[c(9,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 10]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 10])[c(10,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 11]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 11])[c(11,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 12]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 12])[c(12,25:30),],
    #Rocky Hills sites vs Mine sites
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 17]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 17])[c(17,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 18]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 18])[c(18,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 19]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 19])[c(19,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 20]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 20])[c(20,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 21]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 21])[c(21,25:30),],
    
    data.frame(habitat=names(as.matrix(vegdist(transposed,method='bray'))[, 22]),
               bray.dist=as.matrix(vegdist(transposed,method='bray'))[, 22])[c(22,25:30),])

#
dists2<-data.frame(dists2,
    Reference=
      rep(names(as.matrix(vegdist(transposed,method='bray'))[, 1])[c(1:12,17:22)],each=7));
      row.names(dists2)<-NULL
      dists2<-droplevels(dists2)

CommTraits2<-left_join(CommTraits[c(1:12,17:22,25:30),],dists2)
CommTraits2$habitat<-factor(CommTraits2$habitat,
                           levels = levels(Brundrett2013_df$sites))

#Saving the info from this study

CommTraitsBrundrett2013<-CommTraits2
CommTraitsBrundrett2013$Study<-"Brundrett2013"
CommTraitsBrundrett2013<-CommTraitsBrundrett2013[c(1,5,2,3,4,6,7)]

rm(CommTraits,CommTraits1,CommTraits2,dists,dists2,
   transposed,Brundrett2013,Brundrett2013_df,Brundrett2013Traits)

#Further trimming this data. Now I am selecting comparisons of natural sites (savana-Woodlands)
#with disturbed sites (mines) that are close together based on the papers of Brundrett etal 
#1996 a and b. I also creat a new column to describe the habitat type

CommTraitsBrundrett2013$habitatType<-NA

            CommTraitsBrundrett2013$
              habitatType[grep("^WOOD",CommTraitsBrundrett2013$habitat)]<-"savanna-woodlands_flat"
            
            CommTraitsBrundrett2013$
              habitatType[grep("^ROCK",CommTraitsBrundrett2013$habitat)]<-"savanna-woodlands_hill"  
            
            CommTraitsBrundrett2013$
              habitatType[grep("^MINE",CommTraitsBrundrett2013$habitat)]<-"savanna-disturbed (waste mine)"

#plot all comparisons among disturbed sites with all woodlands_flat and woodlans_hill

  PlotBrundrett<-
  CommTraitsBrundrett2013%>%
  ggplot(aes(x=bray.dist,y=log10(CWMean),size=IQR,col=habitatType,label=habitatType))+
  geom_point(alpha=0.5)+#+scale_y_log10()+
  #geom_text(aes(label=habitatType),hjust=-0.3,vjust=0)+
  theme(axis.text.x = element_text(size = 5))


#Plot comparisons among disturbed sites and woodlands sites that are the same geographic are

      #First I need to find the correct comparions,
      #the following code (which is now just comments) was used to find them.
          # CommTraitsBrundrett2013[
          # which(CommTraitsBrundrett2013$Reference=="WOOD1"|
          #         CommTraitsBrundrett2013$Reference=="WOOD6"|
          #         CommTraitsBrundrett2013$Reference=="WOOD16"|
          #         CommTraitsBrundrett2013$Reference=="WOOD15"|
          #         CommTraitsBrundrett2013$Reference=="WOOD17"|
          #         CommTraitsBrundrett2013$Reference=="WOOD20"),c(1,6)]
          # 
          # CommTraitsBrundrett2013[
          #   which(CommTraitsBrundrett2013$Reference=="ROCK21"&
          #           CommTraitsBrundrett2013$habitat=="ROCK21"),]

  
PlotBrundrett<-
CommTraitsBrundrett2013[c(5,2,7,8,9,12,18,27,36,43,44,48,61,62,66,74,77,92,95,110,113),]%>%
  ggplot(aes(x=bray.dist,y=log10(CWMean),size=IQR,col=Reference))+
  geom_point(alpha=0.5)+
  #+scale_y_log10()+
  geom_text(aes(label=habitat,size=2000),hjust=-0.1,vjust=0)+
  theme(axis.text.x = element_text(size = 5))+
  #scale_colour_discrete(guide=FALSE)+
  scale_size_continuous(guide=FALSE)+
  scale_colour_discrete(guide=FALSE)+
  ggtitle(paste("Undisturbed Eucalypt savanna vs", "\n",
                "disturbed (dump mine waste) Eucalypt savanna"))
