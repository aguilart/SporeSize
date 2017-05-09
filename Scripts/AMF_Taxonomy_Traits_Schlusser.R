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

