###########Creating a complete database based on Schlusser website######################

# #Incorporate the work of Stefan  (Data incomplete)
# 
# AMF_All<-merge(AMF_CurrentNames,AMF_TraitDataIncomplete,by="Current name",all = TRUE)
# 
# #Doing some cleaning
# 
# # AMF_All[206,1]<-"Racocetra alborosea"
# # AMF_All[207,1]<-"Racocetra castanea"
# # AMF_All[,1]<-sub(" ","_",AMF_All[,1])#Adding an underscore in between the genus and the species in the Current names
# # names(AMF_All)[1]<-"good.names"
# grep("heterogama",AMF_All[,3])
# AMF_All[67,3]<-"Endogone_heterogama Gigaspora_heterogama Scutellospora_heterogama Fuscutata_heterogama"
# #There was an extra "Glomus_australe" for some reason, so I remove it.
# which(AMF_All$good.names=="Glomus_australe")
# AMF_All<-AMF_All[-115,]

##LOADING THE TAXONOMIC REFERENCE
AMF_All<-read.csv("AMF_NamesTraitDataComplete.csv",header = TRUE,stringsAsFactors = FALSE)

#UDATING THE SPORE AREA COLUMN
AMF_All$SporeArea<-
  ((AMF_All$dim1.min+AMF_All$dim1.max)/4)*
  ((AMF_All$dim2.min+AMF_All$dim2.max)/4)*
  pi

#CREATING A COPY OF THE TAXONOMIC REFERENCE AND REMOVING THE ORIGINAL FROM WORKSPACE
AMF_All_Copy<-AMF_All
rm(AMF_All)


#Creating a new dataframe just for AMF taxonomy, where each correct names gets repeated 
#acccording the number of synonyms it has.

## 1. Creating the function (Will Cornwell)
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


## 2 Applying the function to create a new dataframe just with AMF taxonomy:
#current names and synonyms

AMF_Taxonomy<-restructure(AMF_All_Copy)

# ###
# 
# Reduce(function(...)merge(...,by="good.names",all=TRUE),
#        list(AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],Oehlspecies_df[,1]))),c(1,13)],
#             AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],TchabiAllDataCopy[,1]))),c(1,13)],
#             AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],SturmerAllDataCopy[,1]))),c(1,13)]))
        

