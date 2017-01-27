###########Creating a complete database based on Schlusser website######################


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


## 2. Using "reestructure" to get a table of "accepted names" and "synonyms". 
#Then adding underscore between genus and species name

AMF_Taxonomy<-restructure(AMF_All_Copy)

AMF_Taxonomy[,1]<-sub("\\s","_",AMF_Taxonomy[,1])
