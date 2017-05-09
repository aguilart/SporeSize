library(ggplot2)
library(tidyr)
library(dplyr)

#loding the file and tiding up

Violli2008<-read.csv("Violi_etal2008.csv",header = TRUE,stringsAsFactors = FALSE)

Violli2008$AMF_species<-sub("\\s","_",Violli2008$AMF_species)
Violli2008<-Violli2008[-grep("sp\\.",Violli2008$AMF_species),]

Violli2008$AMF_species[
  which(Violli2008$AMF_species=="Acaulospora_denticulate")]<-
  "Acaulospora_denticulata"
Violli2008$AMF_species[
  which(Violli2008$AMF_species=="Acausospora_elegans")]<-
  "Acaulospora_elegans"
Violli2008$AMF_species[
  which(Violli2008$AMF_species=="Ggaspora_rosea")]<-
  "Gigaspora_rosea"

Violli2008$AMF_species[
  which(Violli2008$AMF_species=="Glomus_rubriformis")]<-
  "Glomus_rubiforme" 

Violli2008$AMF_species[
  which(Violli2008$AMF_species=="Glomus_spuricum")]<-
  "Glomus_spurcum" 

Violli2008$AMF_species[
  which(Violli2008$AMF_species=="Scutellospora_erythropa")]<-
  "Scutellospora_erythropus"

#1. Correct the names

Violli2008[!Violli2008[,1]%in%AMF_Taxonomy[,1],1]

Violli2008<-secondTrial(AMF_Taxonomy,Violli2008)

#2. Check what new spores data need to be entried

AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],
                                Violli2008[,1]))),
             c(1,13)]

#3. Change the format of the dataframe to do the desired ggplot

Violli2008_df<-gather(Violli2008,key=sites,
                      value=abundance,Milpa_Bazom:Pasture_Veracruz)

names(Violli2008_df)[1]<-"good.names"

rm(Violli2008)

#4. Adding trait data and order the factor levels from no disturbance to more disturbance

Violli2008_df<-left_join(Violli2008_df,AMF_All_Copy[,c(1,13)])

Violli2008_df$sites<-factor(Violli2008_df$sites,
                          levels = c("Mature.forest_Bazom","Mature.forests_Tzontehuitz","Mature.Forests_Veracruz",
                                     "Pine.forest_Bazom","Selectively.logged.areas_Tzontehuitz","Pasture_Veracruz",
                                     "Milpa_Bazom","Burned.areas_Tzontehuitz"))



#5. Performing the ggplot for all the data
Violli2008_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 7))+
  ggtitle("Violli2008 presence")