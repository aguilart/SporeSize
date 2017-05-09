library(ggplot2)
library(tidyr)
library(dplyr)

#loading the data and tiding up

datos<-read.csv("datos.csv",header = TRUE,stringsAsFactors = FALSE)


#1. Correct the names

Datos[!Datos[,1]%in%AMF_Taxonomy[,1],1]

Datos<-secondTrial(AMF_Taxonomy,Datos)

#2. Check what new spores data need to be entried

AMF_All_Copy[which(!is.na(match(AMF_All_Copy[,1],
                                Oehlspecies_df[,1]))),
             c(1,13)]

#3. Change the format of the dataframe to do the desired ggplot

Bonfin2013_df<-gather(Bonfin2013,key=sites,
                      value=abundance,Rainy_NT:Dry_R05)

names(Bonfin2013_df)[1]<-"good.names"

rm(Bonfin2013)

#4. Adding trait data and order the factor levels from no disturbance to more disturbance

Oehlspecies_df<-left_join(Oehlspecies_df,AMF_All_Copy[,c(1,13)])

Oehl2003_df$sites<-factor(Oehl2003_df$sites,
                          levels = c("W","V","G","O","L","F","S","R"))

#5. Performing the ggplot for all the data
Bonfin2013_df %>%
  filter(abundance!=0) %>%
  ggplot(aes(x=sites,y=SporeArea,size=abundance,col=good.names))+
  geom_point(alpha=0.5)+scale_y_log10()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 5))+
  ggtitle("Bonfin2013 abundance with singletons")