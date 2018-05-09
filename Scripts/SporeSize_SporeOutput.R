#########################################################################
###Correlations spore size with other traits#############################
#########################################################################

#loading packages
library(smatr)
library(tidyverse)

#Conversion factors of different units for mycelia and spores to biomass

LengthToBiomass_Cf<-((4*10^-6)^2)*pi*((100)^3)*1.09*.21*((10)^6) #Based on Bakken and Olsen (1983)

SporeVolumeToBiomass_Cf<-1.5/4113430 #Based on Beilby and Kidby (1980)

#Data preparation. This data correspond to the ones published in Hart and Reader 2002
#and Hart and Reader 2004 as mentioned in the manuscript. 
Jeff_AMF_Traits<-read.table("Jeff_SporeAbundance_undisturbed_traits.txt",header = T,stringsAsFactors = F)

                  Jeff_AMF_Traits$tempNames<-Jeff_AMF_Traits$Genus_species;
                  Jeff_AMF_Traits$tempNames<-sub("\\d$","",Jeff_AMF_Traits$tempNames);
                  Jeff_AMF_Traits$tempNames<-sub("[F,I,J,K,Q]","",Jeff_AMF_Traits$tempNames);
                  Jeff_AMF_Traits$tempNames<-sub("_$","",Jeff_AMF_Traits$tempNames)
                  
                  Jeff_AMF_Traits<-Jeff_AMF_Traits[c(16,1:15)]
                  
                  Jeff_AMF_Traits<-secondTrial(AMF_Taxonomy,Jeff_AMF_Traits)
                  
                  Jeff_AMF_Traits$tempNames[c(25,26,27,28)]<-"Rhizophagus_aggregatus"
                  names(Jeff_AMF_Traits)[1]<-"good.names"
                  
                  Jeff_AMF_Traits<-left_join(Jeff_AMF_Traits,AMF_All_Copy[,c(1,35)])
                  
                  Jeff_AMF_Traits$root_fungalBiomass<-((Jeff_AMF_Traits$root_ergo+0.18)/0.4)*LengthToBiomass_Cf
                  Jeff_AMF_Traits$soil_fungalBiomass<-Jeff_AMF_Traits$hyphlength*LengthToBiomass_Cf
                  
                  Jeff_AMF_Traits$total_fungalBiomass<-Jeff_AMF_Traits$root_fungalBiomass+
                    Jeff_AMF_Traits$soil_fungalBiomass
                  
                  #I also create a summary of this data, with just the means of each variable
                  #for each fungus per host.
                  Jeff_Traits_Summary<-aggregate(Jeff_AMF_Traits[5:20],
                                        by=list(Jeff_AMF_Traits$host,Jeff_AMF_Traits$good.names),
                                        mean,na.rm=T);
                                        names(Jeff_Traits_Summary)[1]<-"host";
                                        names(Jeff_Traits_Summary)[2]<-"good.names"
                  
                  Jeff_Traits_Summary$SporeMass<-Jeff_Traits_Summary$SporeVolume*
                                                  SporeVolumeToBiomass_Cf
                  
                  Jeff_Traits_Summary$no_spore_FungMass<-Jeff_Traits_Summary$no_spores/
                                                        Jeff_Traits_Summary$total_fungalBiomass
                  
                  Jeff_Traits_Summary$Total_MassForSpores<-Jeff_Traits_Summary$no_spores*
                                                              Jeff_Traits_Summary$SporeMass
                  
                  #Just to know the spore dimension of the fungi that Jeff has in his data
                  
                  AMF_All_Copy[AMF_All_Copy$good.names%in%Jeff_Traits_Summary$good.names,
                               c(1,7,8,11,12,15,35)][
                                 order(AMF_All_Copy[AMF_All_Copy$good.names%in%Jeff_Traits_Summary$good.names,
                                                    c(1,7,8,11,12,15,35)]$SporeVolume),]
                  
######################################################################################                  
#####  SPORE OUTPUT~SPORE MASS #######################################################
######################################################################################

#First correlation, Spore output and spore mass. We expect a negative correlation indicating that, under
# a given amount of resources, the fungus has to decide in between producing a lot of small spores or few
# large one. Testing this relationship in the log-log scales also indicate if the total amount of resources
# allocated to reproduction is constant, that is regardless which strategy, the fungus allocate equal amount
# of resources in spore production

                  
#First a simple correlation between the two, no corrections

#############
##Figure 3a##
#############
#This one correspond to Figure 3a in the Manuscript
Jeff_Traits_Summary%>%
  ggplot()+
  aes(x=SporeMass,y=no_spores,
      color=host,size=2)+
  geom_point()+
  scale_y_log10(breaks=c(10^0.5,10^1,10^1.5),
  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  #geom_text(aes(label=good.names))+
  labs(x=expression("Spore mass"~"("*mu*g*")"),
      y=expression(Number~spores~fungus^{-1}))+
  ggtitle(label="Spore output vs. spore size",
          subtitle = "slope= -0.6 (C.I= -0.81,-0.50), r2 = 0.13  (p = 0.004)" )+
  geom_abline(slope = -0.64,intercept = 1.07)+
  #geom_abline(slope = -0.23,intercept = 1.07,lty=2)+
  geom_abline(slope=-1,intercept = 1.07,lty=2)+
  theme(title = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size=10),
        legend.position = "none")
  # scale_y_continuous(limits = c(0,2.5))+
  # scale_x_continuous(limits = c(-1.5,1.5))
  
  
  #Normal regression
  confint(
  #summary.lm(
    lm(log10(no_spores)~log10(SporeMass),data = 
          #Jeff_AMF_Traits))
          Jeff_Traits_Summary))
  
  #Standarized major axis series of analysis:
  # 1. Testing wehther host influces the slope of the relationship & wehther the relationship
  # is inversely proportinal (testing wehther slope = -1)
      summary(
      sma(no_spores ~ SporeMass*host,
          data = Jeff_Traits_Summary, log = "xy",slope.test = -1))
  # 2. Testing whether the host influences the intercept (aka elevation of the relationship) 
      summary(
        sma(no_spores ~ SporeMass+host,
            data = Jeff_Traits_Summary, log = "xy",slope.test = -1))
      
  #Results: 1) Plant host does not alter the slope of the relationship between 
  #           spore production and spore size, nor the intercept (FYI, in this package
  #           intercept is called elevation)
  #         2) The slope is different from -1, meaning the relationship is not 
  #           inversely proportional. 
  # Based on this, I decided to ignore host as a driver, however I decided not just average
  # the spore output for each host, as they are real replicates. 
  #      (maybe a point of conflict, am I inflating the degrees of freedom?)
  # Thus, the model in the end looks like:
      summary(
        sma(no_spores ~ SporeMass,
            data = Jeff_Traits_Summary, log = "xy",slope.test = -1))
  # Which returns the following parameters:
  # Coefficients:
      #             elevation      slope
      # estimate     1.038594 -0.6412302
      # lower limit  0.921332 -0.8170478
      # upper limit  1.155856 -0.5032462
      # 
      # H0 : variables uncorrelated
      # R-squared : 0.1333479 
      # P-value : 0.0041194 
  
   

# Second. Correcting for total fungal size (measured in biomass of fungi)

###############
###Figure 3b###      
###############
      
#This one correspond to figure 3b in the manuscript
  Jeff_Traits_Summary%>%
    ggplot()+
    aes(x=SporeMass,
        y=(no_spores/total_fungalBiomass),color=host,size=2)+
    geom_point()+
    scale_y_log10(breaks=c(10^-1.5,10^-1,10^-0.5),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    #geom_text(aes(label=good.names))+
    labs(x=expression("Spore mass"~"("*mu*g*")"),
         y=expression(Number~spores~(gram.fungus)^{-1}))+
    ggtitle(label="Spore output per gram of fungus vs. spore size",
            subtitle = "slope= -0.59 (CI=-0.75,-0.47), r2= 0.2 (p=0.0002)")+
    geom_abline(slope = -0.59,intercept = -0.94)+
    #geom_abline(slope = -0.23,intercept = 1.07,lty=2)+
    geom_abline(slope=-1,intercept = -0.94,lty=2)+
    theme(title = element_text(size = 25),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size=10)#,
          #legend.position = "none"
          )
  
  #normal regression
  confint(
  #summary.lm(
    lm(log10(no_spore_FungMass)~
       log10(SporeMass),
       data = 
         Jeff_Traits_Summary))
        #Jeff_AMF_Traits))
  
  #Standardized major axis
  summary(
    sma(no_spore_FungMass ~ SporeMass+host,
        data = Jeff_Traits_Summary, log = "xy",slope.test=-1))
  #Results from analysis:
  # Similar when total fungal biomass is not taken into account (see above),
  # - Host has not influence neither in the slope nor the intercept of the relationship
  # - The relationship is different from -1 (spore output per gram not inversely proportional
  # - to spore size)
  # - Thus, the anlysis used is: 
  summary(
    sma(no_spore_FungMass ~ SporeMass,
        data = Jeff_Traits_Summary, log = "xy",slope.test = -1))
  # -Resulting in the following parameters:
  # Coefficients:
  #             elevation      slope
  # estimate    -0.9436634 -0.5969789
  # lower limit -1.0447225 -0.7526987
  # upper limit -0.8426042 -0.4734746
  # 
  # H0 : variables uncorrelated
  # R-squared : 0.2083189 
  # P-value : 0.00024703 
  

######################################################################################                  
#####  TOTAL AMOUNT OF RESOURCES FOR SPORE PRODUCTION ~ SPORE MASS ###################
######################################################################################  

###############
###FIGURE 3c###
###############
  
  #This one correspond to figure 3c in the manuscript
  Jeff_Traits_Summary%>%
    ggplot()+
    aes(x=SporeMass,
        y=Total_MassForSpores,
        color=host,size=2)+
    geom_point()+
    scale_y_log10(breaks=c(10^0,10^0.5,10^1,10^1.5,10^2),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_x_log10(
      labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    #geom_text(aes(label=good.names))+
    labs(x=expression("Spore mass"~"("*mu*g*")"),
         y= expression(Mass~allocated~to~spore~production~"("*mu*g*")"))+
    ggtitle(label = "Mass allocated to reproduction vs. spore size",
            subtitle ="slope= 0.97 (CI=0.82,1.14), r2=0.62 (p<<0.001)" )+
    #geom_abline(slope=0.76, intercept = 1.07)+
    geom_abline(slope = 0.97,intercept = 1.08,lty=2)+
    theme(title = element_text(size = 25),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position =  "none")
  
  #Regression analysis
  confint(
  #summary.lm(
    lm(log10(Total_MassForSpores)~
         log10(SporeMass),
       data = 
         Jeff_Traits_Summary))
  #Jeff_AMF_Traits))
  
  #Standardized major axis
  summary(
    sma(Total_MassForSpores ~ SporeMass,
        data = Jeff_Traits_Summary, log = "xy"))
  
  #Results:
  # Coefficients:
  #            elevation     slope
  # estimate    1.0882362 0.9710110
  # lower limit 0.9856066 0.8267049
  # upper limit 1.1908657 1.1405065
  # 
  # H0 : variables uncorrelated
  # R-squared : 0.6220586 
  # P-value : 7.2854e-14 
  
  
  #Results from this analysis indicate that
  #1. As expected based on the previous analysis, host has no influence
  #   neither in the slope nor the intercept of this relationship
  #2. There is a positive correlation between spore size and total amount of resources
  #   allocated to spore production. 
  #3. The slope is "quite high" still the slope of the regression is shallower (and congruent
  #   with the slope obtained in the analysis no_spores~sporeMass). For some weird reason the 
  #   the slope of the SMA is not congruent with the analyis of no_spores~sporeMass.
  # The results are:
          #Coefficients:
          #             elevation     slope
          # estimate    1.0882362 0.9710110
          # lower limit 0.9856066 0.8267049
          # upper limit 1.1908657 1.1405065
          # 
          # H0 : variables uncorrelated
          # R-squared : 0.6220586 
          # P-value : 7.2854e-14 
  

######################################################################################                  
#####  SPORE OUTPUT ~ FUNGAL BIOMASS  ################################################
######################################################################################

###############
###FIGURE 4a###
###############
  
  #This one correspond to figure 4a in the manuscript 
  Jeff_Traits_Summary%>%
    ggplot()+
    aes(y=no_spores,
        x=total_fungalBiomass,
        color=host,size=2)+
    geom_point()+
    scale_y_log10(breaks=c(10^0.5,10^1,10^1.5),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_x_log10(breaks=c(10^1.7,10^1.8,10^1.9,10^2.0,10^2.1),
      labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    labs(x=expression("Total fungal mass"~"("*mu*g*")"),
         y=expression(Number~spores~fungus^{-1}))+
    ggtitle(label="Spore output vs Fungal biomass",
            subtitle = "r2= 0.13 (p=0.0035)")+
    theme(title = element_text(size = 25),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = "none")
  
  summary.lm(
    lm(log10(no_spores)~
         log10(total_fungalBiomass),
       data = Jeff_Traits_Summary))
  
  summary(
    sma(no_spores ~ total_fungalBiomass,
        data = Jeff_Traits_Summary, log = "xy"))
  
  
  #Results:
  # Coefficients:
  #            elevation    slope
  # estimate    -5.355673 3.246103
  # lower limit -6.933817 2.548910
  # upper limit -3.777528 4.133997
  # 
  # H0 : variables uncorrelated
  # R-squared : 0.1371333 
  # P-value : 0.0035864 
  
  
  
######################################################################################                  
#####  TOTAL FUNGAL BIOMASS ~ SPORE MASS  ############################################
######################################################################################

###############
###FIGURE 4b###
###############
  
#This one correspond to Figure 4b in the manuscript
  Jeff_Traits_Summary%>%
    ggplot()+
    aes(x=total_fungalBiomass,
        y=SporeMass,
        color=host,size=2)+
    geom_point()+
    scale_y_log10(breaks=c(10^-1,10^-0.5,10^0,10^0.5,10^1),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_x_log10(breaks=c(10^1.7,10^1.8,10^1.9,10^2.0,10^2.1),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    labs(x=expression("Total fungal mass"~"("*mu*g*")"),
         y=expression("Spore mass"~"("*mu*g*")"))+
    ggtitle(label="Spore mass vs fungal biomass",
            subtitle = "r2=0.03 (p=0.14)")+
  theme(title = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position =  "none")
  
  summary.lm(
    lm(log10(SporeMass)~
         log10(total_fungalBiomass),data = Jeff_Traits_Summary))

  summary(
    sma(SporeMass ~ total_fungalBiomass,
        data = Jeff_Traits_Summary, log = "xy"))
  
  #Results
  # Coefficients:
  #              elevation    slope
  # estimate    -10.134039 5.062306
  # lower limit -12.734241 3.922583
  # upper limit  -7.533837 6.533179
  # 
  # H0 : variables uncorrelated
  # R-squared : 0.03762316 
  # P-value : 0.13754 

  
##############################################################################################  
###############Phylogenetic corrections ######################################################
##############################################################################################

  
library(picante)
library(phytools)

#loading tree and creating one just for Jeff´s data. Tree was done by Stefan
AMF_Tree<-read.tree("tree.for.carlos.tre")

AMF_Tree_JeffData<-
  drop.tip(AMF_Tree,
           AMF_Tree$tip.label[!AMF_Tree$tip.label%in%Jeff_AMF_Traits$good.names])

#Creating a vector with the variables of interest
Jeff_Traits_SummaryBySpecies<-aggregate(Jeff_AMF_Traits[5:20],
                      by=list(Jeff_AMF_Traits$good.names),
                      mean,na.rm=T);names(Jeff_Traits_SummaryBySpecies)[1]<-"good.names"

Jeff_SporeSize<-Jeff_Traits_SummaryBySpecies[,14];# using volume
                names(Jeff_SporeSize)<-Jeff_Traits_SummaryBySpecies[,1]

Jeff_SporeOutput<-Jeff_Traits_Summary[,9];# using no_spores
                  names(Jeff_SporeOutput)<-Jeff_Traits_Summary[,1]

#testing for phylogenetic conservatism for only the Jeff Data
phylosig(AMF_Tree_JeffData,Jeff_SporeSize,method = "lambda",test = T)
phylosig(AMF_Tree_JeffData,Jeff_SporeOutput,method = "lambda",test = T)
phylosig(AMF_Tree_JeffData,Jeff_SporeSize,method = "K",test = T)

#creating a new vector with the phylogenetic independent contrasts, the
#fungus Septoglomus_constrictum and Dentiscutata_heterogama are not in the
#tree and need to be removed from the dataset.

pic_SporeVol<-pic(log10(Jeff_SporeSize[-c(6,15)]),AMF_Tree_JeffData)
pic_SporeOutput<-pic(log10(Jeff_SporeOutput[-c(6,15)]),AMF_Tree_JeffData)

summary.lm(
  lm(pic_SporeOutput~pic_SporeVol))

ggplot()+
  aes(x=pic_SporeVol,
      y=pic_SporeOutput)+
  geom_point()

