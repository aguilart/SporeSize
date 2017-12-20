# SporeSize

Part I.
This part refers to the analysis performed for the manusript "Bridging reproductive and microbial ecology: a case study in AM fungi". In this manuscript the variation in spore size for AMF is described through histograms and compared to that of spores of other soil ascomycetes, seed sizes of plants and egg sizes (birds). Then it uses the data from the experiment of Miranda Hart (as reported in Hart and Reader 2002) for testing whether there is a trade-off between spore size and spore output in AMF following the Smith and Fretwell model

For conducting the analysis you need:

a) the scripts: 
AMF_Taxonomy_Traits_Schlusser.R  
#(here you find how to create figure 1 and 2 (except for the tree which Stefan is in charge), it is appropriately labelled in the script the specifc code for the figures)

SporeSize_SporeOutput.R 
#(here you find how to create figure 3 and 4, it is appropriately labelled in the script the specifc code for the figures)

b) the data:
AMF_Spore_Database_Volume.csv
ConidiaDataCompendiumAscos/ConidiaFromCompendium.csv
"tree.for.carlos.tre
Jeff_SporeAbundance_undisturbed_traits.txt





Part II.
This is for the project of "AMF spore size project along perturbation". NOTE!!! As for december 2017 this is in standby modus and needs to be updated. The comments below are old and might not apply anymore.

If you want to repeat the analysis you need two .csv files: a) AMF_NamesTraitDataComplete.csv (containing  the database with the AMF species accepted names and the spore size data); b) a particular csv. containg the AMF community compostion along a gradient of perturbation. For example: "Bonfin_etal2013.csv".

Then you will need the script: "AMF_Taxonomy_Traits_Schlusser.R". In this one you will update the spore size data and you will create a new dataframe containing the AMF accepted names and their synoyms. Iis necesary because it translate the names used in particular study to the accepted names. Then, spore size data can be assigned.

After running it, should have in your workspace the following dataframes:
1. "AMF_All_Copy" (the database with the AMF species accepted names and the spore size data);
2. "AMF_Taxonomy" (the data with the AMF accepted names and their synoyms.

Then, you can use the script I wrote for each particular study, for "Bonfin_etal2013.csv" it should be "Bonfin_etal2013.R". Running the script should return the figures with spore ranges along perturbation gradients.
