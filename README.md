# SporeSize
AMF spore size project along perturbation
If you want to repeat the analysis you need two .csv files: a) AMF_NamesTraitDataComplete.csv (containing  the database with the AMF species accepted names and the spore size data); b) a particular csv. containg the AMF community compostion along a gradient of perturbation. For example: "Bonfin_etal2013.csv".

Then you will need the script: "AMF_Taxonomy_Traits_Schlusser.R". In this one you will update the spore size data and you will create a new dataframe containing the AMF accepted names and their synoyms. Iis necesary because it translate the names used in particular study to the accepted names. Then, spore size data can be assigned.

After running it, should have in your workspace the following dataframes:
1. "AMF_All_Copy" (the database with the AMF species accepted names and the spore size data);
2. "AMF_Taxonomy" (the data with the AMF accepted names and their synoyms.

Then, you can use the script I wrote for each particular study, for "Bonfin_etal2013.csv" it should be "Bonfin_etal2013.R". Running the script should return the figures with spore ranges along perturbation gradients.
