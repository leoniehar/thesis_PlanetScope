## FOLDER "scripts of random forest models"
the folder includes the following scripts:
> general disclaimer: scripts of 'biomass interpolated' have nicer, more extensive comments
### some abbrevations etc.
- if 'layer' or 'timestep' is used somewhere it reffers to 'mosaic'
- 'month' refers to season 
- (spring, beginning = 'J', autumn, end of growing season = 'S')
- J = June and S = September
-  'explrtr', 'ex' = biodiversity Exploratory (study side)
-  sometimes short names of biomass/biomass interpolated are used
> - bio, Bio = biomass
> - interpolated_biomass, inter, bio_inter = biomass_interpolated

### 01_prepro
- preprocessing of field data set, joining with PlanetScope data, scaling PlanetScope data, Vegetation indice calculation
- output is "all_reduced.csv" which was used in the scripts of the actual models

### 02_plot, 03_plot, 04_plot XXX
- 02_plot & 03_plot: scripts of the plots created during exploratory data analysis/ preprocessing of field data
- 03_plot: plots created during exploratory data analysis of extracted pixel values from PlanetSocpe

### 10_rf_XX
- named after 10_rf_*response variable*_*type of training data set split*
- the scripts are the same, only the response variable and type of split is adapted as indicated in the name
- **the script '10_rf_interpolated_biomass_all' has extenive comments**
- **for a version including a split see '10_rf_interpolated_biomass_explrtr'**
> additionatly, at the beginning of a script there is a DISCLAIMER comment that gives information 
  if script is the nicely, extensive commented version and at what script to look if not 

### 20_read_RData 
- plots for evaluation of results 

### 20_over_accuarcy
- extract all performance metrics of the 10_rf_XX models into a .csv table

## FOLDER: "Processing model QGIS" 
- includes model which was used to extract the pixel values of the PlanetScope images
- applied to each on the mosaics separatly 

## FOLDER: "Plots_from_rendomforestmodel"
- includes the plots resulting from the 10_rf_XX scripts 
- structured as followed:
> response variable
>> Mtry (per model of the iterations from 5-fold-repeated-cv)

>> Outliers (after datasplit (perMonth,perExplrtr etc.), before training/testing data split)

>> Scatterplots_testing_rf (predicted and in-situ values of testing set per models)

>> Variable_importance_rf (predictors importance per model) 

## FOLDER: "sciprts_mappingofresults"
- includes scripts of "reduced" models with less predictors that were trained to make the models less complex for mapping
-  sub folder of response variable include
> #### new_models
>> - training of reduced models (scripts are the same as 10_rf_XX only with other selection of predictors)
> #### mappen
>> - **01_get_model**: import before trained models
>> - **03_*mosaic* _ *response_variable* _ *splittype* _ *variant from split*

- subfolder: "Getbandvalues" includes the script of importing the raster data as a dataframe in R

 
