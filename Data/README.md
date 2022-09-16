## FODLER DATA
### Csv files:
- overview_metadata_Planet = metadata extracted from json-metadata files which got provided with the PlanetSocpe imagery by SEBAS
- SEBAS_FieldDATA_2020-2021-30032022 = Field data (provided by SEBAS)
- sat_data_mosaic = extracted pixel values from PlanetScope mosaics
- all = joined field and satellite data incl. different columns from preprocessing 
- **all_reduced = final data used in the models** (selection of columns from "all.csv")

### extract_pixel_values_gpkgs
#### quadrats
- gpkg with BEs (Explo_ALB etc), Plots (UAVPlots), and quadrats (quadrats_complete) (BE boundaries from BExIS website, rest SEBAS )

#### pixel_value_extraction_corr_plot_ids_mosaic
- quadrats_complete_corr_plotid_CLEAN = vector layer with quadrats ("clean" because only with relevent columns)
- ALB, HAI, SCH_zone33 = quadrats vector layer separated by BE in CRS of coresponding mosaics (SCH UTM zone 33, ALB/HAI zone 32)
- ALB_2020_06_XX etc. = vector layers with extracted pixel values with "zonal statistics" (outputs of processing model) of quadrats
- var1_clean = joined vector layers of all the 9 timestamps 
- var2_clean = ignore (different selection of mosaics)
- sat_data_mosaic = extracted pixel values that were then exported as csv file to "sat_data_mosaic" (join of var1_clean and var2_clean)

### FOLDER Mosaics_PlanetScope
- https://uni-bonn.sciebo.de/s/8kOpqRkj2TfpPTa pw: thesis_PlanetScope
- Mosaics created per timestamp of PlanetScope images (©PlanetScope)
- In "Overview_metadata_Planet" are listed all scenes with their metadata info
- all scenes of the same Exploratory and same timestamp (see in column "path") were used for a mosaic


### FOLDER mappen
- results of mapping some of the final models

### FOLDER profile lines
- shapefiles used for creating profile lines of mosaics

