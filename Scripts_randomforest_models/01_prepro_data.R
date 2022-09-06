##clear workspace 
rm(list = ls())
##set working directory, file name of field and sat data 
##\ müsste doppelt oder / (linux), wird hiermit ersetzt 
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

#-------------------------------------------------------------------------
#### Join satellite and field data and add unique id ####

## names of csv files mit satelite and field data
sat_all_csv_file_name= "sat_data_mosaic.csv"
field_all_csv_file_name= "SEBAS_FieldData_2020-2021_30032022 (1).csv"

## csv Datei Import ##
sat_all= read.csv(sat_all_csv_file_name)
field_all = read.csv2(field_all_csv_file_name)

## change "layer" in sat all to mosaic 
sat_all$mosaic = sat_all$layer
sat_all = subset(sat_all, select = -layer)

sat_all$var2_mosaic = sat_all$var2_layer
sat_all = subset(sat_all, select = -var2_layer)

## check struture
str(sat_all)
str(field_all)

## add unique id to field data 
field_all$unique_id = 1:1150
for (i in nrow(field_all)){
  field_all$unique_id[]= paste(field_all$Qnum[],field_all$month[],field_all$year[], sep='_')
}

## add timestep/mosaic to field data 
field_all$timestep = 1:1150
for (i in nrow(field_all)){
  field_all$timestep[]= paste(field_all$explo[],field_all$month[],field_all$year[], sep='_')
}

## join all (sat and field)
all = merge(field_all, sat_all, by='unique_id')

## remove field and sat from r environment 
rm(field_all)
rm(sat_all)

#-------------------------------------------------------------------------
#### FIELD DATA consistency/ units (exploratory data analysis)####

#### LAI ####
## add extra column for corrected LAI
all$LAI_corr <- all$HHLAI

## convert to conventional unit (irrelevant for LAI)

## see range 
summary(all$LAI_corr)
summary(!is.na(all$LAI_corr))
# => no values below 0 
# => 83 NAs, 1067 sample values

## (look at plots)

## define range where LAI values are considered consistent 
# below 0 does not make sense because of formula => would be calculation error/typo
# 15 s above maximum value 
range_LAI = c(0,15) 

## set all values that are out of range to NA (delete)
is.na(all['LAI_corr']) <- (all['LAI_corr'] < range_LAI[1] | all['LAI_corr'] > range_LAI[2])
summary(!is.na(all$LAI_corr))
# => 1067 values and 83 NAs 

#### biomass ####
## add extra column for corrected biomass
all$biomass_corr <- all$Biomass_g_per60cm2

## convert to conventional unit
# divide by 0.36 because uni before was g/0.36m^2 (60cm*60cm = 0.36m^2)
all$biomass_corr <- all$biomass_corr/0.36
# => biomass unit now: g/m^2

## safe column with correct unit 
all$Biomass_g_per_m2 <- all$biomass_corr

## see range 
summary(all$biomass_corr)
summary(!is.na(all$biomass_corr))
# => no values below 0 
# => 825 NAs, 325 sample values

## (look at plots)

## define range where biomass values are considered consistent 
range_biomass = c(0,1500) 

## set all values that are out of range to NA (delete)
is.na(all['biomass_corr']) <- (all['biomass_corr'] < range_biomass[1] | all['biomass_corr'] > range_biomass[2])
summary(!is.na(all$biomass_corr))
# => 825 NAs, 325 sample values

#### biomass intepolated ####
## add extra column for corrected LAI
all$biomass_interpolated_corr <- all$Biomass_interpolated

## convert to conventional unit
# divide by 0.36 because uni before was g/0.36m^2 (60cm*60cm = 0.36m^2)
all$biomass_interpolated_corr <- all$biomass_interpolated_corr/0.36
# => biomass_interpolated unit now: g/m^2

## safe column with correct unit 
all$Biomass_interpolated_per_m2 <- all$biomass_interpolated_corr

## see range 
summary(all$biomass_interpolated_corr)
summary(!is.na(all$biomass_interpolated_corr))
# => 1150 sample values 

## (look at plots)

"# for biomass interpolation, RPM values were used. If RPM values are inconsistent
biomass interpolated values should be not be used (only if biomass was sampled because
 then the value of the direct sample was used)
 after reviewing the data: RPM values of 0 are only apparent in quadrats that were not visited
 (date_releves = NA or no data for any other sample variable)
 summary: if (RPM <=0 OR RPM is NA) AND (biomass is not NA) 
             then biomass_interpolated = NA
#"

## see amount of cases inconsistent RPM
summary(all$RPMcalc_cm <= 0 |  is.na(all$RPMcalc_cm))
  #=>in total 77 (where RPMcalc should not be used for interpolation)#

## if biomass was sampled, RPM value is not used=> filter these cases 
summary((all$RPMcalc_cm <= 0 |  is.na(all$RPMcalc_cm))
        & is.na(all$Biomass_g_per60cm2))
  #=> total 72 remaining 

## replace the 72 values with NAs
is.na(all$biomass_interpolated_corr)<- ((all$RPMcalc_cm <= 0 |  is.na(all$RPMcalc_cm))
                                        & is.na(all$Biomass_g_per60cm2))
summary(is.na(all$biomass_interpolated_corr))
#=> 72 values replaced by NA

## define range where biomass_interpolated values are considered consistent 
# negativ biomass not possible, maximum biomass is below 200 
range_biomass_interpolated = c(0,1500) 

## see if in range
summary(all$biomass_interpolated_corr>range_biomass_interpolated[1])
summary(all$biomass_interpolated_corr<range_biomass_interpolated[2])
# => 8 values below 0 (happens when RPM is small because linear regression)
# and 72 NAs

## set all values that are out of range to NA (delete) 
is.na(all['biomass_interpolated_corr']) <- (all['biomass_interpolated_corr'] < range_biomass_interpolated[1] | all['biomass_interpolated_corr'] > range_biomass_interpolated[2])
summary(!is.na(all$biomass_interpolated_corr))
# => 80 NAs, 1070 sample values 

#### RPM ####
## add extra column for corrected RPM
all$RPM_corr <- all$RPMcalc_cm

## convert to conventional unit
# cm ok

## see range 
summary(all$RPM_corr)
summary(!is.na(all$RPM_corr))
# => 1135 sample values  (15 NAs )

## (look at plots)

## values of 0 are considered inconsistent (see biomass_interpolated)
# see amount of cases
summary(all$RPMcalc_cm <= 0 )
# 62 values below/equal to zero AND 15 NAs

## define range where RPM values are considered consistent 
range_RPM = c(0,100) 

## see if in range  (>= so zero is excluded)
summary(all$RPM_corr<=range_RPM[1])
summary(all$RPM_corr>range_RPM[2])
# => 62 values below/equal to 0 and 15 NAs

## set all values that are out of range to NA (delete)
is.na(all['RPM_corr']) <- (all['RPM_corr'] <= range_RPM[1] | all['RPM_corr'] > range_RPM[2])
summary(!is.na(all$RPM_corr))
# => 77 NAs, 1073 sample values 

## add final columns with pretty names 
## add final field (same as corr, changed, pretty colname)
all$biomass = all$biomass_corr
all$biomass_interpolated = all$biomass_interpolated_corr
all$LAI = all$LAI_corr
all$RPM = all$RPM_corr
#-------------------------------------------------------------------------
#### SATELLITE DATA (exploratory data anaylsis) ####
#### units ####
## planet data SR values are scaled by 10.000; conversion to [0;1]

## original band names
satbands_og = c("b1_mean_round","b2_mean_round","b3_mean_round","b4_mean_round"
             ,"var2_b1_mean_round","var2_b2_mean_round","var2_b3_mean_round","var2_b4_mean_round") 

## scale corrected band names
satbands = c("b1_mean_round_scaled","b2_mean_round_scaled","b3_mean_round_scaled","b4_mean_round_scaled"
             ,"var2_b1_mean_round_scaled","var2_b2_mean_round_scaled","var2_b3_mean_round_scaled","var2_b4_mean_round_scaled")
## divide by 10000
all[,satbands] = all[,satbands_og]/10000

#### indices ####

## vector for both variants prefix
## var2 for different combination of mosaics etc (in the end not used)
variants = c('','var2_')
a = 1:length(variants)

## loop returning pretty band names and indices calculation 
for (i in a){
  pre= variants[i]
  
  ## column names depending on variant
  b=paste(pre,'b1_mean_round_scaled',sep='')
  g=paste(pre,'b2_mean_round_scaled',sep='')
  r=paste(pre,'b3_mean_round_scaled',sep='')
  n=paste(pre,'b4_mean_round_scaled',sep='')
  
  ## extract bands from dataset
  blue=all[,b]
  green= all[,g]
  red=all[,r]
  nir=all[,n]
  
  ## indices calculation 
  ## ndvi Rouse 1973
  ndvi  = (nir-red)/(nir+red)
  
  ## evi2 Zhangyan Jiang et al. 2008; factors planet Reinermann 2020
  evi2  = (2.5*(nir - red))/(1+ nir + 2.4*red)
  
  ## evi Huete 2002; factors for planet data Reinermann 2020
  G = 2.5 
  f1= 6
  f2= 7.5
  L = 1
  evi   =  G*((nir - red)/(nir + f1*red - f2*blue + L))
  
  ## savi Huete 1988; factors planet Reinermann / Pecina
  L=0.5
  savi  = ((nir - red) / (nir+ red + L))*(1+L)
  
  ## dvi Richardson and Wiegand 1977
  alpha = 0.96916
  dvi =nir-alpha*red
  
  ## gndvi Gitelson 1996
  gndvi = (nir-green)/(nir+green)
  
  ## green ratio vegetation index (grvi) Gitelson 2002
  grvi = green/red
  
  ## simple ratio Jordan 1969
  sr_nirr = nir/red 
  
  ## add to data frame 
  ## bands 
  all[paste(pre,'blue',sep='')]= blue
  all[paste(pre,'green',sep='')]= green
  all[paste(pre,'red',sep='')]= red
  all[paste(pre,'nir',sep='')]= nir
  
  ## indices 
  all[paste(pre,'ndvi',sep='')]= ndvi
  all[paste(pre,'evi2',sep='')]=evi2
  all[paste(pre,'evi',sep='')]=evi
  all[paste(pre,'savi',sep='')]=savi
  all[paste(pre,'dvi',sep='')]=dvi
  all[paste(pre,'gndvi',sep='')]=gndvi
  all[paste(pre,'grvi',sep='')]=grvi
  all[paste(pre,'sr_nirr',sep='')]=sr_nirr
}
write.csv2(all, file= 'all.csv')

#### reduce data set ####
## print column names
names<- colnames(all)
print(names)

## select columns
all_reduced = all[,c(
  "unique_id","timestep","explrtr","Useful_PlotID","Qnum","month"
  ,"year","date_releves"
  #og fields
  ,"HHLAI","Biomass_g_per60cm2","Biomass_interpolated","RPMcalc_cm"
  #biomass and biomass interpolated og but corect unit
  ,"Biomass_g_per_m2","Biomass_interpolated_per_m2"
  #corr field
  ,"LAI_corr","biomass_corr","biomass_interpolated_corr","RPM_corr" 
  # final field fields 
  ## add final field (same as corr, changed, pretty colname)
  , "LAI","biomass","biomass_interpolated","RPM"
  # mosaic (date and id) of sat
  ,"mosaic", "var2_mosaic"
  #og var 1 (round)
  ,"b1_mean_round","b2_mean_round","b3_mean_round","b4_mean_round" 
  #og var 2 (round)
  ,"var2_b1_mean_round","var2_b2_mean_round","var2_b3_mean_round","var2_b4_mean_round" 
  #final bands (same as corr_scaled) and indices var1
  ,"blue","green","red","nir"
  ,"ndvi", "evi2","evi", "savi","dvi","gndvi","grvi"                  
  ,"sr_nirr"  
  #final bands and indices var2
  ,"var2_blue","var2_green","var2_red","var2_nir"
  ,"var2_ndvi", "var2_evi2","var2_evi", "var2_savi","var2_dvi","var2_gndvi","var2_grvi"                  
  ,"var2_sr_nirr" 
)]
rm(all)

## write csv to read in for plot scripts
write.csv2(all_reduced, file= 'all_reduced.csv')

"#
plots for field data against NIR 
// before and after only for biomass_interpolated and RPM because for 
// biomass and LAI before = after

plots LAI against biomass

plots for distribution of bands 
#"

