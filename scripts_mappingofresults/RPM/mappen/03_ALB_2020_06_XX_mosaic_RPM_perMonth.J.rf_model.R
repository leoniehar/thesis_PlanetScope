### einmalig einstellen ####
## clear workspace 
rm(list = ls())

## set working directory, file name of field and sat data 
##\ müsste doppelt oder / (linux), wird hiermit ersetzt 
## zu mappen!!
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

#### load packages (from https://rpubs.com/jvaldeleon/forest_repeat_cv) ####
## (1) Define the packages that will be needed
packages <- c('dplyr', 'ggplot2', 'caret','tidyr', 'skimr'
              ,'cowplot', 'raster', 'rgdal')
## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
## (3) Load the packages into R session
invisible(lapply(packages, library, character.only = TRUE))

## import models
models = readRDS("models.RData")

## select type of model and estimation variable
print(names(models))
var = "RPM_perMonth.J.rf_model"
model = models[[var]]

rm(models)

## check
str(model)

## select to which mosaic applied
a = "ALB_2020_06_XX_mosaic"
skriptname= paste("03",a,var,sep="_")
print(skriptname)

## import rstDF RData file 
## this file includes the bands plus before calculated indices
rstDF = readRDS("ALB_2020_06_XX_mosaiconlyBANDS.RData")

## check 
head(rstDF)

## calculate vegetation indices
## indices calculation 
## ndvi Rouse 1973
rstDF$ndvi  = (rstDF$nir-rstDF$red)/(rstDF$nir+rstDF$red)

## evi2 Zhangyan Jiang et al. 2008; factors planet Reinermann 2020
rstDF$evi2  = (2.5*(rstDF$nir - rstDF$red))/(1+ rstDF$nir + 2.4*rstDF$red)

## evi Huete 2002; factors for planet data Reinermann 2020
G = 2.5 
f1= 6
f2= 7.5
L = 1
rstDF$evi   =  G*((rstDF$nir - rstDF$red)/(rstDF$nir + f1*rstDF$red - f2*rstDF$blue + L))

## savi Huete 1988; factors planet Reinermann / Pecina
L=0.5
rstDF$savi  = ((rstDF$nir - rstDF$red) / (rstDF$nir+ rstDF$red + L))*(1+L)

## dvi Richardson and Wiegand 1977
alpha = 0.96916
rstDF$dvi =rstDF$nir-alpha*rstDF$red

## gndvi Gitelson 1996
rstDF$gndvi = (rstDF$nir-rstDF$green)/(rstDF$nir+rstDF$green)

## green ratio vegetation index (grvi) Gitelson 2002
rstDF$grvi = rstDF$green/rstDF$red

## simple ratio Jordan 1969
rstDF$sr_nirr = rstDF$nir/rstDF$red 

## check if calculated
head(rstDF)

## 
rstDF_new = subset(rstDF, select = -c(blue,green,red,nir,ndvi,evi2))

rstDF = rstDF_new

rm(rstDF_new)

# apply RF model to raster df (get values)
predict_RST <- predict(model, rstDF)

## check
head(predict_RST)

## select the image to which model gets applied
rst = paste(a,".TIF", sep= "")
print(rst)

## import raster image
RS_rst <- stack(rst)

# create raster variable based on one image band (blanco raster)
RSTpred <- RS_rst[[1]]


# assign values to raster (assgin values to blanco raster)
values(RSTpred)[!is.na(values(RSTpred))] <- predict_RST


# save  raster
writeRaster(RSTpred, filename=paste0("Raster",var,rst, ".tif"), format="GTiff", overwrite=TRUE)
