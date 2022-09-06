#### einmalig einstellen ####
## clear workspace 
rm(list = ls())

## set working directory, file name of field and sat data 
##\ müsste doppelt oder / (linux), wird hiermit ersetzt 
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

#### import model lists #####
## get list of .Rdata of models
## df is a list containing all the info lists from the randomforest models 
list = c(list.files(path=".", pattern="LAI.RData", all.files=TRUE,
                    full.names=TRUE, recursive = TRUE)
         ,list.files(path=".", pattern="LAI.RData", all.files=TRUE,
                     full.names=TRUE, recursive = TRUE)
         ,list.files(path=".", pattern="LAI_interpolated.RData", all.files=TRUE,
                     full.names=TRUE, recursive = TRUE)
         ,list.files(path=".", pattern="RPM.RData", all.files=TRUE,
                     full.names=TRUE, recursive = TRUE)
)

## create list
df = list()

#loop for import Rdata
for (i in list){
  df[[i]] = readRDS(i)
  #names(df[i])<- name[i]
}

## set wd to subfolder
setwd("./mappen")
getwd()

## creat short, pretty names for imported models (instead of paths)
print(list)
name = c("LAI_all","LAI_perExplrtr","LAI_perMonth")

## change names of df list
names(df)<- name

#### extract sublists/ data frame from df ####
## flatten/ sublists 
flat = unlist(df, recursive = FALSE)

## get index of lists with comparison, selection or dataset
# df is a data frame with all the information saved from scripts from model train
# level 1 sublists are .compare, .selection, .dataset and list of models
# here the lists of models are extracted

## get the positions where sublist that are NOT of intrest here 
## its done this way because the names of list of models vary more
select = paste(grep(".compare",names(flat)), grep(".selection",names(flat))
               ,grep(".dataset",names(flat)))

## split string (return is a list)
select = strsplit(select,split=" ")

## flatten relationship, to numric, sort 
select = sort(as.numeric(unlist(select)))

## select elements of list that are not of the above
# variants includes only the sublists of the models
variants = flat[-select]

## name var
var = "all"

## remove flat and select for clearing work space 
rm(select)

#### extract lists/dataframes from variants lists ####
## extract: rf_model lists, variable importance and validation (list containing 
## predicted and in-situ data a.o)

## prepare structure for plots
## flatten variants
flat_var =  unlist(variants, recursive = FALSE)

## extract actual models 
## variants level 1 sublists are containing a lot of information 
## not only the actual model
## the actual rf model are sublists of variants
## lists with rf_model is output from caret/ repeated 5 fold dv / model train

## search for position of actual models
## models is a list of all the rf models
models = flat_var[grep(".rf_model",names(flat_var))]

## print model names
print(names(models))


## clear workspace
rm(variants)
rm(flat)
rm(flat_var)
rm(df)

## for exectuting somewhere else
saveRDS(models, file='models.RData')
rm(models)



##### XXX bis hier dann skript ende