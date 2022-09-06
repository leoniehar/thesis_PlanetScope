#### this scipt creates a csv file with basic performance metric, charcteristics of models
## based on the csv comparison files created in the random forest scripts

##clear workspace 
rm(list = ls())
##set working directory, file name of field and sat data 
## copy path to clipboard, next line changes windows path to linux
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

## get list of csv 
## searches for files with pattern in the wd
## list then contains all the csv file names
list = list.files(path=".", pattern="comparison .csv", all.files=TRUE,
                  full.names=TRUE, recursive = TRUE)

## create list
df = list()

## read in the file with loop so evrything in stored in list 
for (i in list){
  data = read.csv2(i)
  df [[i]] = as.data.frame(data)
}

## create data frame from list 
overview = Reduce(function(x, y) merge(x, y, all=TRUE), df)

## write csv
write.csv2(overview,file = "20_rf_overview_accuarcy.csv")
