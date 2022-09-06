## DISCLAIMER: for a nicely commented version see "10_rf_interpolated_biomass_explrtr"
## difference here: split per mosaic instead of Explrtr

#### Random forest model for interpolated biomass and split per mosaic

##clear workspace 
rm(list = ls())
##set working directory, file name of field and sat data 
##\ müsste doppelt oder / (linux), wird hiermit ersetzt 
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

all_csv_file_name= "all_reduced.csv"
## csv Datei Import 
all= read.csv2(all_csv_file_name)

## set wd to variable folder
setwd("./rf_biomass_interpolated")
getwd()

## load packages
## (1) Define the packages that will be needed
packages <- c('dplyr', 'ggplot2', 'caret','tidyr', 'skimr')
## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
## (3) Load the packages into R session
invisible(lapply(packages, library, character.only = TRUE))

#### sort and select data ####
# ids are for identifying 
# est_var is response variable
# predictors are model input
# variables are for creating data subset with input plus response data and no other
# all is variables plus ids
ids =  c("unique_id","mosaic","explrtr","Useful_PlotID"
         ,"Qnum","month","year")
est_var= 'biomass_interpolated'  
predictors = c('blue','green','red','nir',
               "ndvi","evi2","evi","savi","dvi","gndvi","grvi"
               ,"sr_nirr"
               )
variables= c(est_var #response (estimation) variable 
            ,predictors #predictors (explanatory vars)
            )
## data frame with selected data
all=all[c(ids,variables)]


## create list for storing information of model
info = list()
info$selection = list(ids = ids
                      ,est_var = est_var
                      ,predictors = predictors
                      ,variables = variables
                      )

info$dataset$all_og = all

## delete NA
all = all %>% drop_na(est_var)

## convert band values (int) to numeric
all[c('blue','green','red','nir')] <- lapply(all[c('blue','green','red','nir')], as.numeric)

#### split data ####
info_variant = 'perMosaic'

## change from mosaic name to short form ("timpestep")
A_J_2020 <- all[all$mosaic == "ALB_2020_06_XX",]
A_S_2020 <- all[all$mosaic == "ALB_2020_09_13",]
A_S_2021 <- all[all$mosaic == "ALB_2021_09_23",]
H_S_2020 <- all[all$mosaic == "HAI_2020_09_13",]
H_J_2020 <- all[all$mosaic == "HAI_2020_06_01",]
H_J_2021 <- all[all$mosaic == "HAI_2021_05_09",]
H_S_2021 <- all[all$mosaic == "HAI_2021_09_08",]
S_J_2021 <- all[all$mosaic == "SCH_2021_05_30",]
S_S_2021 <- all[all$mosaic == "SCH_2021_09_09",]

ex = list(A_J_2020,A_S_2020,A_S_2021,H_S_2020,H_J_2020
          ,H_J_2021,H_S_2021,S_J_2021,S_S_2021)

ex_name = c("A_J_2020","A_S_2020","A_S_2021","H_S_2020","H_J_2020"
            ,"H_J_2021","H_S_2021","S_J_2021","S_S_2021")
a = 1:length(ex)

info$A_J_2020 = list()
info$A_S_2020 = list()
info$A_S_2021 = list()
info$H_S_2020 = list()
info$H_J_2020 = list()
info$H_J_2021 = list()
info$H_S_2021 = list()
info$S_J_2021 = list()
info$S_S_2021 = list()

for (i in a){
'#------------------------ ab hier alles mit subsets-------------------#'
  ## Dataset 
  dataset = ex[[i]][c(est_var, predictors,ids)]
  
  info[[ex_name[i]]][['ids']]['year'] =  if (length(unique(dataset[,'year']))==1) unique(dataset[,'year'])
  else NA
  
  info[[ex_name[i]]][['ids']]['explrtr'] =  if (length(unique(dataset[,'explrtr']))==1) unique(dataset[,'explrtr'])
  else NA
  
  info[[ex_name[i]]][['ids']]['month'] =  if (length(unique(dataset[,'month']))==1) unique(dataset[,'month'])
  else NA
  
  info[[ex_name[i]]][['ids']]['mosaic'] = if (length(unique(dataset[,'mosaic']))==1) unique(dataset[,'mosaic'])
  else NA

## View first few rows
  head(dataset)
  
####  outliers ####
## detect outlier by quantils/boxplot  (Q13+-1.5*(Q3-Q1))
result = boxplot(dataset[est_var], plot = FALSE)
info[[ex_name[i]]][['boxplot']] = result

## oveview of deleted outliers (for convenience)
outliers = list(sort(result[["out"]]))
info[[ex_name[i]]][['outliers']] = outliers

## outliers set to NAs
dataset['biomass_interpolated_noout'] = dataset[est_var]

dataset <-  dataset%>% mutate(biomass_interpolated_noout = replace(biomass_interpolated_noout, biomass_interpolated_noout < result[["stats"]][1], NA))
dataset <-  dataset%>% mutate(biomass_interpolated_noout = replace(biomass_interpolated_noout, biomass_interpolated_noout > result[["stats"]][5], NA))

## plot with ggplot2 
#########mit cowplot als alle in einem mit einheitlichen layout??
box_out = ggplot(dataset, aes(y = biomass_interpolated, x = ex_name[i])) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15)+ 
  ## layout for plot
  theme_bw() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20),
  )+
  geom_boxplot(outlier.color = "red")+
  xlab('')+   
  ylab('biomass interpolated [g/(m^2)]')+
  theme(axis.text.x = element_text(color = 'black', size = 15))


ggsave(filename= paste(ex_name[i],info_variant,est_var,"boxplot_outliers.png", sep='_'), plot = last_plot() ,height= 6, width = 8)

## rename columns 
dataset <- dataset %>% 
  rename(
    #syntax: newname = oldname
    biomass_interpolated_with = biomass_interpolated,
    biomass_interpolated = biomass_interpolated_noout
  )
## delete column that includes outliers
dataset <- subset (dataset, select = -biomass_interpolated_with)

## delete NAs from outliers
info[[ex_name[i]]][['count']]['with_outliers'] = length(dataset[[est_var]][])
dataset = dataset %>% drop_na(est_var)

info[[ex_name[i]]][['count']]['without_outliers'] = length(dataset[[est_var]][])
info[[ex_name[i]]][['count']]['amount_outliers'] = length(outliers[ex_name[i]])

'#------------------------rf --------------------------------#'
#### traning validation split ####
## set seed for reproducibility
set.seed(123)

## function returning a vector (logical) indicating if a row is included in the training set
## 1) subjects takes a random sample from the unqiue Qnum values (id for the different quadrats)
## 2) value is a logical vector indicating if row of dataset is in subjects hence in traing set 
## 3) split training and testing dataset based on value and calculate ratio to each other
## 4) if more than 30% of the data is in testing (ratio >43) => A
## 5) if less than 20% of the data is in testing (ratio <25) => B 
## A: passes one random Qnum not included in subjects to subjects till ratio is between 25 and 43
## B: deletes one random Qnum from subjects till ratio is between 25 and 43

value_split <- function(x) {
  
  set.seed(123)
  ## random sample by Qnum group (means complete groups are either training or validation)
  subjects <- sample(unique(x$Qnum),length(unique(x$Qnum))*0.7, replace = TRUE)
  
  ## Logical if row is included in training or testing
  value = x$Qnum %in% subjects
  
  ## create subsets of dataset
  training_set = x[value,]
  testing_set = x[!value,]
  
  ## ratio of training/testing rounded by 0 digits 
  # because Qnum IS NOT a "unique id" for identifying rows (more samples in one quadrat)
  # 70% of qnum =/= 70% of est_var values 
  ratio = round((as.numeric(length(testing_set[,est_var]))/as.numeric(length(training_set[,est_var])))*100,0)
  
  ## A
  if (ratio >43){
    repeat {
      ## vector with Qnum for testing data 'left out' from training 
      leftout = setdiff(unique(dataset$Qnum), subjects)
      
      ## set seed fo reproducibility
      set.seed(123)
      
      ## put one random Qnum from testing to training
      subjects = c(subjects,sample(leftout,1))
      
      ## Logical if row is included in training or testing
      value= x$Qnum %in% subjects
      
      ## create subsets of dataset
      training_set = x[value,]
      testing_set = x[!value,]
      
      ## ratio (s.a.)
      ratio = round((as.numeric(length(testing_set[,est_var]))/as.numeric(length(training_set[,est_var])))*100,0)
      
      ## 30/70 = 42.86 and 20/80 = 25 
      ## break if between 20 and 30 percent of the data is in validation/testing set 
      if (ratio < 43 & ratio >25) break 
    }
  }
  
  ## B
  if (ratio < 30){
    repeat {
      ## set seed fo reproducibility
      set.seed(123)
      
      ## put one random Qnum from testing to training
      subjects = subjects[!subjects %in% sample(subjects,1)]
      
      ## Logical if row is included in training or testing
      value= x$Qnum %in% subjects
      
      ## create subsets of dataset
      training_set = x[value,]
      testing_set = x[!value,]
      
      ## ratio (s.a.)
      ratio = round((as.numeric(length(testing_set[,est_var]))/as.numeric(length(training_set[,est_var])))*100,0)
      
      ## 30/70 = 42.86 and 20/80 = 25 
      ## break if between 20 and 30 percent of the data is in validation/testing set 
      if (ratio < 43 & ratio >25) break 
    }
  }
  return(value)
}
value = value_split(dataset)

## create subsets of dataset
training_set = dataset[value,]
testing_set = dataset[!value,]

## save ratio for later include in info (only for checking later)
ratio = round((as.numeric(length(testing_set[,est_var]))/as.numeric(length(training_set[,est_var])))*100,0)

## delete ids from dataset, training and testing set
b= 1:length(ids)
for (x in b){
  dataset = dataset[,-c(which(colnames(dataset)==ids[x]))]
  training_set = training_set[,-c(which(colnames(training_set)==ids[x]))]
  testing_set = testing_set[,-c(which(colnames(testing_set)==ids[x]))]
}

## save validation dataset 
info[[ex_name[i]]][["validation"]][["testing_set"]] = testing_set[[est_var]]
info[[ex_name[i]]][["validation"]][["ratio"]] = ratio

#### Train RF model with repeated cross validation ####
## Set seed for reproducibility
set.seed(123)

## Define repeated cross validation with 5 folds and three repeats
repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3)

#### actual model ####
## Set seed for reproducibility
set.seed(123)

## Train a random forest model
forest <- train(
  
  # Formula
  biomass_interpolated~., 
  
  # Source of data; remove the Species variable
  data=training_set[], 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  
  # Accuracy to measure the performance of the model
  # alternativ: RMSE or Rsquared
  metric='Rsquared'
)

## Print out the details about the model
forest$finalModel
info[[ex_name[i]]][['rf_model']] = forest

## hyper parameter (only mtry)
ggplot(forest)+
  ## Some layout for the plot
  theme_bw() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )
ggsave(filename= paste(ex_name[i],info_variant,est_var,"mtry.png", sep='_'), plot = last_plot(),height= 6, width = 8)


#### variable imortance ####
## Get variable importance, and turn into a data frame
var_imp <- varImp(forest, scale= TRUE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall)

## Create a plot of variable importance
var_imp %>%
  ## Sort the data by importance
  arrange(importance) %>%
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  ## Add x-axis label
  xlab('variables') +
  ## Add a title
  labs(title='Random forest variable importance') + 
  ## Some layout for the plot
  theme_bw() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )
ggsave(filename= paste(ex_name[i],info_variant,est_var,"var_importance.png", sep = '_'), plot = last_plot(),height= 6, width = 8)
info[[ex_name[i]]][['var_importance']]  = var_imp

#### Validation ####
## Generate predictions
predicted <- predict(
  ## Random forest object
  object=forest, 
  ## Data to use for predictions; remove the Species
  newdata=testing_set[,-(which(colnames(testing_set)==est_var ))])

## scatter plot for evaluation
scatter = ggplot(testing_set,aes(x=biomass_interpolated, y = predicted))+
  geom_point(col='black')+
  ## Some layout for the plot
  theme_bw() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )+
  xlab('biomass (ground truth, interpolated) [g/m^2]')+
  ylab('predicted biomass [g/m^2]')+
  scale_x_continuous(expand = c(0, 0), limits = c(0,(max(testing_set$biomass_interpolated)+100))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, (max(predicted)+100)))+
  geom_abline(intercept = 0, slope = 1, col= 'blue',linetype='dashed')+
  annotate(geom="text", x=max(testing_set$biomass_interpolated)/5, y=9*(max(predicted)/10), label=paste("n = ",length(predicted)),
           color="black")
ggsave(filename= paste(ex_name[i],info_variant,est_var,"scatter_predicted_vali.png", sep='_'), plot = last_plot(),height= 6, width = 8)


# RMSE
rmse = sqrt(mean((testing_set$biomass_interpolated - predicted)^2))

# relative (normalized) RMSE
nrmse = (rmse/mean(info[[ex_name[i]]][["validation"]][["testing_set"]]))*100

# R^2
r2 = cor(testing_set$biomass_interpolated, predicted) ^ 2

info[[ex_name[i]]][['validation']][['predicted']] = predicted
info[[ex_name[i]]][['validation']][['validation']] = testing_set$biomass

info[[ex_name[i]]][['validation']][['rmse']] = rmse
info[[ex_name[i]]][['validation']][['nrmse']] = nrmse
info[[ex_name[i]]][['validation']][['r2']] = r2

## put validation in separate list for comparison
info[["compare"]][["variant"]][i] = ex_name[i]
info[["compare"]][["r2"]][i] = info[[ex_name[i]]][['validation']][['r2']]
info[["compare"]][["rmse"]][i] = info[[ex_name[i]]][['validation']][['rmse']]
info[["compare"]][["nrmse"]][i] = info[[ex_name[i]]][['validation']][['nrmse']]

}
## add more info to compare data frame (so it is identifiable which model (grob))
info[["compare"]][['estimation variable']] = info$selection$est_var
info[["compare"]][['predictors']] = toString(info$selection$predictors)
info[["compare"]][['comment variant']] = info_variant


## convert list of compare to dataframe
info[["compare"]] = bind_rows(info[["compare"]])

## name for saving
info_name = paste('info', info_variant, est_var, sep = '_')

## save as csv (for quick overview)
## write csv 
write.csv2(info[["compare"]], file=paste (info_name,'comparison',".csv"))

## info liste speichern
saveRDS(info, file= paste(info_name, '.RData', sep = ''))
#readRDS("test.RData") 

print(info)
# ende

