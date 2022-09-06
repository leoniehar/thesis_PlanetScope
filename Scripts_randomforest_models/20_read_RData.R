## clear workspace 
rm(list = ls())

## set working directory, file name of field and sat data 
##\ müsste doppelt oder / (linux), wird hiermit ersetzt 
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

## set color palettes
#response_color = "Set1"
split_per_color = "Set3"
response_color = list('bio' = '#7fc97f'
                      ,'int' = '#fdc086'
                      ,'LAI' = '#ffff99'
                      ,'RPM' = '#beaed4')

#### load packages (from https://rpubs.com/jvaldeleon/forest_repeat_cv) ####
## (1) Define the packages that will be needed
packages <- c('dplyr', 'ggplot2', 'caret','tidyr', 'skimr', 'caretEnsemble'
              , 'mlbench','MASS','purrr','cowplot')
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
         ,list.files(path=".", pattern="biomass.RData", all.files=TRUE,
                     full.names=TRUE, recursive = TRUE)
         ,list.files(path=".", pattern="biomass_interpolated.RData", all.files=TRUE,
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
setwd("./compare")
getwd()

## creat short, pretty names for imported models (instead of paths)
print(list)
name = c("LAI_all","LAI_perExplrtr","LAI_perMonth","LAI_perMosaic"
         ,"bio_all","bio_perExplrtr","bio_perMonth","bio_perMosaic"
         ,"bio_inter_all","bio_inter_perExplrtr","bio_inter_perMonth","bio_inter_Mosaic"
         ,"RPM_all","RPM_perExplrtr","RPM_perMonth","RPM_perMosaic")

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
models = flat_var[grep(".rf_model",names(flat_var))]

## extract validations (in-situ and predicted datasets)
validations = flat_var[grep(".validation",names(flat_var))]

## extract var importance
eval = flat_var[grep(".var_importance",names(flat_var))]

## remove
rm(flat_var)

#### variable importance (avergage) ####
## extract values
var_imps_flat = unlist(eval, recursive = FALSE)
var_imps = rbind.data.frame(var_imps_flat[grep("importance.importance"
                                           ,names(var_imps_flat))])

## change rownames with bands/inidces name 
# Lai_all.all only exampleric, could be any of the model lists
# column names are named after the variant/ list 
row.names(var_imps) <- variants[["LAI_all.all"]][["var_importance"]][["variables"]]

## remove 
rm(var_imps_flat)

## change col to rows
var_imps= as.data.frame(t(var_imps))

## add column indicating response variable  based on row names
## first substring of first 5 digits
var_imps$response = substr(rownames(var_imps),1,5)

## change to pretty names of length 3 
var_imps$response[substr(var_imps$response,1,3) == "LAI"] <- "LAI"
var_imps$response[substr(var_imps$response,1,3) == "RPM"] <- "RPM"
var_imps$response[substr(var_imps$response,1,5) == "bio_i"] <- "int"
var_imps$response[substr(var_imps$response,1,3) == "bio"] <- "bio"

## change data frame to list with separate sublist for each response variable
var_imps = list(LAI = var_imps[var_imps$response== "LAI", ]
                ,RPM = var_imps[var_imps$response== "RPM", ]
                ,int = var_imps[var_imps$response== "int", ]
                ,bio= var_imps[var_imps$response== "bio", ]
            )

## vector of variants/response variables
names= c("LAI","RPM","int","bio")
a = 1:length(var_imps)

## loop over the different response variables to average var imp
for (i in names ){
  ## delete response colnum 
  var_imps[[i]] = var_imps[[i]][-grep("response",colnames(var_imps[[i]]))]
  ## change col to rows
  var_imps[[i]]= as.data.frame(t(var_imps[[i]]))
  ## add averaged column
  var_imps[[i]][["average"]] = rowMeans(var_imps[[i]])
  ##add sd column
  var_imps[[i]][["sd"]]<-apply(var_imps[[i]], 1, sd)  
  
  ## Create a plot of variable importance
  var_imps[[i]] %>%
    ## Sort the data by importance
    arrange(average) %>%
    ## Create a ggplot object for aesthetic
    ggplot(aes(x=reorder(rownames(var_imps[[i]]), average), y=average, fill=response_color[[i]])) + 
    ## Plot the bar graph
    geom_bar(stat='identity') + 
    ## Flip the graph to make a horizontal bar plot
    coord_flip() + 
    ## Add x-axis label
    xlab('Variables') +
    ylab(paste("average",i))+
    ## Add a title
    labs(title='Random forest variable importance') + 
    ## Some layout for the plot
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 15), 
          plot.title = element_text(size = 20), 
    )+
    geom_errorbar(aes(ymin = average- sd, ymax = average + sd), width = 0.2)
  
  ggsave(filename= paste(var,i,"averaged_var_importance.png", sep = '_'), plot = last_plot(),height= 6, width = 8)
  
}

#### opimal mtry (average) ####
## hyper parameter (only mtry)

## get mtry from "bestTune" info from models and prepare structure for plot 
mtrys = flatten(flatten(models)[grep("bestTune",names(flatten(models)))])
mtrys = data.frame(mtry = unlist(mtrys))

## plot mty of best tune for all the models 
mtry_plot =ggplot(data = mtrys, aes(x= mtry))+
  geom_histogram()+
  scale_x_continuous(breaks = sort(unique(mtrys$mtry)),
                     labels = sort(unique(mtrys$mtry)))+
  stat_bin(binwidth = 1)+
  theme_bw() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )

## print 
mtry_plot

## save
ggsave(filename= paste(var,"averaged_mtry.png", sep='_'), plot = last_plot(),height= 6, width = 8)


#### evaluate performance based on metrics (rsquared nrmse) ####
## first: structure data ####
## flatten validation list
flat = unlist(validations, recursive = FALSE)

## put evaluation para in data frame
eval = cbind(
  data.frame(unlist(flat[grep("validation.rmse",names(flat))]))
  ,data.frame(unlist(flat[grep("validation.nrmse",names(flat))]))
  ,data.frame(unlist(flat[grep("validation.r2",names(flat))]))

)

## give colnames
colnames(eval)<- c("rmse","nrmse","rsquared")

## shorten row names
rownames(eval) <- substr(rownames(eval), 1 , nchar(rownames(eval))-16)

## add column indicating response variable 
eval$response = substr(rownames(eval),1,5)

eval$response[substr(eval$response,1,3) == "LAI"] <- "LAI"
eval$response[substr(eval$response,1,3) == "RPM"] <- "RPM"
eval$response[substr(eval$response,1,5) == "bio_i"] <- "inter"
eval$response[substr(eval$response,1,3) == "bio"] <- "biomass"

## Add  variant (per explrtr) and model (J)
eval$split_per = rep(NA,nrow(eval))
eval[grep("Month",rownames(eval)),'split_per']= 'month'
eval[grep("Mosaic",rownames(eval)),'split_per']= 'mosaic'
eval[grep("Explrtr",rownames(eval)),'split_per']= 'explrtr'
eval[grep("all",rownames(eval)),'split_per']= 'all'


#### plots  ####
names = c('LAI','RPM','inter','biomass')
for (i in names){
  ## create dataset
  data = eval[eval$response== i, ]
  
  ## rsquared
  rsquared = ggplot(data, aes(y = rsquared, x= ''))+
    geom_boxplot()+
    ylab("R^2")+
    xlab(i)+
  annotate('text', x = 0.8, y = 0.85
           , label = paste(paste('n','=',length(data$rsquared),sep=' ')
                           , paste ('mean', '=', round(mean(data$rsquared),4), sep= ' ')
                           ,paste ('variance', '=', round(var(data$rsquared),4), sep= ' ')
                           , sep="\n")
           ,col = "black", size = 4)
  
  ##print
  rsquared
  
  ## nrmse
  nrmse = ggplot(data, aes(y = nrmse, x= ''))+
    geom_boxplot()+
    ylab("nRMSE")+
    xlab(i)+
    annotate('text', x = 0.8, y = 54
             , label = paste(paste('n','=',length(data$rmse),sep=' ')
                             , paste ('mean nrmse', '=', round(mean(data$nrmse),2), sep= ' ')
                             ,paste ('variance nrmse', '=', round(var(data$nrmse),2), sep= ' ')
                             , paste ('mean rmse', '=', round(mean(data$rmse),2), sep= ' ')
                             ,paste ('variance rmse', '=', round(var(data$rmse),2), sep= ' ')
                             , sep="\n")
             ,col = "black", size = 4)
  nrmse
  
  ## join rsquared and rmse
  together = plot_grid(rsquared,nrmse,ncol=2, labels = c('a)','b)'))
  together
  
  ## save
  ggsave(filename= paste(var,i,"validation_boxplots.png", sep='_'), plot = last_plot(),height= 6, width = 8)
  
  
  #### plots with per split fill ####
  
  ## rsquared
  rsquared = ggplot(data, aes(y = rsquared, x= split_per, fill =split_per))+
    geom_boxplot()+
    ylab("R^2")+
    scale_fill_brewer(palette=split_per_color)+
    xlab(i)
 # +theme(legend.position='none')
 
  rsquared
  
  ## nrmse
  nrmse = ggplot(data, aes(y = nrmse, x= split_per, fill=split_per))+
    geom_boxplot()+
    ylab("nRMSE")+
    scale_fill_brewer(palette=split_per_color)+
    xlab(i)
  nrmse
  
  ## join rsquared and rmse
  together = plot_grid(rsquared,nrmse,ncol=1,labels = c('a)','b)'))
  together
  
  ## save
  ggsave(filename= paste(var,i,"validation_boxplots_fill_per_split.png", sep='_'), plot = last_plot(),height= 6, width = 8)
  
  #### plots with facet per split per ####
  ## nrmse
  nrmse = ggplot(data, aes(y = nrmse, x= ''))+
    geom_boxplot()+
    ylab("nRMSE")+
    facet_grid(~split_per)+
    xlab(i)
  
  nrmse
  
  ## rsquared
  rsquared = ggplot(data, aes(y = rsquared, x= ''))+
    geom_boxplot()+
    ylab("R^2")+
    facet_grid(~split_per)+
    xlab(i)
  
  rsquared
  
  ## join rsquared and rmse
  together = plot_grid(rsquared,nrmse,ncol=1,labels = c('a)','b)'))
  together
  
  ## save
  ggsave(filename= paste(var,i,"validation_boxplots_per_split_tacit.png", sep='_'), plot = last_plot(),height= 6, width = 8)

  
  #### rank models by best performance (all) nRMSE ####
  ## Create a ggplot object for aesthetic
  nrmse_try1 =  ggplot(data= data
                       ,aes(x=reorder(rownames(data), nrmse, decreasing = TRUE)
                            ,y=nrmse
                            ,fill = split_per
                       )) + 
    ## Plot the bar graph
    geom_bar(stat='identity') + 
    ## Flip the graph to make a horizontal bar plot
    coord_flip() + 
    ## Add x-axis label
    xlab(i) +
    ylab(paste('nRMSE'))+
    ## Add a title
    labs(title='nRMSE of Random Forest Model') + 
    ## Some layout for the plot
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 15), 
          plot.title = element_text(size = 20)
    )+
    scale_fill_brewer(palette=split_per_color)
  nrmse_try1
  
  ## save
  ggsave(filename= paste(var,i,"validation_nrmse_try1_barplot.png", sep='_'), plot = last_plot(),height= 6, width = 8)
  
  ## Create a ggplot object for aesthetic
  nrmse_try2 =  ggplot(data= data
                       ,aes(x=reorder(rownames(data), nrmse, decreasing = TRUE)
                            ,y=nrmse
                       )) + 
    ## Plot the bar graph
    geom_bar(stat='identity') + 
    ## Flip the graph to make a horizontal bar plot
    coord_flip() + 
    ## Add x-axis label
    xlab(i) +
    ylab(paste('nRMSE'))+
    ## Add a title
    labs(title='nRMSE of Random Forest Model') + 
    ## Some layout for the plot
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 20), 
          plot.title = element_text(size = 20), 
          axis.text.y = element_text(size = 10),
          legend.text = element_text (size = 20),
          legend.title = element_text(size = 20, face = 'bold')
    )
  
  nrmse_try2
  ggsave(filename= paste(var,i,"validation_nrmse_try2_barplot.png", sep='_'), plot = last_plot(),height= 12, width = 16)
  
  #### rank models by best performance (all) RSQUARED ####
  ## Create a ggplot object for aesthetic
  rsquared_try1 =  ggplot(data= data
                          ,aes(x=reorder(rownames(data), rsquared, decreasing = FALSE)
                               ,y=rsquared
                               ,fill = split_per
                          )) + 
    ## Plot the bar graph
    geom_bar(stat='identity') + 
    ## Flip the graph to make a horizontal bar plot
    coord_flip() + 
    ## Add x-axis label
    xlab(i) +
    ylab(paste('R^2'))+
    ## Add a title
    labs(title='rsquared of Random Forest Model') + 
    ## Some layout for the plot
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 15), 
          plot.title = element_text(size = 20), 
    )+
    scale_fill_brewer(palette=split_per_color)
  
  rsquared_try1
  ggsave(filename= paste(var,i,"validation_rsquared_try1_barplot.png", sep='_'), plot = last_plot(),height= 6, width = 8)
  
  ## Create a ggplot object for aesthetic
  rsquared_try2 =  ggplot(data= data
                          ,aes(x=reorder(rownames(data), rsquared, decreasing = FALSE)
                               ,y=rsquared
                          )) + 
    ## Plot the bar graph
    geom_bar(stat='identity') + 
    ## Flip the graph to make a horizontal bar plot
    coord_flip() + 
    ## Add x-axis label
    xlab(i) +
    ylab(paste('R^2'))+
    ## Add a title
    labs(title='rsquared of Random Forest Model') + 
    ## Some layout for the plot
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 15), 
          plot.title = element_text(size = 20), 
    )
  
  rsquared_try2
  ggsave(filename= paste(var,i,"validation_rsquared_try2_barplot.png", sep='_'), plot = last_plot(),height= 6, width = 8)
  
  together = plot_grid(nrmse_try2,rsquared_try2, ncol=2,labels = c('a)','b)'))
  together
  
  ggsave(filename= paste(var,i,"validation_try2_compare.png", sep='_'), plot = last_plot(),height= 6, width = 14)
  
  together = plot_grid(nrmse_try1,rsquared_try1,labels = c('a)','b)'))
  together
  ggsave(filename= paste(var,i,"validation_try1_compare.png", sep='_'), plot = last_plot(),height= 6, width = 14)
  
  
  #### heat map results metrics (eval)####
  write.csv2(data, paste(i,"eval_metrics_heat.csv"))
}


#### rank split per average performance of models ####
#### scatter plots ####

## put evaluation para in data frame
scat = cbind(
  data.frame(unlist(flat[grep(".testing_set",names(flat))]))
  ,data.frame(unlist(flat[grep(".predicted",names(flat))]))
)
colnames(scat)<-c("testing","predicted")

## add column indicating response variable 
scat$response = substr(rownames(scat),1,5)

scat$response[substr(scat$response,1,3) == "LAI"] <- "LAI"
scat$response[substr(scat$response,1,3) == "RPM"] <- "RPM"
scat$response[substr(scat$response,1,5) == "bio_i"] <- "inter"
scat$response[substr(scat$response,1,3) == "bio"] <- "biomass"

## Add  variant (per explrtr) and model (J)
scat$split_per = rep(NA,nrow(scat))
scat[grep("Month",rownames(scat)),'split_per']= 'month'
scat[grep("Mosaic",rownames(scat)),'split_per']= 'mosaic'
scat[grep("Explrtr",rownames(scat)),'split_per']= 'explrtr'
scat[grep("all",rownames(scat)),'split_per']= 'all'

#### all together (black)
## loop over response variables (names s.o.)
scatter= list()
a = c('biomass','inter','LAI','RPM')
for (i in a){
## subset 
data = scat[scat$response==i,]

scatter[[i]] = ggplot(data,aes(x=testing, y = predicted))+
  geom_point()+
  ## Some layout for the plot
  theme_bw() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )+
  xlab(paste('in-situ data (ground truth)',i))+
  ylab(paste('predicted values',i))+
  scale_x_continuous(expand = c(0, 0), limits = c(0,(max(data["testing"])+((max(data["testing"]))/20)))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, (max(data["predicted"])+((max(data["predicted"]))/20))))+
  geom_abline(intercept = 0, slope = 1, col= 'blue',linetype='dashed')+
  annotate(geom="text", x=max(data$testing)/5, y=9*(max(data$predicted)/10), label=paste("n = ",length(data$predicted)),
          color="black")
}
## save separate from lists
a= scatter[["biomass"]]
b= scatter[["inter"]]
c= scatter[["LAI"]]
d= scatter[["RPM"]]

## joint plot
scatter_all = plot_grid(plotlist= scatter,align="v", nrow=2,ncol=2)
scatter_all
ggsave(filename= paste(var,"compare_scatter_predicted_vali.png", sep='_'), plot = last_plot(),height= 12, width = 18)

## save separate bei response varibales
ggsave(filename= paste(var,"compare_scatter_predicted_vali_biomass.png", sep='_'), plot = a,height= 6, width = 8)
ggsave(filename= paste(var,"compare_scatter_predicted_vali_biomass_interpolated.png", sep='_'), plot = b,height= 6, width = 8)
ggsave(filename= paste(var,"compare_scatter_predicted_vali_LAI.png", sep='_'), plot = c,height= 6, width = 8)
ggsave(filename= paste(var,"compare_scatter_predicted_vali_RPM.png", sep='_'), plot = d,height= 6, width = 8)



## scatter plot with split per color separate 
## loop over response variables (names s.o.)
scatter= list()
a = c('biomass','inter','LAI','RPM')
for (i in a){
  ## subset 
  data = scat[scat$response==i,]
  
  scatter[[i]] = ggplot(data,aes(x=testing, y = predicted, shape= split_per))+
    geom_point()+
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 15), 
          plot.title = element_text(size = 20), 
    )+
    xlab(paste('in-situ data (ground truth)',i))+
    ylab(paste('predicted values',i))+
    scale_x_continuous(expand = c(0, 0), limits = c(0,(max(data["testing"])+((max(data["testing"]))/20)))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, (max(data["predicted"])+((max(data["predicted"]))/20))))+
    geom_abline(intercept = 0, slope = 1, col= 'blue',linetype='dashed')+
    annotate(geom="text", x=max(data$testing)/5, y=9*(max(data$predicted)/10), label=paste("n = ",length(data$predicted)),
             color="black")
    #scale_color_brewer(palette=split_per_color)
}
a= scatter[["biomass"]]
b= scatter[["inter"]]
c= scatter[["LAI"]]
d= scatter[["RPM"]]

## join all together
scatter_all = plot_grid(plotlist= scatter,align="v", nrow=2,ncol=2)
scatter_all

## save
ggsave(filename= paste(var,"compare_scatter_predicted_vali_with_split_per.png", sep='_'), plot = last_plot(),height= 12, width = 18)

## save separate bei response varibales
ggsave(filename= paste(var,"compare_scatter_predicted_vali_biomass_shape.png", sep='_'), plot = a,height= 6, width = 8)
ggsave(filename= paste(var,"compare_scatter_predicted_vali_biomass_interpolated_shape.png", sep='_'), plot = b,height= 6, width = 8)
ggsave(filename= paste(var,"compare_scatter_predicted_vali_LAI_shape.png", sep='_'), plot = c,height= 6, width = 8)
ggsave(filename= paste(var,"compare_scatter_predicted_vali_RPM_shape.png", sep='_'), plot = d,height= 6, width = 8)

