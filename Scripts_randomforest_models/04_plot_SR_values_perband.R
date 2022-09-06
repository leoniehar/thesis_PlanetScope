#### different violin plots/distributions of satellite data/pixel values 

##clear workspace 
rm(list = ls())

## set working directory, file name of field and sat data 
## copy path to clipboard, change windows path to linux
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

####  load packages ####
## define needed packages
packages <- c('ggplot2', 'MASS','viridis', 'cowplot','gridExtra','grid','ggcorrplot')

## Install if not
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## load packages in R 
invisible(lapply(packages, library, character.only = TRUE))

## import csv data "all_reduced"
all_reduced= read.csv2('all_reduced.csv')

## select bands and indices
vars = c(names(all_reduced))
print(vars)
a = 34:45
vars = vars[a]
vars_df = data.frame(band = vars, row.names = vars)
print(vars)

#### Violin plots ####
## reduce dataset
all_selected = all_reduced[,vars]

## separte dataset for plots (or just use all_selected)
data= all_selected
data$explrtr = all_reduced[,"explrtr"]
data$month = all_reduced[,"month"]
data$mosaic = all_reduced[,"mosaic"]

#### violin plots per predictor ####
## all together ("perBand") 
dens_list = list()
## loop over different band/indices columns 
## get violin plot for each column
for (i in vars){
  data$values = all_selected[,i] 
  dens_list[[i]] = ggplot(data, aes(x='',y=values))+
    geom_violin(draw_quantiles = c(0,0.5,1)
                , fill = 'grey'
                , color = 'black'
                , scale = 'width'
                
    )+
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10), #change from standard 15 to 10 so same like y axis
          axis.text.y = element_text(color = 'black'),
          plot.title = element_text(size = 20)
    )+
    xlab(vars_df[i,])+
    ylab('')
  
}

## print one plot to check results
dens_list[1]

## join all plots to one image
joint= plot_grid(plotlist = dens_list)

## print to check
joint

## save
ggsave("04_plot_SR_values_perband.png",plot=last_plot(),height= 10, width = 10)


####  violin plots per predictor per explrtr ####
## violin plots for all the predictors separated by Explrtr (x axis)

## create list for storing plots
dens_list= list()

## loop over different band/indices columns 
## get violin plot for each column
for (i in vars){
  data$values = all_selected[,i] 
  dens_list[[i]] = ggplot(data, aes(x=explrtr,y=values, fill = explrtr))+
    geom_violin(draw_quantiles = c(0,0.5,1)
               , color = 'black'
               , scale = 'width'
                
    )+
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10), #change from standard 15 to 10 so same like y axis
          axis.text.y = element_text(color = 'black'),
          plot.title = element_text(size = 20)
          ,legend.position = "none"
    )+
    xlab(vars_df[i,])+
    ylab('')
  
}

## print one to control
dens_list[1]

## join all the created plots 
joint= plot_grid(plotlist = dens_list)

## check
joint

## save
ggsave("04_plot_SR_values_perband_perexplrtr.png",plot=last_plot(),height= 10, width = 10)


#### violin plots per predictor per month ####
## violin plots for all the predictors separated by month (x axis)

## loop over different band/indices columns 
## get violin plot for each column
## with per month (=x), fill by month
for (i in vars){
  data$values = all_selected[,i] 
  dens_list[[i]] = ggplot(data, aes(x=month,y=values, fill = month))+
    geom_violin(draw_quantiles = c(0,0.5,1)
                , color = 'black'
                , scale = 'width'
                
    )+
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10), #change from standard 15 to 10 so same like y axis
          axis.text.y = element_text(color = 'black'),
          plot.title = element_text(size = 20)
          ,legend.position = "none"
    )+
    xlab(vars_df[i,])+
    ylab('')
  
}

## print one to check results
dens_list[1]

## joint all plots to one image
joint= plot_grid(plotlist = dens_list)

## print to check
joint

## save
ggsave("04_plot_SR_values_perband_per_month.png",plot=last_plot(),height= 10, width = 10)

#### violin plots per predictor per mosaic  #####

## new list to collect plots 
dens_list_2= list()

## loop over different band/indices columns 
## get violin plot for each column
## (x = mosaic), filled by mosaic
for (i in vars){
  data$values = all_selected[,i] 
  dens_list_2[[i]] = ggplot(data, aes(x=mosaic,y=values, fill = mosaic))+
    geom_violin(draw_quantiles = c(0,0.5,1)
                , scale = 'width'
                
    )+
    theme_bw() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10), #change from standard 15 to 10 so same like y axis
          axis.text.y = element_text(color = 'black'),
          plot.title = element_text(size = 20),
          legend.position = "none",
          axis.text.x= element_blank())+
   xlab(vars_df[i,])+
    ylab('')
  
}

## print one to check results
dens_list_2[1]

## joint all plots in one image
joint= plot_grid(plotlist = dens_list_2)

## print
joint

## save
ggsave("04_plot_SR_values_perband_permosaic.png",plot=last_plot(),height= 10, width = 10)

## Plot again for legend
## legend was hidden in plot because names to long for space at the ex axis
legend_plot = ggplot(data, aes(x=mosaic,y=values, fill = mosaic))+
  geom_violin()

## save legend
legend <- get_legend(legend_plot) 

## plot legend 
grid.newpage()
grid.draw(legend)

## save legend
ggsave("04_plot_SR_values_perband_permosaic_Legend.png",plot=legend,height= 10, width = 10)


#### correlation plots ####
## https://www.geeksforgeeks.org/visualization-of-a-correlation-matrix-using-ggplot2-in-r/
## c reating dataset
data_corr = all_selected
data_corr$biomass = all_reduced[,'biomass']
data_corr$biomass_interpolated = all_reduced[,'biomass_interpolated']
data_corr$LAI = all_reduced[,'LAI']
data_corr$RPM = all_reduced[,'RPM']

## Computing correlation matrix
correlation_matrix <- round(cor(x= data_corr, use = "pairwise.complete.obs"),1)

## check
head(correlation_matrix[, 1:4])

## Computing correlation matrix with p-values
corrp.mat <- cor_pmat(data_corr)

## check 
head(corrp.mat[, 1:4])  

## Visualizing the correlation matrix using square method
ggcorrplot(correlation_matrix, method ="square")+
  theme_bw()

## save
ggsave("04_plot_SR_values_correlation_matrix.png",plot=last_plot(),height= 10, width = 10)

