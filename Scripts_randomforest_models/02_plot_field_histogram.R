#### Histograms for LAI and biomass (not included in rest of plot up to here)

##clear workspace 
rm(list = ls())

## set working directory, file name of field and sat data 
## copy path to clipboard, change windows path to linux
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

####  load packages  and data import ####
## define needed packages
## MASS and viridis used for density plots
packages <- c('ggplot2', 'MASS','viridis', 'cowplot', 'ggExtra')

## Install if not
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## load packages in R 
invisible(lapply(packages, library, character.only = TRUE))

## import csv data "all_reduced"
all_reduced= read.csv2('all_reduced.csv')

#### prepare data / reduce dataset and set values ####
## select columns
all_selected = all_reduced[c('biomass'
                             ,'LAI'
)]
rm(all_reduced)

## select column names
print(names(all_selected))
a<- "biomass" 
b<- 'LAI' 

## select data 
x1 <- all_selected[,a]
x2<- all_selected[,b]

## select labels
xname= 'Biomass [g/(m^2)]'
xname2= 'LAI []'

## create dataframe for plots
dat = data.frame(x1,x2)

#### biomass histogram ####
# with annotated statistics
bio_hist = ggplot(data = dat, aes( x = x1))+
  geom_histogram()+
  theme_bw()+
  xlab(xname)+ylab('count')+
  annotate('text', x = 1000, y = 40
           , label = paste(paste('n','=',nrow(na.omit(all_selected[a])),sep=' ')
                           , paste ('mean', '=', round(mean(na.omit(all_selected[[a]])),2), sep= ' ')
                           , paste ('RSD', '=', round((sd(na.omit(all_selected[[a]]))/mean(na.omit(all_selected[[a]])))*100,2),'%', sep= ' ')
                           , paste ('range = [',round(min(na.omit(all_selected[[a]])),0),';', round(max(na.omit(all_selected[[a]])),0),']', sep= '')
                           , sep="\n")
           ,col = "black", size = 4)
## print
bio_hist

## save
ggsave(filename="02_plot_field_histograms_biomass.png", plot = bio_hist, height= 4, width = 8)


#### LAI histogram ####
# with annotated statistics
LAI_hist = ggplot(data = dat, aes( x = x2))+
  geom_histogram()+
  theme_bw()+
  xlab(xname2)+ylab('count')+
  annotate('text', x = 10, y = 65
           , label = paste(paste('n','=',nrow(na.omit(all_selected[b])),sep=' ')
                           , paste ('mean', '=', round(mean(na.omit(all_selected[[b]])),2), sep= ' ')
                           , paste ('RSD', '=', round((sd(na.omit(all_selected[[b]]))/mean(na.omit(all_selected[[b]])))*100,2),'%', sep= ' ')
                           , paste ('range = [',round(min(na.omit(all_selected[[b]])),2),';', round(max(na.omit(all_selected[[b]])),2),']', sep= '')
                           , sep="\n")
           ,col = "black", size = 4)
## print
LAI_hist

## save
ggsave(filename="02_plot_field_histograms_LAI.png", plot = LAI_hist, height= 4, width = 8)


## join both 
joint = plot_grid(bio_hist, LAI_hist, ncol= 1,  labels = c('a)','b)'),
                  label_size = 10, label_x = 0.2
                 , vjust=2
                 ,hjust = 4.2
                  )
joint

## save
ggsave(filename="02_plot_field_histograms_joint.png", plot = joint, height= 6, width = 6)
