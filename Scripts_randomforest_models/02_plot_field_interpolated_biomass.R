## DISCLAIMER: This is the commented version of "02_plot_field_xxxx" scripts

#### plots biomass interpolated against NIR to see distributions
## inter_bio = biomass_interpolated
## Ex = Biodiv. Exploratory

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


#### BEFORE set up ####
## "before part" includes all data
## "after" part only the cleared, preprocessed data (without inconsistent values)

## select columns
all_selected = all_reduced[c('unique_id','explrtr'
                             ,'month'
                             ,'mosaic'
                             ,'Biomass_interpolated_per_m2'
                             ,'biomass_interpolated'
                             ,'nir'
)]
rm(all_reduced)

## select column names
print(names(all_selected))
a<- "nir" 
b<- 'Biomass_interpolated_per_m2' 

## select data 
x1 <- all_selected[,a]
y1<- all_selected[,b]

## select labels
xname= 'NIR [reflectance]'
yname= 'Biomass interpolated (before) [g/(m^2)]'


#### Plots before ####
## per BE
Inter_bio_Ex_before= ggplot(data = all_selected, aes(x = x1 , y = y1, colour=explrtr))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])), sep=' '),col = "black", size = 4)

## per season
Inter_bio_month_before = ggplot(data = all_selected, aes(x = x1 , y = y1, colour=month))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)

## point density plot

## get data frame with only the 2 selected columns and delete NA
# continue only with complete cases
dat <- na.omit(data.frame(x=x1,y=y1))

## function for getting densiyt
## returing density f n by n grid
## adapted from https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

## set seed for reproducibility
set.seed(1)

## estimate amount of points in a square resulting from 100 by 100 grid
dat$density <- get_density(dat$x, dat$y, n = 100)

## create biomass density plot before
## include characteristics of data (n, mean, variance, sd, range)
Inter_bio_dens_without_before = ggplot(data = dat, aes(x = x , y = y, colour=density))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+
  annotate('text', x = 0.25, y = 1000
           , label = paste(paste('n','=',nrow(na.omit(all_selected[b])),sep=' ')
                           , paste ('y-mean', '=', round(mean(na.omit(all_selected[[b]])),2), sep= ' ')
                           ,paste ('y-variance', '=', round(var(na.omit(all_selected[[b]])),2), sep= ' ')
                           , paste ('y-RSD', '=', round((sd(na.omit(all_selected[[b]]))/mean(na.omit(all_selected[[b]])))*100,2),'%', sep= ' ')
                           , paste ('y-range = [',round(min(na.omit(all_selected[[b]])),2),';', round(max(na.omit(all_selected[[b]])),2),']', sep= '')
                           , sep="\n")
           ,col = "black", size = 4)+
  scale_color_viridis()
 
## add densigram in margin 
Inter_bio_dens_before = ggExtra::ggMarginal(Inter_bio_dens_without_before, type = 'densigram',margins = 'y')


#### facit grid before #####
## instead of fill by explrtr etc. with facet grid 
## per BE
Inter_bio_Ex_facit_before= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~explrtr)

## per season
Inter_bio_month_facit_before= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~month)

## per mosaic
Inter_bio_mosaic_facit_before= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~mosaic)


#### save plots before ####
ggsave(filename="02_plot_field_Inter_bio_Ex_before.png", plot = Inter_bio_Ex_before, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_month_before.png", plot = Inter_bio_month_before, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_dens_before.png", plot = Inter_bio_dens_before, height= 4, width = 8)

ggsave(filename="02_plot_field_Inter_bio_Ex_facit_before.png", plot = Inter_bio_Ex_facit_before, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_month_facit_before.png", plot = Inter_bio_month_facit_before, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_mosaic_facit_before.png", plot = Inter_bio_mosaic_facit_before, height= 6, width = 24)



####  ####
##select columnnames of all_selected
a<- "nir" 
b<- 'biomass_interpolated' 

##Data und Beschriftung Selection
x2 <- all_selected[,a]
y2<- all_selected[,b]

xname= 'NIR [reflectance]'
yname= 'Biomass interpolated (after) [g/(m^2)]'


#### Plots AFTER ####
## per BE
Inter_bio_Ex_after= ggplot(data = all_selected, aes(x = x2 , y = y2, colour=explrtr))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])), sep=' '),col = "black", size = 4)

## per season
Inter_bio_month_after = ggplot(data = all_selected, aes(x = x2 , y = y2, colour=month))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)

## point density plots after

## get data frame with only the 2 selected columns and delete NA
# continue only with complete cases
dat <- na.omit(data.frame(x=x2,y=y2))

## create function for point density
## adapted from https://slowkow.com/notes/ggplot2-color-by-density/ 
## returing density f n by n grid
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

## set seed for reproducibility
set.seed(1)

## estimate amount of points in a square resulting from 100 by 100 grid
dat$density <- get_density(dat$x, dat$y, n = 100)

## create biomass density plot after
## include characteristics of data (n, mean, variance, sd, range)
Inter_bio_dens_without_after = ggplot(data = dat, aes(x = x , y = y, colour=density))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+
  annotate('text', x = 0.25, y = 1000
           , label = paste(paste('n','=',nrow(na.omit(all_selected[b])),sep=' ')
                           , paste ('y-mean', '=', round(mean(na.omit(all_selected[[b]])),2), sep= ' ')
                           ,paste ('y-variance', '=', round(var(na.omit(all_selected[[b]])),2), sep= ' ')
                           , paste ('y-RSD', '=', round((sd(na.omit(all_selected[[b]]))/mean(na.omit(all_selected[[b]])))*100,2),'%', sep= ' ')
                           , paste ('y-range = [',round(min(na.omit(all_selected[[b]])),2),';', round(max(na.omit(all_selected[[b]])),2),']', sep= '')
                           , sep="\n")
           ,col = "black", size = 4)+
  scale_color_viridis()

## add density in margin 
Inter_bio_dens_after = ggExtra::ggMarginal(Inter_bio_dens_without_after, type = 'densigram',margins = 'y')

#### facit grid after ####
## instead of fill by explrtr etc. with facet grid 
## per BE
Inter_bio_Ex_facit_after= ggplot(data = all_selected, aes(x = x2 , y = y2))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[,b])),sep=' '),col = "black", size = 4)+
  facet_grid(.~explrtr)

## per season
Inter_bio_month_facit_after= ggplot(data = all_selected, aes(x = x2 , y = y2))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)+
  facet_grid(.~month)

## per mosaic
Inter_bio_mosaic_facit_after= ggplot(data = all_selected, aes(x = x2 , y = y2))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)+
  facet_grid(.~mosaic)


#### save plots after ####
ggsave(filename="02_plot_field_Inter_bio_Ex_after.png", plot = Inter_bio_Ex_after, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_month_after.png", plot = Inter_bio_month_after, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_dens_after.png", plot = Inter_bio_dens_after, height= 4, width = 8)

ggsave(filename="02_plot_field_Inter_bio_Ex_facit_after.png", plot = Inter_bio_Ex_facit_after, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_month_facit_after.png", plot = Inter_bio_month_facit_after, height= 4, width = 8)
ggsave(filename="02_plot_field_Inter_bio_mosaic_facit_after.png", plot = Inter_bio_mosaic_facit_after, height= 6, width = 24)

#### compare before after (only dens)#### 
##### join both plots #####
joint= plot_grid(Inter_bio_dens_before, Inter_bio_dens_after, labels = c('Before','After'),
                 label_size = 10, label_x = 0.2, vjust=2.5,hjust = 2, ncol= 1, nrow= 2
)
joint
ggsave(filename="02_plot_field_Inter_bio_beforeafter.png", plot = joint, height= 6, width = 8)


