## DISCLAIMER: for a nicely commented version see "02_plot_field_interpolated_biomass"
## that script does the same for biomass interpolated instead of biomass
## difference : with LAI its only the "Before" part

#### plots biomass against NIR to see distributions
## Ex = Biodiv. Exploratory
## bio = biomass


##clear workspace 
rm(list = ls())

## set working directory, file name of field and sat data 
## copy path to clipboard, change windows path to linux
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

####  load packages ####
## define needed packages 
## MASS and viridis used for density plots
packages <- c('ggplot2', 'MASS','viridis')

## Install if not already
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## load packages in R 
invisible(lapply(packages, library, character.only = TRUE))

## import csv data "all_reduced" containing data
all_reduced= read.csv2('all_reduced.csv')

## select columns from imported dataset
all_selected = all_reduced[c('unique_id','explrtr'
                     ,'month'
                     ,'mosaic'
                     ,'biomass'
                     ,'nir'
                     )]
rm(all_reduced)

## select column names
print(names(all_selected))
a<- "nir" 
b<- 'biomass' 

## select data
x1 <- all_selected[,a]
y1<- all_selected[,b]

## select labels
xname= 'NIR [reflectance]'
yname= 'Biomass [g/(m^2)]'

#### Plots####
## per BE
Bio_Ex= ggplot(data = all_selected, aes(x = x1 , y = y1, colour=explrtr))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])), sep=' '),col = "black", size = 4)

## per season
Bio_month = ggplot(data = all_selected, aes(x = x1 , y = y1, colour=month))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 1000, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)

## point density plots 
## adapted from https://slowkow.com/notes/ggplot2-color-by-density/
dat <- na.omit(data.frame(x=x1,y=y1))
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

## set seed for reproducibilit
set.seed(1)

## estimate amount of points in a square resulting from 100 by 100 grid
dat$density <- get_density(dat$x, dat$y, n = 100)

## Plot biomass after
Bio_dens_without = ggplot(data = dat, aes(x = x , y = y, colour=density))+
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

## add margins
Bio_dens = ggExtra::ggMarginal(Bio_dens_without, type = 'densigram',margins = 'y')

#### facet grid ####
## per BE
Bio_Ex_facit= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~explrtr)

## per season
Bio_month_facit= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~month)

## per mosaic
Bio_mosaic_facit= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~mosaic)


#### save plots ####
ggsave(filename="02_plot_field_Bio_Ex.png", plot = Bio_Ex, height= 4, width = 8)
ggsave(filename="02_plot_field_Bio_month.png", plot = Bio_month, height= 4, width = 8)
ggsave(filename="02_plot_field_Bio_dens.png", plot = Bio_dens, height= 4, width = 8)

ggsave(filename="02_plot_field_Bio_Ex_facit.png", plot = Bio_Ex_facit, height= 4, width = 8)
ggsave(filename="02_plot_field_Bio_month_facit.png", plot = Bio_month_facit, height= 4, width = 8)
ggsave(filename="02_plot_field_Bio_mosaic_facit.png", plot = Bio_mosaic_facit, height= 6, width = 24)

