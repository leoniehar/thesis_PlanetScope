## DISCLAIMER: for a nicely commented version see "02_plot_field_interpolated_biomass"
## that script does the same for biomass interpolated instead of RPM

#### plots RPM against NIR to see distributions

##clear workspace 
rm(list = ls())

## set working directory, file name of field and sat data 
## copy path to clipboard, change windows path to linux
wd = gsub("\\\\", "/", readClipboard())
setwd(wd)
getwd()

####  load packages  and data import ####
## define needed packages
packages <- c('ggplot2', 'MASS','viridis', 'cowplot')

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
## select columns
all_selected = all_reduced[c('unique_id','explrtr'
                             ,'month'
                             ,'mosaic'
                             ,'RPMcalc_cm'
                             ,'RPM'
                             ,'nir'
)]
rm(all_reduced)

##select columnnames of all_selected
a<- "nir" 
b<- 'RPMcalc_cm' 

##Data und Beschriftung Selection
x1 <- all_selected[,a]
y1<- all_selected[,b]

xname= 'NIR [reflectance]'
yname= 'RPM (before) [cm]'


#### Plots before####
## per BE
RPM_Ex_before= ggplot(data = all_selected, aes(x = x1 , y = y1, colour=explrtr))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 50, label = paste('n','=',nrow(na.omit(all_selected[b])), sep=' '),col = "black", size = 4)

## per season
RPM_month_before = ggplot(data = all_selected, aes(x = x1 , y = y1, colour=month))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 50, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)

## density
## adapted from https://slowkow.com/notes/ggplot2-color-by-density/
dat <- na.omit(data.frame(x=x1,y=y1))
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

set.seed(1)
dat$density <- get_density(dat$x, dat$y, n = 100)

RPM_dens_without_before = ggplot(data = dat, aes(x = x , y = y, colour=density))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+
  annotate('text', x = 0.25, y = 50
           , label = paste(paste('n','=',nrow(na.omit(all_selected[b])),sep=' ')
                           , paste ('y-mean', '=', round(mean(na.omit(all_selected[[b]])),2), sep= ' ')
                           ,paste ('y-variance', '=', round(var(na.omit(all_selected[[b]])),2), sep= ' ')
                           , paste ('y-RSD', '=', round((sd(na.omit(all_selected[[b]]))/mean(na.omit(all_selected[[b]])))*100,2),'%', sep= ' ')
                           , paste ('y-range = [',round(min(na.omit(all_selected[[b]])),2),';', round(max(na.omit(all_selected[[b]])),2),']', sep= '')
                           , sep="\n")
           ,col = "black", size = 4)+
  scale_color_viridis()

RPM_dens_before = ggExtra::ggMarginal(RPM_dens_without_before, type = 'densigram',margins = 'y')


#### facit grid before ####
## per BE
RPM_Ex_facit_before= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~explrtr)

## per season
RPM_month_facit_before= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~month)

## per mosaic
RPM_mosaic_facit_before= ggplot(data = all_selected, aes(x = x1 , y = y1))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  facet_grid(.~mosaic)


#### save plots before ####
ggsave(filename="02_plot_field_RPM_Ex_before.png", plot = RPM_Ex_before, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_month_before.png", plot = RPM_month_before, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_dens_before.png", plot = RPM_dens_before, height= 4, width = 8)

ggsave(filename="02_plot_field_RPM_Ex_facit_before.png", plot = RPM_Ex_facit_before, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_month_facit_before.png", plot = RPM_month_facit_before, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_mosaic_facit_before.png", plot = RPM_mosaic_facit_before, height= 6, width = 24)


#### 
#### AFTER (corr) set up ####
##select columnnames of all_selected
a<- "nir" 
b<- 'RPM' 

##Data und Beschriftung Selection
x2 <- all_selected[,a]
y2<- all_selected[,b]

xname= 'NIR [reflectance]'
yname= 'RPM (after) [cm]'


#### Plots after####
## per BE
RPM_Ex_after= ggplot(data = all_selected, aes(x = x2 , y = y2, colour=explrtr))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 50, label = paste('n','=',nrow(na.omit(all_selected[b])), sep=' '),col = "black", size = 4)

## per season
RPM_month_after = ggplot(data = all_selected, aes(x = x2 , y = y2, colour=month))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 50, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)

## density
## adapted from https://slowkow.com/notes/ggplot2-color-by-density/
dat <- na.omit(data.frame(x=x2,y=y2))
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

set.seed(1)
dat$density <- get_density(dat$x, dat$y, n = 100)

RPM_dens_without_after = ggplot(data = dat, aes(x = x , y = y, colour=density))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+
  #annotate('text', x = 0.25, y = 10, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)+
  annotate('text', x = 0.25, y = 50
           , label = paste(paste('n','=',nrow(na.omit(all_selected[b])),sep=' ')
                           , paste ('y-mean', '=', round(mean(na.omit(all_selected[[b]])),2), sep= ' ')
                           ,paste ('y-variance', '=', round(var(na.omit(all_selected[[b]])),2), sep= ' ')
                           , paste ('y-RSD', '=', round((sd(na.omit(all_selected[[b]]))/mean(na.omit(all_selected[[b]])))*100,2),'%', sep= ' ')
                           , paste ('y-range = [',round(min(na.omit(all_selected[[b]])),2),';', round(max(na.omit(all_selected[[b]])),2),']', sep= '')
                           , sep="\n")
           ,col = "black", size = 4)+
  scale_color_viridis()

RPM_dens_after = ggExtra::ggMarginal(RPM_dens_without_after, type = 'densigram',margins = 'y')



#### facit grid after ####
## per BE
RPM_Ex_facit_after= ggplot(data = all_selected, aes(x = x2 , y = y2))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 50, label = paste('n','=',nrow(na.omit(all_selected[,b])),sep=' '),col = "black", size = 4)+
  facet_grid(.~explrtr)

## per season
RPM_month_facit_after= ggplot(data = all_selected, aes(x = x2 , y = y2))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 50, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)+
  facet_grid(.~month)

## per mosaic
RPM_mosaic_facit_after= ggplot(data = all_selected, aes(x = x2 , y = y2))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank())+
  annotate('text',  x = 0.25, y = 50, label = paste('n','=',nrow(na.omit(all_selected[b])),sep=' '),col = "black", size = 4)+
  facet_grid(.~mosaic)


#### save plots after ####
ggsave(filename="02_plot_field_RPM_Ex_after.png", plot = RPM_Ex_after, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_month_after.png", plot = RPM_month_after, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_dens_after.png", plot = RPM_dens_after, height= 4, width = 8)

ggsave(filename="02_plot_field_RPM_Ex_facit_after.png", plot = RPM_Ex_facit_after, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_month_facit_after.png", plot = RPM_month_facit_after, height= 4, width = 8)
ggsave(filename="02_plot_field_RPM_mosaic_facit_after.png", plot = RPM_mosaic_facit_after, height= 6, width = 24)

#### compare before after (only dens)#### 
##### join both plots #####
joint= plot_grid(RPM_dens_before, RPM_dens_after, labels = c('Before','After'),
                 label_size = 10, label_x = 0.2, vjust=2.5,hjust = 2, ncol= 1, nrow= 2
)
joint
ggsave(filename="02_plot_field_RPM_beforeafter.png", plot = joint, height= 6, width = 8)

