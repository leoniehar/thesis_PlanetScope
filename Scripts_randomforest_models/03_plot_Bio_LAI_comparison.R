##clear workspace 
rm(list = ls())
####  load packages  and data import ####
## define needed packages
packages <- c('ggplot2', 'cowplot')
## Install if not
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
## load packages in R 
invisible(lapply(packages, library, character.only = TRUE))

## set wd if not aleady 
#setwd("C:/Users/Leonie/Documents/BA_lokal/Ergebnisse/R/002_rf")
getwd()
## import csv data "all_reduced"
all_reduced= read.csv2('all_reduced.csv')

#### select columns from imported dataset####
all_selected = all_reduced[c('unique_id','explrtr'
                     ,'month'
                     ,'mosaic'
                     ,'biomass','biomass_interpolated'
                     ,'LAI', 'RPM'
)]
rm(all_reduced)

## select columns
print(names(all_selected))
a<- "LAI" 
b<- 'biomass_interpolated'


## select data
x1 <- all_selected[,a]
y1<- all_selected[,b]

## select lables 
xname= 'LAI []'
yname= 'Biomass interpolated [g/(cm^2)]'

#### Plots for fill by exprtr, month and mosaic #### 
## scatter plot
Bio_LAI_ex_raw= ggplot(data = all_selected, aes(x = x1 , y = y1, colour=explrtr))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank(), legend.position = 'bottom',legend.margin=margin(t = 0, unit='cm'))

## density curve in margin
Bio_LAI_ex= ggMarginal(Bio_LAI_ex_raw,
  type = "density",
  margins = "both",
  size = 2,
  xparams = list(),
  yparams = list(),
  groupColour = TRUE,
  groupFill = TRUE
)

## print 
Bio_LAI_ex

## same for month
Bio_LAI_month_raw= ggplot(data = all_selected, aes(x = x1 , y = y1, colour=month))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank(), legend.position = 'bottom',legend.margin=margin(t = 0, unit='cm'))

Bio_LAI_month= ggMarginal(Bio_LAI_month_raw,
                       type = "density",
                       margins = "both",
                       size = 2,
                       xparams = list(),
                       yparams = list(),
                       groupColour = TRUE,
                       groupFill = TRUE
)

Bio_LAI_month

## same for mosaic
Bio_LAI_mosaic_raw= ggplot(data = all_selected, aes(x = x1 , y = y1, colour=mosaic))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank(), legend.position = 'bottom',legend.margin=margin(t = 0, unit='cm'))

## same for mosaic
Bio_LAI_mosaic= ggMarginal(Bio_LAI_mosaic_raw,
                       type = "density",
                       margins = "both",
                       size = 2,
                       xparams = list(),
                       yparams = list(),
                       groupColour = TRUE,
                       groupFill = TRUE
)

Bio_LAI_mosaic


##### join plots #####
joint_3= plot_grid(Bio_LAI_ex, Bio_LAI_month,Bio_LAI_mosaic, labels = c('Exploratory', 'Month','mosaic'),
                 label_size = 2, label_x = 0.2, vjust=2.5, 
                 ncol=1
)
joint_3
ggsave(filename="03_plot_Bio_LAI_comparison.png", plot = joint_3, height= 10, width = 6)

joint_4 = plot_grid(Bio_LAI_ex, Bio_LAI_month, labels = c('Exploratory', 'Month'),
                    label_size = 2, label_x = 0.2, vjust=2.5, 
                    ncol=2
)

## only month and Season
joint_4
ggsave(filename="03_plot_Bio_LAI_comparison_only_explrtr_month.png", plot = joint_4, height= 6, width = 12)

#### Plots without any split ####
## scatter plot
Bio_LAI_raw= ggplot(data = all_selected, aes(x = x1 , y = y1, colour=explrtr, shape = explrtr))+
  geom_point()+
  theme_bw()+
  xlab(xname)+ylab(yname)+ 
  theme(legend.title=element_blank(), legend.position = 'bottom',legend.margin=margin(t = 0, unit='cm'))

## density curve in margin
Bio_LAI= ggMarginal(Bio_LAI_raw,
                       type = "densigram",
                       margins = "both",
                       size = 3,
                       xparams = list(),
                       yparams = list(),
                       groupColour = FALSE,
                       groupFill = FALSE
)

## print 
Bio_LAI
ggsave(filename="03_plot_Bio_LAI_comparison.png", plot = Bio_LAI, height= 6, width = 6)


