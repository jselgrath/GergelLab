# Jennifer Selgrath
# Data Exploration
# See Zuur et al. 2010: A protocol for data exploration...

###############################################################

# packages ###############
library(ggplot2); library(dplyr); library(plyr); library(tidyr); library(magrittr)

# load data###############################
remove(list=ls())
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/GergelLabmeeting/")

# load Jenny and Kevin's data
d.j<-read.csv(file="./data/dataset_watershed_w_samplepoints.csv",header=T) #note I renamed this with no spaces in file name
d.k<-read.csv(file="./data/testdata_ggplot_jenny.csv",header=T)

# data exploration with Jenny's data #########

# look at data
head(d.j)
names(d.j)

#1. outliers X and Y
# tiff(filename = "./doc/WatershedDataViz1_%d.tif", compression="lzw") #this saves the files below

# Outliers
g1<-ggplot(d.j,aes(x=FID,y=imperv))+geom_point()

g1+
	ylab("Range of the data")+
	xlab("Order of the data")+
	ggtitle("Percent Impervious")+
	theme(text=element_text(size=15))

ggplot(d.j,aes(x=FID,y=conif))+geom_point()+
	ylab("Range of the data")+
	xlab("Order of the data")+
	ggtitle("Percent Conif")+
	theme(text=element_text(size=15))

# dev.off() # this stops outputting graphics

# correlations ################
# look for correlations between x variables
cor(d.j$conif,d.j$imperv) # cutoff depends on test and strength of relationship. Often 0.7, but weak relationships need lower cutoff

###################################
# Normality ########################
# checking, but need to use residuals to assess normality

# histogram
ggplot(data=d.j,aes(x=do_w))+geom_histogram(bins=100)

# qq-plot 
qqnorm(d.j$do_w)
qqline(d.j$do_d)

# kernel density function
d<-density(d.j$do_w); plot(d) #kernel-density plot

#######################################
# Relationships X & Y ################

# scatterplot
ggplot(d.j,aes(y=conif,x=imperv))+geom_point(shape=1, colour="grey50")


# box plot for categories ############

#first need to rearrange data
# subset to just what you want to graph. here do wet and dry

# METHOD 1
#wet
d.j2a<-dplyr::select(d.j,FID,do=do_w) # note keeping FID so can rejoin
d.j2a$season<-"wet"
names(d.j2a)
str(d.j2a)

#dry
d.j2b<-dplyr::select(d.j,FID,do=do_d)
d.j2b$season<-"dry"
names(d.j2b)
head(d.j2b)

#combine these files by stacking them on top of each other
d.j3<-rbind(d.j2a,d.j2b)
head(d.j3)
tail(d.j3)

# METHOD 2
# NOTE: original data is wide format, now change to long format
# The arguments to gather():
	# - data: Data object
	# - key: Name of new key column (made from names of data columns)
	# - value: Name of new value column
	# - ...: Names of source columns that contain values
	# - factor_key: Treat the new key column as a factor 
d.j2c<-dplyr::select(d.j,FID,do_w,do_d) #subset data
d.j2c$FID<-as.factor(d.j2c$FID)
d.j4<- gather(d.j2c, season, do, do_w:do_d)
head(d.j4)

# rename wet and dry
d.j4$season<-as.character(d.j4$season)
d.j4$season[d.j4$season=="do_w"]<-"wet"
d.j4$season[d.j4$season=="do_d"]<-"dry"

# boxplot
ggplot(data=d.j4,aes(x=season,y=do))+geom_boxplot()+
	theme(axis.text.x  = element_text(angle=90))

# checking residuals
# homogeneity tests based on models
m1<-lm(do_w~do_d+imperv,data=d.j)
e1<-rstandard(m1)

ggplot(data=d.j,aes(x=factor(Municipal), y=e1))+geom_boxplot()+	ylab("residuals") # this does not work because NAs in data are making e1 and data different lengths. Remove NAs from the columns you used then graph.


