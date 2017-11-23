# Jennifer Selgrath
# graphing

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
head(d.k)
names(d.k)

# wide to long format ############
d.k$Field_ID<-as.factor(d.k$Field_ID)
d.k2<-gather(d.k,treatment,value,X20m:Control)#, factor_key=TRUE)
head(d.k2)

d.k2$treatment<-gsub("X","",d.k2$treatment) #remove leading X
head(d.k2)


# basic plot

ggplot(d.k2,aes(x=treatment,y=value))+geom_boxplot()

# for bar graph need to reorganize so that there is a mean and se or sd value 
d.k2a<-na.omit(d.k2)

d.k3<-ddply(d.k2a,c("treatment"),summarise,
						n=length(value),
						value.u=mean(value),
						value.sd=sd(value),
						value.se=value.sd/sqrt(n))

head(d.k3)


# graph

g1<-ggplot(d.k3,aes(x=treatment,y=value.u))+geom_bar(stat="identity")
g1+ geom_errorbar(aes(ymin=value.u-value.se, ymax=value.u+value.se), width=.2)


#########################
# Fancier Graphs Setup
#########################

ppi<-300 #pixels per inch


##############
# set theme
deets<-	theme_bw(base_size=22) + #
	theme(panel.grid.minor=element_blank(), 
				panel.grid.major=element_blank(),
				panel.background=element_rect(fill=NA),
				panel.border=element_blank(), #element_rect(fill=NA)
				axis.line = element_line(size=1.25),
				axis.ticks = element_line(size=1),
				axis.title = element_text(size=rel(1.3),colour="black", face="bold", lineheight=1),
				axis.text = element_text(size=rel(1), lineheight=.8,  colour="black", face="bold"),
				axis.title.y=element_text(vjust=1,lineheight=.8),
				axis.title.x=element_text(vjust=.5),
				legend.title=element_text(size=rel(1.3)),
				legend.text=element_text(size=rel(1.2)),
				legend.position=c(.125,.75), #legend.position=c(.75,.15),
				legend.key = element_rect(fill=NA),
				legend.background = element_rect(colour = 'black'), 
				plot.title = element_text(hjust = 0.5,size=rel(1.5),colour="black", face="bold"),
				plot.subtitle = element_text(hjust = 0.5,size=rel(1.3),colour="black"),
				legend.key.width = unit(1.5, "cm"))

# theme(legend.background = element_rect(col="black"))


######################
# save graphs #############
# tiff(paste("./doc/fields%d.tiff",sep=""), width = (8.7/2.54)*ppi, height=(6/2.54)*ppi)

# same graph from above
g1<-ggplot(d.k3,aes(x=treatment,y=value.u))+geom_bar(stat="identity")


g1+ 
	geom_errorbar(aes(ymin=value.u-value.se, ymax=value.u+value.se), width=.2)+
	ylab("Treatment")+
	xlab("Value (mean (sem))")+
	deets+
	# ylim(c(0,80000))+
	ggtitle("Stuff about fields", subtitle = "great stuff!")

dev.off() # turns off graph exporting

################
# Below is example code for annotating ##############

# annotate("text", x=1965,y=108, label="Limited\n(1950-1971)", size = 8)+
# 	annotate("text", x=1979,y=108, label="Productivity\n(1972-1985)", size = 8)+
# 	annotate("text", x=1992,y=108, label="Decentralized\n(1986-1997)", size = 8)+
# 	annotate("text", x=2005,y=108, label="Co-management\n(1998-2010)", size = 8)+
# 	
# 	geom_segment(aes(x = 1971, y = -Inf, xend = 1971, yend = 96)) +
# 	geom_segment(aes(x = 1986, y = -Inf, xend = 1986, yend = 96))+
# 	geom_segment(aes(x = 1998, y = -Inf, xend = 1998, yend = 96))+
	
	

