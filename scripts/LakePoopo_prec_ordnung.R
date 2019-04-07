############################################
##### CORRELATION OF ENVIRONMENTAL DATA ####
############## PREC AND ET #################
########### WITH WATER LEVELS ##############
############################################

library(data.table)
#library(ggplot2)
#devtools::install_github("thomasp85/patchwork")
#library(patchwork)
library(scales)
library(tidyverse)
#library(reshape2)
#library(corrplot)

################################################################################
#--------------------------------------------------------------------------------
# data import
#--------------------------------------------------------------------------------
################################################################################
###precipitation###
#all months 1989 - 2018
prec_mean_all <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\prec_mean.csv", header=T, sep=";")
if(names(prec_mean_all)[1]=="ï..YEAR"){
  names(prec_mean_all)[1]<- "YEAR"
}
prec_mean_all[[1]] <- as.Date(prec_mean_all[[1]])   #read as date

#only April 1989 - 2018
prec_mean_april <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\precipitation_mean_date_april.csv", header=T, sep=";")
if(names(prec_mean_april)[1]=="ï..YEAR"){
  names(prec_mean_april)[1]<- "YEAR"
}
prec_mean_april[[1]] <- as.Date(prec_mean_april[[1]])

#only July 1989 - 2018
prec_mean_july <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\precipitation_mean_date_july.csv", header=T, sep=";")
if(names(prec_mean_july)[1]=="ï..YEAR"){
  names(prec_mean_july)[1]<- "YEAR"
}
prec_mean_july[[1]] <- as.Date(prec_mean_july[[1]])

#-------------------------------------------------------------------------------
###evapotranspiration###
#all months 1989 - 2018
et_mean_all <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\ET_mean.csv", header=T, sep=";")
if(names(et_mean_all)[1]=="ï..YEAR"){
  names(et_mean_all)[1]<- "YEAR"
}
et_mean_all[[1]] <- as.Date(et_mean_all[[1]])

#only April 1989 - 2018
et_mean_april <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\ET_mean_date_april.csv", header=T, sep=";")
if(names(et_mean_april)[1]=="ï..YEAR"){
  names(et_mean_april)[1]<- "YEAR"
}
et_mean_april[[1]] <- as.Date(et_mean_april[[1]])

#only July 1989 - 2018
et_mean_july <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\ET_mean_date_july.csv", header=T, sep=";")
if(names(et_mean_july)[1]=="ï..YEAR"){
  names(et_mean_july)[1]<- "YEAR"
}
et_mean_july[[1]] <- as.Date(et_mean_july[[1]])

#------------------------------------------------------------------------------
###lake area###
#only April 1989 - 2018
area_april<- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\area_april.csv", header=T, sep=";")
if(names(area_april)[1]=="ï..YEAR"){
  names(area_april)[1]<- "YEAR"
}
area_april[[1]] <- as.Date(area_april[[1]])

#only July 1989 - 2018
area_july<- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\area_july.csv", header=T, sep=";")
if(names(area_july)[1]=="ï..YEAR"){
  names(area_july)[1]<- "YEAR"
}
area_july[[1]] <- as.Date(area_july[[1]])

#------------------------------------------------------------------------------
###lake area Titicaca###
titicaca <- read.table("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Titicaca\\time_series_titicaca", sep = "", header = T, fill = T)
if(names(titicaca)[1]=="ï..YEAR"){
  names(titicaca)[1]<- "YEAR"
}
titicaca[[1]] <- as.Date(titicaca[[1]])

#----------------------------------------------------------------------------
###save all data in one dataframe###
#all April and July data
df_data <- merge (area_april, area_july,by="YEAR", all=T)
df_data <- merge (df_data, prec_mean_april, by="YEAR", all=T)
df_data <- merge (df_data, prec_mean_july, by="YEAR", all=T)
df_data <- merge (df_data, et_mean_april, by="YEAR", all=T)
df_data <- merge (df_data, et_mean_july, by="YEAR", all=T)
names(df_data) <- c ("YEAR", "A_APRIL", "A_JULY", "PREC_APRIL", "PREC_JULY", "ET_APRIL", "ET_JULY")

#----------
#all months 1989 - 2018
df_data_all <- merge (et_mean_all, prec_mean_all,by="YEAR", all=T)
names(df_data_all) <- c ("YEAR", "ET", "PREC")

#save all months 1989 - 2018 in one dataframe (all data in one column)
df_reshape_all<- df_data_all
df_reshape_all <- melt(df_data_all, id=c("YEAR"))


################################################################################
#--------------------------------------------------------------------------------
# 1.) precipitation
#--------------------------------------------------------------------------------
################################################################################
ggplot(data=df_data_all, aes(x=YEAR, y=PREC, group=1, color=legend)) +
  #data
  geom_bar(data=df_data_all, aes(x=YEAR, y=PREC, color="precipitation"), stat="identity", alpha=0.7, fill="red")+#, color="blue")+
  #linear regression
  geom_smooth(method='lm',formula=y~x, color="red", linetype="dashed", se=F)+
  #themes
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 6),
        axis.text.y=element_text(angle = 45, size = 9))+
  #axis
  xlab("year")+
  ylab("precipitation (mm)")+
  ggtitle("Mean precipitation in the Lake Poopó area (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15) 

#----------------------------
#precipitation April and July
#----------------------------

prec_plot <- ggplot(data=df_data, aes(x=YEAR, y=PREC_APRIL, group=1, color=legend)) +
  #data
  geom_bar(data=df_data, aes(x=YEAR, y=PREC_APRIL, color="April"),  stat="identity", alpha=0.3, width=300, fill="transparent", size=1) +#color="orangered4"
  geom_bar(data=df_data, aes(x=YEAR, y=PREC_JULY, color="July"),  stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+#color="aquamarine4",
  #linear regression
  geom_smooth(aes(x=YEAR, y=PREC_APRIL), formula=y~x, method='lm', color="#F8766D", linetype="dashed",  se=F)+
  geom_smooth(aes(x=YEAR, y=PREC_JULY), formula=y~x, method='lm', color="#00BFC4", linetype="dashed", se=F)+
  #themes
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 6),
        axis.text.y=element_text(angle = 45, size = 9))+
  #axis
  xlab("year")+
  ylab("precipitation (mm)")+
  scale_x_date(breaks = as.Date(c("1989-04-01", "1995-04-01", "1999-04-01",
                                  "2005-04-01", "2009-04-01", "2013-04-01",
                                  "2014-04-01", "2015-04-01", "2016-04-01", 
                                  "2017-04-01","2018-04-01")),date_labels = "%Y")+
  #scale_fill_manual("legend", values = c("A" = "black", "B" = "orange"))+
  ggtitle("Mean precipitation in the Lake Poopó area in April and July (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15)
prec_plot


##################################################################################
#--------------------------------------------------------------------------------
# 2.) evapotranspiration
#--------------------------------------------------------------------------------
##################################################################################
ggplot(data=df_data_all, aes(x=YEAR, y=ET, group=1, color=legend)) +
  #data
  geom_bar(data=df_data_all, aes(x=YEAR, y=ET, color="evapotranspiration"), stat="identity", alpha=0.7, fill="red")+ #color="orangered4"
  #linear regression
  geom_smooth(method='lm',formula=y~x, color="red", linetype="dashed", se=F)+
  #themes
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 6),
        axis.text.y=element_text(angle = 45, size = 9))+
  #axis
  xlab("year")+
  ylab("evapotranspiration (mm)")+
  ggtitle("Mean evapotranspiration in the Lake Poopó area (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15)

#-----------------------------------
#evapotranspiration April and July
#-----------------------------------
et_plot <- ggplot(data=df_data, aes(x=YEAR, y=ET_APRIL, group=1, color=legend)) +
  #data
  geom_bar(data=df_data, aes(x=YEAR, y=ET_APRIL, color="April"), stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+ #color="orangered4",
  geom_bar(data=df_data, aes(x=YEAR, y=ET_JULY, color="July"), stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+
  #linear regression
  geom_smooth(data=df_data, aes(x=YEAR, y=ET_APRIL), method='lm',formula=y~x, color="#F8766D", linetype="dashed", se=F)+
  geom_smooth(data=df_data, aes(x=YEAR, y=ET_JULY), method='lm',formula=y~x, color="#00BFC4", linetype="dashed", se=F)+
  #themes
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 6),
        axis.text.y=element_text(angle = 45, size = 9))+ 
  #axis
  xlab("year")+
  ylab("evapotranspiration (mm)")+
  scale_x_date(breaks = as.Date(c("1989-04-01", "1995-04-01", "1999-04-01",
                                  "2005-04-01", "2009-04-01", "2013-04-01",
                                  "2014-04-01", "2015-04-01", "2016-04-01", 
                                  "2017-04-01","2018-04-01")),date_labels = "%Y")+
  ggtitle("Mean evapotranspiration in the Lake Poopó area in April and July (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15)
et_plot


################################################################################
#--------------------------------------------------------------------------------
# 3.) water area
#--------------------------------------------------------------------------------
################################################################################
water_plot <- ggplot(data=df_data, aes(x=YEAR, y=A_APRIL, group=1, color=legend)) +
  #data
  geom_bar(data=df_data, aes(x=YEAR, y=A_APRIL, color="April"), stat="identity", alpha=0.2, width=300, fill="transparent", size=1)+ #color="orangered4"
  geom_bar(data=df_data, aes(x=YEAR, y=A_JULY, color="July"), stat="identity", alpha=0.2, width=300, fill="transparent", size=1)+ #color="aquamarine4"
  #linear regression
  geom_smooth(data=df_data, aes(x=YEAR, y=A_APRIL), method='lm',formula=y~x, color="#F8766D", linetype="dashed", se=F)+
  geom_smooth(data=df_data, aes(x=YEAR, y=A_JULY), method='lm',formula=y~x, color="#00BFC4", linetype="dashed", se=F)+ #linear regression
  #themes
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 6),
        axis.text.y=element_text(angle = 45, size = 9))+
  #axis
  xlab("year")+
  ylab("area (km²)")+
  scale_x_date(breaks = as.Date(c("1989-04-01", "1995-04-01", "1999-04-01",
                                  "2005-04-01", "2009-04-01", "2013-04-01",
                                  "2014-04-01", "2015-04-01", "2016-04-01", 
                                  "2017-04-01","2018-04-01")),date_labels = "%Y")+
  #scale_color_manual(values = c('April' = 'black', 'July' = 'aquamarine4'))+
  #scale_fill_manual(name='legendname',
                    #values=c('orangered4','aquamarine4'),
                    #labels=c('APRIL','JULY'))+
  ggtitle("Water area of Lake Poopó in April and July (1989-2018)")+
  labs(caption = "data: based on NDWI calculations (Landsat 5 and 8)")+
  theme_gray(base_size = 15)
water_plot
water_plot+et_plot
water_plot+prec_plot


###################################################################################
#---------------------------------------------------------------------------------
# 4.) Lake Titicaca
#--------------------------------------------------------------------------------
###################################################################################

water_titicaca <- ggplot(data=titicaca, aes(x=date, y=heights, group=1))+
  geom_line(color="green4")+
  geom_point(size=1)+
  geom_smooth(method='lm',formula=y~x, se=F, color="green4")+ #linear regression
  #stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 2, raw=TRUE),colour="yellow")  #3rd polynomial regression
  ggtitle("Mean water level in Lake Titicaca (2002-2018)")+        #title name
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))+      #title size, specification
  theme(legend.title = element_text(size = 10, face = "bold"))+                 #legend title size, specification
  theme(legend.text = element_text(size = 6))+                                  #legend element size
  theme(axis.text.y=element_text(angle = 90, hjust=1, size = 9))+
  #scale_x_continuous(breaks = seq(2002, 2018, 1))+
  #scale_x_date(date_labels="%b %y",date_breaks  ="6 month")+
  #scale_x_discrete(limits=c("April *", "July *"))+
  #scale_x_discrete(name ="date", limits=c("Apr","Jul"))+
  xlab("year")+
  ylab("water level (m a.s.l.)")+
  #scale_x_continuous(breaks = c(11))
  #scale_x_date(breaks = as.Date(c("2002-07-01", "2003-07-21", "2004-07-05", "2005-07-25", "2006-07-10", "2007-07-30",
  #"2008-07-14", "2009-07-01", "2010-07-19", "2011-07-27", "2012-07-02",
  #"2013-07-05", "2014-07-09", "2015-07-12", "2016-07-01", "2017-07-11", "2018-07-24")))+
  #minor_breaks = as.Date(c("Jan 89", "Jan 99", "Jan 00", "Jan 01", "Jan 02", "Jan 03",
  #"Jan 04", "Jan 05", "Jan 06", "Jan 07", "Jan 08", "Jan 09",
  #"Jan 10", "Jan 11", "Jan 12", "Jan 13", "Jan 14", "Jan 15",
  #"Jan 16", "Jan 17", "Jan 18"))) +
  labs(caption = "data: \nSchwatke et al. (2015): \nDAHITI - an innovative approach for estimating water level time series over inland waters using multi-mission satellite altimetry")+
  theme_gray(base_size = 15)
water_titicaca+water_plot


####################################################################################
#---------------------------------------------------------------------------------
# 5.) CORRELATIONS
#--------------------------------------------------------------------------------
####################################################################################

corr <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\correlation.csv", header=T, sep=";")
head(corr, 6)
colnames(corr)[1:4] <- c("evapotranspiration", "precipitation", "area Poopó", "area Titicaca") # handling missing values
corr_na <-cor(corr, use = "complete.obs")
res <- cor(corr_na)
round(res, 2)

#plot correlations
corrplot(res, method="number", tl.col = "black", type="upper", order="hclust", tl.srt = 45)

