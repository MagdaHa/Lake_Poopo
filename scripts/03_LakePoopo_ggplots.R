##########################################################################################
#### Lake Poopo, Bolivia: ggplots
#### author: Magdalena Halbgewachs
#### April 2019
##########################################################################################

##########################################################################################
#### Background information

### Study area: Lake Poopó, Bolivia
### content:
### 1.) data import
### 2.) Plot precipitation
### 3.) Plot evapotranspiration
### 4.) Plot water area, combined with precipitation/evapotranspiration 
### 5.) Plot water area Lake Titicaca, combined with water area Lake Poopó 
### 6.) Plot correlations of all environmental factors April and July 1989 - 2018

##########################################################################################

#loading required packages
library(ggplot2)
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(reshape2)
library(corrplot)

##########################################################################################
# 1.) data import
##########################################################################################
###precipitation and evapotranspiration all months 1989 - 2018
df_data_all <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\prec_et_mean_all.csv", header=T, sep=";")
if(names(df_data_all)[1]=="ï..YEAR"){             #replaces mysterious column name
  names(df_data_all)[1]<- "YEAR"
}
df_data_all[[1]] <- as.Date(df_data_all[[1]], origin="1989-01.01")   #read as date
names(df_data_all) <- c ("YEAR", "PREC", "ET")  #rename columns

#----------------------------------------------------------------------------------------
###precipitation and evapotranspiration April and July 1989 - 2018
df_data <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\prec_et_april_july.csv", header=T, sep=";")
if(names(df_data)[1]=="ï..YEAR"){             #replaces mysterious column name
  names(df_data)[1]<- "YEAR"
}
df_data[[1]] <- as.Date(df_data[[1]], origin="01-04-1989")   #read as date
names(df_data) <- c ("YEAR", "PREC_APRIL", "PREC_JULY", "ET_APRIL", "ET_JULY")

#----------------------------------------------------------------------------------------
###lake area

#only April 1989 - 2018 (based on script 01 (area calculation))
area_april <- area_df_april
if(names(area_april)[1]=="ï..YEAR"){
  names(area_april)[1]<- "YEAR"
}
area_april[[1]] <- as.Date(area_april[[1]], origin="1989-04-01")

#only July 1989 - 2018 (based on df of index calculations)
area_july <- area_df_july
if(names(area_july)[1]=="ï..YEAR"){
  names(area_july)[1]<- "YEAR"
}
area_july[[1]] <- as.Date(area_july[[1]], origin="1989-07-1")

#------------------------------
##only if df is not in current environment (not loaded from script 01)

#area_april<- read.csv("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\area_april.csv", header=T, sep=",")
#area_april <- area_april[-c(1)] #remove first column
#if(names(area_april)[1]=="ï..YEAR"){
#names(area_april)[1]<- "YEAR"
#}
#area_april[[1]] <- as.Date(area_april[[1]], origin="1989-04-1")

##only if df is not in current environment
#area_july <- read.csv("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\01_Landsat\\area_july.csv", header=T, sep=",")
#area_july <- area_july[-c(1)] #remove first column
#if(names(area_july)[1]=="ï..YEAR"){
  #names(area_july)[1]<- "YEAR"
#}
#area_july[[1]] <- as.Date(area_july[[1]], origin="1989-07-1")

#----------------------------------------------------------------------------------------
###save all data from April and July in one dataframe
df_data <- merge (df_data, area_april,by="YEAR", all=T)
df_data <- merge (df_data, area_july, by="YEAR", all=T)
names(df_data) <- c ("YEAR", "PREC_APRIL", "PREC_JULY", "ET_APRIL", "ET_JULY", "A_APRIL", "A_JULY")


#----------------------------------------------------------------------------------------
###lake area Titicaca 2002 - 2017

titicaca <- read.table("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Titicaca\\time_series_titicaca", sep = "", header = T, fill = T)
if(names(titicaca)[1]=="ï..YEAR"){
  names(titicaca)[1]<- "YEAR"
}
titicaca[[1]] <- as.Date(titicaca[[1]])


################################################################################
# 2.) precipitation
################################################################################

#precipitation all months between 1989 and 2018
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
  scale_x_date(breaks = as.Date(c("1989-04-01", "1994-04-01", "1999-04-01",
                                  "2004-04-01", "2009-04-01", "2014-04-01","2018-04-01")),date_labels = "%Y")+
  ggtitle("Mean precipitation in the Lake Poopó area (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15) 

#----------------------------------------------------------------------------------------
#precipitation only April and July 1989 to 2019
prec_plot <- ggplot(data=df_data, aes(x=YEAR, y=PREC_APRIL, group=1, color=legend)) +
  #data
  geom_bar(data=df_data, aes(x=YEAR, y=PREC_APRIL, color="April"),  stat="identity", alpha=0.3, width=300, fill="transparent", size=1) +
  geom_bar(data=df_data, aes(x=YEAR, y=PREC_JULY, color="July"),  stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+
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
  ggtitle("Mean precipitation in the Lake Poopó area in April and July (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15)
prec_plot


##########################################################################################
# 3.) evapotranspiration
##########################################################################################

#evapotranspiratoin all months between 1989 and 2018
ggplot(data=df_data_all, aes(x=YEAR, y=ET, group=1, color=legend)) +
  #data
  geom_bar(data=df_data_all, aes(x=YEAR, y=ET, color="evapotranspiration"), stat="identity", alpha=0.7, fill="red")+
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
  scale_x_date(breaks = as.Date(c("1989-04-01", "1994-04-01", "1999-04-01",
                                  "2004-04-01", "2009-04-01", "2014-04-01","2018-04-01")),date_labels = "%Y")+
  ggtitle("Mean precipitation in the Lake Poopó area (1989-2018)")+
  ggtitle("Mean evapotranspiration in the Lake Poopó area (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15)

#----------------------------------------------------------------------------------------
#evapotranspiration only April and July 1989 to 2018
et_plot <- ggplot(data=df_data, aes(x=YEAR, y=ET_APRIL, group=1, color=legend)) +
  #data
  geom_bar(data=df_data, aes(x=YEAR, y=ET_APRIL, color="April"), stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+
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


##########################################################################################
# 4.) water area Lake Poopó
##########################################################################################

#water area in April and July 1989 to 2019
water_plot <- ggplot(data=df_data, aes(x=YEAR, y=A_APRIL, group=1, color=legend)) +
  #data
  geom_bar(data=df_data, aes(x=YEAR, y=A_APRIL, color="April"), stat="identity", alpha=0.2, width=300, fill="transparent", size=1)+
  geom_bar(data=df_data, aes(x=YEAR, y=A_JULY, color="July"), stat="identity", alpha=0.2, width=300, fill="transparent", size=1)+
  #linear regression
  geom_smooth(data=df_data, aes(x=YEAR, y=A_APRIL), method='lm',formula=y~x, color="#F8766D", linetype="dashed", se=F)+
  geom_smooth(data=df_data, aes(x=YEAR, y=A_JULY), method='lm',formula=y~x, color="#00BFC4", linetype="dashed", se=F)+
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
  ggtitle("Water area of Lake Poopó in April and July (1989-2018)")+
  labs(caption = "data: based on NDWI calculations (Landsat 5 and 8)")+
  theme_gray(base_size = 15)
water_plot

water_plot+et_plot       #water area and evapotranspiration for April and July 1989 to 2018
water_plot+prec_plot     #water area and precipitation for April and July 1989 to 2018


##########################################################################################
# 5.) water area Lake Titicaca
##########################################################################################

#water area monthly 2002 to 2017
water_titicaca <- ggplot(data=titicaca, aes(x=date, y=heights, group=1))+
  #vizualisation
  geom_line(color="green4")+
  geom_point(size=1)+
  geom_smooth(method='lm',formula=y~x, se=F, color="green4")+
  #themes
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))+
  theme(legend.title = element_text(size = 10, face = "bold"))+
  theme(legend.text = element_text(size = 6))+
  theme(axis.text.y=element_text(angle = 90, hjust=1, size = 9))+
  #axis
  xlab("year")+
  ylab("water level (m a.s.l.)")+
  ggtitle("Mean water level in Lake Titicaca (2002-2018)")+
  labs(caption = "data: \nSchwatke et al. (2015): \nDAHITI - an innovative approach for estimating water level time series over inland waters using multi-mission satellite altimetry")+
  theme_gray(base_size = 15)

water_titicaca+water_plot #water area Lake Titicaca and Poopó 1989 to 2018


##########################################################################################
# 6.) CORRELATIONS
##########################################################################################

#loading data only April and July 1989 - 2018
corr <- read.csv("C:\\02_Studium\\02_Master\\01_Semester 1\\00_paper_work\\01_Lakes\\Lake_Poopó\\correlation.csv", header=T, sep=";")
head(corr, 6)
#rename columns
colnames(corr)[1:4] <- c("evapotranspiration", "precipitation", "area Poopó", "area Titicaca") # handling missing values
#matrix
corr_na <-cor(corr, use = "complete.obs")
res <- cor(corr_na)
round(res, 2)

#plot correlations
corrplot(res, method="number", tl.col = "black", type="upper", order="hclust", tl.srt = 45)

