################
####titicaca####
################

#Schwatke, C., Dettmering, D., Bosch, W., and Seitz, F.:
#DAHITI - an innovative approach for estimating water level time series over inland waters using multi-mission satellite altimetry:
#Hydrol. Earth Syst. Sci., 19, 4345-4364, doi:10.5194/hess-19-4345-2015, 2015 

library(ggplot2)
#install.packages("colorspace")
#install.packages("gganimate")
library(gganimate)

titicaca <- read.table("D:\\01_Uni\\02_Master\\MB1_Digital Image Analysis and GIS\\00_final_project\\Titicaca\\time_series_titicaca", sep = "", header = T, fill = T)
titicaca[[1]] <- as.Date(titicaca[[1]])

ggplot(data=titicaca, aes(x=date, y=heights, group=1))+
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
  theme_gray()


#---------------------------------------------------
# combining water area Titicaca and Poopó
#---------------------------------------------------

ggplot(data=water_area, aes(x=ï..YEAR, y=AREA, group=1)) +
  geom_bar(data=water_area,stat="identity", aes(x=ï..YEAR, y=AREA), alpha=0.7, color="orangered4", fill="orangered3")+
  #geom_line(linetype = "dashed", color="orangered3")+
  #geom_point(color="red", size=1)+
  geom_smooth(method='lm',formula=y~x, color="orangered3", se=F)+
  #geom_line(linetype = "dashed", color="red")+
  #geom_point()+
  geom_smooth(method='lm',formula=y~x, se=F)+ #linear regression
  #stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 2, raw=TRUE),colour="yellow")  #3rd polynomial regression
  ggtitle("Comparison of water levels in Lake Poopó and Lake Titicaca (1989-2018)")+        #title name
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))+      #title size, specification
  theme(legend.title = element_text(size = 10, face = "bold"))+                 #legend title size, specification
  theme(legend.text = element_text(size = 6))+                                  #legend element size
  theme(axis.text.y=element_text(angle = 45, size = 9))+ 
  xlab("year")+
  ylab("area (km²)")+
  scale_x_date(breaks = as.Date(c("1989-04-01", "1995-04-01", "1999-04-01",
                                  "2005-04-01", "2009-04-01", "2013-04-01",
                                  "2014-04-01", "2015-04-01", "2016-04-01", 
                                  "2017-04-01","2018-04-01")),date_labels = "%Y")+
  theme_gray()+
  geom_bar(data=titicaca2, stat="identity", aes(x=ï..YEAR, y=AREA/2, group=1), color="green4")+
  #geom_point(data=titicaca2, aes(x=ï..YEAR, y=AREA*0.8, group=1))+
  #geom_smooth(data=titicaca2, aes(x=ï..YEAR, y=AREA*0.8, group=1), method='lm',formula=y~x, se=F, color="green4")
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "titicaca"))#, limits = c(3809, 3811))
  #scale_y_continuous(limits = c(3809, 3811))
