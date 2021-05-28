# load packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readxl)
library(extrafont)

# set working directory
setwd("XXXX")

# load data that includes the seven indicator variables and the LEED Building rate
GB <- read.csv("./XXX.csv")

# Identify outliers in the independent variable.
hist(GB$GB_rate)
boxplot(GB$GB_rate)

# Tracts with LEED Building rates greater than 3 are outliers and would skew the results
#Remove the outliers from the data
GB_out_rem <- GB %>% filter(GB_rate<3)

# run the models
#Education of Bachelor's Degree or Higher
edu.lm <- lm(EDU.CHNG~GB_rate, data = GB_out_rem)
summary(edu.lm)

#Household Income
inc.lm <- lm(INC.CHNG ~ GB_rate, data = GB_out_rem)
summary(inc.lm)

#Median Rent Cost
rent.lm <- lm(RENT.CHNG ~ GB_rate, data = GB_out_rem)
summary(rent.lm)

#Average Home Value
home.lm <- lm(HOME.VAL.CHNG ~ GB_rate, data = GB_out_rem)
summary(home.lm)

#White Population
wht.lm <- lm(WHT.CHNG ~ GB_rate, data = GB_out_rem)
summary(wht.lm)

#Average Parcel Value
parvl.lm <- lm(AVE.PAR.CHNG ~ GB_rate, data = GB_out_rem)
summary(parvl.lm)

#Change in Residential Area
res.lm <- lm(RES.CHNG ~ GB_rate, data = GB_out_rem)
summary(res.lm)

#Change in Commercial Area
com.lm <- lm (COM.CHNG ~ GB_rate, data = GB_out_rem)
summary(com.lm)

# Residential area change and commercial area change have significant results
# Plot each model to check for heteroskedasticity and normal residuals
plot(res.lm)
plot(com.lm)
#Signs of heteroskedasticity, interpret with care
# Create plots of those two models
# Residential area
ggplot(data = GB_out_rem, aes(x=GB_rate, y=RES.CHNG)) +
  geom_hline(yintercept = 0, size =1)+
  geom_point(shape=21, size=3.5, color="#100770", fill="#044BBD40", stroke=1.5)+
  geom_smooth(method="lm", se=FALSE, color="#A32800", linetype="longdash", size=1.25)+
  labs(x="LEED Building Rate", y = "Change in Residential Area")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = c("0%", "0.5%", "1.0%", "1.5%"))+
  theme_calc()+
  theme(text=element_text(size=18, family="Tahoma"), 
        panel.border = element_rect(linetype = "solid", colour = "black"),
        axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(hjust=0.35))

#Commercial area
ggplot(data = GB_out_rem, aes(x=GB_rate, y=COM.CHNG)) +
  geom_hline(yintercept = 0, size =1)+
  geom_point(shape=21, size=3.5, color="#100770", fill="#044BBD40", stroke=1.5)+
  geom_smooth(method="lm", se=FALSE, color="#A32800", linetype="longdash", size=1.25)+
  labs(x="LEED Building Rate", y = "Change in Commercial Area")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = c("0%", "0.5%", "1.0%", "1.5%"))+
  theme_calc()+
  theme(text=element_text(size=18, family="Tahoma"), 
        panel.border = element_rect(linetype = "solid", colour = "black"),
        axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(hjust=0.35))
