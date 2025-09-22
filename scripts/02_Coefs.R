### code by Toby Maxwell, with slight modifications by Becca Nelson
### common garden soil vs air coefficients 

###Climate offsets Bromecast Experimental####
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggpmisc)

ambientdata<-read.csv("data/common_gardens/Ambient_dataset.csv")


posambdata<-ambientdata[ambientdata$hourlyairTemp_C>0,]

#####amb vs amb eqs#####
ggplot(posambdata, aes(y=ambient, x=hourlyairTemp_C))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~Site) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label))),
               f.digits = 4) +
  #stat_poly_eq(rr.digits = 2) +
  geom_abline(slope=1, color='red')+
  xlab("Ambient air temperature (C)")+
  ylab("Ambient soil temperature (C)")

coefs<-summary(lm(ambient~hourlyairTemp_C*Site, posambdata))
coefs<-coefs$coefficients[c("hourlyairTemp_C",'hourlyairTemp_C:SiteSheepStation','hourlyairTemp_C:SiteWildcat'),]
coefs[2,1]<-coefs[2,1]+coefs[1,1]
coefs[3,1]<-coefs[3,1]+coefs[1,1]
######xontinuous data by season 0-5 cm######
posambdata$Date<-as.Date(posambdata$Date)
posambdata$Month<-month(posambdata$Date)
posambdata$Season<-NA
posambdata[posambdata$Month %in% c(10,11,12),]$Season<-"Fall"
posambdata[posambdata$Month %in% c(1,2,3),]$Season<-"Winter"
posambdata[posambdata$Month %in% c(4,5,6),]$Season<-"Spring"
posambdata[posambdata$Month %in% c(7,8,9),]$Season<-"Summer"

posambdata.wide<-posambdata%>%
    pivot_longer(names_to = "Color", values_to = "Soiltemp_C", c(Black:White))

posambdata$BW_diff<-posambdata$Black-posambdata$White
posambdata

posambdata.means2<-posambdata%>%
  filter(DayNight=="Day")%>%
  select(Site, Season, BW_diff)%>%
  na.omit%>%
  group_by(Site, Season)%>%
  summarize(n=length(BW_diff), se=sd(BW_diff)/sqrt(n) ,BW_diff=mean(BW_diff))

#####continuous data averaged across all data per site#####
posambdata.means3<-posambdata%>%
  filter(DayNight=="Day")%>%
  select(Site, BW_diff)%>%
  na.omit%>%
  group_by(Site)%>%
  summarize(n=length(BW_diff), se=sd(BW_diff)/sqrt(n), BW_diff=mean(BW_diff))


###### 0-1 cm handheld###

handheldTCs<-read.csv("data/common_gardens/thermocouples_allsites_alldata.csv")
handheldTCs%>%filter(Site=="SheepStation")
handheldTCmeans<-handheldTCs%>%
  dplyr::select(Date, Site, Color, Soil_temp_1cm_C)%>%
  group_by(Date, Site, Color)%>%
  na.omit()%>%
  dplyr::summarise(Soil_temp_1cm_C=mean(Soil_temp_1cm_C))%>%
  pivot_wider(names_from = Color, values_from = Soil_temp_1cm_C)%>%
  na.omit()%>%
  mutate(BW_diff=Black-White)%>%
  group_by(Site)%>%
  summarize(n=length(BW_diff), se=sd(BW_diff)/sqrt(n), BW_diff=mean(BW_diff))


#####5cm handheld#####
handheldTCmeans5cm<-handheldTCs%>%
  dplyr::select(Date, Site, Color, Soil_temp_5cm_C)%>%
  group_by(Date, Site, Color)%>%
  na.omit()%>%
  dplyr::summarise(Soil_temp_5cm_C=mean(Soil_temp_5cm_C))%>%
  pivot_wider(names_from = Color, values_from = Soil_temp_5cm_C)%>%
  na.omit()%>%
  mutate(BW_diff=Black-White)%>%
  group_by(Site)%>%
  summarize(n=length(BW_diff), se=sd(BW_diff)/sqrt(n), BW_diff=mean(BW_diff))

####plots of possible ways to assign trt diffs#####

ggplot(handheldTCmeans, aes(y=BW_diff, x=Site))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymax=BW_diff+se, ymin=BW_diff-se), width=0.1)

ggplot(handheldTCmeans5cm, aes(y=BW_diff, x=Site))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymax=BW_diff+se, ymin=BW_diff-se), width=0.1)

ggplot(posambdata.means3, aes(y=BW_diff, x=Site))+
  geom_bar(stat = 'identity', position = position_dodge(0.9))+
  geom_errorbar(aes(ymax=BW_diff+se, ymin=BW_diff-se), width=0.1)

ggplot(posambdata.means2, aes(y=BW_diff, x=Season))+
  geom_bar(aes(fill=Site), stat='identity', position=position_dodge())+
  geom_errorbar(aes(ymax=BW_diff+se, ymin=BW_diff-se, group = interaction(Site, Season)),width=.2, position = position_dodge(0.9))


tempdiffoptions<-rbind(handheldTCmeans, handheldTCmeans5cm, posambdata.means3)
tempdiffoptions$method<-c(rep("Handheld 0-1 cm depth", 4), rep("Handheld 0-5cm depth", 4), rep("average datalogged trt diff 0-5cm", 3))

slopes<-data.frame(c("Cheyenne", "SheepStation", "Wildcat"), coefs[,1])
colnames(slopes)<-c("Site", "Slope of SoilTemp~Airtemp")
tempdiffoptions<-merge(tempdiffoptions, slopes, all=T)
tempdiffoptions
tempdiffoptions$Climatetocausetempdiff<-tempdiffoptions$BW_diff/tempdiffoptions$`Slope of SoilTemp~Airtemp`
tempdiffoptions

#### Baltzor Code by Megan Vahsen:
# Get average difference between black and white gravel treatments at Baltzor

# Load libraries
library(tidyverse); library(lubridate)

# Read in logger data
temp <- read_csv("data/BCtemploggers.csv")

# Clean and format logger data for plotting
temp %>% 
  filter(Site == "Balzor") %>% 
  # Cut all data to be between Jan-1 and Jun-22 (where both gravel treatments
  # have continuous data)
  filter(Date >= "2022-01-01" & Date <= "2022-06-22") -> baltzor_data 

# Make a plot to look at it
baltzor_data %>% 
  ggplot(aes(x = Date, y = Temp_C, color = Color)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Site) +
  scale_color_manual(values = c("black", "gray67")) +
  labs(y = "Temperature (°C) at 0-5 cm soil depth",
       x = "Date") 

# Get mean and standard errors for each treatment at the difference
baltzor_data %>% 
  select(Date, Color, Temp_C) %>% 
  spread(key = Color, value = Temp_C) %>% 
  mutate(Diff = Black - White) %>% 
  summarize(mean_black = mean(Black, na.rm = T),
            se_black = sd(Black, na.rm = T)/sqrt(n()),
            mean_white = mean(White, na.rm = T),
            se_white = sd(White, na.rm = T)/sqrt(n()),
            mean_diff = mean(Diff, na.rm = T),
            se_diff = sd(Diff, na.rm = T)/sqrt(n()))

