###########################################################################
# Compare locations of past 6 months to closest locations to member's saved
# addresses
# 12/18/16
###########################################################################

require(zipcarFunctions)

#if(!exists("detachAllPackages", mode="function")) source("C:\\Users\\babbenante\\OneDrive - Avis Budget Group\\My Stuff\\code\\utils\\utils.r")
#unload all previously loaded packages
detachAllPackages()

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

#load required libraries
if (!require(readr)) {
  install.packages('readr') # read preformatted dates
  require(read)
}
if (!require(dplyr)) {
  install.packages('dplyr') # consistent data.frame operations
  require(dplyr)
}
if (!require(purrr)) {
  install.packages('purrr') # consistent & safe list/vector munging
  require(purrr)
}
if (!require(tidyr)) {
  install.packages('tidyr') # consistent data.frame cleaning
  require(tidyr)
}
if (!require(lubridate)) {
  install.packages('lubridate') # date manipulation
  require(lubridate)
}
if (!require(ggplot2)) {
  install.packages('ggplot2') # date manipulation
  require(ggplot2)
}
if (!require(scales)) {
  install.packages('scales') # date manipulation
  require(scales)
}
if (!require(devtools)) {
  install.packages('devtools') # date manipulation
  require(devtools)
}
if (!require(here)) {
  devtools::install_github("krlmlr/here") #relative working directory
  require(here)
}
#set working directory
setwd(here())


members_rfm <- read_delim("C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/reports/rfm/rfmscores20170130.txt"
                          ," ", escape_double = FALSE
                          , col_types = cols(MEMBER_ID = col_character())
                          ,trim_ws = TRUE)

Reservation_Details_Past_Year <- read_delim("C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/Data/Reservations/Reservation-Details-Past-Year.txt"
                                            , "\t"
                                            , escape_double = FALSE
                                            , col_types = cols(LOCATION_ID = col_character()
                                                               , MEMBER_ID = col_character()
                                                               , RESERVATION_DATE = col_datetime(format = "%m/%d/%Y %H:%M")
                                                               , RESERVATION_ID = col_character())
                                            , trim_ws = TRUE)

rfm.compact <- members_rfm %>% 
  select(MEMBER_ID,Profile)

Reservation_Details_Past_Year <- left_join(Reservation_Details_Past_Year,rfm.compact,by='MEMBER_ID')


monthly.channel.trend.last<-Reservation_Details_Past_Year %>% 
  mutate(reporting.date=rollback(RESERVATION_DATE,roll_to_first=TRUE,preserve_hms=FALSE)) %>% 
#  group_by(reporting.date,MEMBER_ID) %>%
  group_by(reporting.date) %>% 
  summarise(tot_web=sum(WEB_RES)
            ,tot_app=sum(APP_RES)
            ,tot_oth=n()-tot_web-tot_app)

monthly.channel.trend.orig<-Reservation_Details_Past_Year %>% 
  mutate(reporting.date=rollback(RESERVATION_DATE,roll_to_first=TRUE,preserve_hms=FALSE)) %>% 
  #  group_by(reporting.date,MEMBER_ID) %>%
  group_by(reporting.date) %>% 
  summarise(tot_web=sum(ORIG_WEB_RES)
            ,tot_app=sum(ORIG_APP_RES)
            ,tot_oth=n()-tot_web-tot_app)


monthly.user.trend.orig<-Reservation_Details_Past_Year %>% 
  mutate(reporting.date=rollback(RESERVATION_DATE,roll_to_first=TRUE,preserve_hms=FALSE)) %>% 
  group_by(reporting.date,MEMBER_ID) %>%
  summarise(tot_web=sum(ORIG_WEB_RES)
            ,tot_app=sum(ORIG_APP_RES)
            ,tot_oth=n()-tot_web-tot_app
            ,tot_res=n()) %>% 
  mutate(UserPreference=ifelse(tot_app>0,ifelse(tot_web>0,"Both","App")
                               ,ifelse(tot_web>0,"Web","Other"))) %>% 
  group_by(reporting.date,UserPreference) %>% 
  summarise(members=n())

quarterly.user.trend.orig<-Reservation_Details_Past_Year %>% 
  mutate(reporting.date=rollback(RESERVATION_DATE,roll_to_first=TRUE,preserve_hms=FALSE)) %>% 
  filter(reporting.date<as.Date('2016-12-31'))%>%
  mutate(reporting.date=quarter(RESERVATION_DATE))%>%
  group_by(reporting.date,MEMBER_ID) %>%
  summarise(tot_web=sum(ORIG_WEB_RES)
            ,tot_app=sum(ORIG_APP_RES)
            ,tot_oth=n()-tot_web-tot_app
            ,tot_res=n()) %>% 
  mutate(UserPreference=ifelse(tot_app>0,ifelse(tot_web>0,"Both","App")
                               ,ifelse(tot_web>0,"Web","Other"))) %>% 
  group_by(reporting.date,UserPreference) %>% 
  summarise(members=n())

           
user.trend.orig<-Reservation_Details_Past_Year %>% 
  group_by(MEMBER_ID) %>%
  summarise(tot_web=sum(ORIG_WEB_RES)
            ,tot_app=sum(ORIG_APP_RES)
            ,tot_oth=n()-tot_web-tot_app
            ,tot_res=n()) %>% 
  mutate(UserPreference=ifelse(tot_app>0,ifelse(tot_web>0,"Both","App")
                               ,ifelse(tot_web>0,"Web","Other"))) 

save(mem.summary,monthly.channel.trend.last,monthly.channel.trend.orig,monthly.channel.trend.orig.poweruser,quarterly.user.trend.orig,user.trend.orig,file="c:/users/babbenante/downloads/FebAnalysisData.rmd")

#Reservations by last booking channel
gather(monthly.channel.trend.last,channel,reservations,tot_web:tot_oth)%>%
  filter(reporting.date<as.Date("2017-01-31"))%>%
  ggplot(aes( reporting.date, reservations))+
  geom_area(aes(fill= channel), position = 'stack') +
  fte_theme() +
  #  scale_fill_brewer(palette="Greens")+
  scale_fill_manual(values=zipcar_color_palette(3))+
  labs(title="Monthly Reservations by Last Booking Channel ", x="Date", y="Reservations") +
  scale_y_continuous(labels = comma)+
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 

#Reservations by original booking channel 
gather(monthly.channel.trend.orig,channel,reservations,tot_web:tot_oth)%>%
  filter(reporting.date<as.Date("2017-01-31"))%>%
  ggplot(aes( reporting.date, reservations))+
  geom_area(aes(fill= channel), position = 'stack') +
  fte_theme() +
  #  scale_fill_brewer(palette="Greens")+
  scale_fill_manual(values=zipcar_color_palette(3))+
  labs(title="Monthly Reservations by Original Booking Channel ", x="Date", y="Reservations") +
  scale_y_continuous(labels = comma)+
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom')

#Members by channel usage (monthly)
monthly.user.trend.orig%>%
  filter(reporting.date<as.Date("2017-01-31"))%>%
  ggplot(aes( reporting.date, members))+
  geom_area(aes(fill= UserPreference), position = 'stack') +
  fte_theme() +
  #  scale_fill_brewer(palette="Greens")+
  scale_fill_manual(values=zipcar_color_palette(4))+
  labs(title="Members by Original Booking Preference ", x="Date", y="Members") +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom')


#Members by channel usage (quarterly)
quarterly.user.trend.orig%>%
  ggplot(aes( reporting.date, members))+
  geom_area(aes(fill= UserPreference), position = 'stack') +
  fte_theme() +
  #  scale_fill_brewer(palette="Greens")+
  scale_fill_manual(values=zipcar_color_palette(4))+
  labs(title="Members by Original Booking Preference ", x="Quarter", y="Members") +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom')


members_rfm<-left_join(members_rfm,user.trend.orig,by="MEMBER_ID")

mem.summary<-members_rfm%>%
  group_by(Profile,UserPreference)%>%
  summarise(members=n()
            ,web_res=mean(tot_web)
            ,app_res=mean(tot_app))

monthly.channel.trend.orig.poweruser<-Reservation_Details_Past_Year %>% 
  filter(Profile %in% c('Power User'))%>%
  mutate(reporting.date=rollback(RESERVATION_DATE,roll_to_first=TRUE,preserve_hms=FALSE)) %>% 
  #  group_by(reporting.date,MEMBER_ID) %>%
  group_by(reporting.date) %>% 
  summarise(tot_web=sum(ORIG_WEB_RES)
            ,tot_app=sum(ORIG_APP_RES)
            ,tot_oth=n()-tot_web-tot_app)

#Top User Reservations by original booking channel 
gather(monthly.channel.trend.orig.poweruser,channel,reservations,tot_web:tot_oth)%>%
  filter(reporting.date<as.Date("2017-01-31"))%>%
  ggplot(aes( reporting.date, reservations))+
  geom_area(aes(fill= channel), position = 'stack') +
  fte_theme() +
  #  scale_fill_brewer(palette="Greens")+
  scale_fill_manual(values=zipcar_color_palette(3))+
  labs(title="Monthly Power User Reservations by Original Booking Channel ", x="Date", y="Reservations") +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom')
