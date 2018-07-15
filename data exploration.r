##############################################################################################
###   Zipcar Member Reservation Source Segment Explorer
###   2011-2015
###
###   built on: 12/10/15
###
##############################################################################################



#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\r\\member rental patterns\\")


#load the required libraries
require(plyr) || install.packages("plyr", repos="http://cran.rstudio.org") 
library(plyr)

require(dplyr) || install.packages("dplyr", repos="http://cran.rstudio.org") 
library(dplyr)

require(tidyr) || install.packages("tidyr", repos="http://cran.rstudio.org") 
library(tidyr)

require(data.table) || install.packages("data.table", repos="http://cran.rstudio.org") 
library(data.table)

require(shiny) || install.packages("shiny", repos="http://cran.rstudio.org") 
library(shiny)

#require(devtools) || install.packages("devtools", repos="http://cran.rstudio.org") 
#library(devtools)
#install_github('ramnathv/rCharts')

require(rCharts) 

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()


##############################################################################################
### if running for the first time on a new computer 
### or if data has been refreshed
### uncomment the following code
### otherwise skip right to the load
##############################################################################################



#rental_history=read.csv("C:\\Users\\babbenante\\documents\\my stuff\\data\\Member Reservation Source Profile\\Quarterly Member Rez Type - 2011-2015.txt")

#names(rental_history)[1]<-paste("MEMBER_ID")

#rental_history$MEMBER_ID<-as.factor(rental_history$MEMBER_ID)
#rental_history$RES_YEAR<-as.factor(rental_history$RES_YEAR)
#rental_history$RES_QUARTER<-as.factor(rental_history$RES_QUARTER)
#rental_history$APP_USER<-as.factor(rental_history$APP_USER)
#rental_history$WEB_USER<-as.factor(rental_history$WEB_USER)

##############################################################################
##HELPER TABLE
##we need to build this table ahead of time because when we mutate the rental YQ 
##it is a char not a factor.  So we factor to then convert to numeric to 
##get the total number of quarters a member's reservations spanned
##############################################################################


#um.tmp<-rental_history %>%
#  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
#  group_by(MEMBER_ID) %>%
#  summarise(first_res=min(RES_YQ),last_res=max(RES_YQ))

#um.tmp$first_res=as.factor(um.tmp$first_res)
#um.tmp$last_res=as.factor(um.tmp$last_res)
#um.tmp$res_span<-as.numeric(um.tmp$last_res)-as.numeric(um.tmp$first_res)+1
#um.tmp$first_res<-NULL
#um.tmp$last_res<-NULL

#yq_counter = data.frame("YQ"=c("2011-1","2011-2","2011-3","2011-4",
#               "2012-1","2012-2","2012-3","2012-4",
#               "2013-1","2013-2","2013-3","2013-4",
#               "2014-1","2014-2","2014-3","2014-4",
#               "2015-1","2015-2","2015-3","2015-4"),
#               "YQ_Count"=c(1,2,3,4,
#                 5,6,7,8,
#                 9,10,11,12,
#                 13,14,15,16,
#                 17,18,19,20
#                 ))

#save(rental_history, um.tmp, file="data\\Member_Panel_Rental_Data.RData")

load("C:\\Users\\babbenante\\documents\\my stuff\\data\\Member Reservation Source Profile\\RData\\Member_Panel_Rental_Data_Quarterly.RData")


#Reservations by Channel by YQ 
rez.channel<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep=""),
         OTH_RES = TOT_RES-APP_RES-WEB_RES) %>%
  group_by(RES_YQ) %>%
  summarise(APP_RES = sum(APP_RES)
            ,WEB_RES = sum(WEB_RES)
            ,OTH_RES = sum(OTH_RES))



#Reservations by Channel by YQ and Segment
mx.1<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep=""),
         OTH_RES = TOT_RES-APP_RES-WEB_RES) %>%
  group_by(RES_YQ,USER_TYPE) %>%
  summarise(MEMBERS=n_distinct(MEMBER_ID)
            ,APP_RES = sum(APP_RES)
            ,WEB_RES = sum(WEB_RES)
            ,OTH_RES = sum(OTH_RES))

#Reservations by Channel by YQ and Segment
mx.origchan<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep=""),
         OTH_RES = TOT_RES-APP_RES-WEB_RES) %>%
  group_by(FIRST_RES_TYPE) %>%
  summarise(MEMBERS=n_distinct(MEMBER_ID)
            ,APP_RES = sum(APP_RES)
            ,WEB_RES = sum(WEB_RES)
            ,OTH_RES = sum(OTH_RES))

#counts of user types
usertype.orig.tot<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(MEMBER_ID,FIRST_RES_TYPE) %>%
  summarise(act_qrts=n_distinct(RES_YQ)
            ,web_usr_ct=sum(USER_TYPE=="Web")
            ,app_usr_ct=sum(USER_TYPE=="App")
            ,both_usr_ct=sum(USER_TYPE=="Both")
            ,oth_usr_ct=sum(USER_TYPE=="Other")) %>%
  group_by(FIRST_RES_TYPE,act_qrts) %>%
  summarise(n_distinct(MEMBER_ID)
            ,sum(web_usr_ct)
            ,sum(app_usr_ct)
            ,sum(both_usr_ct)
            ,sum(oth_usr_ct))



#Unique Yearly reserving Members
um.YR<-rental_history %>%
  group_by(RES_YEAR) %>%
  summarise(n_distinct(MEMBER_ID))

#Unique Yearly reserving Members, reservations and revenue
um.YR<-rental_history %>%
  group_by(RES_YEAR) %>%
  summarise(Member_Count=n_distinct(MEMBER_ID),Avg_Res=round(mean(TOT_RES),1),Avg_Rev=round(mean(TOT_REVENUE),2))


#Unique YQ reserving Members
um.YQ<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(RES_YQ) %>%
  summarise(n_distinct(MEMBER_ID))

#Unique YQ reserving Members
um.YQ.type<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(RES_YQ,USER_TYPE) %>%
  summarise(n_distinct(MEMBER_ID))

#Members by User Type  by First Reservation Cohort (for 2011-15)
um.FR.frq<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  filter(RES_YQ == FIRST_RES_COHORT) %>%
  group_by(USER_TYPE,RES_YQ) %>%
  summarise(Members=n_distinct(MEMBER_ID))%>%
  spread(.,USER_TYPE,Members)



#Members by First Reservation Cohort (for 2011-15)
um.FR<-rental_history %>%
  filter(grepl('2011|2012|2013|2014|2015',FIRST_RES_COHORT)) %>%
  group_by(FIRST_RES_COHORT) %>%
  summarise(n_distinct(MEMBER_ID)) 

#Members by First Reservation and User Type Cohort (for 2011-15)
um.FR<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  filter(grepl('2011|2012|2013|2014|2015',FIRST_RES_COHORT)) %>%
  filter(FIRST_RES_COHORT==RES_YQ) %>%
  group_by(FIRST_RES_COHORT,USER_TYPE) %>%
  summarise(n_distinct(MEMBER_ID),TOT_REV=sum(TOT_REVENUE),TOT_RES=sum(TOT_RES)) 

#Members by First Reservation and Number of Active Months cohorts
um.AQ<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(MEMBER_ID,FIRST_RES_COHORT) %>%
  summarise(act_qrts=n_distinct(RES_YQ)) %>%
  group_by(FIRST_RES_COHORT,act_qrts) %>%
  summarise(n_distinct(MEMBER_ID))

#Members, Channel, Reservations by First Reservation Type 
um.FRT<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(MEMBER_ID,FIRST_RES_TYPE) %>%
  group_by(FIRST_RES_TYPE) %>%
  summarise(n_distinct(MEMBER_ID),APP_RES=sum(APP_RES),WEB_RES=sum(WEB_RES),TOT_RES=sum(TOT_RES))


#Members by Number of USER_TYPES per year 
um.MT<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(MEMBER_ID,RES_YEAR) %>%
  summarise(MEM_TYPES=n_distinct(USER_TYPE)) %>%
  group_by(RES_YEAR,MEM_TYPES) %>%
  summarise(n_distinct(MEMBER_ID))

#Members by Number of USER_TYPES per year 
um.MT.2<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(MEMBER_ID,RES_YEAR, USER_TYPE) %>%
  summarise(RESERVATIONS=sum(TOT_RES))


#Quarterly Revenue by Active Quarters cohort
um.AQ<-rental_history %>%
  filter(grepl('2011|2012|2013|2014|2015',FIRST_RES_COHORT)) %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(MEMBER_ID,FIRST_RES_COHORT) %>%
  summarise(act_qrts=n_distinct(RES_YQ),TOT_RESERVATIONS=sum(TOT_RES),TOT_REVENUE=sum(TOT_REVENUE)) %>%
  group_by(act_qrts) %>%
  summarise(Members=n_distinct(MEMBER_ID),TOTAL_RESERVATIONS=sum(TOT_RESERVATIONS),TOTAL_REVENUE=sum(TOT_REVENUE))

rental_history=data.table(rental_history)
um.tmp<-data.table(um.tmp)

#Quarterly Revenue by Active Quarters cohort
um.AQ<-rental_history %>%
  filter(grepl('2011|2012|2013|2014|2015',FIRST_RES_COHORT)) %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  .[, ACTIVE_QTRS := uniqueN(RES_YQ), by=MEMBER_ID] %>%
  .[,ACTIVE_QTR_COUNT := order(RES_YQ),by=MEMBER_ID] %>%
  filter(ACTIVE_QTR_COUNT==2)%>%
  inner_join(.,um.tmp,by="MEMBER_ID")%>%
  group_by(ACTIVE_QTRS) %>%
  summarise(Members=n_distinct(MEMBER_ID),TOT_RESERVATIONS=sum(TOT_RES),TOT_REVENUE=sum(TOT_REVENUE),RENTAL_SPAN=round(mean(res_span),1))

#Quarterly Revenue by Active Quarters cohort
um.AQ<-rental_history %>%
  filter(grepl('2011|2012|2013|2014|2015',FIRST_RES_COHORT)) %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  .[, ACTIVE_QTRS := uniqueN(RES_YQ), by=MEMBER_ID] %>%
  .[,ACTIVE_QTR_COUNT := order(RES_YQ),by=MEMBER_ID] %>%
  filter(ACTIVE_QTR_COUNT==2)%>%
  inner_join(.,um.tmp,by="MEMBER_ID")%>%
  mutate(REV_QT = TOT_REVENUE/res_span) %>%
  group_by(ACTIVE_QTRS,res_span) %>%
  summarise(Members=n_distinct(MEMBER_ID),TOT_RESERVATIONS=sum(TOT_RES),TOT_REVENUE=sum(TOT_REVENUE))

#Quarterly Revenue by Active Quarters cohort
um.TQ<-rental_history %>%
  filter(grepl('2011|2012|2013|2014|2015',FIRST_RES_COHORT)) %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  .[, ACTIVE_QTRS := uniqueN(RES_YQ), by=MEMBER_ID] %>%
  .[,ACTIVE_QTR_COUNT := order(RES_YQ),by=MEMBER_ID] %>%
  inner_join(.,um.tmp,by="MEMBER_ID")%>%
  filter(ACTIVE_QTRS==5)%>%
  #  mutate(REV_QT = TOT_REVENUE/res_span) %>%
  group_by(ACTIVE_QTR_COUNT,res_span) %>%
  summarise(Members=n_distinct(MEMBER_ID),TOT_RESERVATIONS=sum(TOT_RES),TOT_REVENUE=sum(TOT_REVENUE))%>%
  arrange(res_span,ACTIVE_QTR_COUNT)

#Reservations, Revenue, Length and Distance by Channel by Quarter
mx.channel<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  replace(., is.na(.),0) %>%
  group_by(RES_YQ) %>%
  summarise(TOT_RES=sum(TOT_RES)
            ,WEB_RES=sum(WEB_RES)
            ,APP_RES=sum(APP_RES)
            ,TOT_BOOK_TO_DRIVE_HRS=sum(TOT_BOOK_TO_DRIVE_HRS)
            ,WEB_BOOK_TO_DRIVE_HRS=sum(WEB_BOOK_TO_DRIVE_HRS)
            ,APP_BOOK_TO_DRIVE_HRS=sum(APP_BOOK_TO_DRIVE_HRS)
            ,TOT_DISTANCE_DRIVEN=sum(TOT_DISTANCE_DRIVEN)
            ,WEB_DISTANCE_DRIVEN=sum(WEB_DISTANCE_DRIVEN)
            ,APP_DISTANCE_DRIVEN=sum(APP_DISTANCE_DRIVEN)
            ,TOT_HOURS_RENTED=sum(TOT_HOURS_RENTED)
            ,WEB_HOURS_RENTED=sum(WEB_HOURS_RENTED)
            ,APP_HOURS_RENTED=sum(APP_HOURS_RENTED)
            ,TOT_REVENUE=sum(TOT_REVENUE)
            ,WEB_REV=sum(WEB_REV)
            ,APP_REV=sum(APP_REV))%>%
  mutate(TOT_time_to_res = TOT_BOOK_TO_DRIVE_HRS/TOT_RES
         ,WEB_time_to_res = WEB_BOOK_TO_DRIVE_HRS/WEB_RES
         ,APP_time_to_res = APP_BOOK_TO_DRIVE_HRS/APP_RES
         ,TOT_avg_len = TOT_HOURS_RENTED/TOT_RES
         ,WEB_avg_len = WEB_HOURS_RENTED/WEB_RES
         ,APP_avg_len = APP_HOURS_RENTED/APP_RES
         ,TOT_avg_dist = TOT_DISTANCE_DRIVEN/TOT_RES
         ,WEB_avg_dist = WEB_DISTANCE_DRIVEN/WEB_RES
         ,APP_avg_dist = APP_DISTANCE_DRIVEN/APP_RES
         ,TOT_avg_rev = TOT_REVENUE/TOT_RES
         ,WEB_avg_rev = WEB_REV/WEB_RES
         ,APP_avg_rev = APP_REV/APP_RES) %>%
  group_by(RES_YQ) %>%
  summarise(TOT_RES
            ,WEB_RES
            ,APP_RES
            ,TOT_time_to_res=round(TOT_time_to_res,0)            
            ,WEB_time_to_res=round(WEB_time_to_res,0)
            ,APP_time_to_res=round(APP_time_to_res,0)
            ,TOT_avg_len=round(TOT_avg_len,0)
            ,WEB_avg_len=round(WEB_avg_len,0)
            ,APP_avg_len=round(APP_avg_len,0)
            ,TOT_avg_dist=round(TOT_avg_dist,0)
            ,WEB_avg_dist=round(WEB_avg_dist,0)
            ,APP_avg_dist=round(APP_avg_dist,0)
            ,TOT_avg_rev=round(TOT_avg_rev,0)
            ,WEB_avg_rev=round(WEB_avg_rev,0)
            ,APP_avg_rev=round(APP_avg_rev,0)
  ) %>%
  replace(., is.na(.),0) %>%
  arrange(RES_YQ)

#Reservations, Revenue, Length and Distance by Channel by Quarter by Segment
mx.segment<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  replace(., is.na(.),0) %>%
  group_by(RES_YQ, USER_TYPE) %>%
  summarise(TOT_RES=sum(TOT_RES)
            ,WEB_RES=sum(WEB_RES)
            ,APP_RES=sum(APP_RES)
            ,TOT_BOOK_TO_DRIVE_HRS=sum(TOT_BOOK_TO_DRIVE_HRS)
            ,WEB_BOOK_TO_DRIVE_HRS=sum(WEB_BOOK_TO_DRIVE_HRS)
            ,APP_BOOK_TO_DRIVE_HRS=sum(APP_BOOK_TO_DRIVE_HRS)
            ,TOT_DISTANCE_DRIVEN=sum(TOT_DISTANCE_DRIVEN)
            ,WEB_DISTANCE_DRIVEN=sum(WEB_DISTANCE_DRIVEN)
            ,APP_DISTANCE_DRIVEN=sum(APP_DISTANCE_DRIVEN)
            ,TOT_HOURS_RENTED=sum(TOT_HOURS_RENTED)
            ,WEB_HOURS_RENTED=sum(WEB_HOURS_RENTED)
            ,APP_HOURS_RENTED=sum(APP_HOURS_RENTED)
            ,TOT_REVENUE=sum(TOT_REVENUE)
            ,WEB_REV=sum(WEB_REV)
            ,APP_REV=sum(APP_REV))%>%
  mutate(TOT_time_to_res = TOT_BOOK_TO_DRIVE_HRS/TOT_RES
         ,WEB_time_to_res = WEB_BOOK_TO_DRIVE_HRS/WEB_RES
         ,APP_time_to_res = APP_BOOK_TO_DRIVE_HRS/APP_RES
         ,TOT_avg_len = TOT_HOURS_RENTED/TOT_RES
         ,WEB_avg_len = WEB_HOURS_RENTED/WEB_RES
         ,APP_avg_len = APP_HOURS_RENTED/APP_RES
         ,TOT_avg_dist = TOT_DISTANCE_DRIVEN/TOT_RES
         ,WEB_avg_dist = WEB_DISTANCE_DRIVEN/WEB_RES
         ,APP_avg_dist = APP_DISTANCE_DRIVEN/APP_RES
         ,TOT_avg_rev = TOT_REVENUE/TOT_RES
         ,WEB_avg_rev = WEB_REV/WEB_RES
         ,APP_avg_rev = APP_REV/APP_RES) %>%
  group_by(RES_YQ, USER_TYPE) %>%
  summarise(TOT_RES
            ,WEB_RES
            ,APP_RES
            ,TOT_time_to_res=round(TOT_time_to_res,0)            
            ,WEB_time_to_res=round(WEB_time_to_res,0)
            ,APP_time_to_res=round(APP_time_to_res,0)
            ,TOT_avg_len=round(TOT_avg_len,0)
            ,WEB_avg_len=round(WEB_avg_len,0)
            ,APP_avg_len=round(APP_avg_len,0)
            ,TOT_avg_dist=round(TOT_avg_dist,0)
            ,WEB_avg_dist=round(WEB_avg_dist,0)
            ,APP_avg_dist=round(APP_avg_dist,0)
            ,TOT_avg_rev=round(TOT_avg_rev,0)
            ,WEB_avg_rev=round(WEB_avg_rev,0)
            ,APP_avg_rev=round(APP_avg_rev,0)
  ) %>%
  replace(., is.na(.),0) %>%
  arrange(RES_YQ, USER_TYPE)


t2.oth<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep=""),
         OTH_RES = TOT_RES-APP_RES-WEB_RES,
         OTH_REV = TOT_REVENUE-APP_REV-WEB_REV) %>%
  group_by(RES_YQ) %>%
  summarise(TOT_RES = sum(OTH_RES)
            ,TOT_REV = sum(OTH_REV)) %>%
  mutate(USER_TYPE="Other")

t2.app<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(RES_YQ) %>%
  summarise(TOT_RES = sum(APP_RES)
            ,TOT_REV = sum(APP_REV)) %>%
  mutate(USER_TYPE="APP")

t2.web<-rental_history %>%
  mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep="")) %>%
  group_by(RES_YQ) %>%
  summarise(TOT_RES = sum(WEB_RES)
            ,TOT_REV = sum(WEB_REV)) %>%
  mutate(USER_TYPE="WEB")

t2<-rbind(t2.web,t2.app)
t2<-rbind(t2,t2.oth)%>%
  inner_join(.,yq_counter,by="RES_YQ")




#levels(rental_history$FIRST_RES_TYPE)
#levels(rental_history$FIRST_RES_TYPE) <- c("Other","App","Other","App","Other","Web","Web","Other","Other","Other")

um.MT.2$row <- 1:nrow(um.MT.2)
t<-um.MT.2 %>%
  unite(YR_TYPE,RES_YEAR,USER_TYPE) %>%
  spread(MEMBER_ID,YR_TYPE)

df <- data.frame(month=rep(1:3,2),
                 student=rep(c("Amy", "Bob"), each=3),
                 A=c(9, 7, 6, 8, 6, 9),
                 B=c(6, 7, 8, 5, 6, 7))

t<-um.MT.2 %>% 
  gather(variable, value, -(MEMBER_ID:USER_TYPE)) %>%
  unite(temp, USER_TYPE, variable) %>%
  spread(temp, value)


#Monthly Revenue by First Res cohort
um.FR.rev<-rental.history.all %>%
  filter(grepl('2015-07|2015-08|2015-09|2015-10|2015-11|2015-12',FIRST_RES_COHORT)) %>%
  group_by(FIRST_RES_COHORT,RES_MONTH) %>%
  summarise(n_distinct(MEMBER_ID), sum(TOT_REVENUE)) 
