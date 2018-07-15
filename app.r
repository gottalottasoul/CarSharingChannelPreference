##############################################################################################
###   Zipcar Member Reservation by platform (app vs web)
###   2011-2015
###
###   built on: 12/10/15
###
##############################################################################################



#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\rental platform trends\\")


#load the required libraries
#require(plyr) || install.packages("plyr", repos="http://cran.rstudio.org") 
#library(plyr)

require(dplyr) || install.packages("dplyr", repos="http://cran.rstudio.org") 
library(dplyr)

#require(tidyr) || install.packages("tidyr", repos="http://cran.rstudio.org") 
#library(tidyr)

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
### script in rfm-explorer needs to be run to generate the dataset loaded below
##############################################################################################



load("C:\\Users\\babbenante\\documents\\my stuff\\data\\Member Reservation Source Profile\\Member_Rental_Pattern.RData")




#########################################################


#load("data\\Member_Rental_Data_Shaped.RData")
#load("Member_Rental_Data_Shaped.RData")
options(RCHART_LIB='polycharts')
options(RCHART_WIDTH=900)
options(RCHART_HEIGHT=500)


ui<- fluidPage(
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    ")),
    tags$script(type="text/javascript", src = "http://d3js.org/d3.v3.min.js", charset="utf-8"),
    tags$script(type="text/javascript", src = "nv.d3.min.js", charset="utf-8"),
    tags$link(rel="stylesheet", type="text/css", href = "nv.d3.css")
    
    #    tags$head(tags$style("
    #                         #my_plot{width:45%;}"
    #                         ),
    #              tags$style("
    #                         #my_other_plot{width:25%;}"
    #                         )
    #              )
    ),
  fluidRow(
    column(width=2, 
           radioButtons('pick_data', 'How to view the data:', c('User','Reservation'),
                        selected = c('User')),
           br(),
          radioButtons('show_vars', 'What to Chart:', c("RES"="TOT_RES","REV"="TOT_REV"),
                        selected = c('TOT_RES'))    
    ),
    column(width=8
           ,showOutput("my_plot",lib="polycharts")
    )
  ),
  fluidRow(
    column(width=2,
           textOutput("text1")
    ),
    
    column(width=8,
           dataTableOutput("my_table")
    )
  )
    )


server<- (function(input, output) {

    my.data<-reactive({
      if (input$pick_data=="Reservation")
      {
        
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
      } else {
        rental_history %>%
          mutate(RES_YQ = paste(RES_YEAR,"-",RES_QUARTER,sep=""),
                 OTH_RES = TOT_RES-APP_RES-WEB_RES,
                 OTH_REV = TOT_REVENUE-APP_REV-WEB_REV) %>%
          group_by(RES_YQ,USER_TYPE) %>%
          summarise(TOT_RES = sum(TOT_RES)
                    ,APP_RES = sum(APP_RES)
                    ,WEB_RES = sum(WEB_RES)
                    ,OTH_RES = sum(OTH_RES)
                    ,TOT_REV = sum(TOT_REVENUE)
                    ,APP_REV = sum(APP_REV)
                    ,WEB_REV = sum(WEB_REV)
                    ,OTH_REV = sum(OTH_REV)) %>%
          data.table(.)%>%
          inner_join(.,yq_counter,by="RES_YQ") %>%
          #add a bunch of rows because the app didnt exist until the end of 2011
          rbind(.,data.table(RES_YQ="2011-1",USER_TYPE="Both",TOT_RES=0,APP_RES=0,WEB_RES=0,OTH_RES=0,TOT_REV=0,APP_REV=0,WEB_REV=0,OTH_REV=0,YQ_Count=1)) %>%
          rbind(.,data.table(RES_YQ="2011-1",USER_TYPE="App",TOT_RES=0,APP_RES=0,WEB_RES=0,OTH_RES=0,TOT_REV=0,APP_REV=0,WEB_REV=0,OTH_REV=0,YQ_Count=1)) %>%
          rbind(.,data.table(RES_YQ="2011-2",USER_TYPE="Both",TOT_RES=0,APP_RES=0,WEB_RES=0,OTH_RES=0,TOT_REV=0,APP_REV=0,WEB_REV=0,OTH_REV=0,YQ_Count=2)) %>%
          rbind(.,data.table(RES_YQ="2011-2",USER_TYPE="App",TOT_RES=0,APP_RES=0,WEB_RES=0,OTH_RES=0,TOT_REV=0,APP_REV=0,WEB_REV=0,OTH_REV=0,YQ_Count=2)) %>%
          rbind(.,data.table(RES_YQ="2011-3",USER_TYPE="Both",TOT_RES=0,APP_RES=0,WEB_RES=0,OTH_RES=0,TOT_REV=0,APP_REV=0,WEB_REV=0,OTH_REV=0,YQ_Count=3)) %>%
          rbind(.,data.table(RES_YQ="2011-3",USER_TYPE="App",TOT_RES=0,APP_RES=0,WEB_RES=0,OTH_RES=0,TOT_REV=0,APP_REV=0,WEB_REV=0,OTH_REV=0,YQ_Count=3)) %>%
          arrange(RES_YQ,USER_TYPE)
          
      }
    })
    

  
  output$my_table = renderDataTable({
    #since we're using the same function for both the graph and table
    #we have to re-summarize here to format the numbers nice-like
    my.data()%>%
      group_by(RES_YQ,USER_TYPE) 
#    %>%
#      summarise(TOT_RES = prettyNum(sum(TOT_RES),big.mark=",",decimal.mark=".")
#                ,APP_RES = prettyNum(sum(APP_RES),big.mark=",",decimal.mark=".")
#                ,WEB_RES = prettyNum(sum(WEB_RES),big.mark=",",decimal.mark=".")
#                ,OTH_RES = prettyNum(sum(OTH_RES),big.mark=",",decimal.mark=".")
#                ,TOT_REV = prettyNum(sum(TOT_REV),big.mark=",",decimal.mark=".")
#                ,APP_REV = prettyNum(sum(APP_REV),big.mark=",",decimal.mark=".")
#                ,WEB_REV = prettyNum(sum(WEB_REV),big.mark=",",decimal.mark=".")
#                ,OTH_REV = prettyNum(sum(OTH_REV),big.mark=",",decimal.mark="."))
  },options = list(lengthMenu = c(4, 8, 20), pageLength = 4))
  
 
  output$my_plot = renderChart2({
    p1<-nPlot(x = "YQ_Count"
              ,y = input$show_vars
              ,group="USER_TYPE"
              ,data = my.data()
#                filter(grepl('2012|2013|2014|2015',RES_YQ))
              ,type = "stackedAreaChart")
    p1$yAxis(axisLabel = 'Quarterly Reservations')
    p1$xAxis(axisLabel = 'YQ')
    p1$set(width=900,height=600)
    return(p1)
  })
  

  
  output$text1<-renderText({paste("You have selected:",input$pick_data,sep="")})
  

})

shinyApp(ui=ui,server=server)

