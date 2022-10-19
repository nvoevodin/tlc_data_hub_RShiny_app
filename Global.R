

# ---- Overview ----

# This file contains all the imports (libs and data) + all the DB connection, tutorial step and intro popup functions. 

# ---- Library ----
library(shiny)
library(shinydashboard)
library(lubridate)
library(data.table)
library(dplyr)
library(openxlsx)
library(tidyr)
library(fst)
library(leaflet)
library(stringr)
library(rgdal)
library(leaflet.extras)
library(stringi)
library(rgeos)
library(echarts4r)
library(shinydashboardPlus)
library(shinycssloaders)
library(DT)
library(zoo)
library(scales)
library(shinyWidgets)
library(tibble)
library(Hmisc)
library(plotly) 
library(shinyalert)
library(shinyjs)
library(rintrojs)
library(flexdashboard)
library(arrow)
library(raster)
 

# ---- Options ----

options(scipen = 999)
options(shiny.host = '0.0.0.0')
#options(shiny.host = '10.151.36.87')
# options(shiny.port = 8889)




# ---- Data Imports ----

get_financial_data <- function(fare_input, fare_melt_input) {
  returns <- list()
  
  returns[[1]] <- fread(fare_input)[, -1]
  fare_melt <- fread(fare_melt_input)[, -1]
  fare_melt$year_month <- lubridate::ymd(fare_melt$year_month)
  fare_melt <-
    fare_melt %>% dplyr::arrange(year_month, desc(value))
  fare_melt$value <- round(fare_melt$value, 2)
  returns[[2]] <- fare_melt
  
  return(returns)
  
}



for(i in c('yellow', 'green', 'hvfhv')) {
  data <-
    get_financial_data(paste0('data/', i, '_fare.csv'),
                       paste0('data/', i, '_fare_melt.csv'))
  
  assign(paste0(i, '_fare'), data[[1]])
  assign(paste0(i, '_fare_melt'), data[[2]])
  
}

# ---- Legacy Code for Data Imports ----

# yellow <- get_financial_data('data/yellow_fare.csv','data/yellow_fare_melt.csv')
# 
# yellow_fare <- yellow[[1]]
# yellow_fare_melt <- yellow[[2]]
# 
# yellow <- get_financial_data('data/yellow_fare.csv','data/yellow_fare_melt.csv')
# 
# yellow_fare <- yellow[[1]]
# yellow_fare_melt <- yellow[[2]]
# 
# yellow <- get_financial_data('data/yellow_fare.csv','data/yellow_fare_melt.csv')
# 
# yellow_fare <- yellow[[1]]
# yellow_fare_melt <- yellow[[2]]
# 
# 
# yellow_fare <- fread('data/yellow_fare.csv')[,-1]
# yellow_fare_melt <- fread('data/yellow_fare_melt.csv')[,-1]
# yellow_fare_melt$year_month <- lubridate::ymd(yellow_fare_melt$year_month)
# yellow_fare_melt <- yellow_fare_melt %>% dplyr::arrange(year_month, desc(value)) 
# yellow_fare_melt$value <- round(yellow_fare_melt$value,2)
# 
# green_fare <- fread('data/green_fare.csv')[,-1]
# green_fare_melt <- fread('data/green_fare_melt.csv')[,-1]
# green_fare_melt$year_month <- lubridate::ymd(green_fare_melt$year_month)
# green_fare_melt <- green_fare_melt %>% dplyr::arrange(year_month, desc(value))
# green_fare_melt$value <- round(green_fare_melt$value,2)
# 
# hvfhv_fare <- fread('data/hvfhv_fare.csv')[,-1]
# hvfhv_fare_melt <- fread('data/hvfhv_fare_melt.csv')[,-1]
# hvfhv_fare_melt$year_month <- lubridate::ymd(hvfhv_fare_melt$year_month)
# hvfhv_fare_melt <- hvfhv_fare_melt %>% dplyr::arrange(year_month, desc(value))
# hvfhv_fare_melt$value <- round(hvfhv_fare_melt$value,2)
  
  

# mydb <- dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = "nikita99",
#   host = "nikitatest.ctxqlb5xlcju.us-east-2.rds.amazonaws.com",
#   username = "fallenangel1",
#   password = "nvoevodin"
# )
# onStop(function() {
#   poolClose(mydb)
# })



# ---- Data Imports (Shapefiles and data dictionaries) ----

last_month <- floor_date(Sys.Date()
                         %m-% months(2)
                         , 'months') 
  

shape <- shapefile('data/taxi_zones.shp')
dd <- shape@data
centr <- gCentroid(shape, byid = TRUE)
centr <- as.data.table(centr@coords)
shape@data <- bind_cols(shape@data, centr)

dict <- read.xlsx('https://github.com/analytics-tlc/datahub/blob/main/data/Dict.xlsx?raw=true')
shape@data$OBJECTID <- as.character(shape@data$OBJECTID)
colnames(shape@data)[1] <- "id"


id <- fread('data/idall.csv')
lookup <- read.fst('data/lookup.fst')[,c(1,3)]
lookup$LocationID <-as.character(lookup$LocationID)
colnames(lookup)[1] <- 'id'



# ---- Data Imports (code for parquet imports) ----

myfile <- tempfile()
download.file(
  'https://github.com/analytics-tlc/datahub/blob/main/data/trips_updated.parquet?raw=true',
  myfile,
  mode = "wb"
)
data1 <- read_parquet(myfile)


# ---- Data Imports (industry indicators and main trip counts) ----

#data1 <- read_parquet('data/trips_updated.parquet')
data1$id <- as.character(data1$id)
data1 <-
  data1 %>% dplyr::select(year_month, id, company, Z, count, Zone, metric)
data1$year_month <- ymd(data1$year_month)


myfile <- tempfile()
download.file(
  'https://github.com/analytics-tlc/datahub/blob/main/data/ind_metr_updated.fst?raw=true',
  myfile,
  mode = "wb"
)



industry_metrics <- read_fst(myfile)

exindustry_metrics <- read_fst(myfile)

extra_metrics = fread('data/extra_metrics.csv')


fhvs <- fread('https://raw.githubusercontent.com/analytics-tlc/datahub/main/data/fhv3.csv') %>% dplyr::select(year_month,id,count,industry) %>% dplyr::filter(year_month <= last_month                                                                                                                                     



# ---- Clear Cache ----

gc()



# ---- Palette for future use ----

pal = c("black", "maroon", "blue", "orange", "dark green", "gold")



# ---- UI dropdown choices ----


choices = c('NYC (all boroughs and airports)'='Citywide',
            'Manhattan – Congestion Zone'='Congestion_Zone',
            'Manhattan – Core'='Core', 
            'Manhattan – Central Business District (CBD)'='CBD', 
            'Manhattan – Midtown'='Midtown', 
            'Manhattan – Sub Midtown'='Sub_Midtown',
            'NYC Excluding Manhattan Core'='Non_Core',
            'NYC Airports (JFK, LGA and EWR)'='Airports')

choices1 = c('Yellow Taxi'='YELLOW',
             'Green Taxi'='GREEN',
             'For-Hire Vehicles (excluding HVFHS)'='FHV', 
             'High Volume For Hire Services'='HVFHV', 
             'Shared'='SHARED', 
             'All Vehicles'='ALL'
)


choices2 = c(
  "Trips per Day" = "trips_per_day",
  "Trips per Day Shared" = "trips_per_day_shared",
  "Farebox per Day" = "farebox_per_day",
  "Unique Drivers" = "unique_drivers",
  "Unique Vehicles" = "unique_vehicles",
  "Averageg Minutes per Trip" = "avg_minutes_per_trip",
  "Average Days Vehicles on Road" = "avg_days_vehicles_on_road", 
  "Average Hours per Day per Vehicle" = "avg_hours_per_day_per_vehicle",
  "Average Days Drivers on Road" = "avg_days_drivers_on_road",
  "Average Hours per Day per Driver" = "avg_hours_per_day_per_driver", 
  "Percent of Trips Paid with Credit Card" = "percent_of_trips_paid_with_credit_card")


# ---- Modal Popups to be imported to the server ----

feedback_accepted <- function(){showModal(modalDialog(
  
  title = "Thank You! Your Feedback Is Accepted!",
  ""
))}

welcome <- function(){shinyalert("Welcome To
              TLC Data Hub!
              (Beta)", paste0("TLC Data Hub offers users a convenient location to access and visualize taxi and for hire vehicle industry data.  TLC Data Hub uses public data available on Open Data and the TLC website and does not use, track or display any private information of the drivers or companies. The Hub currently consists of three dashboards. The 'Trip Data' dashboard allows the public to run queries on TLC-collected trip data; the 'Industry Metrics' dashboard provides standard visualizations of monthly industry trends; and the 'Fare & Earnings' dashboard allows you to explore detailed information on passenger fares, driver pay, and industry revenue. For your convenience, there are 'Help' buttons throughout the Data Hub (shown as 'Eye' icons) that include helpful information and data explanations.


                         ","Give us your feedback at:
                              
                  ",
                              "research@tlc.nyc.gov
                              
                              ",
                              "*****"), imageUrl = 'http://tlc-mag.com/images/Oct15/TLCNYC_LOGO.jpg'
           ,
           callbackR = function() {
             # Legecy CODE#############################
             #startTime <- Sys.time()
               # dbExecute(mydb,paste("INSERT INTO monitor_visits
               #                                      (count, time)
               #                                      VALUES('1','",Sys.time(),"');", sep=""))
             #############################################
             showModal(modalDialog(
               title = "Tutorial",
               'Would you like a walkthrough?',
               footer = tagList(
                 modalButton("No"),
                 actionButton(inputId = 'btnYes',label = "Yes")
               )
             ))
             
            
           }
)
}

# ---- Tutorial steps for interactive tutorial ----

steps <- c("This is the control panel. Here you can switch between the available dashboards, choose metrics, and download the reports",
           "Choose your area of interest here. Click the eye icon to see the options and definitions.",
           "Choose your metric. Click the eye icon to see the options and definitions.",
           "Choose the industry. Click the eye icon to see the options and definitions.",
           "Choose the year and month that you are interested in. Click the eye icon to see the available range.",
           "Here you will be able to generate and download an html report based on the selections that you have made so far.",
           "This is the main view. Here you can see your selections being reflected on the map. The map is fully interactive. You can zoom in and out and click on different sections to
           get familiar with the data gegraphically",
           "This is the main stats box. Here you can see the numbers reflecting the selections that you have made in the controls section. You can also close this box if you want",
           "Switch between monthly and daily averages.",
           "This value box shows the total number for the selected metric,zone,industry, and period.",
           "This value box shows the ratio of the picked metric,zone,industry, and priod to the total for the picked metric, industry, and priod.",
           "Here you can select the number of months to go back to to compare the period that you selected in the controls section. Example: selecting May 2019 in the controls section and 12 months here will give you the difference between May 2019 and May 2018.",
           "Here you can see the total difference between selected periods.",
           "Here you can see the percent difference between selected periods.",
           "Here you will be able to see and analyze the same data in the form of trends and charts.",
           "Switch between Taxi Zone and Servise Zone (Servise Zones are larger areas like the Congestion Zone or Sub Midtown, Taxi Zones are smaller areas like Union Square or Flatiron.",
           "This is a fully interactive chart. You can zoom in and out to see the particular periods. You can also deselect the metrics on top of the chart if you want.",
           "If you open this box, you will see the raw data used to build the dashboard in the table format.",
           "If you open this box, you will see the secondary stats box that mimics the main one on top but only for the taxi zones that you selected in the trends section.",
           "Please, leave your feedback here.")




# ---- Modal Popups to be imported to the server ----

tuto1<- function(){showModal(modalDialog(
  title = "Tutorial",
  '',
  HTML(paste0("*** TLC Data Hub uses public data available 
                    on Open Data and the TLC website and does not use, track or display any private information 
                    of the drivers or companies. 
                    
                                ")),br(),br(),
  HTML(paste0(
    "TLC Data Hub offers users a convenient location to access and visualize taxi and for hire vehicle industry data.  TLC Data Hub uses public data available on Open Data and the TLC website and does not use, track or display any private information of the drivers or companies. The Hub currently consists of three dashboards. The 'Trip Data' dashboard allows the public to run queries on TLC-collected trip data; the 'Industry Metrics' dashboard provides standard visualizations of monthly industry trends; and the 'Fare & Earnings' dashboard allows you to explore detailed industry fare breakdowns, average and total industry revenues. For your convenience, there are 'Help' buttons throughout the Data Hub (shown as 'Eye' icons) that include helpful information and data explanations.",br(),br(),
    
    
    "The 'Trip Data' dashboard displays and compares the pickup, dropoff, trip, trip fare, trip time, and trip distance data across different locations, industries and periods.",br(),br(),
    "Left Bar - follow the steps to select: zone, metric, industry, period. Below, you can download a report based on these selections.",br(),br(),
    "Main View - all the selections that you've made in the control panel will be represented on the map and info box. The info box allows you to switch between the monthly
                  and daily levels of data.",br(),br(),
    "Trends - the section below the map allows you to monitor how the industry that you selected behaved in the selected zone over time. Additionally,
                  you can select a taxi zone to see a trend in a smaller area of interest.",br(),br(),
    "Secondary Stats - In the bottom left corner of the dashboard you will see the table with raw data based on the zone that you selected. In the bottom right
                  you will see a similar info box that you saw in the main view. That info box displays the same information but on the taxi zone (small area) level.
                  For the more detailed tutorial, please watch the video below:",br(),br()
    
  )),br(),
  HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/PLM3snDhxuE?rel=0&amp;showinfo=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
  
))}


tuto2 <- function(){showModal(modalDialog(
  title = "Tutorial",
  '',
  HTML(paste0("*** TLC Data Hub uses public data available 
                    on Open Data and the TLC website and does not use, track or display any private information 
                    of the drivers or companies. 
                    
                                ")),br(),br(),
  HTML(paste0(
    
    "TLC Data Hub offers users a convenient location to access and visualize taxi and for hire vehicle industry data.  TLC Data Hub uses public data available on Open Data and the TLC website and does not use, track or display any private information of the drivers or companies. The Hub currently consists of three dashboards. The 'Trip Data' dashboard allows the public to run queries on TLC-collected trip data; the 'Industry Metrics' dashboard provides standard visualizations of monthly industry trends; and the 'Fare & Earnings' dashboard allows you to explore detailed information on passenger fares, driver pay, and industry revenue. For your convenience, there are 'Help' buttons throughout the Data Hub (shown as 'Eye' icons) that include helpful information and data explanations.",br(),br(),
    
                         
    
    "The industry metrics dashboard visualizes our monthly industry indicators published on ", "<a href='https://www1.nyc.gov/site/tlc/about/aggregated-reports.page' target='_blank'>","OUR WEBSITE</a>",". 
                              These indicators cover a wide variety of topics from trips per day to the number of unique vehicles by industry sector.",br(),br(),br(),
    "Left Bar - As with the previous dashboard you can use the controls section to filter for specific date ranges and choose your metric of interest.",br(),br(),
    "Main View - Note that as you make these selections, the main view trend lines and value boxes change to reflect those selections.",br(),br(),
    "Below Main View - On the bottom part of the main view you will see four graphs which display data on aggregated statistics. Switch between the two main themes to see different visualizations." ,br(),br(),
    "Finally, you can access a flash report by clicking the download page button. It summarizes some of your selections into a portable and interactive report." ,br(),br(),
    "For the more detailed tutorial, please watch the video below:",br(),br()
    
  )),br(),
  HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/EeScaX_etu4?rel=0&amp;showinfo=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
  
))}

tuto3 <- function(){showModal(modalDialog(
  title = "Tutorial",
  '',
  HTML(paste0("*** TLC Data Hub uses public data available 
                    on Open Data and the TLC website and does not use, track or display any private information 
                    of the drivers or companies. 
                    
                                ")),br(),br(),
  
  HTML(paste0(
    
    "TLC Data Hub offers users a convenient location to access and visualize taxi and for hire vehicle industry data.  TLC Data Hub uses public data available on Open Data and the TLC website and does not use, track or display any private information of the drivers or companies. The Hub currently consists of three dashboards. The 'Trip Data' dashboard allows the public to run queries on TLC-collected trip data; the 'Industry Metrics' dashboard provides standard visualizations of monthly industry trends; and the 'Fare & Earnings' dashboard allows you to explore detailed information on passenger fares, driver pay, and industry revenue. For your convenience, there are 'Help' buttons throughout the Data Hub (shown as 'Eye' icons) that include helpful information and data explanations.",br(),br(),
    

    br(),br(),
    
    
    "The databank section allows you to scroll through, download, and get more familiar with the raw data used to create the dashboards.",br(),br(),br(),
    "Main View - Switch between 2 datasets used for the dashboards (trip data and monthly_metrics).",br(),br(),
    "Download - Each tab has a separate download button associated with it. Click it to download a CSV file with raw data.",br(),br(),
    "Data Dictionary - Click on 'more' to access the data dictionary.",br()
    
    
  ))
  
  
))}



tuto4 <- function(){showModal(modalDialog(
  title = "Tutorial",
  '',
  HTML(paste0("*** TLC Data Hub uses public data available 
                    on Open Data and the TLC website and does not use, track or display any private information 
                    of the drivers or companies. 
                    
                                ")),br(),br(),
  
  HTML(paste0(
    
    "TLC Data Hub offers users a convenient location to access and visualize taxi and for hire vehicle industry data. The Hub currently consists of three dashboards. The 'Trip Data' dashboard allows the public to run queries on TLC-collected trip data; the 'Industry Metrics' dashboard provides standard visualizations of monthly industry trends; and the 'Fare & Earnings' dashboard allows you to explore detailed information on passenger fares, driver pay, and industry revenue. For your convenience, there are 'Help' buttons throughout the Data Hub (shown as 'Eye' icons) that include helpful information and data explanations.",br(),br(),
    

    br(),br(),
    
    
    "In this dashboard, we cover three metrics:",br(),br(),br(),
    "Fare Breakdown Over Time - displays average or total fares breakdown over time. You can deselect parts of the fare to explore a particular section.",br(),br(),
    "Fare Breakdown Box - displays average and total fare breakdowns for a selected year/month and industry.",br(),br(),
    "Fare Distribution Piechart - Displays average or total fare ratios for a selected year/month and industry.",br(),br(),br(),
    "*NOTE: For data integrity purposes, we eliminate certain outliers in our data before presenting it in this dashboard. The trip thresholds that we used for our calculations are: trip mileage must be more than 0 miles and less than 100 miles, base passenger fare must be more than $0 and less than $250.",br()
    
    
  ))
  
  
))}


# ---- Modal Popups prompting Rmd reports ----

download1 <- function(){showModal(modalDialog(
  title = "Summary Report",
  "This will download a summary report of the current page with the selections that you made in the control bar. The format of the file is 'HTML'. THe size is approx. 8mb.",
  downloadButton("btnn", "Download")
))}


download2 <- function(){showModal(modalDialog(
  title = "Summary Report",
  "This will download a summary report of the current page with the selections that you made in the control bar. The format of the file is 'HTML'. THe size is approx. 8mb.",
  downloadButton("im_btnn", "Download")
))}







