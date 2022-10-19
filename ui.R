

# ---- TLC DATAHUB UI CODE ----

shinyUI(
    # Dashboard Page Setup ----------------------------------------------------
    dashboardPage(
        skin = "blue-light",
        dashboardHeader(title = "TLC DATA HUB", titleWidth = 330),
# Dashboard SideBar Setup ----------------------------------------------------
        
        dashboardSidebar(
            width = 330,
            
# Introboxes are wrappers for the interactive tutorial
            introBox(
                sidebarMenu(
                    id = 'side',
                    br(),
                    menuItem(
                        "Menu",
                        tabName = "dashboards"
                        ,
                        icon = icon("dashboard")
                        ,
                        #Main dashboard
                        menuSubItem("Trip Data", tabName = "m1"),
                        #Industry indicators dashboard
                        menuSubItem("Industry Metrics", tabName = "fl"),
                        #Fare dashboard
                        menuSubItem("Fare & Earnings", tabName = "fintab"),
                        #Databank page
                        menuSubItem("Data Bank", tabName = "databank1")
                    ),
                    
# The left sidebar has to dispaly different things depending on the dashboard selected -> conditionalPanel is used for that.
                    shiny::conditionalPanel(
# Left sidebar condition 1 (Trip Dashboard) ----------------------------------------------------
                  
                        condition = "input.side == 'm1'",
# Video Tutorial and General Description prompt ----------------------------------------------------                        
                        actionButton(
                            'start',
                            label = "Info/Tutorial",
                            icon("info-circle"),
                            style = "color: #fff;  border-color: #2e6da4"
                        ),
                        
                        box(
                            solidHeader = T,
                            collapsible = F,
                            collapsed = F,
                            closable = F,
                            title = 'CONTROLS',
                            status = 'primary',
                            width = NULL,
                            introBox(
                                conditionalPanel(
                                    'input.ind != "FHV"',
                                    
# Trip Dashboard - First DropDown ---------------------------------------------------- 
                                    selectInput(
                                        'cbd',
                                        label = 'STEP 1: Select a Service Zone',
                                        choices = c(
                                            'NYC (all boroughs and airports)' = 'Citywide',
                                            'Manhattan – Congestion Zone' = 'Congestion_Zone',
                                            'Manhattan – Core' = 'Core',
                                            'Manhattan – Central Business District (CBD)' = 'CBD',
                                            'Manhattan – Midtown' = 'Midtown',
                                            'Manhattan – Sub Midtown' = 'Sub_Midtown',
                                            'NYC Excluding Manhattan Core' = 'Non_Core',
                                            'NYC Airports (JFK, LGA and EWR)' = 'Airports'
                                        ),
                                        selected = 'Core'
                                    )
                                ),
                                conditionalPanel(
                                    'input.ind == "FHV"',
# Trip Dashboard - First DropDown if FHV ----------------------------------------------------
                                    selectInput(
                                        'cbd',
                                        label = 'STEP 1: Select a Service Zone',
                                        choices = c('NYC (all boroughs and airports)' = 'Citywide'),
                                        selected = 'Citywide'
                                    )
                                )
                                ,
# Trip Dashboard - First Describe Button (Eye) ----------------------------------------------------
                                userPost(
                                    collapsible = T,
                                    collapsed = T,
                                    id = 'help',
                                    image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                    author = NULL,
                                    description = "ZONES INFORMATION -->",
                                    "*NYC (all boroughs and airports) - All 5 boroughs, JFK, LaGuardia, Newark Airport.",
                                    br(),
                                    br(),
                                    "*Manhattan – Congestion Zone - Taxi zones below 96th St in Manhattan (The zone in which NYS charges the Congestion Surcharge).",
                                    br(),
                                    br(),
                                    "*NYC Excluding Manhattan Core - All 5 boroughs (excluding Core zone), JFK, LaGuardia, Newark Airport.",
                                    br(),
                                    br(),
                                    "*Core - Taxi zones below 110th St & 96th St in Manhattan.",
                                    br(),
                                    br(),
                                    "*Manhattan – Central Business District (CBD) - Taxi zones below 59th St in Manhattan.",
                                    br(),
                                    br(),
                                    "*Manhattan – Sub Midtown - 13 Taxi zones in central Manhattan (see map).",
                                    br(),
                                    br(),
                                    "*Manhattan – Midtown - Taxi zones below 66st and above 14th St in Manhattan.",
                                    br(),
                                    br(),
                                    "*NYC Airports (JFK, LGA and EWR) - JFK, LaGuardia, Newark Airport."
                                ),
                                data.step = 2,
                                data.intro = steps[2]
                            ),
                            br(),
                            
                            
                            introBox(
                                conditionalPanel(
                                    'input.ind != "FHV"',
# Trip Dashboard - Second DropDown ----------------------------------------------------                                    
                                    selectInput(
                                        'metric',
                                        label = 'STEP 2: Select a Metric',
                                        choices = c('PickUps', 'DropOffs', 'Trips'),
                                        selected = 'PickUps'
                                    )
                                )
                                ,
# Trip Dashboard - Second DropDown if FHV ----------------------------------------------------
                                conditionalPanel(
                                    'input.ind == "FHV"',
                                    selectInput(
                                        'metric',
                                        label = 'STEP 2: Select a Metric',
                                        choices = c('Trips'),
                                        selected = 'Trips'
                                    )
                                ),
# Trip Dashboard - Second Describe Button (Eye) ----------------------------------------------------
                                userPost(
                                    collapsible = T,
                                    collapsed = T,
                                    id = 'help1',
                                    image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                    author = NULL,
                                    description = "METRIC INFORMATION -->",
                                    "*PickUps - Number of rides that started in the selected zone",
                                    br(),
                                    br(),
                                    "*DropOffs - Number of rides that ended in the selected zone",
                                    br(),
                                    br(),
                                    "*Trips - Number of trips that started in the selected service zone + Number
of trips that started outside of the selected service zone, but ended in it",
                                    br(),
                                    br(),
                                    "*Average Fare - Average fare for trips that started in the selected zone",
                                    br(),
                                    br(),
                                    "*Average Trip Time - Average trip length (in minutes) for trips that started in the selected zone",
                                    br(),
                                    br(),
                                    "*Average Trip Distance - Average trip length (in miles) for trips that started in the selected zone",
                                ),
                                data.step = 3,
                                data.intro = steps[3]
                            ),
                            br(),
                            
                            
                            introBox(
# Trip Dashboard - Third DropDown ----------------------------------------------------
                                selectInput(
                                    inputId = "ind",
                                    label = 'STEP 3: Select an Industry',
                                    choices = c(
                                        'Yellow Taxi' = 'YELLOW',
                                        'Green Taxi' =
                                            'GREEN',
                                        'For-Hire Vehicles (excluding HVFHS)' =
                                            'FHV',
                                        'High Volume For Hire Services' =
                                            'HVFHV',
                                        #'Shared'='SHARED',
                                        'All Vehicles' =
                                            'ALL'
                                    ),
                                    selected = 'YELLOW'
                                ),
# Trip Dashboard - Third Describe Button (Eye) ----------------------------------------------------
                                userPost(
                                    collapsible = T,
                                    collapsed = T,
                                    id = 'help2',
                                    image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                    author = NULL,
                                    description = "INDUSTRY TERMINOLOGY -->",
                                    "*Yellow Taxi - Yellow cabs (Medallions).",
                                    br(),
                                    br(),
                                    "*Green Taxi - Green cabs (Street Hail Liveries).",
                                    br(),
                                    br(),
                                    "*For-Hire Vehicles (excluding HVFHS) - non-high volume FHVs (this includes trips from Livery, Black Car and Luxury Limousine FHV bases).",
                                    br(),
                                    br(),
                                    "*High Volume For Hire Services (HVFHS) - UBER, LYFT, VIA. *JUNO is currently unavailable.",
                                    br(),
                                    br(),
                                    #"*Shared - Trips that have been shared within the HVFHS industry.",br(),br(),
                                    "*All - All industries combined."
                                )
                                ,
                                data.step = 4,
                                data.intro = steps[4]
                            ),
                            br(),
                            
                            
                            introBox(
                                conditionalPanel(
                                    'input.ind != "FHV"',
# Trip Dashboard - Date Slider (server side code) ----------------------------------------------------
                                    uiOutput('slider'),
# Trip Dashboard - Fourth Describe Button (Eye) ----------------------------------------------------
                                    userPost(
                                        collapsible = T,
                                        collapsed = T,
                                        id = 'help3',
                                        image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                        author = NULL,
                                        description = "TIMEFRAME INFORMATION -->",
                                        paste0(
                                            "*Data are available in monthly increments from 2011 to ",
                                            format(ymd(Sys.Date() %m-% months(2)), '%b %Y')
                                        )
                                    )
                                ),
                                data.step = 5,
                                data.intro = steps[5]
                            )
                        ),
                        introBox(
                            box(
                                solidHeader = T,
                                collapsible = F,
                                collapsed = F,
                                closable = F,
                                title = '',
                                status = 'primary',
                                width = NULL,
# Trip Dashboard - Download Rmd report prompt code ----------------------------------------------------                               
                                actionButton(
                                    'report',
                                    label = "Download This Page",
                                    icon("download"),
                                    style =
                                        "color: #fff; border-color: #2e6da4"
                                )
                                
                            ),
                            data.step = 6,
                            data.intro = steps[6]
                        )
                        
                        
                    ),
# Left sidebar condition 2 (Industry Indicators) ----------------------------------------------------                   
                    shiny::conditionalPanel(
   
                        condition = "input.side == 'fl'",
# Video Tutorial and General Description prompt ----------------------------------------------------                        
                        actionButton(
                            'start1',
                            label = "Info/Tutorial",
                            icon("info-circle"),
                            style = "color: #fff;  border-color: #2e6da4"
                        ),
                        
                        box(
                            solidHeader = T,
                            collapsible = F,
                            collapsed = F,
                            closable = F,
                            title = 'CONTROLS',
                            status = 'primary',
# Industry Ind Dashboard - Date Slider ----------------------------------------------------                           
                            sliderInput(
                                "monthdate",
                                "Choose a Date Range",
                                min = ymd('2014-01-01'),
                                max = max(industry_metrics$month_date),
                                value = c(ymd('2015-01-01'), ymd('2018-12-01')),
                                timeFormat = "%b %Y"
                            ),
                            
                            
                            width = NULL,
# Industry Ind Dashboard - First Describe Button (Eye) ----------------------------------------------------
                            userPost(
                                collapsible = T,
                                collapsed = T,
                                id = 'helpf1',
                                image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                author = NULL,
                                description = "RANGE AVAILABLE -->",
                                paste0(
                                    "*Data are available in monthly increments from Jan 2014 to ",
                                    format(ymd(Sys.Date() %m-% months(2)), '%b %Y')
                                    
                                )
                            ),
                            br(),
                            
# Industry Ind Dashboard - Metric Selector Dropdown (2nd input) ----------------------------------------------------                            
                            selectInput(
                                "indMetric1",
                                "Select Your Variable for y-axis",
                                choices = c(
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
                                    "Percent of Trips Paid with Credit Card" = "percent_of_trips_paid_with_credit_card"
                                ),
                                selected = "unique_vehicles"
                            ),
# Industry Ind Dashboard - Second Describe Button (Eye) ----------------------------------------------------
                            userPost(
                                collapsible = T,
                                collapsed = T,
                                id = 'helpf3',
                                image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                author = NULL,
                                description = "METRICS TERMINOLOGY -->",
                                "*Trips Per Day - Average number of trips recorded each day.",
                                br(),
                                br(),
                                "*Farebox Per Day - Total amount, across all vehicles, collected from all fares,
surcharges, taxes, and tolls. Note: this amount does not include
amounts from credit card tips.",
                                br(),
                                br(),
                                "*Unique Drivers - The total unique number of hack drivers who recorded a trip
each month.",
                                br(),
                                br(),
                                "*Unique Vehicles - The total unique number of medallion taxis/SHLS/FHVs and
standby vehicles.",
                                br(),
                                br(),
                                "*Vehicles Per Day - The average unique number of medallion taxis/SHLS/FHVs and
standby vehicles.",
                                br(),
                                br(),
                                "*Avg Days Vehicles on Road - The average number of days each vehicle spent on the road per
month.",
                                br(),
                                br(),
                                "*Avg Hours Per Day Per Vehicle - The average number of hours in which a vehicle recorded a trip.",
                                br(),
                                br(),
                                "*Avg Days Drivers on Road  - The average number of days each driver recorded a trip.",
                                br(),
                                br(),
                                "*Avg Hours Per Day Per Driver - The average number of hours each.",
                                br(),
                                br(),
                                "*Avg Minutes Per Trip - Average trip time from meter‐on to meter‐off.",
                                br(),
                                br(),
                                "*Percent of Trips Paid with Credit Card - Number of trips where passenger paid by credit card out of the
total number of trips.",
                                br(),
                                br(),
                                "*Trips Per Day Shared - Average number of shared trips recorded each day."
                            ),
                            br(),
                            box(
                                solidHeader = T,
                                collapsible = F,
                                collapsed = F,
                                closable = F,
                                title = '',
                                status = 'primary',
                                width = NULL,
                                
# Industry Ind Dashboard - Download Rmd report prompt code ----------------------------------------------------
                                actionButton(
                                    'im_report',
                                    label = "Download This Page",
                                    icon("download"),
                                    style = "color: #fff; border-color: #2e6da4"
                                )
                                
                            )
                            
                        )
                    ),
# Left sidebar condition 3 (Financial Dashboard) ----------------------------------------------------                       
                    shiny::conditionalPanel(
                        condition = "input.side == 'fintab'",
# General Description prompt ----------------------------------------------------
                        actionButton(
                            'start4',
                            label = "Info/Tutorial",
                            icon("info-circle"),
                            style = "color: #fff;  border-color: #2e6da4"
                        ),
                        box(
                            solidHeader = T,
                            collapsible = F,
                            collapsed = F,
                            closable = F,
                            title = 'CONTROLS',
                            status = 'primary',
# Financial Dashboard - Metric Selector Dropdown (1st input) ----------------------------------------------------                            
                            selectInput(
                                "fareMetric0",
                                "STEP 1: Select Your Metric",
                                choices = c('Driver Pay', 'Average Fare', 'Total Fare'),
                                selected = "Driver Pay"
                            ),
# Financial Dashboard - First Describe Button (Eye) ----------------------------------------------------
                            userPost(
                                collapsible = T,
                                collapsed = T,
                                id = 'helpf301',
                                image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                author = NULL,
                                description = "METRICS TERMINOLOGY -->",
                                "*Average Fare - Detailed breakdowns of an average fare for the selected industry.",
                                br(),
                                br(),
                                "*Total Fare - Monthly fare breakdowns by industry.",
                                br(),
                                br(),
                                "*Driver Pay - Breakdown of how much drivers make a month by industry and over time."
                            ),
                            br(),
                            
# Financial Dashboard - Industry Selector Dropdown (2nd input) ----------------------------------------------------
                            selectInput(
                                "fareMetric1",
                                "STEP 2: Select Your Industry",
                                choices = c('Yellow',
                                            #'Green',
                                            'HVFHV'),
                                selected = "Yellow"
                            ),
# Financial Dashboard - Second Describe Button (Eye) ----------------------------------------------------
                            userPost(
                                collapsible = T,
                                collapsed = T,
                                id = 'helpf300',
                                image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                author = NULL,
                                description = "INDUSTRY TERMINOLOGY -->",
                                "*Yellow Taxi - Yellow cabs (Medallions).",
                                br(),
                                br(),
                                #"*Green Taxi - Green cabs (Street Hail Liveries).",br(),br(),
                                
                                "*High Volume For Hire Services (HVFHV) - TLC-licensed FHV bases that dispatch more than 10,000 trips per day. These are companies like UBER and LYFT",
                                br(),
                                br()
                                #,"*Shared - Trips that have been shared within the HVFHS industry.
                                
                            ),
                            br(),
# Financial Dashboard- Date Slider (server side) ----------------------------------------------------                            
                            uiOutput('slider_fin'),
                            width = NULL,
# Financial Dashboard - Third Describe Button (Eye) ----------------------------------------------------
                            userPost(
                                collapsible = T,
                                collapsed = T,
                                id = 'helpf100',
                                image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                author = NULL,
                                description = "RANGE AVAILABLE -->",
                                "*Data are available in monthly increments. The ranges are different for different industries.",
                                br(),
                                br(),
                                paste0("Yellow: 2011 to ", format(ymd(
                                    Sys.Date() %m-% months(2)
                                ), '%b %Y')),
                                br(),
                                br(),
                                #paste0("Green: 2014 to ",format(ymd(Sys.Date()%m-% months(2)),'%b %Y')),br(),br(),
                                paste0("High Volume FHV: Feb 2019 to ", format(ymd(
                                    Sys.Date() %m-% months(2)
                                ), '%b %Y'))
                                
                            ),
                            br()
                            
                        )
                        
                        
                        
                        
                        
                    ),
# Left sidebar condition 4 ----------------------------------------------------   
                    shiny::conditionalPanel(
                        condition = "input.side == 'databank1'",
# General Description prompt ----------------------------------------------------           
                        actionButton(
                            'start2',
                            label = "Info/Tutorial",
                            icon("info-circle"),
                            style =
                                "color: #fff;  border-color: #2e6da4"
                        )
                        
                    ),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    h5(htmlOutput("easterEgg"))
                    
                    
                    
                ),
                data.step = 1,
                data.intro = steps[1]
            )
            
            
            
        ),
# Dashboard Sidebar Setup END ----------------------------------------------------
        
# Dashboard Body Setup ----------------------------------------------------
        dashboardBody(
            useShinyalert(),
            introjsUI(),
# CSS styling from the styles.css file ----------------------------------------------------
            tags$head(includeCSS("styles.css"))
            
            ,
            tabItems(
# Trips Dashboard  ---------------------------------------------------- 
                tabItem(
                    tabName = "m1",
# Traditional FHVs do not provide location data to TLC -> Map can not be used -> need to use conditional Panel                    
# Trips Dashboard Setup for all industries apart from Trad FHV ----------------------------------------------------                    
                    conditionalPanel('input.ind != "FHV"',
# Trips Dashboard Main map and overlap value box setup Start ----------------------------------------------------
                                     fluidRow(
                                         column(width = 12,
                                                introBox(
                                                    box(
                                                        id = 'map_box',
                                                        solidHeader = T,
                                                        collapsible = F,
                                                        collapsed = F,
                                                        closable = F,
                                                        title = 'Industry Trips Map',
# Trips Dashboard Map UI code (main leaflet map code is in the server script) ----------------------------------------------------
                                                        withSpinner(leafletOutput("leaf", height = '1100px')),
                                                        width = NULL
                                                    ),
                                                    data.step = 7,
                                                    data.intro = steps[7]
                                                )),
                                         
                                         
                                         
                                         
# Trips Dashboard Absolute panel is used to display value boxes on top of the map ----------------------------------------------------                                         
                                         absolutePanel(
                                             id = "controls1",
                                             style = "z-index:999;",
                                             class = "panel panel-default",
                                             draggable = F,
                                             top = 170,
                                             right = 25,
                                             width = '35%',
                                             column(width = 1),
                                             column(width = 10,
                                                    introBox(
                                                        box(
                                                            id = 'main_stat_box',
                                                            solidHeader = T,
                                                            collapsible = T,
                                                            collapsed = F,
                                                            closable = F,
                                                            title = "Service Zone Metrics",
                                                            introBox(
# Trips Dashboard Absolute panel - Radio Button to switch between monthly and daily numbers ---------------------------------------------------- 
                                                                selectInput(
                                                                    inputId = "val",
                                                                    'Monthly | Daily Average',
                                                                    choices = c('Monthly', 'Daily Average'),
                                                                    selected = 'Monthly'
                                                                ),
                                                                data.step = 9,
                                                                data.intro = steps[9]
                                                            ),
                                                            
                                                            
                                                            width = NULL,
# Trips Dashboard Absolute panel - Value boxes code Start ----------------------------------------------------                                                             
                                                            #introBox(
                                                            valueBoxOutput('first_main', width = 12)
                                                            #,data.step = 10, data.intro = steps[10])
                                                            ,
                                                            br(),
                                                            #introBox(
                                                            valueBoxOutput('second_main', width = 12)
                                                            #,data.step = 11, data.intro = steps[11])
                                                            ,
                                                            br(),
                                                            introBox(
# Trips Dashboard Absolute panel - DropDown to select the month lag for month-to-month comparisons ----------------------------------------------------
                                                                selectInput(
                                                                    inputId = "lag",
                                                                    label = 'Select a number of months to go back to compare to the selected month.',
                                                                    choices = c(seq(1, 12, by = 1)),
                                                                    selected = 12
                                                                ),
                                                                data.step = 12,
                                                                data.intro = steps[12]
                                                            ),
                                                            #introBox(
                                                            valueBoxOutput('third_main', width = 12)
                                                            #,data.step = 13, data.intro = steps[13])
                                                            ,
                                                            br(),
                                                            #introBox(
                                                            valueBoxOutput('fourth_main', width = 12)
                                                            #,data.step = 14, data.intro = steps[14])
# Trips Dashboard Absolute panel - Value boxes code End ----------------------------------------------------                                                            
                                                        ),
                                                        data.step = 8,
                                                        data.intro = steps[8]
                                                    ))
                                         )
                                         
                                         
                                     )),
# Trips Dashboard Main map and overlap value box setup End ----------------------------------------------------

# Trips Dashboard Trends section Start ---------------------------------------------------- 
                    fluidRow(
                        box(
                            id = 'map_box',
                            title = "Trends",
                            width = 12,
                            solidHeader = T,
                            collapsible = F,
                            collapsed = F,
                            closable = F,
                            introBox(
                                fluidRow(column(
                                    width = 3,
                                    br(),
                                    introBox(
                                        boxPad(
                                            color = "black",
                                            
                                            
# Trips Dashboard Trends section - if FHV - display limited info ----------------------------------------------------                                             
                                            conditionalPanel(
                                                'input.ind == "FHV"',
# Trips Dashboard Trends section - if FHV - first helper info ----------------------------------------------------
                                                userPost(
                                                    collapsible = T,
                                                    collapsed = T,
                                                    id = 'for_fhv',
                                                    image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                                    author = NULL,
                                                    description = "PLOT INFORMATION -->",
                                                    "The plot to the right (-->) renders a citywide month to month trend for the traditional FHV industry.",
                                                    br(),
                                                    br(),
                                                    "***Detailed geographical information is not available at the moment."
                                                ),
                                                br(),
# Trips Dashboard Trends section - if FHV - sub industry dropdown selector ----------------------------------------------------                                                
                                                selectInput(
                                                    inputId = "indFhv",
                                                    label = 'FHV Sub-Industries',
                                                    choices = c(
                                                        'Traditional FHVs Combined' = 'fhv',
                                                        'Black Cars' = 'fhv_black_car',
                                                        'Liveries' = 'fhv_livery',
                                                        'Luxury' = 'fhv_lux_limo'
                                                    ),
                                                    selected = 'Traditional FHVs Combined'
                                                )
                                            ),
                                            
# Trips Dashboard Trends section - if not FHV - display all info ----------------------------------------------------                                            
                                            conditionalPanel(
                                                'input.ind != "FHV"',
# Trips Dashboard Trends section - if not FHV - radio button selector (code in the server script) ----------------------------------------------------
                                                uiOutput('radio'),
                                                
# Trips Dashboard Trends section - if not FHV - if Service Zone (ex: Congestion Zone) ----------------------------------------------------  
                                                conditionalPanel(
                                                    'input.zones == "Service Zone"',
# Trips Dashboard Trends section - if not FHV -if Service Zone (code in the server script, look for 'firstfork') ----------------------------------------------------
                                                    uiOutput('firstfork'),
# Trips Dashboard Trends section - if not FHV - first helper info ----------------------------------------------------                                                    
                                                    userPost(
                                                        collapsible = T,
                                                        collapsed = T,
                                                        id = 'for_service_zone',
                                                        image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                                        author = NULL,
                                                        description = "PLOT INFORMATION -->",
                                                        "The plot to the right (-->) renders a month to month trend for the service zone picked in the step 1.",
                                                        br(),
                                                        br(),
                                                        "Choose 'Trend' option to see trends for all industries side-by-side within the service zone that you picked."
                                                    ),
                                                    br()
                                                ),
                                                
# Trips Dashboard Trends section - if not FHV - if Taxi Zone (ex:Union Square) ----------------------------------------------------  
                                                conditionalPanel(
                                                    'input.zones == "Taxi Zone"',
# Trips Dashboard Trends section - if not FHV -if Taxi Zone (code in the server script, look for 'secondfork') ---------------------------------------------------
                                                    uiOutput('secondfork'),
# Trips Dashboard Trends section - if not FHV -if Taxi Zone (code in the server script, look for 'B') ---------------------------------------------------
                                                    uiOutput('B'),
# Trips Dashboard Trends section - if not FHV -if Taxi Zone - shows where selected area is on the map ---------------------------------------------------
                                                    checkboxInput(
                                                        inputId = 'check',
                                                        label = 'Show on the map',
                                                        value = FALSE,
                                                        width = NULL
                                                    ),
# Trips Dashboard Trends section - if not FHV -if radio Taxi Zone and dropdown Taxi Zone ---------------------------------------------------                                                    
                                                    conditionalPanel(
                                                        'input.ech1 == "Taxi Zone"',
# Trips Dashboard Trends section - if not FHV - helper info if radio Taxi Zone and dropdown Taxi Zone ---------------------------------------------------- 
                                                        userPost(
                                                            collapsible = T,
                                                            collapsed = T,
                                                            id = 'help4',
                                                            image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                                            author = NULL,
                                                            description = "PLOT INFORMATION -->",
                                                            "The plot to the right (-->) renders a month to month trend for the taxi zone picked here.",
                                                            br(),
                                                            br(),
                                                            "Choose 'Trend' option to see trends for all industries side-by-side within the taxi zone that you just picked."
                                                        ),
                                                        br()
                                                    )
                                                    ,
# Trips Dashboard Trends section - if not FHV -if radio Taxi Zone and dropdown Industry Trends --------------------------------------------------- 
                                                    conditionalPanel(
                                                        'input.ech1 == "Trend"',
                                                        br(),
# Trips Dashboard Trends section - if not FHV - helper info if radio Taxi Zone and dropdown Industry Trends ---------------------------------------------------- 
                                                        userPost(
                                                            collapsible = T,
                                                            collapsed = T,
                                                            id = 'help5',
                                                            image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRdgpayOM1rZdWnQqPbXrXgCTkpScar0eRI9w6dPQS7PVR-oaUHZc3aevZbxUP7_YWjAbk&usqp=CAU",
                                                            author = NULL,
                                                            description = "PLOT INFORMATION -->",
                                                            "Trends for all industries side-by-side within the taxi zone that you picked here.",
                                                            br(),
                                                            br(),
                                                            "In the plot to the right (-->), you can eliminate industries by clicking on them, and zoom in and out by moving the slider."
                                                        )
                                                        ,
                                                        br(),
                                                        br(),
                                                        br()
                                                    )
                                                    ,
                                                    width = NULL
                                                )
                                            )
                                        ),
                                        data.step = 16,
                                        data.intro = steps[16]
                                    )
                                ),
                                column(
                                    width = 9,
# Trips Dashboard Trends section - Main Graph (look for 'first1' in the server code) ----------------------------------------------------     
                                    introBox(
                                        withSpinner(echarts4rOutput("first1", height = '450px')),
                                        data.step = 17,
                                        data.intro = steps[17]
                                    )
                                )),
                                data.step = 15,
                                data.intro = steps[15]
                            )
                        )
                    ),
# Trips Dashboard Trends section End ---------------------------------------------------- 


# Trips Dashboard Table and Taxi Zone Value Boxes (only displayed if not FHV) ----------------------------------------------------                   
                    conditionalPanel('input.ind != "FHV"',
                                     fluidRow(
                                         column(
                                             width = 6,
                                             setShadow('box'),
                                             introBox(
                                                 box(
                                                     id = 'map_box',
                                                     solidHeader = T,
                                                     collapsible = T,
                                                     collapsed = T,
                                                     closable = F,
                                                     title = 'Industry Trips Table',
# Trips Dashboard Table and Taxi Zone Value Boxes (only displayed if not FHV) - Table with raw numbers ----------------------------------------------------
                                                     withSpinner(DT::dataTableOutput('dth')),
                                                     width = NULL,
                                                     br(),
                                                     br(),
                                                     br()
                                                 ),
                                                 data.step = 18,
                                                 data.intro = steps[18]
                                             )
                                         ),
                                         
                                         column(width = 6,
                                                introBox(
                                                    box(
                                                        id = 'map_box',
                                                        solidHeader = T,
                                                        collapsible = T,
                                                        collapsed = T,
                                                        closable = F,
                                                        title = "Taxi Zone Metrics",
                                                        h5(htmlOutput("text1")),
# Trips Dashboard Table and Taxi Zone Value Boxes (only displayed if not FHV) - radio button selectors ----------------------------------------------------
                                                        radioButtons(
                                                            inputId = "vald",
                                                            'Monthly || Daily Average',
                                                            choices = c('Monthly', 'Daily Average'),
                                                            selected = 'Monthly'
                                                        ),
                                                        width = NULL,
# Trips Dashboard Table and Taxi Zone Value Boxes (only displayed if not FHV) - value boxes Start ----------------------------------------------------                                                        
                                                        valueBoxOutput('first_secondary', width = 12),
                                                        br(),
                                                        valueBoxOutput('second_secondary', width = 12),
                                                        br(),
                                                        valueBoxOutput('third_secondary', width = 12),
                                                        br(),
                                                        valueBoxOutput('fourth_secondary', width = 12)
# Trips Dashboard Table and Taxi Zone Value Boxes (only displayed if not FHV) - value boxes End ---------------------------------------------------- 
                                                    ),
                                                    data.step = 19,
                                                    data.intro = steps[19]
                                                ))
                                     ))
                    
                    
                ),
# Financial Dashboard Start  ----------------------------------------------------                
                tabItem(tabName = "fintab",
                        fluidRow(
                            box(
                                id = 'fin_box',
                                title = 'Fare & Earnings Dashboard (Beta)',
                                box(
                                    id = 'fin_box_1',
# Financial Dashboard - Main Graph section Start ----------------------------------------------------                                  
                                    fluidRow(
                                        box(
                                            id = 'fin_graphs',
                                            title = '',
                                            sidebar = boxSidebar(
                                                id = "mycardsidebar1",
                                                width = 45,
                                                background = "#f39c12",
                                                icon = icon("info-circle"),
                                                paste0(
                                                    'Fare/Earnings Over Time - displays average fares, total fares by month, and drivers cut of total fares over time. Data are available in monthly increments from 2011 to ',
                                                    format(ymd(Sys.Date() %m-% months(2)), '%b %Y'),
                                                    ' (see industry-specific ranges).
If the metric is Driver Pay: the barplot will display an average total take-home amount of a driver in the selected industry and and over the range of available data. (Average monthly tips are displayed separately).
The Average per Trip switch will make the plot reflect the same metrics but on the per trip level.
If the metric is Average Fare: the barplot will reflect the total average fare for the selected industry and over the range of available data. You can check what is included in the individual fare in the Breakdown box below.
If the metric is Total Fare: the barplot will reflect the sum of monthly fares for the selected industry and over the range of available data. You can check what is included in the total fare in the Breakdown box below.
The Add Driver Pay & Tips switch will add the total driver take-home and tips to the plot for you conveniece.'
                                                )
                                            ),
                                            
                                            
# Financial Dashboard - Main Graph section - if metric is Total Fare ----------------------------------------------------                                            
                                            shiny::conditionalPanel(
                                                condition = "input.fareMetric0 == 'Total Fare'",
# Financial Dashboard - Main Graph section - if metric is Total Fare - option to add driver pay and tips ---------------------------------------------------- 
                                                checkboxInput(
                                                    inputId = 'check_driver_pay',
                                                    label = 'Add Driver Pay & Tips',
                                                    value = FALSE,
                                                    width = NULL
                                                )                                                                     # ,
                                                
                                            ),
# Financial Dashboard - Main Graph section - if metric is Driver Pay ----------------------------------------------------         
                                            shiny::conditionalPanel(
                                                condition = "input.fareMetric0 == 'Driver Pay'",
                                                print(
                                                    '*Note: Some HVFHV drivers receive bonuses for making trips. TLC does NOT receive bonus information as part of HVFHV companies trip record submissions. Because of that, bonus data are not reflected in this dashboard.'
                                                ),
# Financial Dashboard - Main Graph section - if metric is Driver Pay - option to switch to average per trip ----------------------------------------------------
                                                checkboxInput(
                                                    inputId = 'average_pay_line',
                                                    label = 'Switch to Average per Trip',
                                                    value = FALSE,
                                                    width = NULL
                                                )                                                                     # ,
                                                
                                            ),
                                            
# Financial Dashboard - Main Graph section - Graph (for code go to server script and look for 'mainGraph1') ----------------------------------------------------      
                                            withSpinner(echarts4rOutput("mainGraph1", height = '400px')),
                                            width = 12
                                        )
                                        
                                    ),
# Financial Dashboard - Main Graph section End ----------------------------------------------------

# Financial Dashboard - Breakdown and Distribution Views Start ----------------------------------------------------
                                    fluidRow(
                                        box(
                                            id = 'fin_graphs',
                                            title = 'Breakdown',
                                            sidebar = boxSidebar(
                                                id = "mycardsidebar2",
                                                width = 45,
                                                background = "#f39c12",
                                                icon = icon("info-circle"),
                                                print(
                                                    'Breakdown Box - displays average fare, total fare, and earnings breakdowns for the selected year/month and industry.
If the metric is Driver Pay: the number is an average drivers portion of monthly fare. It is calculated differently for different industries. For HVFHVs, the companies send us that information for each trip (tips separately). For the yellow industry, the driver pay is calculated by adding the base fare amount and any surcharges (extras), tips are separate.
If the metric is Average Fare or Total Fare: the breakdowns will include everyting that goes into the fares. Note: Different industries have different fare breakdowns. For example, FHVs have sales tax as a separate category, the Yellow industry does not have that (instead, you will see N/A whenever an industry is not reflected in a category).
'
                                                )
                                            ),
                                            
                                            
# Financial Dashboard - Breakdown and Distribution Views - if Driver Pay ----------------------------------------------------                                          
                                            shiny::conditionalPanel(
                                                condition = "input.fareMetric0 == 'Driver Pay'",
                                                userBox(
                                                    collapsed = TRUE,
                                                    title = userDescription(
# Financial Dashboard - Breakdown and Distribution Views - if Driver Pay (text and number - look for in the server script)----------------------------------------------------
                                                        title = htmlOutput("fin_num_total_driver_pay"),
                                                        subtitle = textOutput('fin_num_total_lable_driver_pay'),
                                                        type = 2,
                                                        image = "https://cdn0.iconfinder.com/data/icons/taxi-1/512/g25402-512.png"
                                                    ),
                                                    
                                                    navPills(
                                                        id = "pillItem",
                                                        
                                                        navPillsItem(
                                                            left = 'Fare Amount - only Yellow (included in Driver Pay)',
                                                            color = "green",
# Financial Dashboard - Breakdown and Distribution Views - if Driver Pay (number - look for in the server script) Start----------------------------------------------------
                                                            right = textOutput("fin_num_fare_amount_driver_pay")
                                                        ),
                                                        
                                                        navPillsItem(
                                                            left = 'Extra - only Yellow (included in Driver Pay)',
                                                            color = "green",
                                                            
                                                            right = textOutput("fin_num_extra_driver_pay")
                                                        ),
                                                        
                                                        navPillsItem(
                                                            left = 'Tips (not included in Driver Pay)',
                                                            color = "Blue",
                                                            right = textOutput("fin_num_tips_driver_pay")
                                                        ),
                                                        
                                                        navPillsItem(
                                                            left = 'Bonus (only HVFHV, not included in Driver Pay)',
                                                            color = "blue",
                                                            
                                                            right = 'usually 2-6% of the total pay'
                                                        )
# Financial Dashboard - Breakdown and Distribution Views - if Driver Pay (number - look for in the server script) End----------------------------------------------------                                                        
                                                        
                                                    ),
                                                    footer = tags$a(href = "https://www1.nyc.gov/site/tlc/passengers/taxi-fare.page", "Click here for more info about Fares!"),
                                                    width = 12
                                                )                                                                    # ,
                                                
                                            ),
# Financial Dashboard - Breakdown and Distribution Views - if NOT Driver Pay ----------------------------------------------------                                            
                                            shiny::conditionalPanel(
                                                condition = "input.fareMetric0 != 'Driver Pay'",
                                                # ,

                                                userBox(
                                                    collapsed = TRUE,
                                                    title = userDescription(
# Financial Dashboard - Breakdown and Distribution Views - if NOT Driver Pay (number - look for in the server script) Start---------------------------------------------------- 
                                                        title = htmlOutput("fin_num_total"),
                                                        subtitle = textOutput('fin_num_total_lable'),
                                                        type = 2,
                                                        image = "https://cdn0.iconfinder.com/data/icons/taxi-1/512/g25402-512.png"
                                                    ),
                                                    
                                                    navPills(
                                                        id = "pillItem",
                                                        navPillsItem(
                                                            left = 'Base Fare',
                                                            color = "green",
                                                            right = textOutput("fin_num_fare_amount")
                                                        ),
                                                        navPillsItem(
                                                            left = 'Extra (Special Hours Surcharge) only Yellow',
                                                            color = "green",
                                                            
                                                            right = textOutput("fin_num_extra")
                                                        ),
                                                        
                                                        navPillsItem(
                                                            left = 'Tolls',
                                                            color = "green",
                                                            
                                                            right = textOutput("fin_num_toll")
                                                        ),
                                                        
                                                        navPillsItem(
                                                            left = 'MTA Surcharge (only Yellow)',
                                                            color = "red",
                                                            
                                                            right = textOutput("fin_num_mta")
                                                            
                                                        ),
                                                        navPillsItem(
                                                            left = 'Taxi Improvement Fund (only Yellow)',
                                                            color = "red",
                                                            
                                                            right = textOutput("fin_num_tif")
                                                            
                                                        ),
                                                        navPillsItem(
                                                            left = 'Black Car Fund (only FHVs)',
                                                            color = "red",
                                                            
                                                            right = textOutput("fin_num_bcf")
                                                            
                                                        ),
                                                        navPillsItem(
                                                            left = 'Sales Tax (only FHVs)',
                                                            color = "red",
                                                            
                                                            right = textOutput("fin_num_tax")
                                                            
                                                        ),
                                                        
                                                        navPillsItem(
                                                            left = 'Congestion Surcharge',
                                                            color = "red",
                                                            
                                                            right = textOutput("fin_num_congestion")
                                                            
                                                        ),
                                                        navPillsItem(
                                                            left = 'Airport Fee (only Yellow and FHVs)',
                                                            color = "red",
                                                            
                                                            right = textOutput("fin_num_airport")
                                                            
                                                        ),
                                                        
                                                        navPillsItem(
                                                            left = 'Tips',
                                                            color = "blue",
                                                            
                                                            right = textOutput("fin_num_tip")
# Financial Dashboard - Breakdown and Distribution Views - if NOT Driver Pay (number - look for in the server script) End---------------------------------------------------- 
                                                            
                                                        )
                                                        
                                                        
                                                    ),
                                                    footer = tags$a(href = "https://www1.nyc.gov/site/tlc/passengers/taxi-fare.page", "Click here for more info about Fares!"),
                                                    width = 12
                                                )
                                            ),
                                            
                                            
# Financial Dashboard - Breakdown and Distribution Views - additional info below the breakdowns---------------------------------------------------- 

                                            box(id = 'fl_graphs', textOutput("textbox1"), width = 12),
                                            width = 6
                                        ),
                                        box(
                                            id = 'fin_graphs',
                                            title = 'Distribution',
                                            sidebar = boxSidebar(
                                                id = "mycardsidebar3",
                                                width = 50,
                                                background = "#f39c12",
                                                icon = icon("info-circle"),
                                                print(
                                                    'Distribution Piechart - Displays average fare, total fare or earnings ratios for the selected year/month and industry. '
                                                )
                                            ),
# Financial Dashboard - Breakdown and Distribution Views - if Driver Pay - add checkbox selectors----------------------------------------------------
                                            shiny::conditionalPanel(
                                                condition = "input.fareMetric0 == 'Driver Pay'",
                                                print(
                                                    '*Note: Some HVFHV drivers receive bonuses for making trips. TLC does NOT receive bonus information as part of HVFHV companies trip record submissions. Because of that, bonus data are not reflected in this dashboard.'
                                                ),
                                                checkboxInput(
                                                    inputId = 'driver_pay_avg_pie',
                                                    label = 'Switch to Average per Trip',
                                                    value = FALSE,
                                                    width = NULL
                                                ),
                                                # ,
                                                
                                            ),
                                            
# Financial Dashboard - Breakdown and Distribution Views - Pie Graph (look for code in the server script)----------------------------------------------------
                                            withSpinner(echarts4rOutput("pie1"))
                                            ,
                                            width = 6
                                        )
                                        
                                        
                                    ),
# Financial Dashboard - Breakdown and Distribution Views End ----------------------------------------------------
                                    width = 12
                                ),
                                width = 12
                            )
                            
                        )),
# Financial Dashboard End ---------------------------------------------------- 

# Industry Indicators Dashboard Start ----------------------------------------------------                
                tabItem(tabName = "fl",
                        fluidRow(
                            box(
                                id = 'map_box',
                                title = 'Industry Indicators',
                                fluidRow(withSpinner(
# Industry Indicators Dashboard - Main Trend Graph (code in the server script) ----------------------------------------------------                                   
                                    echarts4rOutput("mainGraph", height = '370px')
                                )),
                                fluidRow(
# Industry Indicators Dashboard - Value Boxes Start (code in the server script) ---------------------------------------------------- 
                                    valueBoxOutput("yellowtripbox", width = 4),
                                    valueBoxOutput("greentripbox", width = 4),
                                    
                                    valueBoxOutput("hvtripbox", width = 3)
                                ),
                                
                                fluidRow(
                                    valueBoxOutput("bctripbox", width = 3),
                                    valueBoxOutput("lxtripbox", width = 3),
                                    valueBoxOutput("lvtripbox", width = 3)
# Industry Indicators Dashboard - Value Boxes End (code in the server script) ----------------------------------------------------                                     
                                    
                                ),
                                width = 12
                            ),
# Industry Indicators Dashboard - Plotly graphs Start (code in the server script) ----------------------------------------------------                            
                            box(
                                id = 'map_box',
                                title = strong("Choose a Dimension to See Different Graphs Below"),
                                fluidRow(column(
                                    5,
                                    selectInput(
                                        inputId = "dimension",
                                        label = '',
                                        choices = c(
                                            'Trips, Drivers & Vehicles' = '1',
                                            'Time & Money' = '2'
                                        ),
                                        multiple = FALSE,
                                        selectize = TRUE
                                    )
                                ), column(7)),
                                fluidRow(
                                    box(
                                        id = 'fl_graphs',
                                        plotlyOutput(outputId = 'trips_per_day'),
                                        width = 6
                                    ),
                                    box(
                                        id = 'fl_graphs',
                                        plotlyOutput(outputId = 'trips_year'),
                                        width = 6
                                    )
                                ),
                                fluidRow(
                                    box(id = 'fl_graphs', plotlyOutput(outputId = 'trips_per_month')),
                                    box(id = 'fl_graphs', plotlyOutput(outputId = 'medallions_per_month'))
                                ),
                                fluidRow(box(id = 'fl_graphs', textOutput("textbox"))#,)
                                         ,
                                         width = 12
                                )
# Industry Indicators Dashboard - Plotly graphs End (code in the server script) ----------------------------------------------------                                
                            )),
# Industry Indicators Dashboard End ----------------------------------------------------   

# Databank Start ----------------------------------------------------                        
                        tabItem(tabName = "databank1",
                                
                                
                                fluidRow(
                                    column(
                                        width = 12,
# Databank - if Trip Data selected - Download prompt code (look for in the server script) ----------------------------------------------------    
                                        conditionalPanel(
                                            'input.dataset == "Trip Data"',
                                            downloadButton('downloadData1', 'Download Data Set')
                                        ),
# Databank - if Monthly_Metrics selected - Download prompt code (look for in the server script) ---------------------------------------------------- 
                                        conditionalPanel(
                                            'input.dataset == "Monthly_Metrics"',
                                            downloadButton('downloadData4', 'Download Data Set')
                                        )
                                    ),
                                    
                                    
                                    column(
                                        width = 12,
                                        box(
                                            solidHeader = T,
                                            collapsible = T,
                                            collapsed = F,
                                            closable = F,
                                            title = 'DATA',
                                            status = 'warning',
                                            width = NULL,
# Databank - Tables View Start (look for in the server script) ----------------------------------------------------
                                            tabsetPanel(
                                                id = 'dataset',
                                                tabPanel('Trip Data', DT::dataTableOutput('pu')),
                                                
                                                tabPanel('Monthly_Metrics', DT::dataTableOutput('MM')),
                                                tabPanel('Data Dictionary', DT::dataTableOutput('dict'))
                                            ),
# Databank - Tables View End (look for in the server script) ----------------------------------------------------
                                            br(),
                                            br(),
                                            br(),
                                            br()
                                        )
                                        
                                    ),
                                    br(),
                                    br(),
                                    br(),
                                    br()
                                ))
# Databank End ----------------------------------------------------
                        
                        
                        
                )

            ))
            ,
# Right Sidebar code (not in use at the moment - ignore) ----------------------------------------------------
            controlbar = dashboardControlbar(
                width = 350,
                disable = T,
                collapsed = T,
                overlay = T,
                skin = "light",
                controlbarMenu(
                    id = "menu",
                    controlbarItem(
                        "Trip Estimator",
                        selectInput(
                            'xxx',
                            'From',
                            choices = unique(lookup$Zone),
                            selected = 'Flatiron'
                        ),
                        checkboxInput(
                            inputId = 'check1',
                            label = 'Show on the map',
                            value = FALSE,
                            width = NULL
                        ),
                        selectInput(
                            'zzz',
                            'To',
                            choices = unique(lookup$Zone),
                            selected = 1
                        ),
                        checkboxInput(
                            inputId = 'check2',
                            label = 'Show on the map',
                            value = FALSE,
                            width = NULL
                        ),
                        valueBoxOutput("info", width = 12),
                        br(),
                        valueBoxOutput("avg_fare", width = 12),
                        br(),
                        valueBoxOutput("avg_time", width = 12),
                        br(),
                        valueBoxOutput("avg_mileage", width = 12)
                        
                    ),
                    
                    controlbarItem("Skins", skinSelector())
                )
            )
            
            
        )
    )
    