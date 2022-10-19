


# Server Logic Start ---------------------------------------------------
shinyServer(function(input, output, session) {
 
  
# Welcome and tutorial modals Start ---------------------------------------------------  
 
  
   welcome() # welcome popup when app is launched
  
  
  
  observeEvent(input$btnYes, { # close welcome popup
    removeModal()
    introjs(session)
    
    
  })
  
  
  observeEvent(input$start, { # trigger Trip Dashboard Tutorial
    tuto1()
    
  })
  
  
  observeEvent(input$start1, { # trigger Indicators Dashboard Tutorial
    tuto2()
    
  })
  
 
  observeEvent(input$start2, { # trigger databank Dashboard Tutorial
    tuto3()
    
  })
  
  observeEvent(input$start4, { # trigger financial Dashboard Tutorial
    tuto4()
    
  })
  
  
  observeEvent(input$report, { # trigger Trips Dash RMD
    download1()
  })
  
  
  observeEvent(input$im_report, { # trigger Indicators Dash RMD
    download2()
  }) 
  
# Welcome and tutorial modals End ---------------------------------------------------
 
  
# Legacy Code 1 Start ---------------------------------------------------  
   
#    beginning <- Sys.time()
# 
#   onStop(function(){
# 
#       time  <- difftime(Sys.time(), beginning,
#                         units = "mins")
#       dbExecute(mydb,paste("INSERT INTO visit_time
# (stamp,time)
# VALUES('",Sys.time(),"','",time,"');", sep=""))
# 
#   })
  
#shinyjs::runjs("$('#textarea1').attr('maxlength', 80)")
#shinyjs::runjs("$('#textarea2').attr('maxlength', 80)")
  
#   observeEvent(input$submit11,{
#     dbExecute(mydb,paste("INSERT INTO monitor_feedback
# (name,occupation,comments1,comments2,grade,time)
# VALUES('",input$textarea,"','",input$areyou,"','",input$textarea1,"','",input$textarea2,"','",input$rate,"','",Sys.time(),"');", sep=""))
#     feedback_accepted()
# 
#   })
    
# Legacy Code 1 End ---------------------------------------------------   

   

# Right Sidebar Valueboxes Start ---------------------------------------------------
  output$info <- renderValueBox({
    shinydashboard::valueBox(
      'HVFHV',
      subtitle = paste0('INFO'),
      icon = icon('fas fa-exclamation-circle'),
      color = 'yellow',
      width = 12
    )
    
  })
  
  output$avg_fare <- renderValueBox({
    shinydashboard::valueBox(
      'HVFHV',
      subtitle = paste0('INFO'),
      icon = icon('fas fa-exclamation-circle'),
      color = 'yellow',
      width = 12
    )
    
  })
  
  output$avg_time <- renderValueBox({
    shinydashboard::valueBox(
      'HVFHV',
      subtitle = paste0('INFO'),
      icon = icon('fas fa-exclamation-circle'),
      color = 'yellow',
      width = 12
    )
    
  })
  
  
  output$avg_mileage <- renderValueBox({
    shinydashboard::valueBox(
      'HVFHV',
      subtitle = paste0('INFO'),
      icon = icon('fas fa-exclamation-circle'),
      color = 'yellow',
      width = 12
    )
    
  })
# Right Sidebar Valueboxes End ---------------------------------------------------  

  
# Trips Dashboard creating reactive dataset out of the main data based on params ------------------------------------------------  

data_m <- reactive ({
  setDT(data1)[Z == input$cbd &
                 company == input$ind &
                 metric == input$metric, c('year_month', 'id', 'company', 'count', 'Zone', 'metric')]
  
})  
  
      
    
# Trips Dashboard Trends section buttons and selectors Start ------------------------------------------------  
    
  output$radio <- renderUI({
    radioButtons(
      inputId = "zones",
      label = '',
      choices = c("Taxi Zone", "Service Zone"),
      selected = 'Taxi Zone'
    )
  })
  
  output$firstfork <- renderUI({
    selectInput(
      inputId = "ech",
      label = paste0('Trends in ', names(choices)[choices == input$cbd]),
      choices = c("Service Zone", "Industry Trends in Selected Service Zone"),
      selected = "Service Zone"
    )
  })
  
  output$secondfork <- renderUI({
    selectInput(
      inputId = "ech1",
      label = paste0('Trends in ', input$ZONE),
      choices = c("Taxi Zone", "Industry Trends in Selected Taxi Zone"),
      selected = "Taxi Zone"
    )
  })
    
    
# Trips Dashboard Trends section buttons and selectors End ------------------------------------------------   
    


  

# Trips Dashboard creating time range slider dynamically based on params ------------------------------------------------

  output$slider <- renderUI({
    sliderInput(
      "year1",
      label = 'STEP 4: Select Period',
      min = min(data_m()$year_month),
      max = max(data_m()$year_month),
      value = as.Date('2022-01-01'),
      timeFormat = "%Y-%b"
    )
  })
  
  
# Trips Dashboard - Formatting the date to fit our data --------------------------------------------

month <- reactive({
  req(input$year1)
  print(paste0(substr(input$year1, 1, 7), "-01"))
})



# Trips Dashboard - Calculating the citywide trip numbers based on inputs --------------------------------------------

all <- reactive({
  setDT(data1)[metric == 'PickUps' &
                 company == input$ind &
                 year_month == month() &
                 Z %in% c('Core', 'Non_Core'), 'count'] %>% summarise(count = sum(count))
  
})

# Trips Dashboard - First value box dynamic calculations Start --------------------------------------------    
box_1st_main <- reactive({
  if (input$val == 'Monthly') {
    req(input$year1)
    data <-
      data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month) %>% summarise(count = ifelse(
        input$metric %in% c('Average Fare', 'Average Trip Time', 'Average Trip Distance'),
        mean(count),
        sum(count)
      ))
    data1 <-
      data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>% dplyr::group_by(year_month) %>% summarise(count1 = ifelse(
        input$metric %in% c('Average Fare', 'Average Trip Time', 'Average Trip Distance'),
        mean(count),
        sum(count)
      ))
    
  }
  else if (input$val == 'Daily Average') {
    req(input$year1)
    a <-
      as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
    b <-
      as.numeric(days_in_month(as.Date(month()) %m-% months(as.numeric(input$lag))))
    data <-
      data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month) %>% summarise(count = ifelse(
        input$metric %in% c('Average Fare', 'Average Trip Time', 'Average Trip Distance'),
        mean(count),
        sum(count) / a
      ))
    data1 <-
      data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>% dplyr::group_by(year_month) %>% summarise(count1 = ifelse(
        input$metric %in% c('Average Fare', 'Average Trip Time', 'Average Trip Distance'),
        mean(count),
        sum(count) / b
      ))
  }
  
  
  if (nrow(data1) > 0) {
    data <- bind_cols(data, data1)
  } else {
    data <- data %>% add_column(count1 = 0)
  }
  data <-
    data %>% dplyr::mutate(dif = count - count1) %>% dplyr::mutate(ratio = dif /
                                                                     (count1) * 100)
  # data <- data %>% dplyr::mutate(dif = count - count1) %>% dplyr::mutate(ratio = dif/count1 * 100)
  
  
  #print(data)
})

# Trips Dashboard - First value box dynamic calculations END --------------------------------------------


# Trip Dashboard main chart reactive data prep Start --------------------------------------------      
for_chart <- reactive({
  #id <- lookup
  
  if (input$ind != 'FHV') {
    test <-
      data1 %>% dplyr::filter(Z == input$cbd, metric == input$metric)
  } else {
    print(input$indFhv)
    test <-
      fhvs %>% dplyr::filter(industry == input$indFhv) %>% dplyr::arrange(desc(count))
  }
  print(test)
})
# Trip Dashboard main chart reactive data prep End --------------------------------------------

# Trip Dashboard main chart - Taxi zone dynamic selector based on main inputs --------------------------------------------  
output$B <- renderUI({
  selectInput(
    inputId = "ZONE",
    label = 'Pick a Zone',
    choices = unique(for_chart()$Zone),
    selected = "Upper East Side South"
  )
})

# Trips Dashboard - Second value box dynamic calculations Start --------------------------------------------
box_1st_secondary <- reactive({
  if (input$ind != 'FHV') {
    req(input$ZONE)
    if (input$vald == 'Monthly') {
      req(input$year1)
      data <-
        data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month, Zone) %>% summarise(count = ifelse(
          input$metric %in% c(
            'Average Fare',
            'Average Trip Time',
            'Average Trip Distance'
          ),
          mean(count),
          sum(count)
        )) %>% dplyr::filter(Zone == input$ZONE)
      data1 <-
        data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>% dplyr::group_by(year_month, Zone) %>% summarise(count1 = ifelse(
          input$metric %in% c(
            'Average Fare',
            'Average Trip Time',
            'Average Trip Distance'
          ),
          mean(count),
          sum(count)
        )) %>% dplyr::filter(Zone == input$ZONE)
    }
    else if (input$vald == 'Daily Average') {
      req(input$year1)
      a <-
        as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
      b <-
        as.numeric(days_in_month(as.Date(month()) %m-% months(as.numeric(input$lag))))
      data <-
        data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month, Zone) %>% summarise(count = ifelse(
          input$metric %in% c(
            'Average Fare',
            'Average Trip Time',
            'Average Trip Distance'
          ),
          mean(count),
          sum(count) / a
        )) %>% dplyr::filter(Zone == input$ZONE)
      data1 <-
        data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>%  dplyr::group_by(year_month, Zone) %>% summarise(count1 = ifelse(
          input$metric %in% c(
            'Average Fare',
            'Average Trip Time',
            'Average Trip Distance'
          ),
          mean(count),
          sum(count) / b
        )) %>% dplyr::filter(Zone == input$ZONE)
    }
    
    if (nrow(data1) > 0) {
      data <- bind_cols(data, data1)
    } else {
      data <- data %>% add_column(count1 = 0)
    }
    data <-
      data %>% dplyr::mutate(dif = count - count1) %>% dplyr::mutate(ratio = dif /
                                                                       (count1) * 100)
  }
})

# Second value box dynamic calculations End --------------------------------------------    


# Trip Dashboard main map reactive data prep Start --------------------------------------------

for_map <- reactive({
  if (input$ind != 'FHV') {
    req(input$year1)
    
    data_m() %>%
      dplyr::filter(year_month == month()) %>% dplyr::select(id, count)
    
  }
})


pal <- reactive({
  if (input$ind != 'FHV') {
    data <- data_m()
    data$year_month <- floor_date(data$year_month, 'months')
    if (input$metric %in% c('PickUps', 'DropOffs', 'Trips')) {
      data <- data %>% dplyr::group_by(year_month, company, id) %>%
        dplyr::summarise(count = sum(count))
    }
    else {
      data <- data %>% dplyr::group_by(year_month, company, id) %>%
        dplyr::summarise(count = mean(count))
    }
  }
})

# Trip Dashboard main map reactive data prep End --------------------------------------------  


# Trip Dashboard main map leaflet code Start -------------------------------------------- 

# Map ----------------------      
output$leaf <- renderLeaflet({
  if (input$ind != 'FHV') {
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        preferCanvas = TRUE,
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      )
    ) %>%
      setView(-73.809354, 40.737084, zoom = 11) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
      addProviderTiles(providers$CartoDB.Positron, group = 'Standard Gray') %>%
      addTiles(group  = 'City View') %>%
      
      addResetMapButton() %>%
      addLayersControl(
        position = 'topleft',
        baseGroups = c("Standard Gray", 'Satellite', "City View"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
})

# Map updates ----------------------------    
observe({
  if (input$ind != 'FHV') {
    req(input$ZONE)
    
    shape@data <- left_join(shape@data, for_map(), by = "id")
    #str(for_map())
    req(input$year1)
    
    
    pal <-
      colorNumeric(palette = "OrRd", if (input$ind == 'YELLOW') {
        domain = shape@data$count
      } else {
        domain = c(min(pal()$count), max(pal()$count))
      })
    
    #print(shape@data)
    marker <- shape@data %>% dplyr::filter(zone == input$ZONE)
    marker_start <-
      shape@data %>% dplyr::filter(zone == input$xxx)
    marker_end <-
      shape@data %>% dplyr::filter(zone == input$zzz)
    
    a = leafletProxy("leaf", deferUntilFlush = F) %>%
      clearShapes() %>%
      clearMarkers() %>%
      
      addPolygons(
        data = shape,
        weight = 1.3,
        color = ~ pal(count),
        fillOpacity = 0.9,
        popup = paste0(
          "Zone: ",
          shape@data$zone,
          "<br>",
          "Count: ",
          format(shape@data$count, big.mark = ",")
        ),
        highlightOptions = highlightOptions(
          weight = 6,
          color = "orange",
          bringToFront = TRUE
        )
      )
    if (input$check == T) {
      a <- a %>% addMarkers(lng = marker$x, lat = marker$y)
    } else {
      a <- a
    }
    
    
    #######################
    
    if (input$check1 == T) {
      a <-
        a %>% addMarkers(
          lng = marker_start$x,
          lat = marker_start$y,
          icon = list(iconUrl = 'http://icons.iconarchive.com/icons/martz90/hex/256/car-icon.png',
                      iconSize = c(25, 50))
        )
      
    } else {
      a <- a
    }
    
    if (input$check2 == T) {
      a <- a %>% addMarkers(
        lng = marker_end$x,
        lat = marker_end$y,
        icon = list(iconUrl = 'http://icons.iconarchive.com/icons/iconshock/super-vista-business/256/checkered-flag-icon.png',
                    iconSize = c(25, 50))
      ) %>%
        
        addPolylines(
          lng = c(marker_start$x, marker_end$x),
          lat = c(marker_start$y, marker_end$y),
          
          opacity = 0.8,
          color = "blue",
          weight = 3
        )
      
      
    } else {
      a <- a
    }
    
    ############################
    
    a <- a %>% clearControls() %>%
      
      addLegend("bottomleft",
                pal <- pal,
                values = shape@data$count,
                title = "Trip Counts")
    
    
  }
})

# Trip Dashboard main map leaflet code End -------------------------------------------- 

# Trips Dashboard Table (only displayed if not FHV) - Table with raw numbers ----------------------------------------------------  
output$dth <- DT::renderDataTable(
  for_chart(),
  extensions = 'Scroller',
  options = list(
    deferRender = TRUE,
    scrollY = 390,
    scroller = TRUE
  )
)


# Trips Dashboard Trends section - Main Graph (echarts) Start ----------------------------------------------------     
output$first1 <- renderEcharts4r({
  if (input$ind != 'FHV') {
    withoutFHV <-
      for_chart() %>% dplyr::filter(!company %in% c('FHV', 'ALL'))
    
    if (input$zones == 'Service Zone') {
      if (input$ech == 'Service Zone') {
        ec <-
          withoutFHV %>% dplyr::filter(company == input$ind) %>% dplyr::group_by(year_month, company, metric) %>% summarise(count = ifelse(
            input$metric %in% c(
              'Average Fare',
              'Average Trip Time',
              'Average Trip Distance'
            ),
            mean(count),
            sum(count)
          ))
        
        ec <- ec %>% group_by(company) %>%
          e_charts(year_month) %>%
          e_bar(count) %>%
          e_theme("westeros") %>%
          e_tooltip(trigger = 'axis') %>%
          echarts4r::e_legend(type = 'scroll')
      }
      
      else if (input$ech == 'Industry Trends in Selected Service Zone') {
        ec <-
          withoutFHV %>%  dplyr::group_by(year_month, company, metric) %>% summarise(count =
                                                                                       ifelse(
                                                                                         input$metric %in% c(
                                                                                           'Average Fare',
                                                                                           'Average Trip Time',
                                                                                           'Average Trip Distance'
                                                                                         ),
                                                                                         mean(count),
                                                                                         sum(count)
                                                                                       ))
        
        ec <- ec %>%
          group_by(company) %>%
          e_charts(year_month) %>%
          e_line(count) %>%
          e_theme("westeros") %>%
          e_tooltip(trigger = 'axis') %>%
          e_datazoom(type = 'slider') %>%
          echarts4r::e_legend(type = 'scroll') %>%
          e_color(c(#'white',
            "green", "black", 'blue', 'yellow'))
      }
    } else if (input$zones == 'Taxi Zone') {
      if (input$ech1 == 'Taxi Zone') {
        ec <-
          withoutFHV %>% dplyr::filter(company == input$ind, Zone == input$ZONE) %>% dplyr::group_by(year_month, company, metric) %>% summarise(count = ifelse(
            input$metric %in% c(
              'Average Fare',
              'Average Trip Time',
              'Average Trip Distance'
            ),
            mean(count),
            sum(count)
          ))
        
        ec <- ec %>% group_by(company) %>%
          e_charts(year_month) %>%
          e_bar(count) %>%
          e_theme("westeros") %>%
          e_tooltip(trigger = 'axis') %>%
          echarts4r::e_legend(type = 'scroll')
      }
      
      else if (input$ech1 == 'Industry Trends in Selected Taxi Zone') {
        ec <-
          withoutFHV %>% dplyr::filter(Zone == input$ZONE) %>% dplyr::group_by(year_month, company, metric) %>% summarise(count = ifelse(
            input$metric %in% c(
              'Average Fare',
              'Average Trip Time',
              'Average Trip Distance'
            ),
            mean(count),
            sum(count)
          ))
        
        ec <- ec %>%
          group_by(company) %>%
          e_charts(year_month) %>%
          e_line(count) %>%
          e_theme("westeros") %>%
          e_tooltip(trigger = 'axis') %>%
          e_datazoom(type = 'slider') %>%
          echarts4r::e_legend(type = 'scroll') %>%
          e_color(c(#'white',
            "green", "black", 'blue', 'yellow'))
      }
    }
    
  } else
  {
    withFHV <- for_chart()
    
    withFHV$year_month <- lubridate::ymd(withFHV$year_month)
    
    withFHV %>%
      group_by(industry) %>%
      e_charts(year_month) %>%
      e_bar(count) %>%
      e_theme("westeros") %>%
      e_tooltip(trigger = 'axis') %>%
      e_datazoom(type = 'slider') %>%
      echarts4r::e_legend(type = 'scroll')
    
    
  }
})

# Trips Dashboard Trends section - Main Graph (echarts) End ----------------------------------------------------  


# Trips Dashboard Map section - Main Value Boxes Start ----------------------------------------------------
output$first_main <- renderValueBox({
  sign <-
    ifelse(
      input$metric == 'Average Fare',
      '$',
      ifelse(
        input$metric == 'Average Trip Time',
        'Minutes',
        ifelse(input$metric == 'Average Trip Distance', 'Miles', input$metric)
      )
    )
  
  
  a <-
    as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
  shinydashboard::valueBox(
    paste(format(
      round(box_1st_main()$count, 1), big.mark = ","
    ), sign),
    subtitle = paste0(
      if (input$ind == 'ALL') {
        'Combined '
      } else {
        paste0(names(choices1)[choices1 == input$ind], ' industry ')
      },
      input$val,
      " ",
      "'",
      names(choices)[choices == input$cbd],
      "'",
      " ",
      input$metric,
      ' in ',
      as.yearmon(input$year1, "%Y-%m-%d")
    ),
    icon = icon("fas fa-taxi"),
    color = 'blue',
    width = 12
  )
})


output$second_main <- renderValueBox({
  if (input$metric %in% c('PickUps', 'DropOffs', 'Trips')) {
    shinydashboard::valueBox(
      if (input$val == 'Monthly') {
        paste0(round(box_1st_main()$count / all()$count * 100), '%')
      } else if (input$val == 'Daily Average') {
        paste0(round(box_1st_main()$count / (
          all()$count / as.numeric(days_in_month(
            as.Date(input$year1, format = "%Y-%m-%d")
          ))
        ) * 100), '%')
      },
      subtitle = paste0(
        if (input$ind == 'ALL') {
          'Combined '
        } else {
          paste0(names(choices1)[choices1 == input$ind], ' industry ')
        },
        input$val,
        " ",
        "'",
        names(choices)[choices == input$cbd],
        "'",
        " ",
        input$metric,
        ' to all ',
        names(choices1)[choices1 == input$ind],
        ' ',
        input$metric,
        ' in ',
        as.yearmon(input$year1, "%Y-%m-%d")
      ),
      icon = icon("fas fa-taxi"),
      color = 'blue',
      width = 12
    )
  }
})

output$third_main <- renderValueBox({
  shinydashboard::valueBox(
    if (box_1st_main()$dif == box_1st_main()$count) {
      paste0('NO DATA')
    } else {
      format(round(box_1st_main()$dif), big.mark = ",")
    },
    if (box_1st_main()$dif == box_1st_main()$count) {
      subtitle = paste0(
        "*Data are avaliable in ranges from ",
        first(data_m()$year_month),
        " to ",
        last(data_m()$year_month),
        "."
      )
    }
    else {
      subtitle = paste0(
        if (input$ind == 'ALL') {
          'Combined '
        } else {
          paste0(names(choices1)[choices1 == input$ind], ' industry ')
        },
        "'",
        names(choices)[choices == input$cbd],
        "'",
        " ",
        input$val,
        " ",
        input$metric,
        " number difference between ",
        as.yearmon(input$year1, "%Y-%m-%d"),
        " and ",
        as.yearmon(input$year1 %m-% months(as.numeric(input$lag)))
      )
    },
    icon = icon("fas fa-taxi"),
    if (box_1st_main()$dif > 0) {
      color = 'green'
    } else if (box_1st_main()$dif < 0) {
      color = 'red'
    } else {
      color = 'blue'
    },
    width = 12
  )
})


output$fourth_main <- renderValueBox({
  shinydashboard::valueBox(
    if (box_1st_main()$ratio == Inf) {
      paste0('NO DATA')
    } else {
      paste(format(round(box_1st_main()$ratio), big.mark = ","), " %")
    },
    if (box_1st_main()$ratio == Inf) {
      subtitle = paste0(
        "*Data are avaliable in ranges from ",
        first(data_m()$year_month),
        " to ",
        last(data_m()$year_month),
        "."
      )
    }
    else {
      subtitle = paste0(
        names(choices1)[choices1 == input$ind],
        ' industry ',
        "'",
        names(choices)[choices == input$cbd],
        "'",
        " ",
        input$val,
        " ",
        input$metric,
        " percent difference between ",
        as.yearmon(input$year1, "%Y-%m-%d"),
        " and ",
        as.yearmon(input$year1 %m-% months(as.numeric(input$lag)))
      )
    },
    icon = icon("fas fa-taxi"),
    if (box_1st_main()$ratio > 0) {
      color = 'green'
    } else if (box_1st_main()$ratio < 0) {
      color = 'red'
    } else {
      color = 'blue'
    },
    width = 12
  )
})
# Trips Dashboard Map section - Main Value Boxes End ----------------------------------------------------


# Trips Dashboard Table and Taxi Zone Value Boxes section - Secondary Value Boxes Start ----------------------------------------------------  
output$first_secondary <- renderValueBox({
  sign <-
    ifelse(
      input$metric == 'Average Fare',
      '$',
      ifelse(
        input$metric == 'Average Trip Time',
        'Minutes',
        ifelse(input$metric == 'Average Trip Distance', 'Miles', input$metric)
      )
    )
  a <-
    as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
  shinydashboard::valueBox(
    paste(format(
      round(box_1st_secondary()$count, 1), big.mark = ","
    ), sign),
    subtitle = paste0(
      input$ZONE,
      " ",
      input$vald,
      " ",
      input$metric,
      ' in ',
      as.yearmon(input$year1, "%Y-%m-%d")
    ),
    icon = icon("fas fa-taxi"),
    color = 'blue',
    width = 12
  )
})


output$second_secondary <- renderValueBox({
  if (input$metric %in% c('PickUps', 'DropOffs', 'Trips')) {
    print(box_1st_secondary()$count)
    print(box_1st_main()$count)
    print(as.numeric(days_in_month(
      as.Date(input$year1, format = "%Y-%m-%d")
    )))
    
    shinydashboard::valueBox(
      if (input$vald == 'Monthly') {
        paste0(round(box_1st_secondary()$count / box_1st_main()$count * 100),
               '%')
      } else if (input$vald == 'Daily Average') {
        paste0(round(
          box_1st_secondary()$count / (box_1st_main()$count / as.numeric(days_in_month(
            as.Date(input$year1, format = "%Y-%m-%d")
          ))) * 100
        ), '%')
      }
      
      ,
      subtitle = paste0(
        'Ratio of ' ,
        input$ZONE,
        " ",
        input$vald,
        " ",
        input$metric,
        ' to all ',
        input$metric,
        ' in ',
        as.yearmon(input$year1, "%Y-%m-%d")
      ),
      icon = icon("fas fa-taxi"),
      color = 'blue',
      width = 12
    )
  }
})

output$third_secondary <- renderValueBox({
  shinydashboard::valueBox(
    if (box_1st_secondary()$dif == box_1st_secondary()$count) {
      paste0('NO DATA')
    } else {
      format(round(box_1st_secondary()$dif), big.mark = ",")
    },
    if (box_1st_secondary()$dif == box_1st_secondary()$count) {
      subtitle = paste0(
        "*Data are avaliable in ranges from ",
        first(data_m()$year_month),
        " to ",
        last(data_m()$year_month),
        "."
      )
    }
    else {
      subtitle = paste0(
        input$ZONE,
        " ",
        input$vald,
        " ",
        input$metric,
        " number difference between ",
        as.yearmon(input$year1, "%Y-%m-%d"),
        " and ",
        as.yearmon(input$year1 %m-% months(as.numeric(input$lag)))
      )
    },
    icon = icon("fas fa-taxi"),
    if (box_1st_secondary()$dif > 0) {
      color = 'green'
    } else if (box_1st_secondary()$dif < 0) {
      color = 'red'
    } else {
      color = 'blue'
    },
    width = 12
  )
})


output$fourth_secondary <- renderValueBox({
  shinydashboard::valueBox(
    if (box_1st_secondary()$ratio == Inf) {
      paste0('NO DATA')
    } else {
      paste(format(round(box_1st_secondary()$ratio), big.mark = ","), " %")
    },
    if (box_1st_secondary()$ratio == Inf) {
      subtitle = paste0(
        "*Data are avaliable in ranges from ",
        first(data_m()$year_month),
        " to ",
        last(data_m()$year_month),
        "."
      )
    }
    else {
      subtitle = paste0(
        input$ZONE,
        " ",
        input$vald,
        " ",
        input$metric,
        " percent difference between ",
        as.yearmon(input$year1, "%Y-%m-%d"),
        " and ",
        as.yearmon(input$year1 %m-% months(as.numeric(input$lag)))
      )
    },
    icon = icon("fas fa-taxi"),
    if (box_1st_secondary()$ratio > 0) {
      color = 'green'
    } else if (box_1st_secondary()$ratio < 0) {
      color = 'red'
    } else {
      color = 'blue'
    },
    width = 12
  )
})    

# Trips Dashboard Table and Taxi Zone Value Boxes section - Secondary Value Boxes End ----------------------------------------------------   
    
# Trip Dashboard Rmd Report Generator Func --------------------------------------------
output$btnn <- downloadHandler(
  
  # For PDF output, change this to "report.pdf"
  filename = "main_report.html",
  content = function(file) {
    # dbExecute(mydb,paste("INSERT INTO monitor_report
    #                                       (count, time)
    #                                       VALUES('1','",Sys.time(),"');", sep=""))
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(
      zone = input$cbd,
      metric = input$metric,
      industry = input$ind,
      monthly = round(box_1st_main()$count),
      dif = round(box_1st_main()$dif),
      rat = round(box_1st_main()$ratio),
      all = all()$count,
      mon = input$year1,
      lag = input$lag,
      a = as.numeric(round(box_1st_main()$count)),
      dif = round(box_1st_main()$dif),
      rat = round(box_1st_main()$ratio),
      b = as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d"))),
      test = for_chart(),
      shape = shape,
      for_map = for_map(),
      pal = pal()
    )
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  

# Industry Indicators Dashboard - creating reactive dataset based on inputs Start ----------------------

mainGr <- reactive({
  a = input$indMetric1
  data <-
    setDT(industry_metrics)[, c('license_class',
                                ..a,
                                'month_date',
                                "shared_trips_per_day_percent")]
  data$month_date <- ymd(data$month_date)
  data$license_class <- as.character(data$license_class)
  
  start_date = ymd(input$monthdate[1])
  end_date = ymd(input$monthdate[2])
  
  data =  subset(data,
                 (month_date >= start_date &
                    month_date <= end_date))
  
  print(data)
  
  
})

# Industry Indicators Dashboard - creating reactive dataset based on inputs End ----------------------


# Industry Indicators Dashboard - rendering value boxes Start ----------------------    
output$yellowtripbox = renderValueBox({
  med_trips = head(round(mainGr()[mainGr()$license_class == "Yellow", 2], 2), 1)
  #print(med_trips)
  shinydashboard::valueBox(
    if (is.na(med_trips) == T ||
        input$indMetric1 == 'trips_per_day_shared') {
      paste0('No Data')
    } else {
      paste0(med_trips)
    },
    if (is.na(med_trips) == T ||
        input$indMetric1 == 'trips_per_day_shared') {
      paste0('.')
    } else {
      paste0('Yellow ',
             names(choices2)[choices2 == input$indMetric1],
             ' as of ',
             as.yearmon(input$monthdate[2]))
    },
    icon = icon("fas fa-taxi"),
    color = "yellow",
    width = 4
  )
}) 


output$greentripbox = renderValueBox({
  shl_trips = head(round(mainGr()[mainGr()$license_class == "Green", 2], 2), 1)
  # print(shl_trips)
  shinydashboard::valueBox(
    if (is.na(shl_trips) == T ||
        input$indMetric1 == 'trips_per_day_shared') {
      paste0('No Data')
    } else {
      paste0(shl_trips)
    },
    if (is.na(shl_trips) == T ||
        input$indMetric1 == 'trips_per_day_shared') {
      paste0('.')
    } else {
      paste0('Green ',
             names(choices2)[choices2 == input$indMetric1],
             ' as of ',
             as.yearmon(input$monthdate[2]))
    },
    icon = icon("fas fa-taxi"),
    color = "green",
    width = 4
  )
})


output$hvtripbox = renderValueBox({
  ubers_etc = head(round(mainGr()[mainGr()$license_class == 'FHV - High Volume', 2], 2), 1)
  shinydashboard::valueBox(
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('No Data')
    } else {
      paste0(ubers_etc)
    },
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('.')
    } else {
      paste0('HVFHV ',
             names(choices2)[choices2 == input$indMetric1],
             ' as of ',
             as.yearmon(input$monthdate[2]))
    },
    icon = icon("fas fa-taxi"),
    color = "maroon",
    width = 4
  )
})


output$bctripbox = renderValueBox({
  ubers_etc = head(round(mainGr()[mainGr()$license_class == 'FHV - Black Car', 2], 2), 1)
  shinydashboard::valueBox(
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('No Data')
    } else {
      paste0(ubers_etc)
    },
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('.')
    } else {
      paste0('FHV - Black Car ',
             names(choices2)[choices2 == input$indMetric1],
             ' as of ',
             as.yearmon(input$monthdate[2]))
    },
    icon = icon("fas fa-taxi"),
    color = "navy",
    width = 4
  )
})


output$lxtripbox = renderValueBox({
  ubers_etc = head(round(mainGr()[mainGr()$license_class == 'FHV - Lux Limo', 2], 2), 1)
  print(ubers_etc)
  shinydashboard::valueBox(
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('No Data')
    } else {
      paste0(ubers_etc)
    },
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('.')
    } else {
      paste0('FHV - Lux Limo ',
             names(choices2)[choices2 == input$indMetric1],
             ' as of ',
             as.yearmon(input$monthdate[2]))
    },
    icon = icon("fas fa-taxi"),
    color = "blue",
    width = 4
  )
})


output$lvtripbox = renderValueBox({
  ubers_etc = head(round(mainGr()[mainGr()$license_class == 'FHV - Livery', 2], 2), 1)
  shinydashboard::valueBox(
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('No Data')
    } else {
      paste0(ubers_etc)
    },
    if (is.na(ubers_etc) == T ||
        input$indMetric1 %in% c('trips_per_day_shared',
                                'percent_of_trips_paid_with_credit_card')) {
      paste0('.')
    } else {
      paste0('FHV - Livery ',
             names(choices2)[choices2 == input$indMetric1],
             ' as of ',
             as.yearmon(input$monthdate[2]))
    },
    icon = icon("fas fa-taxi"),
    color = "red",
    width = 4
  )
})


output$hvsharing = renderValueBox({
  recent_date = as.character(industry_metrics[1, "month_date"])
  my_query = "High Volume trips per day are shared as of 'SAMPLE'"
  shared = paste0(first(mainGr()[mainGr()$license_class == 'FHV - High Volume', 4]) * 100, ' %')
  print('before')
  print(shared)
  print('after')
  shinydashboard::valueBox(
    if (shared[1] == "NA %") {
      paste0('No Data')
    } else {
      paste0(shared[1])
    },
    if (shared[1] == "NA %") {
      paste0('.')
    } else {
      paste0('High Volume trips per day that are shared as of ',
             as.yearmon(input$monthdate[2]))
    },
    icon = icon("fas fa-taxi"),
    color = "aqua",
    width = 3
  )
})
# Industry Indicators Dashboard - rendering value boxes End ----------------------

# Industry Indicators Dashboard - rendering text ----------------------

output$textbox = renderText({
  print(
    "*Note that the * next to graphs designates these aggregations are based on daily averages going back and not on summations over selected periods. Boxes above show the values that correspond to the latest month selected"
  )
})

# Industry Indicators Dashboard - Rendering Main Trend Graph ----------------------------------------------------
output$mainGraph <- renderEcharts4r({
  td <- mainGr()
  
  colnames(td)[2] <- 'count'
  
  td <- td %>% group_by(license_class) %>%
    e_charts(month_date) %>%
    e_line(count) %>%
    e_theme("westeros") %>%
    e_tooltip(trigger = 'axis') %>%
    echarts4r::e_legend(type = 'scroll') %>%
    e_color(c("navy", "#ca76b6", 'red', 'blue', 'green', 'yellow'))
  
})


# Industry Indicators Dashboard - rendering plotly Graphs Start ----------------------
#trips per day------------------------
output$trips_per_day = renderPlotly({
  
  td = subset(industry_metrics, 
              (month_date >= input$monthdate[1] & 
                 month_date <= input$monthdate[2]), c('trips_per_day', 'month_date', 'license_class'))
  print(head(td))
  trips = plot_ly(td, x = ~month_date
                  , y = ~trips_per_day
                  ,type = 'scatter'
                  , split = ~license_class
                  , mode = 'lines'
                  ,color = ~license_class
                  #,colors = pal
  )   
  
  trips = layout(trips,             
                 title = "Average Trips per Day each Month", 
                 xaxis = list(           
                   title = "Month & Year",   
                   showgrid = F        
                 ),
                 yaxis = list(           
                   title = "Trips Per Day"      
                 ))
  
  #farebox per day---------------------------
  if (input$dimension == '2') {
    
    td = subset(industry_metrics,
                (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                c('farebox_per_day', 'month_date', 'license_class'))
    
    trips = plot_ly(td, x = ~month_date, y = ~farebox_per_day
                    , type = 'scatter'
                    , split = ~license_class
                    , mode = 'lines'
                    ,color = ~license_class
                    #,colors = pal
    )
    
    trips = layout(trips,              
                   title = "Average Farebox Per Day each Month", 
                   xaxis = list(          
                     title = "Month & Year",    
                     showgrid = F       
                   ),
                   yaxis = list(           
                     title = "Farebox Per Day"     
                   ))
  }
  trips
})

#trips per year-----------------------------------
output$trips_year = renderPlotly({
  
  td = subset(industry_metrics, 
              (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
              c('trips_per_month','trips_per_day','month_date','license_class', 'year','trips_per_week'))
  
  
  nd = aggregate(trips_per_day ~ year + license_class, data = td, FUN = sum)
  
  uniks = plot_ly(nd
                  , x = ~year
                  , y = ~trips_per_day
                  , split = ~license_class
                  , type = 'bar'
                  ,color = ~license_class
                  #,colors = pal
  )
  uniks = layout(uniks,             
                 title = "*Average Trips Per Year", 
                 xaxis = list(           
                   title = "Month & Year",    
                   showgrid = F        
                 ),
                 yaxis = list(          
                   title = "Trips"     
                 ))
  
  #farebox per year------------------------------------------
  if (input$dimension == '2') {
    
    td =  subset(industry_metrics, 
                 (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                 c('farebox_per_day','month_date','license_class', 'year','trips_per_week'))
    
    nd = aggregate(farebox_per_day ~ year + license_class, data = td, FUN = sum)
    
    uniks = plot_ly(nd
                    , x = ~year
                    , y = ~farebox_per_day
                    , split = ~license_class
                    , type = 'bar'
                    ,color = ~license_class
                    #,colors = pal
    )
    uniks = layout(uniks,              
                   title = "*Average Farebox Per Year", 
                   xaxis = list(           
                     title = "Month & Year",     
                     showgrid = F        
                   ),
                   yaxis = list(           
                     title = "Farebox"      
                   ))
  }
  uniks
})

#trips per month------------------------------------------
output$trips_per_month = renderPlotly({
  
  td =  subset(industry_metrics,
               (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
               c('trips_per_month','month_date','license_class', 'year'))
  uniks = plot_ly(td, 
                  x = ~month_date, y = ~trips_per_month
                  , type = 'scatter'
                  , split = ~license_class
                  , mode = 'lines'
                  ,color = ~license_class
                  #,colors = pal
  )
  
  uniks = layout(uniks,             
                 title = "*Trips Per Month Over Time", 
                 xaxis = list(          
                   title = "Month & Year",     
                   showgrid = F      
                 ),
                 yaxis = list(           
                   title = "Trips Per Month"      
                 ))
  
  
  
  #farebox per month------------------------------------- 
  if (input$dimension == '2') {
    
    td =  subset(industry_metrics, 
                 (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                 c('farebox_per_month','month_date','license_class', 'year'))
    
    uniks = plot_ly(td, 
                    x = ~month_date, y = ~farebox_per_month
                    , type = 'scatter'
                    , split = ~license_class
                    , mode = 'lines'
                    ,color = ~license_class
                    #,colors = pal
    )
    
    uniks = layout(uniks,              
                   title = "*Farebox Per Month Over Time", 
                   xaxis = list(           
                     title = "Month & Year",     
                     showgrid = F        
                   ),
                   yaxis = list(           
                     title = "Farebox Per Month"      
                   ))
  }
  uniks     
})

#vehicles per month--------------------------------------
output$medallions_per_month = renderPlotly({
  
  td =  subset(industry_metrics, 
               (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
               c('unique_vehicles','month_date','license_class'))
  uniks = plot_ly(td, 
                  x = ~month_date, y = ~unique_vehicles
                  , type = 'scatter'
                  , split = ~license_class
                  , mode = 'lines'
                  ,color = ~license_class
                  #,colors = pal
  )
  uniks = layout(uniks,              
                 title = "Unique Vehicles Per Month Over Time", 
                 xaxis = list(          
                   title = "Month & Year",    
                   showgrid = F        
                 ),
                 yaxis = list(           
                   title = "Unique Vehicles"      
                 ))
  
  
  #vehicles_per_day --------------------------------------
  if (input$dimension == '2') {
    
    td =  subset(industry_metrics, 
                 (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                 c('vehicles_per_day','month_date','license_class'))
    uniks = plot_ly(td, 
                    x = ~month_date, y = ~vehicles_per_day
                    , type = 'scatter'
                    , split = ~license_class
                    , mode = 'lines'
                    ,color = ~license_class
                    #,colors = pal
    )
    
    uniks = layout(uniks,              
                   title = "Vehicles Per Day Over Time", 
                   xaxis = list(           
                     title = "Month & Year",    
                     showgrid = F       
                   ),
                   yaxis = list(           
                     title = "Vehicless Per Day Per Month"      
                   ))
  }
  uniks     
})

# Industry Indicators Dashboard - rendering plotly Graphs End ----------------------

# add data for im report ----------------------------------------------------
for_im <- reactive({
  data =  subset(industry_metrics,
                 (
                   month_date >= input$monthdate[1] &
                     month_date <= input$monthdate[2]
                 ))
  
})


# Industry Indicators Dashboard Rmd Report Generator Func --------------------------------------------
output$im_btnn <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "industry_metrics_report.html",
  content = function(file) {
    # dbExecute(mydb,paste("INSERT INTO monitor_report1
    #                                       (count, time)
    #                                       VALUES('1','",Sys.time(),"');", sep=""))
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <-
      file.path(tempdir(), "industry_metrics_report.Rmd")
    file.copy("industry_metrics_report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(metric = input$indMetric1,
                   for_im = for_im())
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(
      tempReport,
      output_file = file,
      params = params,
      envir = new.env(parent = globalenv())
    )
  }
)



# Financial Dashboard creating time range slider dynamically based on params ------------------------------------------------    

output$slider_fin <- renderUI({
  #can be simplified
  
  
  if (input$fareMetric1 == 'Yellow') {
    sliderInput(
      "year12",
      label = 'STEP 3: Select Period',
      min = min(yellow_fare$year_month),
      max = max(yellow_fare$year_month),
      value = as.Date('2022-03-01'),
      timeFormat = "%Y-%b"
    )
  } else if (input$fareMetric1 == 'Green') {
    sliderInput(
      "year12",
      label = 'STEP 3: Select Period',
      min = min(green_fare$year_month),
      max = max(green_fare$year_month),
      value = as.Date('2022-03-01'),
      timeFormat = "%Y-%b"
    )
  } else if (input$fareMetric1 == 'HVFHV') {
    sliderInput(
      "year12",
      label = 'STEP 3: Select Period',
      min = min(hvfhv_fare$year_month),
      max = max(hvfhv_fare$year_month),
      value = as.Date('2022-03-01'),
      timeFormat = "%Y-%b"
    )
  }
  
  
})
    

# Financial Dashboard Filtering data based on inputs ------------------------------------------------
  
# Financial Dashboard Filtering data based on inputs (MAIN) ------------------------------------------------
  
  data_fin <- reactive ({
    req(input$year12) # must wait to load this
    if (input$fareMetric0 == 'Average Fare') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare)[year_month == floor_date(input$year12, 'month'), c(
          "avg_total_fare",
          "avg_fare_amount",
          "avg_extra",
          "avg_tip_amount",
          "avg_tolls_amount",
          "avg_distance",
          "count_drivers"
        )]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare)[year_month == floor_date(input$year12, 'month'), c(
          "avg_total_fare",
          "avg_fare_amount",
          "avg_extra",
          "avg_tip_amount",
          "avg_tolls_amount",
          "avg_distance",
          "count_drivers"
        )]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare)[year_month == floor_date(input$year12, 'month'), c(
          "avg_base_passenger_fare",
          "avg_tolls",
          "avg_bcf",
          "avg_sales_tax",
          "avg_tips",
          "avg_distance",
          "count_drivers"
        )]
      }
    } else if (input$fareMetric0 == 'Total Fare') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare)[year_month == floor_date(input$year12, 'month'), c(
          "sum_total_fare",
          "sum_fare_amount",
          "sum_extra",
          "sum_tip_amount",
          "sum_tolls_amount",
          'total_mta_tax',
          'sum_improvement_surcharge',
          'sum_congestion_surcharge',
          'sum_airport_fee',
          "avg_distance",
          "count_drivers"
        )]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare)[year_month == floor_date(input$year12, 'month'), c(
          "sum_total_fare",
          "sum_fare_amount",
          "sum_extra",
          "sum_tip_amount",
          "sum_tolls_amount",
          'total_mta_tax',
          'sum_improvement_surcharge',
          'sum_congestion_surcharge',
          "avg_distance",
          "count_drivers"
        )]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare)[year_month == floor_date(input$year12, 'month'), c(
          "sum_base_passenger_fare",
          "sum_tolls",
          "sum_bcf",
          "sum_sales_tax",
          "sum_tips",
          "sum_congestion_surcharge",
          "sum_airport_fee",
          "avg_distance",
          "count_drivers"
        )]
      }
    } else if (input$fareMetric0 == 'Driver Pay') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare)[year_month == floor_date(input$year12, 'month'), c("pay_per_driver",
                                                                              "tips_per_driver",
                                                                              "extra_per_driver",
                                                                              "fare_per_driver")]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare)[year_month == floor_date(input$year12, 'month'), c(
          "pay_per_driver",
          "tips_per_driver",
          "extra_per_driver",
          'sum_fare_amount',
          "fare_per_driver"
        )]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare)[year_month == floor_date(input$year12, 'month'), c("pay_per_driver", "tips_per_driver")]
      }
    }
    
  })
    
# Financial Dashboard Filtering data based on inputs (for the line graphs) ------------------------------------------------  
    
  data_fin_line <- reactive ({
    req(input$year12) # must wait to load this
    
    if (input$fareMetric0 == 'Average Fare') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare_melt)[variable %in% c(
          "driver_pay",
          "avg_total_fare",
          "avg_fare_amount",
          "avg_extra",
          "avg_tip_amount",
          "avg_tolls_amount",
          "improvement_surcharge",
          "avg_congestion_surcharge",
          "avg_airport_fee",
          "mta_tax"
        ), ]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare_melt)[variable %in% c(
          "driver_pay",
          "avg_total_fare",
          "avg_fare_amount",
          "avg_extra",
          "avg_tip_amount",
          "avg_tolls_amount",
          "improvement_surcharge",
          "avg_congestion_surcharge",
          "avg_airport_fee",
          "mta_tax"
        ), ]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare_melt)[variable %in% c(
          "driver_pay",
          "avg_total_fare",
          "avg_tolls",
          "avg_bcf",
          "avg_sales_tax",
          "avg_tips",
          "avg_congestion_surcharge",
          "avg_airport_fee"
        ), ]
      }
    } else if (input$fareMetric0 == 'Total Fare') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare_melt)[!variable %in% c(
          "driver_pay",
          "fare_per_driver",
          "avg_total_fare",
          "avg_fare_amount",
          "avg_extra",
          "avg_tip_amount",
          "avg_tolls_amount",
          "improvement_surcharge",
          "avg_congestion_surcharge",
          "avg_airport_fee",
          "mta_tax"
        ), ]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare_melt)[!variable %in% c(
          "driver_pay",
          "fare_per_driver",
          "avg_total_fare",
          "avg_fare_amount",
          "avg_extra",
          "avg_tip_amount",
          "avg_tolls_amount",
          "improvement_surcharge",
          "avg_congestion_surcharge",
          "avg_airport_fee",
          "mta_tax"
        ), ]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare_melt)[!variable %in% c(
          "driver_pay",
          "fare_per_driver",
          "avg_total_fare",
          "avg_base_passenger_fare",
          "avg_tolls",
          "avg_bcf",
          "avg_sales_tax",
          "avg_tips",
          "avg_tip",
          "avg_congestion_surcharge",
          "avg_airport_fee"
        ), ]
      }
    } else if (input$fareMetric0 == 'Driver Pay') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare_melt)[variable %in% c("driver_pay",
                                                "pay_per_driver",
                                                "tips_per_driver",
                                                "avg_tip_amount"), ]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare_melt)[variable %in% c("driver_pay",
                                               "pay_per_driver",
                                               "tips_per_driver",
                                               "avg_tip_amount"), ]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare_melt)[variable %in% c("driver_pay",
                                               "pay_per_driver",
                                               "tips_per_driver",
                                               "avg_tips"), ]
      }
      
    }
    
  })
    
# Financial Dashboard Filtering data based on inputs (for the pie charts) ------------------------------------------------   

  data_fin_pie <- reactive ({
    req(input$year12)
    
    if (input$fareMetric0 == 'Average Fare') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                  variable %in% c(
                                    "avg_fare_amount",
                                    "avg_extra",
                                    "avg_tip_amount",
                                    "avg_tolls_amount",
                                    "improvement_surcharge",
                                    "avg_congestion_surcharge",
                                    "avg_airport_fee"
                                  ), ]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                 variable %in% c(
                                   "avg_fare_amount",
                                   "avg_extra",
                                   "avg_tip_amount",
                                   "avg_tolls_amount",
                                   "improvement_surcharge",
                                   "avg_congestion_surcharge",
                                   "avg_airport_fee"
                                 ), ]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                 variable %in% c(
                                   "avg_base_passenger_fare",
                                   "avg_tolls",
                                   "avg_bcf",
                                   "avg_sales_tax",
                                   "avg_tips",
                                   "avg_congestion_surcharge",
                                   "avg_airport_fee"
                                 ), ]
      }
    } else if (input$fareMetric0 == 'Total Fare') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                  !variable %in% c(
                                    "extra_per_driver",
                                    "total_driver_pay",
                                    "pay_per_driver",
                                    "fare_per_driver",
                                    "tips_per_driver",
                                    "driver_pay",
                                    "avg_distance",
                                    "count_drivers",
                                    "avg_total_fare",
                                    "sum_total_fare",
                                    "avg_fare_amount",
                                    "avg_extra",
                                    "avg_tip_amount",
                                    "avg_tolls_amount",
                                    "improvement_surcharge",
                                    "avg_congestion_surcharge",
                                    "avg_airport_fee",
                                    "mta_tax"
                                  ), ]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                 !variable %in% c(
                                   "extra_per_driver",
                                   "total_driver_pay",
                                   "pay_per_driver",
                                   "fare_per_driver",
                                   "tips_per_driver",
                                   "driver_pay",
                                   "avg_distance",
                                   "count_drivers",
                                   "avg_total_fare",
                                   "sum_total_fare",
                                   "avg_fare_amount",
                                   "avg_extra",
                                   "avg_tip_amount",
                                   "avg_tolls_amount",
                                   "improvement_surcharge",
                                   "avg_congestion_surcharge",
                                   "avg_airport_fee",
                                   "mta_tax"
                                 ), ]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                 !variable %in% c(
                                   'sum_driver_pay',
                                   "total_driver_pay",
                                   "pay_per_driver",
                                   "tips_per_driver",
                                   "driver_pay",
                                   "avg_distance",
                                   "count_drivers",
                                   "sum_total_fare",
                                   "avg_total_fare",
                                   "avg_base_passenger_fare",
                                   "avg_tolls",
                                   "avg_bcf",
                                   "avg_sales_tax",
                                   "avg_tips",
                                   "avg_tip",
                                   "avg_congestion_surcharge",
                                   "avg_airport_fee"
                                 ), ]
      }
    } else if (input$fareMetric0 == 'Driver Pay') {
      if (input$fareMetric1 == 'Yellow') {
        setDT(yellow_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                  variable %in% c("driver_pay",
                                                  "pay_per_driver",
                                                  "tips_per_driver",
                                                  "avg_tip_amount"), ]
      } else if (input$fareMetric1 == 'Green') {
        setDT(green_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                 variable %in% c("driver_pay",
                                                 "pay_per_driver",
                                                 "tips_per_driver",
                                                 "avg_tip_amount"), ]
      } else if (input$fareMetric1 == 'HVFHV') {
        setDT(hvfhv_fare_melt)[year_month == floor_date(input$year12, 'month') &
                                 variable %in% c("driver_pay",
                                                 "pay_per_driver",
                                                 "tips_per_driver",
                                                 "avg_tips"), ]
      }
      
    }
    
    
  })
    
  
  

  
    
    

  

  

  
# Financial Dashboard - rendering text ----------------------
    
  output$textbox1 = renderText({
    paste(
      "The Fare breakdown section covers two industries (Yellow and High Volume FHV or HVFHV). You can look at either Average Fare or Total Fare per month. Different industries have different fare breakdowns. For example, FHVs have sales tax as a separate category, the Yellow industry does not have that (instead, you will see 'N/A' whenever an industry is not reflected in a category).",
      "*Note that the totals above do not match sums of its parts.",
      "For the Average Fare that is because the parts of the fare that are listed in the dropdown are averages within their own categories. For example, the Congestion Surcharge is always $2.50 for the Yellow industry, however not all Yellow trips include Congestion Surcharge.",
      "For the Total Fare, the sum of its parts might be higher because some surcharges are included in the flat fares (like Manhattan to JFK) and therefore double-counted in this calculation.",
      sep = "\n"
    )
  })
    

    
# Financial Dashboard - Main Graph section - Rendering Graph Start  ----------------------------------------------------    
  output$mainGraph1 <- renderEcharts4r({
    if (input$fareMetric0 == 'Driver Pay') {
      if (input$average_pay_line) {
        title = 'Average Driver Pay and Tips per Trip'
        td <-
          data_fin_line() %>% dplyr::filter(variable %in% c('driver_pay', 'avg_tip_amount', 'avg_tips'))
      } else {
        title = 'Average Driver Pay and Tips per Month'
        td <-
          data_fin_line() %>% dplyr::filter(variable %in% c('pay_per_driver', 'tips_per_driver'))
      }
      
    } else {
      if (input$check_driver_pay & input$fareMetric0 == 'Total Fare') {
        title = 'Sum of Monthly Total Fares'
        td <-
          data_fin_line() %>% dplyr::filter(
            variable %in% c(
              'sum_total_fare',
              'avg_total_fare',
              'driver_pay',
              'total_driver_pay',
              'avg_tip_amount',
              'avg_tips',
              'sum_tips',
              'sum_tip_amount'
            )
          )
      } else {
        title = 'Average Total Fare'
        td <-
          data_fin_line() %>% dplyr::filter(variable %in% c('sum_total_fare', 'avg_total_fare'))
      }
    }
    
    
    print(td)
    
    colnames(td)[3] <- 'count'
    
    td$variable <- gsub('_', ' ', td$variable)
    td$variable <- str_to_title(td$variable)
    
    td <- td %>% dplyr::group_by(variable) %>%
      dplyr::arrange(year_month, desc(count)) %>%
      e_charts(year_month) %>%
      e_bar(count) %>%
      e_title(title) %>%
      e_theme("westeros") %>%
      e_tooltip(trigger = 'axis') %>%
      e_datazoom(type = 'slider') %>%
      e_legend(show = F)
    
    
    
  })
# Financial Dashboard - Main Graph section - Rendering Graph End  ---------------------------------------------------- 
  
# Financial Dashboard -Breakdown html numbers rendering Start  ----------------------------------------------------
    
  output$fin_num_total_lable = renderText({
    paste0(input$fareMetric0,
           ' amount in the ',
           input$fareMetric1,
           ' industry (click + for more).')
  })
    
  output$fin_num_total = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      paste0(
        '$',
        format(round(as.numeric(
          ifelse(
            input$fareMetric0 == 'Average Fare',
            data_fin()$avg_base_passenger_fare + data_fin()$avg_tolls + data_fin()$avg_bcf + data_fin()$avg_sales_tax + data_fin()$avg_tips + 2.50 + 2.75,
            data_fin()$sum_base_passenger_fare + data_fin()$sum_tolls + data_fin()$sum_bcf + data_fin()$sum_sales_tax + data_fin()$sum_tips + data_fin()$sum_congestion_surcharge + data_fin()$sum_airport_fee
          )
        ), 2), big.mark = ","),
        span(
          ifelse(
            input$fareMetric0 == 'Average Fare',
            paste0(' or $', format(round(
              as.numeric((
                data_fin()$avg_base_passenger_fare + data_fin()$avg_tolls + data_fin()$avg_bcf + data_fin()$avg_sales_tax + data_fin()$avg_tips + 2.50 + 2.75
              ) / data_fin()$avg_distance
              ), 2
            ), big.mark = ","), ' per Mile'),
            paste0('')
          ),
          style = 'font-size:16px; color: blue'
        )
      )
    } else{
      paste0(
        '$',
        format(round(as.numeric(data_fin(
          
        )[1, 1]), 2), big.mark = ","),
        span(
          ifelse(
            input$fareMetric0 == 'Average Fare',
            paste0(' or $', format(round(
              as.numeric(data_fin()[1, 1] / data_fin()$avg_distance), 2
            ), big.mark = ","), ' per Mile'),
            paste0('')
          ),
          style = 'font-size:16px; color: blue'
        )
      )
    }
    
    
    
  })
    
  output$fin_num_total_lable_driver_pay = renderText({
    paste0(input$fareMetric0,
           ' amount in the ',
           input$fareMetric1,
           ' industry (click + for more).')
  })
    
    
    
  output$fin_num_total_driver_pay = renderText({
    paste0('$', format(round(as.numeric(data_fin(
    )[1, 1]), 2), big.mark = ","))
    
    
  })
    
    
  output$fin_num_tips_driver_pay = renderText({
    paste0('$', format(round(as.numeric(data_fin(
    )[1, 2]), 2), big.mark = ","))
    
  })
    
    
  output$fin_num_extra_driver_pay = renderText({
    if (input$fareMetric1 != 'HVFHV') {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 3]), 2), big.mark = ","))
    } else {
      print('N/A')
    }

  })
    

    
  output$fin_num_fare_amount = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 1]), 2), big.mark = ","))
    } else {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 2]), 2), big.mark = ","))
    }
  })
    
    
  output$fin_num_fare_amount_driver_pay = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      print('N/A')
    } else {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 4]), 2), big.mark = ","))
    }
  })
    

  output$fin_num_extra = renderText({
    ifelse(input$fareMetric1 == 'HVFHV',
           print('N/A'),
           paste0('$', format(round(
             as.numeric(data_fin()[1, 3]), 2
           ), big.mark = ",")))
  })
    
  output$fin_num_toll = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 2]), 2), big.mark = ","))
    } else {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 5]), 2), big.mark = ","))
    }
  })
  
    
  output$fin_num_mta = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      print('N/A')
    } else {
      if (input$fareMetric0 == 'Average Fare') {
        print('$0.50')
      } else if (input$fareMetric0 == 'Total Fare') {
        paste0('$', format(round(
          as.numeric(data_fin()$total_mta_tax), 2
        ), big.mark = ","))
      }
    }
    
  })
    
  output$fin_num_tif = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      print('N/A')
    } else {
      if (input$fareMetric0 == 'Average Fare') {
        print('$0.30')
      } else if (input$fareMetric0 == 'Total Fare') {
        paste0('$', format(round(
          as.numeric(data_fin()$sum_improvement_surcharge), 2
        ), big.mark = ","))
      }
    }
  })
    
  output$fin_num_bcf = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      ifelse(input$fareMetric0 == 'Average Fare',
             paste0('$', format(round(
               as.numeric(data_fin()$avg_bcf), 2
             ), big.mark = ",")),
             paste0('$', format(round(
               as.numeric(data_fin()$sum_bcf), 2
             ), big.mark = ",")))
    } else {
      print('N/A')
    }
    
  })
    
  output$fin_num_tax = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      ifelse(input$fareMetric0 == 'Average Fare',
             paste0('$', format(round(
               as.numeric(data_fin()$avg_sales_tax), 2
             ), big.mark = ",")),
             paste0('$', format(round(
               as.numeric(data_fin()$sum_sales_tax), 2
             ), big.mark = ",")))
    } else {
      print('N/A')
    }
  })
  
  output$fin_num_tip = renderText({
    if (input$fareMetric1 == 'HVFHV') {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 5]), 2), big.mark = ","))
    } else {
      paste0('$', format(round(as.numeric(data_fin(
      )[1, 4]), 2), big.mark = ","))
    }
  })
   
    
    
  output$fin_num_congestion = renderText({
    if (input$fareMetric0 == 'Average Fare') {
      ifelse(input$fareMetric1 == 'Yellow',
             print('$2.50'),
             print('$2.75'))
    } else if (input$fareMetric0 == 'Total Fare') {
      paste0('$', format(round(
        as.numeric(data_fin()$sum_congestion_surcharge), 2
      ), big.mark = ","))
    }
  })
    
  output$fin_num_airport = renderText({
    if (input$fareMetric0 == 'Average Fare') {
      ifelse(
        input$fareMetric1 == 'Yellow',
        print('$1.25'),
        ifelse(input$fareMetric1 == 'Green', print('N/A'), print('$2.50'))
      )
    } else if (input$fareMetric0 == 'Total Fare') {
      ifelse(input$fareMetric1 == 'Green',
             print('N/A'),
             paste0('$', format(round(
               as.numeric(data_fin()$sum_airport_fee), 2
             ), big.mark = ",")))
    }
  })
    

    
  output$fin_num_fare_amount_lable = renderText({
    "Base Fare"
  })
  
  output$fin_num_extra_lable = renderText({
    "Tolls"
  })
  
  output$fin_num_toll_lable = renderText({
    "Black Car Fund"
  })
  
  output$fin_num_mta_lable = renderText({
    "MTA Tax"
  })
  
  output$fin_num_tif_lable = renderText({
    "Taxi Improvement Fund"
  })
  
  output$fin_num_tip_lable = renderText({
    "Tips"
  })
  
  output$fin_num_congestion_lable = renderText({
    "Congestion Surcharge"
  })
  
  output$fin_num_airport_lable = renderText({
    "Airport Fee"
  })
    
# Financial Dashboard -Breakdown html numbers rendering End  ----------------------------------------------------    
    
    
# Financial Dashboard -Pie charts rendering Start  ----------------------------------------------------    
    
  output$pie1 <- renderEcharts4r({
    if (input$fareMetric0 == 'Driver Pay') {
      if (input$driver_pay_avg_pie) {
        td <-
          data_fin_pie() %>% dplyr::filter(variable %in% c('avg_tip_amount', 'driver_pay', 'avg_tips'))
      } else {
        td <-
          data_fin_pie() %>% dplyr::filter(variable %in% c('tips_per_driver', 'pay_per_driver'))
      }
    } else {
      td <- data_fin_pie()
    }
    
    td$variable <- gsub('_', ' ', td$variable)
    td$variable <- str_to_title(td$variable)
    
    td |>
      
      e_charts(variable) |>
      e_pie(value) %>% e_legend(type = 'scroll') |>
      e_tooltip(formatter = e_tooltip_pie_formatter("percent")) |>
      e_grid(bottom = '15%') |>
      e_legend(type = 'scroll', bottom = '5')
    
  })
    
    
# Financial Dashboard -Pie charts rendering End  ----------------------------------------------------   

    
    
  
# Databank Dashboard - rendering Industry Ind Table ----------------------
output$mytable = renderDataTable({
  industry_metrics
})

    
    
# DataBank- rendering Trip Data Table ----------------------
  output$pu = DT::renderDataTable({
    DT::datatable(data1)
    
  })
    

# DataBank- rendering Industry Indicators Data Table ----------------------    
  output$MM = DT::renderDataTable({
    DT::datatable(industry_metrics, options = list(scrollX = TRUE))
  })
    
# DataBank- rendering DataDictionary Table ----------------------     
  output$dict = DT::renderDataTable({
    DT::datatable(dict,
                  class = 'cell-border stripe',
                  options = list(columnDefs = list(list(
                    className = 'dt-left', targets = 2
                  ))))
  })
  
  
# DataBank- download dataset func Start ----------------------    
  output$downloadData1 = downloadHandler(
    filename = function() {
      paste('PickUps', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      # dbExecute(mydb,paste("INSERT INTO monitor_download
      #                                       (count, time)
      #                                       VALUES('pu','",Sys.time(),"');", sep=""))
      write.csv(data1, con, row.names = F)
    }
  )
    

    
  output$downloadData4 = downloadHandler(
    filename = function() {
      paste('Monthly_Metrics', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      # dbExecute(mydb,paste("INSERT INTO monitor_download
      #                                       (count, time)
      #                                       VALUES('mtr','",Sys.time(),"');", sep=""))
      write.csv(industry_metrics, con, row.names = F)
    }
  )
# DataBank- download dataset func End ----------------------    
    
  
# Easter Egg Func--------------------------------------------------
  
  output$easterEgg <- renderText({
    HTML(
      paste0(
        "<div style='color:#f9fafc; text-align: center; font-family: monospace;
    font-style: italic  '>",
        paste0(
          'by',
          br(),
          br(),
          'Nikita Voevodin',
          br(),
          br(),
          '&',
          br(),
          br(),
          'Fausto Lopez',
          sep = '<br/><br/>'
        ),
        "</div>"
      )
    )
  })
    

    

    

    
})

# Server Logic Start ---------------------------------------------------
