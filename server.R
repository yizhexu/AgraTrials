function(input, output) {
  
  ###############################
  #### Map Creation Function ####
  ###############################
  
  create_poly <- function(year, week, att){
    #colorData <- ifelse(weekData()[,att] > max, 
    #                    max, 
    #                    ifelse(weekData()[,att] < min, 
    #                           min, weekData()[,att]))
    colorData <- weekData()[,att]
    newPalette <- if(att == "avgPrecipWeek" | att == "avgPrecipWeek3Year" | att == "avgPrecipWeek10Year" | att == "diffPrecipWeek3Year" | att == "diffPrecipWeek10Year"){
      newPalette <- rev(palette)
    } else {newPalette <- palette}
    color <- colorRampPalette(newPalette)
    colorData <- data.frame(color = color(length(colorData))[rank(colorData)])
    rownames(colorData) <- rownames(weekData()) 
    SpatialPolygonsDataFrame(polys,colorData)
  }
  
  ####################
  #### Map Output ####
  ####################
  
  output$map_all <- renderLeaflet({
    m = leaflet::leaflet() %>% addTiles()  
    m = m %>% addPolygons(data = colorPoly(), weight=0.1, color = ~color, fillOpacity = 0.4) 
    m = m %>% addCircleMarkers(data = dataExport, 
                               lng = ~Longitude, 
                               lat = ~Latitude, 
                               popup = ~Farmer, 
                               radius = ~GrainYield, 
                               color = "coral", 
                               fillOpacity = 0,
                               fill = FALSE)
    m
  })
  
  output$map_farmer <- renderLeaflet({
    m = leaflet::leaflet() %>% addTiles()
    m = m %>% addCircleMarkers(data = dataExport, 
                               lng = ~Longitude, 
                               lat = ~Latitude, 
                               popup = ~Farmer, 
                               radius = ~GrainYield, 
                               color = "coral", 
                               fillOpacity = 0,
                               fill = FALSE)
    m = m %>% setView(lng = unique(farmer_data()$Longitude), 
                      lat = unique(farmer_data()$Latitude), 
                      zoom = 15)
    m
  })
  
  output$map_farmer2 <- renderLeaflet({
    m = leaflet::leaflet() %>% addTiles()
    m = m %>% addCircleMarkers(data = dataExport, 
                               lng = ~Longitude, 
                               lat = ~Latitude, 
                               popup = ~Farmer, 
                               radius = ~GrainYield, 
                               color = "coral", 
                               fillOpacity = 0,
                               fill = FALSE)
    m = m %>% setView(lng = unique(farmer_data2()$Longitude), 
                      lat = unique(farmer_data2()$Latitude), 
                      zoom = 15)
    m
  })
  
  ##########################
  #### DataTable Output ####
  ##########################
  
  output$farmer_table <- renderDataTable({
    data_farmer[, input$farmer_vars, drop = FALSE]
  },options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                   pageLength = 5) 
  )
  
  output$farmer_datatable <- renderDataTable({
    farmer_data()[,c(13,22,26,35:36), with = FALSE]
  },options = list(paging = FALSE) 
  )
  
  #########################
  #### Reactive Output ####
  #########################
  
  farmer_weather <- reactive({
    file <- input$farmer_select
    load(paste0("./data/farmer/",file,".RData"))
    weekData
  })
  
  farmer_data <- reactive({
    ok <- dataExport$Farmer == input$farmer_select
    data <- dataExport[ok == TRUE,]
    data
  })
  
  farmer_data2 <- reactive({
    ok <- dataExport$Farmer == input$farmer_select2
    data <- dataExport[ok == TRUE,]
    data
  })
  
  soilTargetDepth <- reactive({
    depth <- soilContent$Depth == input$depth_select
    soilTarget <- soilContent[depth == TRUE, ]
    ok <- dataExport$Farmer == input$farmer_select2
    data <- dataExport[ok == TRUE, ]
    soilTarget[x < unique(data$Longitude) + 5/36/60/2 & x > unique(data$Longitude) - 5/36/60/2]
  })
  
  
  weekData <- reactive({ 
    date <- input$date_select
    file <- paste("./data/weather/week",year(date),paste0(week(date),".RData"),sep = "-")
    load(file)
    weekData
  })
  
  colorPoly <- reactive({
    date <- input$date_select
    att <- input$weather_vars
    file <- paste("./data/weather/week",year(date),paste0(week(date),".RData"),sep = "-")
    load(file)
    min <- min(weekData[,att])
    max <- max(weekData[,att])
    parameter <- c(year(date),week(date),min,max,att)
    create_poly(parameter[1],parameter[2],parameter[5])
  })
  
  #####################
  #### Plot Output ####
  #####################
  
  output$bar_precip <- renderPlot({
    if( input$compare_term == "3 Years"){
      bar_precip <- ggplot(farmer_weather()) +
        geom_bar(aes(w, avgPrecipWeek), 
                 stat = "identity", 
                 fill = "deepskyblue2", 
                 alpha = 0.5) +
        annotate("text", farmer_weather()$w, 
                 farmer_weather()$avgPrecipWeek, 
                 label = round(farmer_weather()$avgPrecipWeek, digits = 1), 
                 size = 3, colour = "deepskyblue2", vjust = -0.7) +
        geom_bar(aes(w, avgPrecipWeek3Year), 
                 stat = "identity", 
                 fill = "gray88", 
                 alpha = 0.5) +
        scale_x_continuous(breaks = farmer_weather()$w, labels = farmer_weather()$w) + 
        ggtitle(paste("Accumulative Precipitation During Trial Weeks \nCompared to", input$compare_term, "Norm (mm)", "for", input$farmer_select, 
                      "\n Trials run from", farmer_data()$PlantingDate, "to", farmer_data()$HarvestingDate)) +
        theme(legend.position = "bottom", 
              legend.background = element_blank(),
              panel.background = element_blank(), 
              plot.background = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_blank(), 
              axis.ticks = element_blank(),  
              axis.title = element_blank())
      bar_precip
    } else{
      bar_precip <- ggplot(farmer_weather()) +
        geom_bar(aes(w, avgPrecipWeek), 
                 stat = "identity", 
                 fill = "deepskyblue2", 
                 alpha = 0.5) +
        annotate("text", farmer_weather()$w, 
                 farmer_weather()$avgPrecipWeek, 
                 label = round(farmer_weather()$avgPrecipWeek, digits = 1), 
                 size = 3, colour = "deepskyblue2", vjust = -0.7) +
        geom_bar(aes(w, avgPrecipWeek10Year), 
                 stat = "identity", 
                 fill = "gray88", 
                 alpha = 0.5) +
        scale_x_continuous(breaks = farmer_weather()$w, labels = farmer_weather()$w) + 
        ggtitle(paste("Accumulative Precipitation During Trial Weeks \nCompared to", input$compare_term, "Norm (mm)", "for", input$farmer_select, 
                      "\n Trials run from", farmer_data()$PlantingDate, "to", farmer_data()$HarvestingDate)) +
        theme(legend.position = "bottom", 
              legend.background = element_blank(),
              panel.background = element_blank(), 
              plot.background = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_blank(), 
              axis.ticks = element_blank(),  
              axis.title = element_blank())
      bar_precip
    }
  })
  
  output$tile_precip <- renderPlot({
    if( input$compare_term == "3 Years"){
      tile_precip <- ggplot(farmer_weather()) +
        geom_tile(aes(w, 1, fill = diffPrecipWeek3Year), 
                  colour = "white") +
        geom_text(aes(w, 1, label = round(diffPrecipWeek3Year, digits = 0)),
                  size = 3, colour = "deepskyblue2") +
        scale_fill_gradient2(limits = a_pre, 
                             breaks = a_pre, 
                             low = "firebrick2", 
                             mid = "grey88", 
                             high = "deepskyblue4", 
                             oob = squish, 
                             labels = c(a_pre[1], a_pre[2]), 
                             guide = guide_colorbar(title = "Absol. rainfall difference (mm)", title.position = "left")) +
        scale_x_continuous(breaks = farmer_weather()$w, labels = farmer_weather()$w) + 
        theme(legend.key = element_blank(),
              legend.position = "bottom",
              legend.background = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              axis.text.x = element_text(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),  
              axis.title = element_blank())
      tile_precip
    } else {
      tile_precip <- ggplot(farmer_weather()) +
        geom_tile(aes(w, 1, fill = diffPrecipWeek10Year), 
                  colour = "white") +
        geom_text(aes(w, 1, label = round(diffPrecipWeek10Year, digits = 0)),
                  size = 3, colour = "deepskyblue2") +
        scale_fill_gradient2(limits = a_pre, 
                             breaks = a_pre, 
                             low = "firebrick2", 
                             mid = "grey88", 
                             high = "deepskyblue4", 
                             oob = squish, 
                             labels = c(a_pre[1], a_pre[2]), 
                             guide = guide_colorbar(title = "Absol. rainfall difference (mm)", title.position = "left")) +
        scale_x_continuous(breaks = farmer_weather()$w, labels = farmer_weather()$w) + 
        theme(legend.key = element_blank(),
              legend.position = "bottom",
              legend.background = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              axis.text.x = element_text(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),  
              axis.title = element_blank())
      tile_precip
    }
  })
  
  dataExport_subset <- dataExport[,c(1:3,8,11,13,22,34:36), with = FALSE]
  
  dataExport_subset$id <- 1:nrow(dataExport_subset)
  all_tooltip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- dataExport_subset[dataExport_subset$id == x$id,]
    paste0(names(row), ": ",format(row), collapse = "<br />")
  }
  
  reactive({
    ok <- dataExport_subset$Farmer == input$farmer_select
    dataExport_subset[ok == TRUE,]
  }) %>%
    ggvis( ~jitter(Treat), ~GrainYield) %>%
    layer_points(fill = ~factor(Variety), shape := "diamond", stroke := "gold", strokeWidth := 3) %>%
    layer_points( ~jitter(Treat), ~GrainYield, key := ~id, data = dataExport_subset, size = 3, fill = ~factor(Variety), opacity := 0.5) %>%
    add_tooltip(all_tooltip, "hover") %>%
    add_axis("x", title = "Treatment Groups", ticks = 6) %>%
    add_axis("x", title = "Compare Farmer's Grain Yield to All Trials", orient = "top", ticks = 0, 
             properties = axis_props(
               axis = list(stroke = "white"),
               title = list(fontSize = 14),
               labels = list(fontSize = 0)
             )) %>%
    add_legend("fill", title = "Type of Variety") %>%
    bind_shiny("plot") 
  
  output$soil_texture <- renderPlot({
    
    ggplot() +
      coord_tern(L="x",T="y",R="z") +
      geom_polygon(data = USDA, aes(y=Clay, x=Sand, z=Silt, color = Label, fill = Label), alpha = 0.75, size = 0.5, color = 'black') +
      geom_point(data = soilTargetDepth(), aes(y=Clay, x=Sand, z=Silt, color = Depth), size = 0.5, alpha = 0.70) +
      geom_text(data = USDA.LAB, aes(y=Clay, x=Sand, z=Silt, color = Label, fill = Label, label = Label, angle = Angle), color = 'black', size = 3.5) +
      theme_rgbw() +
      theme_showsecondary() +
      theme_showarrows() +
      custom_percent("Percent") +
      theme(legend.position = "left",
            axis.tern.padding = unit(0.15, 'npc')) +
      labs(title = 'USDA Textural Classification Chart',
           fill  = c('Textural Class'),
           color = 'Depth')
    
  })
  
  ##############################
  #### Dashboard Box Output ####
  ##############################
  
  output$trials_year <- renderValueBox({
    start = min(farmer_data()$PlantingDate)
    season = ifelse(unique(farmer_data()$Season) == "Long rain", "L", "S")
    valueBox(
      paste0(year(start), ": ",season) , "Year of Planting: Season", icon = icon("cloud", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$trials_status <- renderValueBox({
    start = max(farmer_data()$PlantingDate)
    end = max(farmer_data()$HarvestingDate)
    status = ifelse(end < Sys.Date(), "Fin", "On-Going")
    valueBox(
      paste(end-start,status, sep = ", ") , "Days, Status", icon = icon("calendar", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$n_trails <- renderValueBox({
    trials <- dim(farmer_data())[1]
    valueBox(
      trials, "Number of Trials", icon = icon("tree-deciduous", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$high_trial <- renderValueBox({
    high_trial <- arrange(farmer_data(),desc(GrainYield))[1,]
    valueBox(
      paste(high_trial$GrainYield, sep = ", "), "Best Grain Yield", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue"
    )
    
  })
  
  #####################
  #### Text Output ####
  #####################
  
  output$weather_att_info <- renderUI({
    att <- input$weather_vars
    str1 <- paste0("The attribute you have selected is ", att, ".")
    str2 <- if(att == "avgMaxTempWeek") {
      paste0("This variable is the highest average air temperature recorded during the week of ", input$date_select, ". The unit is °C.")} else if (att == "avgMinTempWeek") {
        paste0("This variable is the lowest average air temperature recorded during the week of ", input$date_select, ". The unit is °C.")} else if (att == "avgTempWeek") {
          paste0("This variable is the daily average air temperature recorded during the week of ", input$date_select, ". The unit is °C.")} else if (att == "avgPrecipWeek") {
            paste0("This variable is the total accumulated precipitation from the first day of the week of ", input$date_select, ". The unit is millimetres.")} else if (att == "diffPrecipWeek3Year") {
              paste0("This variable is the difference between the total accumulated precipitation from the first day of the week of ", input$date_select, ", compare to the precipitation of the same time period 3 years ago. The unit is millimetres.")} else {
                paste0("This variable is the difference between the total accumulated precipitation from the first day of the week of ", input$date_select, ", compare to the precipitation of the same time period 10 years ago. The unit is millimetres.")}
    HTML(paste(str1, str2, sep = '<br/><br/>'))
  })
  
  output$range_info <- renderUI({
    att <- input$weather_vars
    str1 <- paste0("<br/>Below are summary statistics for ", att, ".")
    str2 <- paste("Range: <br/>", format(min(weekData()[,att],na.rm = TRUE),digits = 2), "to ", format(max(weekData()[,att],na.rm = TRUE),digits = 2))
    str3 <- paste("Standard deviation: <br/>", format(sd(weekData()[,att],na.rm = TRUE),digits = 2))
    str4 <- paste("Mean: <br/>", format(mean(weekData()[,att],na.rm = TRUE),digits = 2))
    str5 <- paste("Mode: <br/>", format(median(weekData()[,att],na.rm = TRUE),digits = 2))
    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/><br/>'))
  })
  
  output$tick_info <- renderUI({
    HTML(paste("1 = Control; 2 = Urea; 3 = TSP + urea", "<br/>", "4 = Minjingu powder + urea; 5 = Minjingu granular + urea; 6 = Minjingu mazao + urea; "))
  })
}