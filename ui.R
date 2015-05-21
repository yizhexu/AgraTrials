header <- dashboardHeader(
  title = "AGRA-SHP Trials"
)

sideBar <- dashboardSidebar(
  sidebarMenu(
    menuItem("All Trials", tabName = "trials", icon = icon("pagelines")),
    menuItem("Farm Overview", tabName = "farmer", icon = icon("leaf")),
    menuItem("Soil on Farm", tabName = "soil", icon = icon("globe"))
  ),
  div(style = "padding-left: 15px; padding-top: 20px; padding-right: 15px",
      p(class = "small", "Made with ",
        a("R", href = "http://www.r-project.org/"),
        ", ",
        a("Shiny", href = "http://shiny.rstudio.com/"),
        ", ",
        a("shinydashboard", href = "http://rstudio.github.io/shinydashboard/"),
        ", ",
        a("ggplot2", href = "http://ggplot2.org/"),
        ", &",
        a("leaflet", href = "http://leafletjs.com/")
      ),
      p(class = "small", "Trials data provided by ",
        a("Alliance for a Green Revolution in Africa (AGRA) - Soil Health Programme", href="http://agra-alliance.org/"), 
        br(), " Soil data provided by ",
        a("Africa Soil Information Service (AfSIS)", href = "http://africasoils.net/"),
        br(), " Weather data provided by ", 
        a("aWhere Inc.", href = "http://www.awhere.com/")
        
      ),
      p(class = "small", "This dashboard gives users the opportunity to do in-depth exploration of a subset of trials data from the AGRA Soil Health Programme, and leverage third-party environmental data resources to help explain variation in outcomes. The AGRA SHP trials were undertaken to systematically study the effects of 6 different soil treatments on crop yields across a number of East African countries. The data used for this dashboard includes only Mother Demos of maize in Uganda. The demos were conducted from 2011-2013 during the short and long rainy seasons."
      )
  )
  
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "trials",
			h2("All Trials Overview"),
            br(),
			div(p(class = "medium","The 'All Trials' tab provides a mapped view of all trial sites and tabulates the data collected from them at the bottom of the tab. Each point is sized according to its yield, relative to all other trial sites. A gridded overlay of weekly weather data from 2011-2013 is also provided on the map. Users should be careful in interpreting the spatial distributions of yields in relation to weather patterns, as all trial points are mapped in this tab regardless of season or year. To explore weather patterns particular to a trial site and growing season, click on the 'Farm Overview' tab."),
			br()),
			fluidRow(
              column(width = 8, 
			  box(
                title = "All Maize Trials - Map View", status = "primary", solidHeader = TRUE, width = 12,
                collapsible = TRUE,
                leafletOutput("map_all", height = 1000)
              )),
			  column(width = 4,
              box(
                title = "Weather Parameters", status = "warning", solidHeader = TRUE, width = 12,
                selectInput(inputId = "weather_vars",
                            label = "Weather Attribute",
                            choices = c("avgMaxTempWeek","avgMinTempWeek","avgTempWeek","avgPrecipWeek","diffPrecipWeek3Year","diffPrecipWeek10Year"), 
                            selected = "diffPrecipWeek3Year"),
                dateInput(inputId = "date_select", 
                          label = "Time", value = "2013-01-20", min = "2011-01-01", max = "2013-12-31", format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en")
              ),
			  box(
                title = "Weather Parameters Metadata", status = "warning", solidHeader = TRUE, width = 12,
                htmlOutput(outputId = "weather_att_info"),
                htmlOutput(outputId = "range_info")
              )
              
			  )
            ),
            fluidRow(
              box(
                title = "All Maize Trials - Table View", solidHeader = TRUE, status = "primary", width = 12,
                selectInput(inputId = 'farmer_vars', 
                            label = 'Columns to Shows',
                            choices = names(data_farmer), 
                            selected = c("FarmerId", "plotSize", "harvestArea", "variety", "meanGrainYield"), 
                            multiple = TRUE), 
                dataTableOutput(outputId = "farmer_table")))
    ), 
    tabItem(tabName = "farmer", 
            h2("Farm Overview"),
            br(),
			div(p(class = "medium",
			   "This board and the following ('Soil on Farm') explores specifics of individual trial sites. Here you can select the farm site you wish to explore, visualize it on the map, and generate a display showing its accumulative precipitation over the season. You can also set either a 3-year or 10-year norm to compare to the actual season's rainfall, helping understand whether the farmer is facing increasingly drier or wetter seasons."), 
			   br()
			),
			fluidRow(
              valueBoxOutput("trials_year", width = 3), 
              valueBoxOutput("trials_status", width = 3),
              valueBoxOutput("n_trails", width = 3),
              valueBoxOutput("high_trial", width = 3)
              
            ), 
            fluidRow(
              box(
                title = "Maize Trials", solidHeader = TRUE, width = 5, status = "primary",
                selectInput(inputId = "farmer_select", 
                            label = "Which Farmer?", 
                            choices = gsub(".RData", "", list.files("./data/farmerId/")), 
                            selected = "Farmer-3"),
                selectInput(inputId = "compare_term", 
                            label = "How Many Years of Norm to Compare?",
                            choices = c("10 Years", "3 Years"), 
                            selected = "10 Years"),
                leafletOutput("map_farmer", height = 400),
                br()
              ),
              box(
                title = "Weather Condition During Trials", solidHeader = TRUE, width = 7, status = "warning",
                plotOutput(outputId = "bar_precip"), 
                plotOutput(outputId = "tile_precip", height = "140px"), 
                br()
              )
            ),
            fluidRow(
              box( 
                title = "Yield by Treatment & Repetition", solidHeader = TRUE, width = 12, status = "primary",
                div(align = "center", style="width:auto; height:auto;", ggvisOutput("plot")), 
                br(),
                div(align = "center", htmlOutput(outputId = "tick_info")),
                br()
              )
            ),
            fluidRow(
            div(align = "center", box( 
                title = "Yield by Treatment & Repetition", solidHeader = TRUE, width = 12, status = "primary",
                dataTableOutput(outputId = "farmer_datatable")))
            )
    ), 
    tabItem(tabName = "soil",
            h2("Soil Overview"),
			br(),
			
			fluidRow(
              box(
                title = "Soil Texture Parameters", solidHeader = TRUE, width = 4, status = "warning",
                selectInput(inputId = "farmer_select2", 
                            label = "Which Farmer?", 
                            choices = gsub(".RData", "", list.files("./data/farmerId/")), 
                            selected = "Farmer-3"),
                selectInput(inputId = "depth_select", 
                            label = "What Depth?", multiple = TRUE, 
                            choices = unique(soilContent$Depth), 
                            selected = c(unique(soilContent$Depth)[1],unique(soilContent$Depth)[6])),
                leafletOutput("map_farmer2", height = 400),
                br()
              ), 
              box(
                title = "Soil Texture Classification", solidHeader = TRUE, width = 8, status = "primary",
                plotOutput(outputId = "soil_texture"), 
				htmlOutput(outputId = "soil_explain")
              )
            )
    )
  )
)



dashboardPage(
  header,
  sideBar,
  body
)