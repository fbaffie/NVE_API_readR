# Handle libraries on Windows and Linux

if (.Platform$OS.type == "windows") {
  library('dplyr')
  library('readxl')
  library('shiny')
  library('shinydashboard')
  library('ggplot2')
  library('lubridate')
  library('plotly')
  library('GetoptLong')
  library('leaflet')
  library('jsonlite')
  library('raster')
  library('rgdal')
}

if (.Platform$OS.type == "unix") {
  library('dplyr')
  library('readxl')
  library('shiny')
  library('shinydashboard')
  library('ggplot2')
  library('lubridate')
  library('plotly')
  library('GetoptLong')
  library('leaflet')
  library('jsonlite')
  library('raster')
  library('rgdal')
}

# if (!require('shiny')) install.packages('shiny', repos = "http://cran.us.r-project.org"); library('shiny')
# if (!require('shinydashboard')) install.packages('shinydashboard', repos = "http://cran.us.r-project.org"); library('shinydashboard')
# if (!require('ggplot2')) install.packages('ggplot2', repos = "http://cran.us.r-project.org"); library('ggplot2')
# if (!require('lubridate')) install.packages('lubridate', repos = "http://cran.us.r-project.org"); library('lubridate')
# if (!require('plotly')) install.packages('plotly', repos = "http://cran.us.r-project.org"); library('plotly')
# if (!require('GetoptLong')) install.packages('GetoptLong', repos = "http://cran.us.r-project.org"); library('GetoptLong')
# if (!require('leaflet')) install.packages('leaflet', repos = "http://cran.us.r-project.org"); library('leaflet')
# if (!require('jsonlite')) install.packages('jsonlite', repos = "http://cran.us.r-project.org"); library('jsonlite')
# if (!require('raster')) install.packages('raster', repos = "http://cran.us.r-project.org"); library('raster')
# if (!require('rgdal')) install.packages('rgdal', repos = "http://cran.us.r-project.org"); library('rgdal')


# Add global variables  ----------------------------------------------------

source("global.R")


# User interface -----------------------------------------------------------

ui <- dashboardPage(
  
  
  # Dashboard header
  
  dashboardHeader(title = "Runoff data check"),
  
  
  # Dashboard sidebar
  
  dashboardSidebar(
    
    width = 270,
    
    # Dropdown menu for station selection
    
    selectInput(inputId = "stat_dropdown",
                label = "Select station:",
                choices = stats,
                selected = paste(df_meta$regine_area[1], df_meta$main_no[1], sep = "."),
                multiple = FALSE,
                selectize = FALSE),
    
    # Summary table of station properties
    
    verbatimTextOutput(outputId = "print_summary")
    
  ),
  
  
  # Dashboard body
  
  dashboardBody(
    
    fluidRow(
      
      
      # One column and two rows with time series and mass balance plots
      
      column(width = 7,
             box(
               title = NULL,
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               plotOutput("plot_runoff",
                          height = 300,
                          dblclick = "plot_runoff_dblclick",
                          brush = brushOpts(
                            id = "plot_runoff_brush",
                            direction = "x",
                            resetOnNew = TRUE
                          )
               )
             ),
             box(
               title = NULL,
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               plotOutput("plot_cumsums"),
               
               absolutePanel(id = "cumsums_controls",
                             draggable = FALSE,
                             top = 60, left = 100,
                             right = "auto", bottom = "auto",
                             width = 160, height = "auto",
                             
                             selectInput(inputId = "cumsums_disp",
                                         label = NULL,
                                         choices = c("Precipitation", "Ref runoff", "Ref yearly compare"),
                                         selected = "Precipitation")
                             
                             
                             
               )
             )
             
      ),
      
      
      # One column with map
      
      column(width = 5,
             
             leafletOutput(outputId = "stat_map",
                           height = 750),
             
             absolutePanel(id = "map_controls",
                           draggable = FALSE,
                           top = 10, left = "auto",
                           right = 40, bottom = "auto",
                           width = 150, height = "auto",
                           
                           selectInput(inputId = "map_disp",
                                       label = NULL,
                                       choices = c("Mean runoff", "Mean runoff (txt)", "Runoff efficiency", "Selected stations"),
                                       selected = "Mean runoff")
                           
             )
             
      )
      
    )
    
  )
  
)


# Server -------------------------------------------------------------------

server <- function(input, output, session) { 
  
  # Reactive values used for zooming in the runoff plots
  
  plot_runoff_ranges <- reactiveValues(x = NULL)
  
  
  # Reactive values for reference station
  
  ref_station <- reactiveValues(regine_main = NULL)
  
  
  
  # This observer handles the zooming of the plots
  
  observeEvent(input$plot_runoff_dblclick, {
    brush <- input$plot_runoff_brush
    if (!is.null(brush)) {
      plot_runoff_ranges$x <- c(brush$xmin, brush$xmax)
    } else {
      plot_runoff_ranges$x <- NULL
    }
  })
  
  # This observer updates the station selection and catchment boundary in map
  # when selecting a new station
  
  observeEvent(input$stat_dropdown, {
    
    istat <- which(stats == input$stat_dropdown)
    
    leafletProxy("stat_map", session) %>%
      
      addPolygons(data = data_main[[istat]]$wsh_poly,
                  weight = 2,
                  color = "red",
                  fill = FALSE,
                  opacity = 1,
                  layerId = "selected_poly") %>%
      
      addCircleMarkers(lng = df_meta$longitude[istat],
                       lat = df_meta$latitude[istat],
                       layerId = "selected_stat",
                       color = "black",
                       radius = 8,
                       stroke = TRUE,
                       opacity = 1,
                       fillOpacity = 0)
    
  })
  
  
  # This observer links the selection in the map with the selection in the dropdown menu
  
  observeEvent(input$stat_map_marker_click, {
    
    if(input$stat_map_marker_click$id != "selected_stat") {    # Nothing to do if selecting an already selected station
      
      plot_runoff_ranges$x <- NULL # Reset zoom level
      
      if (input$stat_map_marker_click$id == "ref_stat") {
        
        istat <- which(stats == ref_station$regine_main)   # Handle the case of selecting the reference runoff station
        
        regine_main <- ref_station$regine_main
        
      } else {
        
        istat <- which(stats == input$stat_map_marker_click$id)
        
        regine_main <- input$stat_map_marker_click$id
        
      }
      
      leafletProxy("stat_map", session) %>%
        
        addPolygons(data = data_main[[istat]]$wsh_poly,
                    weight = 2,
                    color = "red",
                    fill = FALSE,
                    opacity = 1,
                    layerId = "selected_poly") %>%
        
        addCircleMarkers(lng = df_meta$longitude[istat],
                         lat = df_meta$latitude[istat],
                         layerId = "selected_stat",
                         color = "black",
                         radius = 8,
                         stroke = TRUE,
                         opacity = 1,
                         fillOpacity = 0)
      
      updateSelectInput(session, "stat_dropdown",
                        label = "Select station:",
                        choices = stats,
                        selected = regine_main)
      
    }
    
  })
  
  
  # This observer is responsible for updating station markers when selecting
  # type of display in the map (for example "Mean runoff" or "Mean runoff (txt)")
  
  observe({
    
    map_disp <- input$map_disp
    
    plot_markers(map_disp, df_meta)
    
  })
  
  
  # Mass balance plots
  
  output$plot_cumsums <- renderPlot({
    
    # Note: This part does not work using a seperate function on linux
    
    if (!is.null(plot_runoff_ranges$x)) {
      plot_runoff_ranges$x <- as.Date(plot_runoff_ranges$x, origin = "1970-01-01")
    }
    
    istat <- which(stats == input$stat_dropdown)
    
    # Plot cumulative precipitation against cumulative runoff
    
    if (input$cumsums_disp == "Precipitation") {
      
      # Remove reference station from map
      
      leafletProxy("stat_map", session) %>%
        
        removeMarker(layerId = "ref_stat")
      
      # Get data and plot results
      
      df <- data.frame(date = data_main[[istat]]$time_vec,
                       runoff = data_main[[istat]]$Runoff,
                       prec = data_main[[istat]]$Prec)
      
      if (!is.null(plot_runoff_ranges$x)) {
        df <- with(df, df[(date >= plot_runoff_ranges$x[1] & date <= plot_runoff_ranges$x[2]), ])
      }
      
      df <- na.omit(df)
      
      runoff_acc <- cumsum(df$runoff)
      
      prec_acc <- cumsum(df$prec)
      
      df <- data.frame(runoff_acc = runoff_acc, prec_acc = prec_acc)
      
      ggplot(data = df, aes(x = runoff_acc, y = prec_acc)) +
        geom_smooth(method = 'lm', se = FALSE, size = 1, linetype = 2, col = "red") +
        geom_point() +
        xlab("Cumulative runoff (mm)") + 
        ylab("Cumulative precipitation (mm)")
      
    } 
    
    
    # Plot cumulative reference runoff against cumulative runoff for target station
    
    else if (input$cumsums_disp == "Ref runoff") {
    
      # Find closest reference runoff station (klimaserier)

      lat_sel <- df_meta$utm_north_z33[istat]

      lon_sel <- df_meta$utm_east_z33[istat]

      df_tmp <- df_meta[-istat, ]

      df_tmp <- df_tmp[which(df_tmp$br34_Hydrologisk_referanseserier_klimastudier == "Y"), ]

      df_tmp$dist = sqrt((df_tmp$utm_north_z33 - lat_sel)^2 + (df_tmp$utm_east_z33 - lon_sel)^2)

      stat_ref <- df_tmp$regine_main[which.min(df_tmp$dist)]

      # Find the reference station in the original dataset
      
      iref <- which(df_meta$regine_main == stat_ref)
      
      # Add reference station to reactive value
      
      ref_station$regine_main <- stat_ref
      
      # Display reference station in map
      
      leafletProxy("stat_map", session) %>%

        addCircleMarkers(lng = df_meta$longitude[iref],
                         lat = df_meta$latitude[iref],
                         layerId = "ref_stat",
                         color = "yellow",
                         radius = 8,
                         stroke = TRUE,
                         opacity = 1,
                         fillOpacity = 0)
      
      # Get the data and plot the results

      df <- data.frame(date = data_main[[istat]]$time_vec,
                       runoff_target = data_main[[istat]]$Runoff,
                       runoff_ref = data_main[[iref]]$Runoff)

      if (!is.null(plot_runoff_ranges$x)) {
        df <- with(df, df[(date >= plot_runoff_ranges$x[1] & date <= plot_runoff_ranges$x[2]), ])
      }

      df <- na.omit(df)

      runoff_target_acc <- cumsum(df$runoff_target)

      runoff_ref_acc <- cumsum(df$runoff_ref)
      
      name <- paste("Reference station",
                    data_main[[iref]]$metadata$regine_main,
                    data_main[[iref]]$metadata$station_name, sep = " - ")
      
      ggplot(data = df, aes(x = runoff_target_acc, y = runoff_ref_acc)) +
        geom_smooth(method = 'lm', se = FALSE, size = 1, linetype = 2, col = "red") +
        geom_point() +
        xlab("Cumulative runoff target (mm)") +
        ylab("Cumulative runoff reference (mm)") +
        ggtitle(label = name)
      
    }
    
    # Plot annual-mean daily runoff against the one of reference station
    
    else if (input$cumsums_disp == "Ref yearly compare") {
      
      #Get station markers
      
      lat_sel <- df_meta$utm_north_z33[istat]
      
      lon_sel <- df_meta$utm_east_z33[istat]
      
      df_tmp <- df_meta[-istat, ]
      
      df_tmp <- df_tmp[which(df_tmp$br34_Hydrologisk_referanseserier_klimastudier == "Y"), ]
      
      df_tmp$dist = sqrt((df_tmp$utm_north_z33 - lat_sel)^2 + (df_tmp$utm_east_z33 - lon_sel)^2)
      
      stat_ref <- df_tmp$regine_main[which.min(df_tmp$dist)]
      
      # Find the reference station in the original dataset
      
      iref <- which(df_meta$regine_main == stat_ref)
      
      # Add reference station to reactive value
      
      ref_station$regine_main <- stat_ref
      
      # Display reference station in map
      
      leafletProxy("stat_map", session) %>%
        
        addCircleMarkers(lng = df_meta$longitude[iref],
                         lat = df_meta$latitude[iref],
                         layerId = "ref_stat",
                         color = "yellow",
                         radius = 8,
                         stroke = TRUE,
                         opacity = 1,
                         fillOpacity = 0)
      
      
      if (!is.null(plot_runoff_ranges$x)) {
        plot_runoff_ranges$x <- as.Date(plot_runoff_ranges$x, origin = "1970-01-01")
      }
      
      istat <- which(stats == input$stat_dropdown)
      
      # Plotting the results
      
      name <- paste(data_main[[istat]]$metadata$regine_main, '-', 
                    data_main[[istat]]$metadata$station_name, '(black)' ,' vs. ',
                    data_main[[iref]]$metadata$regine_main, '-', 
                    data_main[[iref]]$metadata$station_name, '(red)')
      
      time_yearly <- ymd(data_main[[istat]]$time_yearly)
      
      runoff_yearly <- data_main[[istat]]$runoff_yearly
      
      runoff_ref <- data_main[[iref]]$runoff_yearly
      
      
      
      df <- data.frame(time = time_yearly, runoff = runoff_yearly, ref = runoff_ref)
      df <- na.omit(df)
      
      ggplot(data = df, aes(x = time)) + 
        geom_step(aes(y=runoff), col = "black", size = 0.5) +
        geom_step(aes(y=ref), col = "red", size = 0.5) +
        xlab("") +
        ylab("annual-mean daily runoff (mm/day)") +
        ggtitle(label = name) + 
        theme_set(theme_grey(base_size = 12))  +
        coord_cartesian(xlim = plot_runoff_ranges$x, expand = FALSE)   # NEWNEWNEW
      
    }
    
  })
  
  
  # Plot runoff time series
  
  output$plot_runoff <- renderPlot({
    
    # Note: This part does not work using a seperate function on linux
    
    if (!is.null(plot_runoff_ranges$x)) {
      plot_runoff_ranges$x <- as.Date(plot_runoff_ranges$x, origin = "1970-01-01")
    }
    
    istat <- which(stats == input$stat_dropdown)
    
    # Plotting the results
    
    name <- paste(data_main[[istat]]$metadata$regine_main,
                  data_main[[istat]]$metadata$station_name, sep = " - ")
    
    time <- ymd(data_main[[istat]]$time_vec)
    
    runoff <- data_main[[istat]]$Runoff
    
    df <- data.frame(time = time, runoff = runoff)
    
    ggplot(data = df, aes(x = time, y = runoff)) + 
      geom_line(col = "red", size = 1) +
      xlab("") +
      ylab("Runoff (mm/day)") +
      ggtitle(label = name) + 
      theme_set(theme_grey(base_size = 12))  +
      coord_cartesian(xlim = plot_runoff_ranges$x, expand = FALSE)   # NEWNEWNEW
    
    
  })
  
  
  # Plot map with stations
  
  output$stat_map <- renderLeaflet({
    
    plot_map(df_meta, data_main)
    
  })
  
  
  # Print summary table
  
  output$print_summary <- renderPrint({
    
    istat <- which(stats == input$stat_dropdown)
    
    print_summary(data_main, istat)
    
  })
  
}

shinyApp(ui, server)