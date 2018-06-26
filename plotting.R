


# Plot station markers on the map

plot_markers <- function(map_disp, df_meta) {
  
  
  # Markers display selected stations
  
  if (map_disp == "Selected stations") {
    
    # Process excel table

    filename <- "//hdata/fou/Avrenningskart/Serier_Avrenningskart_2017_ARB.xlsx"
    
    df_tmp <- read_metadata_file(filename)
    
    df_tmp$regine_main <- paste(df_tmp$regine_area, df_tmp$main_no, sep = ".")
    
    df_disp <- left_join(df_meta, df_tmp, by = "regine_main")
    
    selection <- rep(0, nrow(df_disp))
    
    selection[df_disp$aktuell_avrenningskart.y == "ja" | df_disp$aktuell_avrenningskart.y == "Ja"] <- 1
    
    selection[df_disp$aktuell_avrenningskart.y == "nei" | df_disp$aktuell_avrenningskart.y == "Nei"] <- 2
    
    selection[df_disp$aktuell_avrenningskart.y == "ikke vurdert"] <- 3
    
    df_disp$selection <- selection
    
    # Colors and legend label
    
    col_pal <- c("#EE7600", "#5ff442", "#f71f07", "#9932CC")
    
    col_breaks <- c(-Inf, 0.5, 1.5, 2.5, Inf)
    
    legend_str <- c("Unknown", "Selected", "Discarded", "Unevaluated")
    
    col_binned <- cut(df_disp$selection, col_breaks, labels = col_pal)
    
    # Update map
    
    leafletProxy("stat_map") %>%
      
      addCircleMarkers(lng = df_disp$longitude.x,
                       lat = df_disp$latitude.x,
                       layerId = paste(df_disp$regine_area.x, df_disp$main_no.x, sep = "."),
                       color = col_binned,
                       radius = 6,
                       stroke = FALSE,
                       opacity = 1,
                       fillOpacity = 1) %>%
      
      addLegend("bottomright",
                colors = col_pal,
                labels = legend_str,
                title = "Selected stations",
                layerId = "stat_legend") %>%
      
      hideGroup(group = "Precipitation")
    
  }

  
  # Markers display runoff efficiency
  
  if (map_disp == "Runoff efficiency") {
    
    # Colors and legend label
    
    col_pal <- c('#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')
    
    col_breaks <- c(0, 0.8, 1.2, 1.6, 2.0, Inf)
    
    legend_str <- c("< 0.8", "0.8 - 1.2", "1.2 - 1.6", "1.6 - 2.0", "> 2.0")
    
    col_binned <- cut(df_meta$runoff_eff, col_breaks, labels = col_pal)
    
    # Update map
    
    leafletProxy("stat_map") %>%
      
      addCircleMarkers(lng = df_meta$longitude,
                       lat = df_meta$latitude,
                       layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                       color = col_binned,
                       radius = 6,
                       stroke = FALSE,
                       opacity = 1,
                       fillOpacity = 1) %>%
      
      addLegend("bottomright",
                colors = col_pal,
                labels = legend_str,
                title = "Runoff efficiency",
                layerId = "stat_legend") %>%
      
      hideGroup(group = "Precipitation")
    
  }
  
  
  # Markers display mean runoff without text
  
  if (map_disp == "Mean runoff") {
    
    # Restrict range
    
    runoff_tmp <- df_meta$runoff_mean
    
    cat(paste("Number of stations with runoff > 7000 mm/year:", sum(runoff_tmp>7000), "\n"))
    
    runoff_tmp[runoff_tmp>7000] <- 7000
    
    # Colors and legend label
    
    pal <- colorBin(palette = "Blues",
                    na.color = "transparent",
                    bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
    
    col_binned <- pal(runoff_tmp)
    
    # Update map
    
    leafletProxy("stat_map") %>%
      
      addCircleMarkers(lng = df_meta$longitude,
                       lat = df_meta$latitude,
                       layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                       fillColor = col_binned,
                       fillOpacity = 1,
                       stroke = TRUE,
                       weight = 1,
                       color = "black",
                       radius = 6,
                       opacity = 1) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = runoff_tmp,
                title = "Mean runoff",
                layerId = "stat_legend") %>%
      
      showGroup(group = "Precipitation")
    
  }
  
  
  # Markers display mean runoff with text
  
  if (map_disp == "Mean runoff (txt)") {
    
    # Restrict range
    
    runoff_tmp <- df_meta$runoff_mean
    
    cat(paste("Number of stations with runoff > 7000 mm/year:", sum(runoff_tmp>7000)))
    
    runoff_tmp[runoff_tmp>7000] <- 7000
    
    # Colors and legend label
    
    pal <- colorBin(palette = "Blues",
                    na.color = "transparent",
                    bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
    
    col_binned <- pal(runoff_tmp)
    
    # Update map
    
    leafletProxy("stat_map") %>%
      
      addCircleMarkers(lng = df_meta$longitude,
                       lat = df_meta$latitude,
                       layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                       fillColor = col_binned,
                       fillOpacity = 1,
                       stroke = TRUE,
                       weight = 1,
                       color = "black",
                       radius = 6,
                       opacity = 1,
                       label = as.factor(round(runoff_tmp, digits = 0)),
                       labelOptions = labelOptions(noHide = T,
                                                   textOnly = TRUE,
                                                   direction = 'right',
                                                   textsize='15px')) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = runoff_tmp,
                title = "Mean runoff",
                layerId = "stat_legend") %>%
      
      showGroup(group = "Precipitation")
    
  }
  
}


# # Plot cumulative precipitation against runoff
# 
# plot_cumsums <- function(data_main, istat, plot_runoff_ranges) {
#   
#   df <- data.frame(date = data_main[[istat]]$time_vec,
#                    runoff = data_main[[istat]]$Runoff,
#                    prec = data_main[[istat]]$Prec)
#   
#  if (!is.null(plot_runoff_ranges$x)) {
#    df <- with(df, df[(date >= plot_runoff_ranges$x[1] & date <= plot_runoff_ranges$x[2]), ])
#  }
#   
#   df <- na.omit(df)
#   
#   runoff_acc <- cumsum(df$runoff)
#   
#   prec_acc <- cumsum(df$prec)
#   
#   df <- data.frame(runoff_acc = runoff_acc, prec_acc = prec_acc)
#   
#   ggplot(data = df, aes(x = runoff_acc, y = prec_acc)) +
#     geom_smooth(method = 'lm', se = FALSE, size = 1, linetype = 2, col = "red") +
#     geom_point() +
#     xlab("Cumulative runoff (mm)") + 
#     ylab("Cumulative precipitation (mm)")
#   
# }


# # Plot runoff time series (note that this plot often produces warnings
# # since the time series can contain missing values)
# 
# plot_runoff <- function(data_main, istat, plot_ranges) {
#   
#   name <- paste(data_main[[istat]]$metadata$regine_main,
#                 data_main[[istat]]$metadata$station_name, sep = " - ")
#   
#   time <- ymd(data_main[[istat]]$time_vec)
#   
#   runoff <- data_main[[istat]]$Runoff
#   
#   df <- data.frame(time = time, runoff = runoff)
#   
#   ggplot(data = df, aes(x = time, y = runoff)) + 
#     geom_line(col = "red", size = 1) +
#     xlab("") +
#     ylab("Runoff (mm/month)") +
#     ggtitle(label = name) + 
#     theme_set(theme_grey(base_size = 12))  #+
#     #coord_cartesian(xlim = plot_runoff_ranges$x, expand = FALSE)   # NEWNEWNEW
#   
# }


# Plot the basic map

plot_map <- function(df_meta, data_main) {
  
  # Colors for precipitation map
  
  pal <- colorBin(palette = "Blues",
                  na.color = "transparent",
                  bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
  
  # Create leaflet map
  
  leaflet() %>%
    
    addTiles("http://opencache.statkart.no/gatekeeper/gk/gk.open_gmaps?layers=topo2graatone&zoom={z}&x={x}&y={y}",
             attribution = "? Kartverket") %>%
    
    addRasterImage(prec_raster,
                   colors = pal,
                   opacity = 0.7,
                   group = "Precipitation",
                   project=FALSE) %>%
    
    fitBounds(min(df_meta$longitude),
              min(df_meta$latitude),
              max(df_meta$longitude),
              max(df_meta$latitude))
  
}






