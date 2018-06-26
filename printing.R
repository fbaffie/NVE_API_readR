

# Print summary table

print_summary <- function(data_main, istat) {
  
  # Station attributes
  
  station_name <- data_main[[istat]]$metadata$station_name
  area         <- data_main[[istat]]$metadata$area_total
  
  area <- round(area, digits = 0)
  
  # Average annual precipitation and runoff
  
  prec_mean <- round(data_main[[istat]]$prec_mean, digits = 0)
  
  runoff_mean <- round(data_main[[istat]]$runoff_mean, digits = 0)
  
  # Runoff efficieny (runoff/precipitation)
  
  runoff_eff <- round(data_main[[istat]]$runoff_eff, digits = 1)
  
  # Percent missing data
  
  frac_missing <- sum(is.na(data_main[[istat]]$Runoff)) / length(data_main[[istat]]$Runoff)
  
  frac_missing <- round(100*frac_missing, digits = 0)
  
  # Artificial influence
  
  regulated <- c(data_main[[istat]]$metadata$regulation_part_area,
                 data_main[[istat]]$metadata$regulation_part_reservoirs,
                 data_main[[istat]]$metadata$transfer_area_in,
                 data_main[[istat]]$metadata$transfer_area_out)
  
  if (any(is.na(regulated))) {
    regulated = "Unknown"
  } else if (any(regulated > 0)) {
    regulated = "Yes"
  } else {
    regulated = "No"
  }
  
  first_year_regulation <- df_meta$first_year_regulation[istat]
  
  if (is.na(first_year_regulation)) {
    first_year_regulation = "-"
  }
  
  # Land use
  
  perc_forest <- round(data_main[[istat]]$metadata$perc_forest)
  perc_lake <- round(data_main[[istat]]$metadata$perc_lake)
  perc_glacier <- round(data_main[[istat]]$metadata$perc_glacier)
  
  # Bruksomraden
  
  br1_middelavrenning_1930_1960 <- data_main[[istat]]$metadata$br1_middelavrenning_1930_1960
  br2_Tilsigsberegning <- data_main[[istat]]$metadata$br2_Tilsigsberegning
  br3_Regional_flomfrekvensanalyse <- data_main[[istat]]$metadata$br3_Regional_flomfrekvensanalyse
  br5_Regional_lavvannsanalyse <- data_main[[istat]]$metadata$br5_Regional_lavvannsanalyse
  br6_Klimastudier <- data_main[[istat]]$metadata$br6_Klimastudier
  br7_Klimascenarier <- data_main[[istat]]$metadata$br7_Klimascenarier
  br9_Flomvarsling <- data_main[[istat]]$metadata$br9_Flomvarsling
  br11_FRIEND <- data_main[[istat]]$metadata$br11_FRIEND
  br23_HBV <- data_main[[istat]]$metadata$br23_HBV
  br24_middelavrenning_1961_1990 <- data_main[[istat]]$metadata$br24_middelavrenning_1961_1990
  br26_TotalAvlop <- data_main[[istat]]$metadata$br26_TotalAvlop
  br31_FlomserierPrim <- data_main[[istat]]$metadata$br31_FlomserierPrim
  br32_FlomserierSekundar <- data_main[[istat]]$metadata$br32_FlomserierSekundar
  br33_Flomkart_aktive_ureg <- data_main[[istat]]$metadata$br33_Flomkart_aktive_ureg
  br34_referanseserier_klimastudier <- data_main[[istat]]$metadata$br34_Hydrologisk_referanseserier_klimastudier
  br38_Flomkart_aktive_ureg_periode <- data_main[[istat]]$metadata$br38_Flomkart_aktive_ureg_periode
  br39_Flomkart_nedlagt_stasjon <- data_main[[istat]]$metadata$br39_Flomkart_nedlagt_stasjon
  
  if (is.na(br1_middelavrenning_1930_1960)) {br1_middelavrenning_1930_1960 = ""}
  if (is.na(br2_Tilsigsberegning)) {br2_Tilsigsberegning = ""}
  if (is.na(br3_Regional_flomfrekvensanalyse)) {br3_Regional_flomfrekvensanalyse = ""}
  if (is.na(br5_Regional_lavvannsanalyse)) {br5_Regional_lavvannsanalyse = ""}
  if (is.na(br6_Klimastudier)) {br6_Klimastudier = ""}
  if (is.na(br7_Klimascenarier)) {br7_Klimascenarier = ""}
  if (is.na(br9_Flomvarsling)) {br9_Flomvarsling = ""}
  if (is.na(br11_FRIEND)) {br11_FRIEND = ""}
  if (is.na(br23_HBV)) {br23_HBV = ""}
  if (is.na(br24_middelavrenning_1961_1990)) {br24_middelavrenning_1961_1990 = ""}
  if (is.na(br26_TotalAvlop)) {br26_TotalAvlop = ""}
  if (is.na(br31_FlomserierPrim)) {br31_FlomserierPrim = ""}
  if (is.na(br32_FlomserierSekundar)) {br32_FlomserierSekundar = ""}
  if (is.na(br33_Flomkart_aktive_ureg)) {br33_Flomkart_aktive_ureg = ""}
  if (is.na(br34_referanseserier_klimastudier)) {br34_referanseserier_klimastudier = ""}
  if (is.na(br38_Flomkart_aktive_ureg_periode)) {br38_Flomkart_aktive_ureg_periode = ""}
  if (is.na(br39_Flomkart_nedlagt_stasjon)) {br39_Flomkart_nedlagt_stasjon = ""}
  
  # Merge strings
  
  str <- c("@{station_name}\n",
           "Runoff (mm/year) = @{runoff_mean}\n",
           "Precipitation (mm/year) = @{prec_mean}\n",
           "Runoff/Prec = @{runoff_eff}\n",
           "Missing data (%) = @{frac_missing}\n",
           "Area = @{area} km2\n",
           "Regulated = @{regulated}\n",
           "First regulation = @{first_year_regulation}\n",
           "Forest = @{perc_forest} %\n",
           "Lake = @{perc_lake} %\n",
           "Glacier = @{perc_glacier} %\n",
           "Bruksomr?den\n",
           "Middelavrenning 1930-1960 = @{br1_middelavrenning_1930_1960}\n",
           "Tilsigsberegning = @{br2_Tilsigsberegning}\n",
           "Regional flomfrekvensanalyse = @{br3_Regional_flomfrekvensanalyse}\n",
           "Regional lavvannsanalyse = @{br5_Regional_lavvannsanalyse}\n",
           "Klimastudier = @{br6_Klimastudier}\n",
           "Klimascenarier = @{br7_Klimascenarier}\n",
           "Flomvarsling = @{br9_Flomvarsling}\n",
           "FRIEND = @{br11_FRIEND}\n",
           "HBV = @{br23_HBV}\n",
           "Middelavrenning_1961_1990 = @{br24_middelavrenning_1961_1990}\n",
           "TotalAvlop = @{br26_TotalAvlop}\n",
           "FlomserierPrim = @{br31_FlomserierPrim}\n",
           "FlomserierSekundar = @{br32_FlomserierSekundar}\n",
           "Flomkart_aktive_ureg = @{br33_Flomkart_aktive_ureg}\n",
           "Referanseserier_klimastudier = @{br34_referanseserier_klimastudier}\n",
           "Flomkart_aktive_ureg_periode = @{br38_Flomkart_aktive_ureg_periode}\n",
           "Flomkart_nedlagt_stasjon = @{br39_Flomkart_nedlagt_stasjon}\n")
  
  qqcat(paste(str, collapse = ""))
  
}






















