

# Construct metadata table from data list

metadata_for_app <- function(data_main)  {
  
  df_meta <- c()
  
  for (i in 1:length(data_main)) {
    
    df_meta <- rbind(df_meta, data_main[[i]]$metadata)
    
  }
  
  df_meta$prec_mean <- sapply(data_main, function(x) x$prec_mean)
  
  df_meta$runoff_mean <- sapply(data_main, function(x) x$runoff_mean)
  
  df_meta$runoff_eff <- sapply(data_main, function(x) x$runoff_eff)
  
  return(df_meta)
  
}


# Compute mean runoff/precipitation and runoff efficiency

comp_stats <- function(data_list) {
  
  # Remove missing data
  
  df <- data.frame(prec = data_list$Prec,
                   runoff = data_list$Runoff)
  
  df <- na.omit(df)
  
  # Annual average precipitation
  
  data_list$prec_mean <- 365*mean(df$prec, na.rm = TRUE)
  
  # Annual average runoff
  
  data_list$runoff_mean <- 365*mean(df$runoff, na.rm = TRUE)
  
  # Runoff efficiency
  
  data_list$runoff_eff <- sum(df$runoff)/sum(df$prec)
  
  return(data_list)
  
}


# Read metadata file (excel table)

read_metadata_file <- function(filename) {
  
  # Read station metadata
  
  meta_data <- read_excel(filename)
  
  meta_data <- tbl_df(meta_data)
  
  # # Keep rows with runoff data (parameter == 1001)
  # 
  # meta_data <- filter(meta_data, param_key==1001)
  
  # Remove duplicated stations
  
  idup <- duplicated(meta_data[, 1:3])
  
  meta_data <- meta_data[!idup, ]
  
  # Add station name as 'regine_area.main_no'
  
  meta_data <- mutate(meta_data, regine_main = paste(regine_area, main_no, sep = "."))
  
  # Add observation series as 'regine_area.main_no.point_no.param_key.version_no_end'
  
  meta_data <- mutate(meta_data, obs_series = paste(regine_area, main_no, point_no, param_key, version_no_end, sep = "."))
  
  return(meta_data)
  
}



