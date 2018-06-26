

# Wind speed data from Mannen
# http://h-web01.nve.no/chartserver/ # defines server
#   ShowData.aspx? # defines function(?)
# req=getchart& # defines request
#   ver=1.0& # defines version number
#   vfmt=json& # defines output format
#   time=20130422T0000;20130521T0000& # defines time interval
#   chs=10x10& # defines chart size
#   lang=no& # defines language
#   chlf=desc& # defines chart legend format
#   chsl=0;+0& # marks an area dependent on start and end dates/times
#   chd=ds=htsre,da=29,id=61410.16,rt=1:00,cht=line,mth=inst| # ???
#   ds=htsry,id=metx[61410;16].6038,mth=inst,rt=1:00,cht=line&nocache=0.33931976436234856 # ???

# # This one works too but returns many different variables (too many, better to call each variable separately)
#   xgeo_url <- paste("http://h-web01.nve.no/chartserver/ShowData.aspx?req=getchart&ver=1.0&vfmt=xml&","time=",start_time,";",stop_time,
#   "&chs=150x150&lang=no&chlf=desc&chsl=0;+0&chd=ds=htsr,", "da=",da,",id=",nb_station,".0.1001.",version,
#   ",rt=1,cht=line,mth=inst,perc=25;75,clr=black,drwd=2,stat=qcm;qc5;qc10;qc20;qc50;qm;q5;q10;q20;q50|ds=tsr,","da=",da,
#   ",id=vepshydra[1.",nb_station,".0.6046.0],rt=1.0:0,cht=line,mth=mean,","time=",start_time,";",stop_time,
#   "&nocache=0.8115327963068484&callback=dojo.io.script.jsonp_dojoIoScript8._jsonpCallback", sep = "")



#' Draft function to call the NVE API
#' Look also at http://nve-wiki.nve.no/index.php/Web_Chart_Service#da_.28DATA_ARCHIVE.29
#' @param data
#' @param start_time
#' @param stop_time
#' @param nb_station
#' @param nb_variable
#'
#' @title read_nve_api Loads various data from xgeo
#' @importFrom dplyr failwith
#' @importFrom XML xmlParse
#' @importFrom XML xmlToDataFrame
#' @importFrom XML getNodeSet
#' @export
read_nve_api <- function(dat = NULL, start_time = NULL, stop_time = NULL, nb_station = NULL, name_station = NULL, nb_variable = 6046) {
  # library(RCurl) for getURL
  
  # Load data from xgeo interface
  version <- 0
  da <- 29  # Data archive
  
  #   # For testing purposes
  #   nb_station <- "2.11"
  #   start_time <- "20160711T0600"
  #   stop_time <- "20160911T0600"
  
  ## Subfunction to parse and save the xml, which should maybe be outside of read_nve_api?
  parse_xml_and_save <- function(xgeo_url) {
    xmldata <- XML::xmlParse(xgeo_url)
    tmp <- XML::xmlToDataFrame(XML::getNodeSet(xmldata,"//Point"),colClasses = c("character","double"))
    
    print(tmp)
    
    if (is.data.frame(tmp) && nrow(tmp) > 0) {
      
      time_vect <- tmp[ , 1]
      
      regine <- rep(nb_station, length(time))
      name <- rep(name_station, length(time))
      
      ODM <- data.frame(regine.main = regine,
                        station.name = name,
                        nbname = paste(regine, "-", name, sep = ""),
                        time = time_vect,
                        Runoff_ODM.sim = tmp$Value)
      
      ODM[ODM == -9999.000] <- NA
      ODM[ODM == -10000.000] <- NA
      ODM <- dplyr::tbl_df(ODM)
      invisible(ODM)
    }
  }
  
  
  if (is.null(dat) == TRUE) {
    
    # nb_variable <- 1001  # this is measured discharge
    # nb_variable <- 6046  # For ODM results. 6011 gives same result.
    
    # address for ODM tset
    xgeo_url <- paste("http://h-web01.nve.no/chartserver/ShowData.aspx?req=getchart&vfmt=xml&ver=1.0&time=",
                      start_time,";",stop_time,
                      "&chd=ds=tsr,da=",da,",id=vepshydra[1.",nb_station,
                      ".0.",nb_variable,".0],rt=1.0:0,cht=line,mth=mean", sep = "")
    
    print(xgeo_url)
    # xgeo_url <- getURL(xgeo_url)
    
    
    failsafe <- failwith(NULL,
                         parse_xml_and_save)
    failsafe(xgeo_url)
    
    # parse_xml_and_save(xgeo_url)
    
    
  } else {
    
    # Start time
    start_time <- format(head(dat$time_vec,1), "%Y%m%dT0000")
    # Stop time
    stop_time <- format(tail(dat$time_vec,1) + lubridate::days(1), "%Y%m%dT0000")
    
    # Address for measured discharge
    xgeo_url <- paste("http://h-web01.nve.no/chartserver/ShowData.aspx?req=getchart&ver=1.0&",
                      "time=",start_time,";",stop_time,
                      "&chd=ds=htsr,da=",da,",id=",nb_station,".0.",nb_variable,".",version,",rt=1&vfmt=xml",sep="")
    
    xmldata <- XML::xmlParse(as.charachter(xgeo_url))
    tmp <- XML::xmlToDataFrame(XML::getNodeSet(xmldata,"//Point"),colClasses = c("character","double"))
    
    if (length(dat$time_vec) == nrow(tmp) ) {
      dat$Runoff <- as.numeric(tmp$Value)
    } else {
      dat$Runoff <- rep(NA,length(dat$time_vec))
    }
    
    return(dat)
  }
}

#' Function which loads ODM model results from the NVE REST API
#'
#' @param start_day
#' @param stop_day
#'
#' @return
#' @export
#'
#' @examples In "load_flood_data
#' ODM <- load_ODM()
#' ODM <- tidyr::gather(ODM, key = Tmp, value = Values, -time, -regine.main, -station.name, -nbname) %>%
#' tidyr::separate(Tmp, into = c("Type", "Variable"), sep = "_")

load_ODM <- function(start_day = Sys.Date() + 1, stop_day = Sys.Date()+10) {
  # Used to be start_day = Sys.Date()-20 but the historical data was the same as the observation rather than being old model results.
  # Ask Bard whether it is a mistake from my understanding of the NVE REST API or whether he just decided to organize his data this way
  
  # Get the regine numbers related to the station names in the HBV output file
  # Read it from the package for use anywhere
  station_ref <- read.table(system.file("demodata/usikkerhet_grd", "HbvFelt147.txt", package = "NVEDATA"))
  regine_ref_nb <- paste(station_ref$V1, ".", station_ref$V2, sep = "")
  
  station_ref_name <- station_ref$V5
  
  tmp_start <- strsplit(as.character(start_day), split = "-")
  tmp_stop <- strsplit(as.character(stop_day), split = "-")
  
  start_time <- paste(tmp_start[[1]][1],tmp_start[[1]][2],tmp_start[[1]][3],"T0000", sep = "")
  stop_time <- paste(tmp_stop[[1]][1],tmp_stop[[1]][2],tmp_stop[[1]][3],"T0000", sep = "")
  
  # Somehow don't manage to use purrr::map in this case
  # ODM_1Y <- purrr::map(.x = regine_ref_nb[1:3], .f = read_nve_api, data = NULL, start_time = "20160711T0600", stop_time = "20161015T0600", nb_station = regine_ref_nb)
  ODM <- data.frame(regine.main = c(),
                    station.name = c(),
                    nbname = c(),
                    time = c(),
                    Runoff_ODM.sim = c())
  
  for (i in seq(along = regine_ref_nb)) {
    single_st_ODM <- read_nve_api(data = NULL, start_time = start_time, stop_time = stop_time, nb_station = regine_ref_nb[i],
                                  name_station = station_ref_name[i], nb_variable = 6046)
    ODM <- dplyr::bind_rows(ODM, single_st_ODM)
  }
  
  ODM$time <- as.Date(as.character(ODM$time), "%m/%d/%Y")
  ODM <- dplyr::tbl_df(ODM)
  
  ODM[ODM == -10000] <- NA
  invisible(ODM)
  
}


# Example of a non working use of HTTR package. It's not really more elegant:
# test <- GET("http://h-web01.nve.no/chartserver/ShowData.aspx", query = list(req = "getchart", ver = "1.0",vmft = "json", time = "20180603T0600;20180702T0600", da = "29", id = "vepshydra[62.5.0.1001.1]", rt = "1", chd = "ds")))
