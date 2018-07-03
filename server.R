

xgeo_url_SoilWater <- paste("http://h-web01.nve.no/chartserver/ShowData.aspx?req=getchart&vfmt=xml&ver=1.0&time=",
                            start_time,";",stop_time,
                            "&chd=ds=tsr,da=",da,",id=vepshydra[1.",nb_station,
                            ".0.", 6014, ".0],rt=1.0:0,cht=line,mth=mean", sep = "")