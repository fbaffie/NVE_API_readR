#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})


xgeo_url_SoilWater <- paste("http://h-web01.nve.no/chartserver/ShowData.aspx?req=getchart&vfmt=xml&ver=1.0&time=",
                            start_time,";",stop_time,
                            "&chd=ds=tsr,da=",da,",id=vepshydra[1.",nb_station,
                            ".0.", 6014, ".0],rt=1.0:0,cht=line,mth=mean", sep = "")
