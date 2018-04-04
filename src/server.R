#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  api_key <- "api_key"
  googleway::set_key(api_key)
  
  lat_lon <- read.csv(file = "../dataset/Lat_long.csv", sep = ";", dec = ".") %>% unique()
  isf <- read_csv2(file = "../dataset/ISF.csv")
  isf <- isf %>%
    left_join(lat_lon)
  
  isf <- isf %>%
    mutate(RADIUS = IMPOT_2014)
  
  
  colfunc <- colorRampPalette(c("yellow", "red"))
  colors <- colfunc(20)
  
  isf_max <- max(isf$P14_POP, na.rm = T)
  isf_min <- min(isf$P14_POP, na.rm = T)
  
  step <- (isf_max-isf_min)/20
  
  isf <- isf %>%
    mutate(color = colors[1]) %>%
    mutate(color = replace(color, P14_POP < isf_max-step, colors[2]),
           color = replace(color, P14_POP < isf_max-step*2, colors[3]),
           color = replace(color, P14_POP < isf_max-step*3, colors[4]),
           color = replace(color, P14_POP < isf_max-step*4, colors[5]),
           color = replace(color, P14_POP < isf_max-step*5, colors[6]),
           color = replace(color, P14_POP < isf_max-step*6, colors[7]),
           color = replace(color, P14_POP < isf_max-step*7, colors[8]),
           color = replace(color, P14_POP < isf_max-step*8, colors[9]),
           color = replace(color, P14_POP < isf_max-step*9, colors[10]),
           color = replace(color, P14_POP < isf_max-step*10, colors[11]),
           color = replace(color, P14_POP < isf_max-step*11, colors[12]),
           color = replace(color, P14_POP < isf_max-step*12, colors[13]),
           color = replace(color, P14_POP < isf_max-step*13, colors[14]),
           color = replace(color, P14_POP < isf_max-step*14, colors[15]),
           color = replace(color, P14_POP < isf_max-step*15, colors[16]),
           color = replace(color, P14_POP < isf_max-step*16, colors[17]),
           color = replace(color, P14_POP < isf_max-step*17, colors[18]),
           color = replace(color, P14_POP < isf_max-step*18, colors[19]),
           color = replace(color, P14_POP < isf_max-step*19, colors[20]),
           color = replace(color, P14_POP < isf_max-step*20, colors[19]))
  
  
  
  output$map <- renderGoogle_map({
    google_map(data = isf) %>%
      add_circles(lat = "LAT", lon = "LONG", radius = "RADIUS", stroke_opacity = 0.5, mouse_over = "COM_FINAL", fill_colour = "color", stroke_colour = "color")
      #add_markers(lat = "lat", lon = "lng", info_window = "info") %>%
  })

  output$result <- renderText({paste("L'impôt est de : ", 
                                     format(0.9064917*as.numeric(input$impot) + 0.0003587*as.numeric(input$patr), digits=9, decimal.mark=",",big.mark=" ",small.mark=".", , small.interval=3),
                                     " €")})
  output$title <- renderText({"FAITES VOTRE ESTIMATION"})
  
  
})
