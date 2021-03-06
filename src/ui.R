#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(googleway)

# Define UI for application that draws a histogram
shinyUI(fluidPage(google_mapOutput("map", height = 800),
                  textOutput("title"),
                  textInput("impot", "impot"),
                  textInput("patr", "patr"),
                  textOutput("result")))
        
