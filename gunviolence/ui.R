
library(shinydashboard)
library(plotly)
library(ggplot2)
suppressPackageStartupMessages(library(googleVis))
library(readr)
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(rlang)
library(stringi)
library(googleVis)
ui <- shinyUI(dashboardPage(
  dashboardHeader(title = "Gun Violence Analysis"),
  dashboardSidebar(
    sidebarUserPanel("Nikhil Taparia"),
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("States", tabName = "states", icon = icon("bar-chart-o")),
      menuItem("Cities", tabName = "cities", icon = icon("bar-chart-o")),
      menuItem("Characteristics", tabName = "characteristics", icon = icon("bar-chart-o"))
      #menuItem("Distributions", tabName = "Yearly Distributions", icon=icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(plotlyOutput("bar")),
                box(htmlOutput("usa"),height = "420px")),
              fluidRow(
                column(width = 7,
                       selectizeInput("selected",
                                      "Select Items to Display",
                                      choices)),
                column(width = 5,
                       title = "Dates",
                       sliderInput("sliders","Time Series:", 2013,2018,2015)))),
      
      tabItem(tabName = "states",
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
               fluidRow(
                box(plotlyOutput("Peer")),
                box(htmlOutput("geo1"), height = "420px"),
                selectizeInput("incident1", "State Gun Violence Rates",
                               choices=choices1))),
      tabItem(tabName = "cities",
              fluidRow(infoBoxOutput("maxBox1"),
                       infoBoxOutput("minBox1"),
                       infoBoxOutput("avgBox1")),
              fluidRow(      
                box(plotlyOutput("geo")),
                fluidRow(box(dataTableOutput("table"))),
                selectizeInput("incident2", "City Gun Violence Rates",
                               choices=choices2))),
      tabItem(tabName = "characteristics",
              fluidRow(
                box(plotlyOutput("bars")),
                box(htmlOutput("graph")),
                selectizeInput("incident",
                               "IncidentCharacteristics",
                               choices = unique(Values$incident_characteristics))))
      #tabItem(tabName = "Yearly Distributions",
              #fluidRow(
                #box(plotlyOutput("bar")),
                #box(htmlOutput("usa"),height = "420px")),
              #fluidRow(
               #column(width = 7,
                      #selectizeInput("selected",
                                               #"Select Items to Display",
                                               #choices
                      
                 #column(width = 5,
                       #title = "Dates",
                       #sliderInput("sliders","Time Series:", 2013,2018,2015))))
      
    )
  )
)
)


