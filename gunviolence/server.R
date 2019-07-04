#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output){
  bar_plot <- reactive({
    gun %>% 
      group_by(year) %>%
      select(year, temp = input$selected) %>% 
      summarise(casualties = sum(temp))
  })
  geo_chart <- reactive({
    gun %>%
      filter(year == input$sliders) %>%
      group_by(state) %>% 
      select(year,temp2 = input$selected) %>%
      summarise(casualties = sum(temp2))
  })
  bar_plot1 <- reactive({
    z %>% group_by(state) %>% select(incidents = input$incident1)})
  bar_plot2 <- reactive({
  d %>% group_by(cities) %>% select(Occurances = input$incident2)})
   
  g <- reactive({
    incident %>% group_by(state) %>% select(label = input$incident1) %>% mutate(label = (log(label)))})
  c_1 <- reactive({
   imp<- d%>% group_by(cities)%>%select(label1 = input$incident2) %>% mutate(label1=log(label1))
  return(imp)})
  
  d_1 <- reactive({
    tmp <- Values %>%
      filter(incident_characteristics == input$incident) %>%
      select(state,city_or_county,incident_characteristics) %>%
      group_by(state, city_or_county) %>%
      summarise(Characteristics= n()) %>%
      mutate(Characteristics = log(n())) %>%
      arrange(desc(Characteristics))
    
    return (tmp) 
    
  })
  
  c_2 <-  reactive({
    Values %>%
      filter(incident_characteristics == input$incident) %>%
      select(state,incident_characteristics) %>%
      group_by(state) %>%
      summarise(Characteristics= n()) %>%
      arrange(desc(Characteristics))
  })
  
  output$maxBox <- renderInfoBox({
    max_value <- max(incident[,input$incident1])
    max_state <-
     incident$state[incident[,input$incident1]==max_value]
    infoBox(max_state, max_value, icon = icon("hand-o-up"))})
  output$minBox <- renderInfoBox({
    min_value <- min(incident[,input$incident1])
    min_state <-
      incident$state[incident[,input$incident1]==min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))})
  output$avgBox <-output$avgBox <- renderInfoBox({
    infoBox(paste("AVG.", input$incident1),
    mean(incident[,input$incident1]),
    icon = icon("calculator"), fill = TRUE)})
  output$maxBox1 <- renderInfoBox({
    max_value1 <- max(city[,input$incident2])
    max_city1 <-
      city$cities[city[,input$incident2]==max_value1]
    infoBox(max_city1, max_value1, icon = icon("hand-o-up"))})
  output$minBox1 <- renderInfoBox({
    min_value1 <- min(city[,input$incident2])
    min_city1 <-
      city$cities[city[,input$incident2]==min_value1]
    infoBox(min_city1, min_value1, icon = icon("hand-o-down"))})
  output$avgBox1 <-output$avgBox1 <- renderInfoBox({
    infoBox(paste("AVG.", input$incident2),
            round(mean(city[,input$incident2]),3),
            icon = icon("calculator"), fill = TRUE)})
  
  output$bar <- renderPlotly({
    p <- ggplot(bar_plot(), aes(x=year, y= casualties, fill = year)) + geom_col()+ ggtitle("Total Victims per Year") + labs(x = "Date", y = "Casualties")
    ggplotly(p)})

  output$usa <- renderGvis({
    gvisGeoChart(geo_chart(), locationvar = "state", colorvar = "casualties", 
                 options = list(region= "US", displayMode = "regions", resolution = "provinces"))})
  output$Peer <- renderPlotly({
    a <- ggplot(bar_plot1(), aes(x = reorder(state,incidents), y =incidents,fill = incidents)) + geom_col() + ggtitle("Comparison of gun violence per State")+labs(x = "States", y = "Incidents")+coord_flip() +scale_fill_gradient(low="yellow", high="red")
    ggplotly(a)})
  
  output$geo <- renderPlotly({
    d <- ggplot(bar_plot2(), aes(x = reorder(cities,Occurances), y = Occurances, fill = Occurances)) + geom_col() + ggtitle("Comparison of gun violence per City") + labs(x = "Cities", y = "Incidents") + coord_flip() + scale_fill_gradient(low = "blue",high = "purple")
    ggplotly(d)
    })
  output$geo1 <-renderGvis({
    gvisGeoChart(g(), locationvar = "state", colorvar = "label", 
                 options = list(region= "US", displayMode = "regions", resolution = "provinces"))})
  
   output$table <- renderDataTable({
      datatable(city, rownames=FALSE) %>%
        formatStyle(input$incident2,
                    background="skyblue", fontWeight='bold')})
  
  output$graph <- renderGvis({
    gvisGeoChart(d_1(), locationvar = "state", colorvar = "Characteristics",
                 options = list(region = "US", displayMode = "regions", resolution = "provinces"))})
  
  output$bars <- renderPlotly({
    l <- ggplot(top_n(c_2(),20,Characteristics), aes(x = reorder(state, Characteristics), y=Characteristics, fill = Characteristics)) + geom_col() + ggtitle("Incident Characteristics") + labs(x = "States", y="Characteristics") + coord_flip() + scale_fill_gradient()
    ggplotly(l)})
})
  


  

