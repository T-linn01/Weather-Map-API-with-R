# official
library(shiny)
library(leaflet)
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(dplyr)
library(shinyMatrix)
library(plotly)
# Function to get infor of current weather from OpenWeatherMap
get_weather_info <- function(lat, lon) {
  api_key <- "aa384ad8da4f15059d2cbb9fb4dfa7b9"
  API_call <-
    "https://api.openweathermap.org/data/2.5/weather?lat=%s&lon=%s&appid=%s"
  complete_url <- sprintf(API_call, lat, lon, api_key)
  json <- fromJSON(complete_url)
  location <- json$name
  temp <- json$main$temp - 273.2
  feels_like <- json$main$feels_like - 273.2
  humidity <- json$main$humidity
  weather_condition <- json$weather$description
  visibility <- json$visibility
  wind_speed <- json$wind$speed
  weather_info <- list(
    Location = location,
    Temperature = temp,
    Feels_like = feels_like,
    Humidity = humidity,
    WeatherCondition = weather_condition,
    Visibility = visibility,
    Wind_speed = wind_speed
  )
  return(weather_info)
}
get_forecast <- function(lat, lon) {
  api_key <- "aa384ad8da4f15059d2cbb9fb4dfa7b9"
  # base_url variable to store url
  API_call = "https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s"
  
  # Construct complete_url variable to store full url address
  complete_url = sprintf(API_call, lat, lon, api_key)
  #print(complete_url)
  json <- fromJSON(complete_url)
  
  df <- data.frame(
    Time = json$list$dt_txt,
    Location = json$city$name,
    feels_like = json$list$main$feels_like - 273.2,
    temp_min = json$list$main$temp_min - 273.2,
    temp_max = json$list$main$temp_max - 273.2,
    pressure = json$list$main$pressure,
    sea_level = json$list$main$sea_level,
    grnd_level = json$list$main$grnd_level,
    humidity = json$list$main$humidity,
    temp_kf = json$list$main$temp_kf,
    temp = json$list$main$temp - 273.2,
    id = sapply(json$list$weather, function(entry)
      entry$id),
    main = sapply(json$list$weather, function(entry)
      entry$main),
    icon = sapply(json$list$weather, function(entry)
      entry$icon),
    humidity = json$list$main$humidity,
    weather_conditions = sapply(json$list$weather, function(entry)
      entry$description),
    speed = json$list$wind$speed,
    deg = json$list$wind$deg,
    gust = json$list$wind$gust
  )
  
  return (df)
}
ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarUserPanel("Vũ Thùy Linh",
                                    subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                                    image = "https://scontent.fhan19-1.fna.fbcdn.net/v/t39.30808-6/399281820_646685407550480_9003224083053831183_n.jpg?_nc_cat=111&ccb=1-7&_nc_sid=5f2048&_nc_ohc=iBbhgKu9tJYAX-3umde&_nc_ht=scontent.fhan19-1.fna&oh=00_AfCLpTbq-V3v9bC7mf-cbiUOarH7YH9gUn-2992hQP3vBg&oe=654BF7EC"
  ),
    sidebarSearchForm(label = "Enter city name", "searchText", "searchButton"),
    sidebarMenu(
      menuItem("Weather", tabName = "weather"),
      menuItem("Forecast", tabName = "forecast")
  )),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
    tabItems(
      tabItem(tabName = "weather",
              fluidRow(
                infoBox(width = 5,
                    title = "Location",
                    textOutput("location"),
                    icon = icon("location-dot"),
                    color = "green"),
                infoBox(width = 5,
                    title = "Humidity",
                    textOutput("humidity"),
                    icon = icon("droplet"),
                    color = "purple"),
                infoBox(
                  width = 5,
                  title = "Temperature",
                  textOutput("temperature"),
                  icon = icon("temperature-three-quarters"),
                  color = "red"
                ),
                #infoBox(width = 5,
                   # title = "Feels Like",
                    #textOutput("feels_like")),
                infoBox(
                  width = 5,
                  title = "Weather Condition",
                  textOutput("weather_condition"),
                  icon = icon("umbrella"),
                  color = "navy"
                ),
                infoBox(width = 5,
                    title = "Visibility",
                    textOutput("visibility"),
                    icon = icon("globe"),
                    color = "orange"),
                infoBox(width = 5,
                    title = "Wind Speed",
                    textOutput("wind_speed"),
                    icon = icon("wind"),
                    color = "lime"),
                box(
                  width = 10,
                  title = "Map",
                  leafletOutput("map"),
                  class = 'map-container'
                )
              )),
      tabItem(
        tabName = "forecast",
        textOutput("location_"),
        # Add forecast content here
        selectInput(
          "feature",
          "Features:",
          list(
            "temp",
            "feels_like",
            "temp_min",
            "temp_max",
            "pressure",
            "sea_level",
            "grnd_level",
            "humidity",
            "speed",
            "deg",
            "gust"
          )
        ),
        box(
          title = "Sample Line Chart",
          plotlyOutput("line_chart")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 105.8341598,
              lat = 21.0277644,
              zoom = 10)
  })
  
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 105.8341598,
              lat = 21.0277644,
              zoom = 10)
  })
  
  click <- NULL
  weather_info <- NULL
  observeEvent(input$map_click, {
    click <<- input$map_click
    
    weather_info <<- get_weather_info(click$lat, click$lng)
    
    output$location <- renderText({
      paste(weather_info$Location)
    })
    
    output$humidity <- renderText({
      paste(weather_info$Humidity, "%")
    })
    
    output$temperature <- renderText({
      paste(weather_info$Temperature, "°C")
    })
    
    output$feels_like <- renderText({
      paste(weather_info$Feels_like, "°C")
    })
    
    output$weather_condition <- renderText({
      paste(weather_info$WeatherCondition)
    })
    
    output$visibility <- renderText({
      paste(weather_info$Visibility)
    })
    
    output$wind_speed <- renderText({
      paste(weather_info$Wind_speed)
    })
    
    
  })
  
  observeEvent(input$feature, {
    # display location
    output$location_ <- renderText({
      paste('Location: ', weather_info$Location)
    })
    
    # set default
    default_lon = 105.8341598
    default_lat = 21.0277644
    data <- get_forecast(default_lat, default_lon)
    output$line_chart <- renderPlotly({
      # Create a line chart using plot_ly
      feature_data <- data[, c("Time", input$feature)]
      # Create a line chart using plot_ly
      plot_ly(data = feature_data, x = ~Time, y = ~.data[[input$feature]], type = 'scatter', mode = 'lines+markers', name = input$feature) %>%
        layout(title = "Sample Line Chart", xaxis = list(title = "Time"), yaxis = list(title = input$feature))
    })
    
    # plot the forecast
    if (!is.null(click)) {
      data <- get_forecast(click$lat, click$lng)
      output$line_chart <- renderPlotly({
        # Create a line chart using plot_ly
        feature_data <- data[, c("Time", input$feature)]
        # Create a line chart using plot_ly
        plot_ly(data = feature_data, x = ~Time, y = ~.data[[input$feature]], type = 'scatter', mode = 'lines+markers', name = input$feature) %>%
          layout(title = "Sample Line Chart", xaxis = list(title = "Time"), yaxis = list(title = input$feature))
      })
    }
  })
}

shinyApp(ui, server)
