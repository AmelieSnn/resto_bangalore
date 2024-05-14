# LIBRARY
library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(leaflet)
library(ggplot2)
library(bslib)
library(thematic)
library(rsconnect)


# DATASET
resto <- read_excel("resto_RS.xlsx")

# Converting column types
resto$longitude <- as.numeric(resto$longitude)
resto$latitude <- as.numeric(resto$latitude)
resto$cost_two_people <- as.numeric(resto$cost_two_people)
resto$rate <- as.integer(resto$rate)

# unique list of cuisine types
cuisine_list <- unique(unlist(strsplit(as.character(resto$cuisines), ", ")))

#UI
ui <- fluidPage(
  theme = bs_theme(preset = "united"),
  br(), 
  titlePanel("Bangaluru_What restaurant for today?"),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      h4("Choose your restaurant",
         style = "color:#FFB700; font-weight:bold;"),
      selectInput(
        inputId = "rest_type",
        label = "Restaurant type ? ",
        choices = unique(resto$rest_type)
      ),
      selectInput(
        inputId = "cuisines",
        label = "Cuisine type ? ",
        choices = cuisine_list
      ),
      radioButtons(
        inputId = "online_order",
        label = "Online order ?",
        choices = c("Yes", "No"),
        selected = "Yes" 
      ),
      hr(),
      h4("Enter your address coordinates",
         style = "color:#FFB700; font-weight:bold;"),
      a(em("(You can find out your coordinates on this website)"),
        href = "https://www.coordonnees-gps.fr/"),
      br(),
      br(),
      textInput(
        "latitude_input",
        "Latitude",
        value = "12.977012804887526"
      ),
      textInput(
        "longitude_input",
        "Longitude",
        value = "77.60892508172257"
      ),
      img(src = "bengaluru.png",
          width = 300,
          height = 120,
          style = "display: block; margin: 0 auto;"
      )
    ),
    mainPanel(
      tabsetPanel(
        # TAB1
        tabPanel(
          h3("List of Restaurants"),
          tableOutput("nom_restaurant"),
          h3("Map"),
          leafletOutput("map")
        ),
        # TAB2
        tabPanel(
          h3("Dataset Analysis"),
          br(),
          br(),
          hr(),
          h3("Boxplot : Rate by Restaurant",
             style = "color:#FFB700; font-weight:bold;"),
          plotOutput(outputId = "boxplot_rates"),
          br(),
          hr(),
          h3("Summary",
             style = "color:#FFB700; font-weight:bold;"),
          verbatimTextOutput("summary"),
          br(),
          hr(),
          h3("Scatterplot : Price VS Rate",
             style = "color:#FFB700; font-weight:bold;"),
          plotOutput(outputId = "price_vs_rate"),
          br(),
          hr(),
          h3("Scatterplot : Mean by city",
             style = "color:#FFB700; font-weight:bold;"),
          dataTableOutput("means")
        ),
        # TAB3
        tabPanel(
          h3("Dataset"),
          DTOutput("resto"),
          br(),
          br(),
          fluidRow(
            column(
              width = 12,
              align = "center",
              downloadButton("download_data", label = "Download Dataset")
            )
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  # Reactive expression (for home coordinates)
  home_coords <- reactive({
    if (!is.na(as.numeric(input$latitude_input)) 
        && !is.na(as.numeric(input$longitude_input))) {
      list(lat = as.numeric(input$latitude_input), lng = as.numeric(input$longitude_input))
    } else {
      NULL
    }
  })
  
  # Reactive expression for filtered dataset
  filtered_restaurants <- reactive({
    filtered <- resto
    # Filter by restaurant type
    if (!is.null(input$rest_type) && input$rest_type != "") {
      filtered <- filtered[filtered$rest_type %in% input$rest_type, ]
    }
    # Filter by cuisine type
    if (!is.null(input$cuisines) && input$cuisines != "") {
      filtered <- filtered[sapply(strsplit(as.character(filtered$cuisines), ", "), function(x) any(x %in% input$cuisines)), ]
    }
    # Filter by online order
    if ("Yes" %in% input$online_order) {
      filtered <- filtered[filtered$online_order == "Yes", ]
    }
    return(filtered)
  })
  
  ## Reactive expression for means by city
  mean_by_city <- reactive({
    resto %>%
      group_by(location) %>%
      summarize(average_rate = round(mean(rate, na.rm = TRUE), 2),
                average_price = round(mean(cost_two_people, na.rm = TRUE), 2))
  })
  
  # TAB1 
  ## list of filtered restaurants
  output$nom_restaurant <- renderUI({
    filtered <- filtered_restaurants()
    if(nrow(filtered) == 0) {
      message <- "<br><h4 style='color: #aad3df;'>Sorry, we couldn't find any restaurants that match your criteria.</h4>
      <h4 style='color: #aad3df;'>Explore new flavors!</h4>
      <img src='smiley.jpeg' alt='Image' width='70' height='70'>"
      HTML(message)
    } else {
      renderTable(filtered[, c("name", "rate", "address", "phone")])
    }
  })
  
  ## Map for home and restaurant
  output$map <- renderLeaflet({
    no_restaurants <- nrow(filtered_restaurants()) == 0
    if (no_restaurants) {
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = home_coords(), lng = ~lng, lat = ~lat, popup = "HOME", label = "HOME", icon = makeIcon("home.png", iconWidth = 28, iconHeight = 37))
    } else {
      leaflet(data = filtered_restaurants()) %>%
        addTiles() %>%
        addCircleMarkers(~as.numeric(longitude), ~as.numeric(latitude), 
                         popup = ~paste("<b>Name:</b>", name, "<br>", "<b>Rate:</b>", rate), 
                         label = ~name,
                         color = "blue") %>%
        addMarkers(data = home_coords(), lng = ~lng, lat = ~lat, popup = "HOME", label = "HOME", icon = makeIcon("home.png", iconWidth = 28, iconHeight = 37))
    }
  })
  
  # TAB2
  ## Boxplot
  output$boxplot_rates <- renderPlot({
    ggplot(resto, aes(y = rate)) +
      geom_boxplot(fill = "skyblue", color = "blue") +
      labs(x = "Restaurants",
           y = "Rate") +
      theme_minimal() +
      theme(axis.text.x = element_blank())
  })
  
  ## Scatterplot
  output$price_vs_rate <- renderPlot({
    ggplot(resto, aes(x = rate, y = cost_two_people)) +
      geom_point(color = "blue") +
      labs(x = "Rate",
           y = "Cost for Two People") +
      theme_minimal()
  })
  
  ## Summary
  output$summary <- renderPrint({
    summary(filtered_restaurants())
  })
  
  ## Mean rate and mean price by city
  output$means <- renderDataTable({
    mean_by_city()
  })
  
  # TAB3 : 
  
   ## Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste("resto", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(resto, file, row.names = FALSE)
    }
  )
  ## the entire dataset
  output$resto <- renderDT({
    resto
  })
  
  # Log download action
  observe({
    if (!is.null(input$download_data)) {
      logAction(action = "Downloaded resto data")
    }
  })
}

# ShinyApp
shinyApp(ui = ui, server = server)