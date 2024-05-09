library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(leaflet)
library(ggplot2)

# CHARGEMENT DU DATASET
resto <- read_excel("resto_RS.xlsx")

# Convertir les colonnes "longitude" et "latitude" en nombres
resto$longitude <- as.numeric(resto$longitude)
resto$latitude <- as.numeric(resto$latitude)
resto$cost_two_people <- as.numeric(resto$cost_two_people)
resto$rate <- as.integer(resto$rate)

# Obtenir une liste unique de types de cuisine
cuisine_list <- unique(unlist(strsplit(as.character(resto$cuisines), ", ")))

#UI
ui <- fluidPage(
  titlePanel("Bangalore_What restaurant for today?"),
  br(),
  sidebarLayout(
    
    sidebarPanel(
      
      h4(
        "Choose your restaurant",
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
      a(em("(You can find out your coords on this website)"),
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
          style = "display: block; margin: 0 auto;")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          h3("List of Restaurants"),
          tableOutput("nom_restaurant"),
          h3("Map"),
          leafletOutput("map")
        ),
        
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
        
        tabPanel(
          h3("Dataset"),
          tableOutput("resto") 
        )
      )
    )
  )
)


# SERVER
server <- function(input, output) {
  
  
  # reactive value (for home coordinates)
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
  
  
  
  # TAB1 : Render table of filtered restaurants
  output$nom_restaurant <- renderUI({
    filtered <- filtered_restaurants()
    
    if(nrow(filtered) == 0) {
      message <- "<h3 style='color: #871b4f;'>Sorry, there are no restaurants matching your criteria.</h3>
                      <h3 style='color: #871b4f;'>Open yourself to new flavors !</h3>
                      <img src='smiley.jpeg' alt='Image' width='100' height='100'>"
      HTML(message)
    } else {
      renderTable(filtered[, c("name", "rate", "address", "phone")])
    }
  })
  
  
  # TAB1 : Render leaflet map
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

  
  # TAB2 : Render boxplot of restaurant rates
  output$boxplot_rates <- renderPlot({
    ggplot(resto, aes(y = rate)) +
      geom_boxplot(fill = "skyblue", color = "blue") +
      labs(x = "Restaurants",
           y = "Rate") +
      theme_minimal() +
      theme(axis.text.x = element_blank())
  })
  
  # TAB2 : Render scatter plot of restaurant price vs rate
  output$price_vs_rate <- renderPlot({
    ggplot(resto, aes(x = rate, y = cost_two_people)) +
      geom_point(color = "blue") +
      labs(x = "Rate",
           y = "Cost for Two People") +
      theme_minimal()
  })
  
  # TAB2 : Render summary
  output$summary <- renderPrint({
    summary(filtered_restaurants())
  })
  
  # TAB2 : Rate mean and price mean by city
  mean_by_city <- reactive({
    resto %>%
      group_by(location) %>%
      summarize(average_rate = round(mean(rate, na.rm = TRUE), 2),
                average_price = round(mean(cost_two_people, na.rm = TRUE), 2))
  })
  
  # TAB2 : mean rate and mean price by city
  output$means <- renderDataTable({
    mean_by_city()
  })
  
  
  # TAB3 : Render the entire dataset
  output$resto <- renderTable({
    resto
  })
  
}

    


# Lancer l'application Shiny
shinyApp(ui = ui, server = server)