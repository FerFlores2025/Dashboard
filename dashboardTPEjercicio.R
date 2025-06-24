#DASHBOARD con datos de Buenos Aires.
# app.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)

# Datos de ejemplo (en un caso real, cargarías datos reales de servicios en BA)
set.seed(123)
categorias <- c("Salud", "Educación", "Transporte", "Seguridad", "Ocio", "Gastronomía")
barrios <- c("Palermo", "Recoleta", "San Telmo", "Puerto Madero", "Belgrano", "Caballito", "Almagro", "Villa Crespo")

servicios <- data.frame(
  id = 1:200,
  nombre = paste("Servicio", 1:200),
  categoria = sample(categorias, 200, replace = TRUE),
  barrio = sample(barrios, 200, replace = TRUE),
  lat = -34.58 + runif(200, -0.05, 0.05),
  lng = -58.43 + runif(200, -0.05, 0.05),
  rating = sample(1:5, 200, replace = TRUE),
  atencion_24hs = sample(c("Sí", "No"), 200, replace = TRUE, prob = c(0.3, 0.7))
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Servicios en Buenos Aires"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Distribución", tabName = "distribucion", icon = icon("chart-bar")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map"))
    ),
    
    selectInput("categoria", "Categoría de Servicio:", 
                choices = c("Todas", unique(servicios$categoria))),
    
    selectInput("barrio", "Barrio:", 
                choices = c("Todos", unique(servicios$barrio))),
    
    sliderInput("rating", "Rating mínimo:", 
                min = 1, max = 5, value = 3),
    
    checkboxInput("atencion24", "Solo 24hs", value = FALSE)
  ),
  
  dashboardBody(
    tabItems(
      # Tab Resumen
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("total_servicios"),
                valueBoxOutput("promedio_rating"),
                valueBoxOutput("porcentaje_24hs")
              ),
              fluidRow(
                box(plotlyOutput("plot_categorias"), width = 6),
                box(plotlyOutput("plot_barrios"), width = 6)
              )
      ),
      
      # Tab Distribución
      tabItem(tabName = "distribucion",
              fluidRow(
                box(plotlyOutput("hist_rating"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("boxplot_rating_cat"), width = 12)
              )
      ),
      
      # Tab Mapa
      tabItem(tabName = "mapa",
              fluidRow(
                box(leafletOutput("mapa_servicios"), width = 12, height = "600px")
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filtrado reactivo de datos
  datos_filtrados <- reactive({
    datos <- servicios
    
    if (input$categoria != "Todas") {
      datos <- datos %>% filter(categoria == input$categoria)
    }
    
    if (input$barrio != "Todos") {
      datos <- datos %>% filter(barrio == input$barrio)
    }
    
    datos <- datos %>% filter(rating >= input$rating)
    
    if (input$atencion24) {
      datos <- datos %>% filter(atencion_24hs == "Sí")
    }
    
    datos
  })
  
  # Value boxes
  output$total_servicios <- renderValueBox({
    total <- nrow(datos_filtrados())
    valueBox(
      total, "Servicios", 
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$promedio_rating <- renderValueBox({
    promedio <- mean(datos_filtrados()$rating, na.rm = TRUE) %>% round(2)
    valueBox(
      promedio, "Rating promedio", 
      icon = icon("star"),
      color = "green"
    )
  })
  
  output$porcentaje_24hs <- renderValueBox({
    porcentaje <- datos_filtrados() %>% 
      summarise(porc = mean(atencion_24hs == "Sí", na.rm = TRUE) * 100) %>% 
      round(1)
    valueBox(
      paste0(porcentaje, "%"), "Atención 24hs", 
      icon = icon("clock"),
      color = "red"
    )
  })
  
  # Gráficos
  output$plot_categorias <- renderPlotly({
    datos_filtrados() %>% 
      count(categoria) %>% 
      plot_ly(labels = ~categoria, values = ~n, type = "pie") %>%
      layout(title = "Distribución por Categoría")
  })
  
  output$plot_barrios <- renderPlotly({
    datos_filtrados() %>% 
      count(barrio) %>% 
      plot_ly(x = ~barrio, y = ~n, type = "bar", color = ~barrio) %>%
      layout(title = "Servicios por Barrio",
             xaxis = list(title = ""),
             yaxis = list(title = "Cantidad"))
  })
  
  output$hist_rating <- renderPlotly({
    plot_ly(datos_filtrados(), x = ~rating, type = "histogram") %>%
      layout(title = "Distribución de Ratings",
             xaxis = list(title = "Rating"),
             yaxis = list(title = "Cantidad"))
  })
  
  output$boxplot_rating_cat <- renderPlotly({
    plot_ly(datos_filtrados(), y = ~rating, color = ~categoria, type = "box") %>%
      layout(title = "Rating por Categoría",
             yaxis = list(title = "Rating"))
  })
  
  # Mapa
  output$mapa_servicios <- renderLeaflet({
    datos <- datos_filtrados()
    
    # Crear paleta de colores por categoría
    pal <- colorFactor(topo.colors(length(unique(datos$categoria))), domain = datos$categoria)
    
    leaflet(datos) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~rating * 2,
        color = ~pal(categoria),
        stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste("<b>", nombre, "</b><br>",
                       "Categoría:", categoria, "<br>",
                       "Barrio:", barrio, "<br>",
                       "Rating:", rating, "<br>",
                       "24hs:", atencion_24hs)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~categoria,
                title = "Categorías", opacity = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server) 
