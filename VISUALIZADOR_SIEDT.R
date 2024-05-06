
# WORKING DIRECTORY -------------------------------------------------------

setwd("C:/Users/alexi/OneDrive/Documentos/RShiny")


# LIBRARY -----------------------------------------------------------------

library(shiny)
library(treemap)
library(openxlsx)
library(shinythemes)
library(fresh)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cowplot)


# DATA --------------------------------------------------------------------

datos_delitos <- read.xlsx("C:/Users/alexi/OneDrive/Documentos/RShiny/sied_territorial_2023.xlsx")


# USER INTERFACE ----------------------------------------------------------

# Configuración de idioma, fuentes y temas
theme_set(theme_grey())  # Se establece un tema minimalista


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("UV", "Seleccionar UV:", choices = c("Todos", unique(datos_delitos$UV)), selected = "Todos", multiple = TRUE),
      selectizeInput("Mes", "Seleccionar Mes:", choices = c("Todos", unique(datos_delitos$Mes)), selected = "Todos", multiple = TRUE),
      selectizeInput("Rango.Hora", "Seleccionar Rango Horario:", choices = c("Todos", unique(datos_delitos$Rango.Hora)), selected = "Todos", multiple = TRUE),
      selectInput("Delito", "Seleccionar Delito:", choices = c("Todos", unique(datos_delitos$Delito)), selected = names(sort(table(datos_delitos$Delito), decreasing = TRUE)[1:10]), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("grafico_barras"),
      plotOutput("matriz_dos_entradas"),
      plotOutput("tree_map"),
      plotOutput("line_plot")
    )
  )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  output$grafico_barras <- renderPlot({
    datos_filtrados <- datos_delitos %>% 
      filter(UV %in% input$UV | "Todos" %in% input$UV,
             Mes %in% input$Mes | "Todos" %in% input$Mes,
             Rango.Hora %in% input$Rango.Hora | "Todos" %in% input$Rango.Hora,
             Delito %in% input$Delito | "Todos" %in% input$Delito)
    
    grafico_barras <- ggplot(datos_filtrados, aes(x = fct_infreq(Delito), fill = Delito)) +
      geom_bar() +
      coord_flip() +
      labs(
        x = "Tipo de delito",
        y = "Cantidad",
        title = "Estadísticas por tipo de delito"
      ) +
      theme(plot.title = element_text(face = "bold"))  # Aplicar formato en negrita al título del gráfico
    
    return(grafico_barras)
  })
  
  output$matriz_dos_entradas <- renderPlot({
    datos_filtrados <- datos_delitos %>% 
      filter(UV %in% input$UV | "Todos" %in% input$UV,
             Mes %in% input$Mes | "Todos" %in% input$Mes,
             Rango.Hora %in% input$Rango.Hora | "Todos" %in% input$Rango.Hora,
             Delito %in% input$Delito | "Todos" %in% input$Delito) %>%
      group_by(Día, Rango.Hora) %>%
      summarise(Suma_Delitos = n())
    
    matriz_dos_entradas <- ggplot(datos_filtrados, aes(x = Día, y = Rango.Hora, fill = Suma_Delitos)) +
      geom_tile() +
      scale_x_discrete(name = "Día de la semana") +
      scale_y_discrete(name = "Rango horario") +
      labs(fill = "Número de delitos", title = "Estadísticas delictuales por rango horario y día de la semana") +
      theme(plot.title = element_text(face = "bold"))
    
    return(matriz_dos_entradas)
  })
  
  output$tree_map <- renderPlot({
    datos_filtrados <- datos_delitos %>% 
      filter(UV %in% input$UV | "Todos" %in% input$UV,
             Mes %in% input$Mes | "Todos" %in% input$Mes,
             Rango.Hora %in% input$Rango.Hora | "Todos" %in% input$Rango.Hora,
             Delito %in% input$Delito | "Todos" %in% input$Delito) %>%
      group_by(UV) %>%
      summarise(Cantidad_Delitos = n())
    
    tree_map <- treemap(datos_filtrados, index = "UV", vSize = "Cantidad_Delitos", title = "Tree Map: Cantidad de Delitos por UV")
    
    return(tree_map)
  })
  
  output$line_plot <- renderPlot({
    datos_filtrados <- datos_delitos %>% 
      filter(UV %in% input$UV | "Todos" %in% input$UV,
             Rango.Hora %in% input$Rango.Hora | "Todos" %in% input$Rango.Hora,
             Delito %in% input$Delito | "Todos" %in% input$Delito) %>%
      group_by(Mes) %>%
      summarise(Cantidad_Delitos = n())
    
    line_plot <- ggplot(datos_filtrados, aes(x = Mes, y = Cantidad_Delitos, group = 1)) +
      geom_line() +
      labs(
        x = "Mes",
        y = "Cantidad de Delitos",
        title = "Cantidad de Delitos por Mes"
      ) +
      scale_x_discrete(limits = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"))  # Asegurar que los meses estén ordenados correctamente en el eje x
    
    return(line_plot)
  })
}


# EJECUTAR LA APLICACION SHINY --------------------------------------------

shinyApp(ui, server)

