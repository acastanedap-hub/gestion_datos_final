# app.R - Trabajo Final - Gestión de Datos
#install.packages("shiny")
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)
library(naniar)
library(VIM)
library(corrplot)

readRenviron(".Renviron")
# Interfaz de usuario
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$span("📊 Trabajo Final - Gestión de Datos", 
                      style = "font-size: 20px; font-weight: bold;"),
    titleWidth = 500
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("🔍 Análisis de Valores Faltantes", tabName = "missing", icon = icon("search")),
      menuItem("📈 Análisis de Correlaciones", tabName = "correlaciones", icon = icon("chart-line")),
      menuItem("📋 Datos Completos", tabName = "datos", icon = icon("table")),
      menuItem("ℹ️ Acerca de", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Filtros opcionales
    br(),
    h4("Opciones de Visualización", style = "padding-left: 15px;"),
    
    sliderInput("obs_limit", "Límite de observaciones:",
                min = 100, max = 5000, value = 1000, step = 100),
    
    selectInput("color_theme", "Tema de colores:",
                choices = c("Azul" = "blue", "Verde" = "green", "Rojo" = "red", "Morado" = "purple"),
                selected = "blue"),
    
    actionButton("actualizar", "🔄 Actualizar Análisis", 
                 style = "background-color: #367fa9; color: white; margin: 15px; width: 90%;")
  ),
  
  # Cuerpo principal
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      # Pestaña de Inicio
      tabItem(tabName = "inicio",
              fluidRow(
                box(
                  title = "Bienvenido al Dashboard de Análisis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h3("Trabajo Final - Gestión de Datos"),
                  p("Este dashboard interactivo permite analizar el dataset de comercios con:"),
                  tags$ul(
                    tags$li("🔍 Análisis exhaustivo de valores faltantes"),
                    tags$li("📈 Matrices de correlación entre variables numéricas"),
                    tags$li("📊 Visualización de patrones de datos"),
                    tags$li("📋 Exploración completa del dataset")
                  ),
                  hr(),
                  h4("Resumen del Dataset:"),
                  uiOutput("resumen_dataset")
                )
              )
      ),
      
      # Pestaña de Análisis de Missing Values
      tabItem(tabName = "missing",
              fluidRow(
                box(
                  title = "Resumen de Valores Faltantes por Columna",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("grafico_missing_barras", height = "500px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Patrones de Valores Faltantes",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_patrones", height = "400px")
                ),
                
                box(
                  title = "Mapa de Calor de Valores Faltantes",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_missing_heatmap", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Tabla Resumen de Valores Faltantes",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("tabla_missing")
                )
              )
      ),
      
      # Pestaña de Correlaciones
      tabItem(tabName = "correlaciones",
              fluidRow(
                box(
                  title = "Matriz de Correlaciones",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("grafico_correlaciones", height = "600px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Opciones de la Matriz de Correlación",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                           selectInput("metodo_corr", "Método de visualización:",
                                       choices = c("Círculos" = "circle", "Cuadrados" = "square", 
                                                   "Elipses" = "ellipse", "Números" = "number"),
                                       selected = "circle")
                    ),
                    column(4,
                           selectInput("tipo_corr", "Tipo de matriz:",
                                       choices = c("Completa" = "full", "Superior" = "upper", 
                                                   "Inferior" = "lower"),
                                       selected = "upper")
                    ),
                    column(4,
                           checkboxInput("orden_corr", "Ordenar variables", value = TRUE)
                    )
                  )
                )
              )
      ),
      
      # Pestaña de Datos Completos
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "Dataset Completo - Smart Alerts",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(style = 'overflow-x: auto', 
                      tableOutput("tabla_datos"))
                )
              )
      ),
      
      # Pestaña Acerca de
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Acerca de este Proyecto",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  h3("Trabajo Final - Gestión de Datos"),
                  p("Dashboard interactivo desarrollado para el análisis exploratorio de datos."),
                  hr(),
                  h4("Tecnologías utilizadas:"),
                  tags$ul(
                    tags$li("R + Shiny para la interactividad"),
                    tags$li("Amazon RDS para la base de datos"),
                    tags$li("MySQL como motor de base de datos"),
                    tags$li("Paquetes de visualización: ggplot2, plotly, corrplot")
                  ),
                  h4("Funcionalidades:"),
                  tags$ul(
                    tags$li("Análisis de valores faltantes"),
                    tags$li("Matrices de correlación"),
                    tags$li("Visualización de patrones de datos"),
                    tags$li("Exploración interactiva del dataset")
                  )
                )
              )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Cargar datos
  # En tu server, modifica la función de conexión:
  # Cargar datos - Versión simple sin tryCatch
  datos_reactivos <- reactive({
    # Cargar variables desde .Renviron con valores por defecto
    db_host <- Sys.getenv("RDS_HOST")
    db_user <- Sys.getenv("RDS_USER")
    db_password <- Sys.getenv("RDS_PASSWORD")
    db_name <- Sys.getenv("RDS_DB")
    db_port <- 3306
    
    # Establecer conexión
    con <- dbConnect(
      MySQL(),
      host = db_host,
      port = db_port,
      dbname = db_name,
      user = db_user,
      password = db_password
    )
    
    # Obtener datos (limitar para no sobrecargar)
    df <- dbGetQuery(con, "SELECT * FROM smart_alerts")
    dbDisconnect(con)
    
    return(df)
  })
  
  # Resumen del dataset
  output$resumen_dataset <- renderUI({
    df <- datos_reactivos()
    tagList(
      p(strong("Número de observaciones:"), nrow(df)),
      p(strong("Número de variables:"), ncol(df)),
      p(strong("Variables numéricas:"), sum(sapply(df, is.numeric))),
      p(strong("Variables categóricas:"), sum(sapply(df, is.character))),
      p(strong("Total de valores faltantes:"), sum(is.na(df)))
    )
  })
  
  # Gráfico de barras de valores faltantes
  output$grafico_missing_barras <- renderPlot({
    df <- datos_reactivos()
    
    na_summary <- data.frame(
      Columna = names(df),
      NAs = sapply(df, function(x) sum(is.na(x))),
      Porcentaje = round(sapply(df, function(x) sum(is.na(x))) / nrow(df) * 100, 2)
    ) %>%
      filter(NAs > 0) %>%
      arrange(desc(NAs))
    
    if(nrow(na_summary) > 0) {
      ggplot(na_summary, aes(x = reorder(Columna, -NAs), y = NAs)) +
        geom_bar(stat = "identity", fill = "#3c8dbc", alpha = 0.8) +
        geom_text(aes(label = paste0(NAs, " (", Porcentaje, "%)")), 
                  vjust = -0.5, size = 4, fontface = "bold") +
        labs(title = "Distribución de Valores Faltantes por Variable",
             x = "Variables",
             y = "Número de Valores Faltantes") +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank()
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, 
                 label = "✅ No se encontraron valores faltantes", 
                 size = 8, fontface = "bold", color = "green") +
        theme_void()
    }
  })
  
  # Gráfico de patrones de missing
  output$grafico_patrones <- renderPlot({
    df <- datos_reactivos()
    
    patterns <- df %>%
      mutate(across(everything(), ~ifelse(is.na(.), 1, 0))) %>%
      unite("pattern", everything(), sep = "")
    
    pattern_counts <- patterns %>%
      count(pattern, sort = TRUE) %>%
      head(20)  # Mostrar solo los 20 patrones más frecuentes
    
    ggplot(pattern_counts, aes(x = reorder(pattern, -n), y = n)) +
      geom_bar(stat = "identity", fill = "#00a65a", alpha = 0.8) +
      labs(title = "Patrones de Valores Faltantes (Top 20)",
           x = "Patrón (1 = faltante, 0 = presente)",
           y = "Frecuencia") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  # Mapa de calor de missing values
  output$grafico_missing_heatmap <- renderPlot({
    df <- datos_reactivos()
    vis_miss(df) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Tabla de resumen de missing values
  output$tabla_missing <- renderTable({
    df <- datos_reactivos()
    
    na_summary <- data.frame(
      Columna = names(df),
      `Nº Valores Faltantes` = sapply(df, function(x) sum(is.na(x))),
      `Porcentaje (%)` = round(sapply(df, function(x) sum(is.na(x))) / nrow(df) * 100, 2)
    ) %>%
      filter(`Nº.Valores.Faltantes` > 0) %>%
      arrange(desc(`Nº.Valores.Faltantes`))
    
    na_summary
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  # Matriz de correlaciones
  # Versión MÁS SIMPLE Y SEGURA:
  output$grafico_correlaciones <- renderPlot({
    df <- datos_reactivos()
    num_df <- df[sapply(df, is.numeric)]
    
    # Filtrar solo columnas numéricas válidas
    num_df <- num_df[, colSums(!is.na(num_df)) > 0]  # Quitar columnas con solo NAs
    num_df <- num_df[, apply(num_df, 2, function(x) length(unique(na.omit(x))) > 1)]  # Quitar constantes
    
    if(ncol(num_df) > 1) {
      # Calcular correlación de forma segura
      corr_matrix <- cor(num_df, use = "complete.obs")
      
      corrplot(corr_matrix, 
               method = input$metodo_corr,
               type = input$tipo_corr,
               tl.col = "black", 
               tl.srt = 60)
    } else {
      plot(1, type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, "No se pueden calcular correlaciones", cex = 1.2)
    }
  })
  
  # Tabla de datos completa
  output$tabla_datos <- renderTable({
    df <- datos_reactivos()
    head(df, 100)  # Mostrar solo las primeras 100 filas
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Ejecutar la aplicación
shinyApp(ui, server)











