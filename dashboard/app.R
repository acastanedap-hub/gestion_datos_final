# app.R - Trabajo Final - Gestión de Datos
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
library(mice)
library(caret)

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
      menuItem("🔍 Análisis Básico Missing", tabName = "missing", icon = icon("search")),
      menuItem("🗺️ Mapa de Calor Missing", tabName = "heatmap", icon = icon("fire")),
      menuItem("📊 Análisis Avanzado Missing", tabName = "avanzado", icon = icon("chart-bar")),
      menuItem("📈 Análisis de Correlaciones", tabName = "correlaciones", icon = icon("chart-line")),
      menuItem("📋 Datos Completos", tabName = "datos", icon = icon("table")),
      menuItem("ℹ️ Acerca de", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Filtros opcionales (CON límite de observaciones)
    br(),
    h4("Opciones de Visualización", style = "padding-left: 15px;"),
    
    sliderInput("obs_limit", "Límite de observaciones:",
                min = 1000, max = 5000, value = 1000, step = 500),
    
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
                    tags$li("🔍 Análisis básico de valores faltantes"),
                    tags$li("🗺️ Mapa de calor de valores faltantes"),
                    tags$li("📊 Análisis avanzado e imputación con MICE"),
                    tags$li("📈 Matrices de correlación entre variables numéricas"),
                    tags$li("📋 Exploración completa del dataset")
                  ),
                  hr(),
                  h4("Resumen del Dataset:"),
                  uiOutput("resumen_dataset")
                )
              )
      ),
      
      # Pestaña de Análisis Básico de Missing Values
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
                  title = "Estadísticas de Missing Values",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  uiOutput("estadisticas_missing")
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
      
      # Pestaña - Mapa de Calor Exclusivo
      tabItem(tabName = "heatmap",
              fluidRow(
                box(
                  title = "🗺️ Mapa de Calor de Valores Faltantes",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  plotOutput("grafico_missing_heatmap", height = "600px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Opciones del Mapa de Calor",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(6,
                           sliderInput("sample_size", "Número de variables a mostrar:",
                                       min = 5, max = 30, value = 15, step = 5)
                    ),
                    column(6,
                           selectInput("heatmap_type", "Tipo de Visualización:",
                                       choices = c("Barras Horizontales" = "bars",
                                                   "Matriz Clásica" = "matrix"),
                                       selected = "bars")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Interpretación del Mapa de Calor",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("interpretacion_heatmap")
                )
              )
      ),
      
      # NUEVA PESTAÑA - Análisis Avanzado Missing
      tabItem(tabName = "avanzado",
              fluidRow(
                box(
                  title = "Selección de Variables para Análisis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(6,
                           selectInput("var_missing", "Variable con Missing:",
                                       choices = NULL, selected = NULL)
                    ),
                    column(6,
                           selectInput("var_comparacion", "Variable para Comparación:",
                                       choices = NULL, selected = NULL)
                    )
                  ),
                  actionButton("analizar_vars", "🔍 Ejecutar Análisis", 
                               style = "background-color: #367fa9; color: white;"),
                  br(), br(),
                  uiOutput("validacion_variables")
                )
              ),
              
              fluidRow(
                box(
                  title = "Análisis de Patrones de Missing",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("boxplot_missing_analysis", height = "400px")
                ),
                
                box(
                  title = "Resultados Test Estadístico",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("test_resultados")
                )
              ),
              
              fluidRow(
                box(
                  title = "Comparación Antes/Después Imputación MICE",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("comparacion_imputacion", height = "400px")
                ),
                
                box(
                  title = "Análisis de Outliers (IQR)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("grafico_outliers", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Información de Imputación MICE",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("info_imputacion")
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
      
      # Pestaña de Datos Completos - CON PAGINACIÓN
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "Dataset Completo - Smart Alerts",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Controles de paginación
                  fluidRow(
                    column(3,
                           numericInput("page_size", "Filas por página:", 
                                        value = 50, min = 10, max = 100, step = 10)
                    ),
                    column(3,
                           actionButton("first_page", "⏮️ Primera", 
                                        style = "background-color: #367fa9; color: white; margin-top: 25px;")
                    ),
                    column(3,
                           actionButton("prev_page", "◀️ Anterior", 
                                        style = "background-color: #367fa9; color: white; margin-top: 25px;")
                    ),
                    column(3,
                           actionButton("next_page", "Siguiente ▶️", 
                                        style = "background-color: #367fa9; color: white; margin-top: 25px;")
                    )
                  ),
                  br(),
                  # Información de paginación
                  uiOutput("pagination_info"),
                  br(),
                  # Tabla de datos
                  div(style = 'overflow-x: auto', 
                      tableOutput("tabla_datos")),
                  br(),
                  # Navegación adicional
                  fluidRow(
                    column(12, style = "text-align: center;",
                           actionButton("last_page", "⏭️ Última", 
                                        style = "background-color: #367fa9; color: white; margin: 5px;"),
                           actionButton("go_to_page", "Ir a página", 
                                        style = "background-color: #00a65a; color: white; margin: 5px;"),
                           numericInput("page_jump", NULL, value = 1, min = 1, width = "80px")
                    )
                  )
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
                    tags$li("Paquetes de visualización: ggplot2, naniar, corrplot, mice")
                  ),
                  h4("Funcionalidades:"),
                  tags$ul(
                    tags$li("Análisis básico y avanzado de valores faltantes"),
                    tags$li("Mapa de calor de missing values"),
                    tags$li("Análisis de patrones e imputación con MICE"),
                    tags$li("Detección de outliers con método IQR"),
                    tags$li("Matrices de correlación"),
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
  
  # Reactive values para la paginación
  pagination <- reactiveValues(
    current_page = 1,
    total_pages = 1,
    page_size = 50
  )
  
  # Cargar datos - CON LÍMITE DE OBSERVACIONES
  datos_reactivos <- reactive({
    db_host <- Sys.getenv("RDS_HOST")
    db_user <- Sys.getenv("RDS_USER")
    db_password <- Sys.getenv("RDS_PASSWORD")
    db_name <- Sys.getenv("RDS_DB")
    db_port <- 3306
    
    con <- dbConnect(
      MySQL(),
      host = db_host,
      port = db_port,
      dbname = db_name,
      user = db_user,
      password = db_password
    )
    
    # Obtener todos los datos primero
    df <- dbGetQuery(con, "SELECT * FROM smart_alerts")
    dbDisconnect(con)
    
    # Aplicar límite de observaciones
    obs_limit <- input$obs_limit
    if(nrow(df) > obs_limit) {
      df <- df[1:obs_limit, ]
    }
    
    return(df)
  })
  
  # Observar cambios en el tamaño de página
  observe({
    pagination$page_size <- input$page_size
    update_calculos_paginacion()
  })
  
  # Observar cambios en los datos para recalcular paginación
  observe({
    df <- datos_reactivos()
    update_calculos_paginacion()
  })
  
  # Función para actualizar cálculos de paginación
  update_calculos_paginacion <- function() {
    df <- datos_reactivos()
    pagination$total_pages <- ceiling(nrow(df) / pagination$page_size)
    
    # Asegurar que la página actual sea válida
    if(pagination$current_page > pagination$total_pages) {
      pagination$current_page <- max(1, pagination$total_pages)
    }
  }
  
  # Botones de navegación
  observeEvent(input$first_page, {
    pagination$current_page <- 1
  })
  
  observeEvent(input$prev_page, {
    if(pagination$current_page > 1) {
      pagination$current_page <- pagination$current_page - 1
    }
  })
  
  observeEvent(input$next_page, {
    if(pagination$current_page < pagination$total_pages) {
      pagination$current_page <- pagination$current_page + 1
    }
  })
  
  observeEvent(input$last_page, {
    pagination$current_page <- pagination$total_pages
  })
  
  observeEvent(input$go_to_page, {
    page_num <- input$page_jump
    if(!is.na(page_num) && page_num >= 1 && page_num <= pagination$total_pages) {
      pagination$current_page <- page_num
    }
  })
  
  # Información de paginación
  output$pagination_info <- renderUI({
    df <- datos_reactivos()
    total_rows <- nrow(df)
    start_row <- (pagination$current_page - 1) * pagination$page_size + 1
    end_row <- min(pagination$current_page * pagination$page_size, total_rows)
    
    tagList(
      h4(style = "color: #367fa9;", 
         paste("Mostrando filas", start_row, "a", end_row, "de", total_rows, "filas totales")),
      h5(style = "color: #666;", 
         paste("Página", pagination$current_page, "de", pagination$total_pages))
    )
  })
  
  # Datos paginados para la tabla
  datos_paginados <- reactive({
    df <- datos_reactivos()
    start_row <- (pagination$current_page - 1) * pagination$page_size + 1
    end_row <- min(pagination$current_page * pagination$page_size, nrow(df))
    
    if(nrow(df) > 0) {
      df[start_row:end_row, ]
    } else {
      df
    }
  })
  
  # Función para obtener variables válidas para análisis
  variables_validas <- reactive({
    df <- datos_reactivos()
    
    # Solo variables numéricas que tengan missing values y al menos 2 niveles en el indicador
    vars_validas <- names(df)[sapply(df, is.numeric)]
    
    # Filtrar variables que tienen missing values
    vars_con_missing <- vars_validas[sapply(df[vars_validas], function(x) sum(is.na(x)) > 0)]
    
    # Para cada variable con missing, verificar que tenga al menos 2 niveles en el indicador
    vars_finales <- c()
    for(var in vars_con_missing) {
      missing_indicator <- ifelse(is.na(df[[var]]), 1, 0)
      if(length(unique(missing_indicator)) >= 2) {
        vars_finales <- c(vars_finales, var)
      }
    }
    
    vars_finales
  })
  
  # Actualizar selectInputs para análisis avanzado con validaciones
  observe({
    df <- datos_reactivos()
    vars_validas_missing <- variables_validas()
    todas_numericas <- names(df)[sapply(df, is.numeric)]
    
    # Variables para missing (solo las válidas)
    updateSelectInput(session, "var_missing", 
                      choices = vars_validas_missing,
                      selected = ifelse(length(vars_validas_missing) > 0, vars_validas_missing[1], NULL))
    
    # Variables para comparación (todas las numéricas)
    updateSelectInput(session, "var_comparacion", 
                      choices = todas_numericas,
                      selected = ifelse(length(todas_numericas) > 1, todas_numericas[2], 
                                        ifelse(length(todas_numericas) > 0, todas_numericas[1], NULL)))
  })
  
  # Validación de variables seleccionadas
  output$validacion_variables <- renderUI({
    var_missing <- input$var_missing
    var_comparacion <- input$var_comparacion
    
    if(!is.null(var_missing) && !is.null(var_comparacion)) {
      df <- datos_reactivos()
      
      # Verificar que la variable de missing tenga al menos 2 niveles
      missing_indicator <- ifelse(is.na(df[[var_missing]]), 1, 0)
      niveles <- length(unique(missing_indicator))
      
      if(niveles < 2) {
        tags$div(
          style = "color: red; font-weight: bold; margin-top: 10px;",
          "❌ La variable seleccionada no tiene suficientes niveles para el análisis."
        )
      } else if(var_missing == var_comparacion) {
        tags$div(
          style = "color: orange; font-weight: bold; margin-top: 10px;",
          "⚠️ Las variables de missing y comparación son la misma."
        )
      } else {
        tags$div(
          style = "color: green; font-weight: bold; margin-top: 10px;",
          "✅ Variables válidas para análisis."
        )
      }
    }
  })
  
  # Datos para análisis avanzado con validación - MODIFICADO CON MICE
  datos_avanzados <- eventReactive(input$analizar_vars, {
    req(input$var_missing, input$var_comparacion)
    
    df <- datos_reactivos()
    var_missing <- input$var_missing
    var_comparacion <- input$var_comparacion
    
    # Validar que las variables sean diferentes
    if(var_missing == var_comparacion) {
      return(NULL)
    }
    
    # Crear variable indicador de missing
    missing_indicator <- ifelse(is.na(df[[var_missing]]), 1, 0)
    
    # Validar que hay al menos 2 niveles
    if(length(unique(missing_indicator)) < 2) {
      return(NULL)
    }
    
    df <- df %>%
      mutate(missing_indicator = missing_indicator)
    
    # IMPUTACIÓN CON MICE (Predictive Mean Matching)
    # Seleccionar columnas para imputación
    cols_imputacion <- c(var_missing, var_comparacion)
    df_impute <- df %>% select(all_of(cols_imputacion))
    
    # Realizar imputación MICE
    imputed_data <- mice(df_impute, m = 5, method = 'pmm', seed = 123, printFlag = FALSE)
    
    # Usar el primer dataset imputado
    df_imputed_complete <- complete(imputed_data, 1)
    
    # Agregar la columna imputada al dataframe original
    var_imputed <- paste0(var_missing, "_imputed_mice")
    df[[var_imputed]] <- df_imputed_complete[[var_missing]]
    
    # ANÁLISIS DE OUTLIERS
    # Función para detectar outliers usando IQR
    detect_outliers_iqr <- function(x) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      outliers <- x[x < lower_bound | x > upper_bound]
      return(outliers)
    }
    
    # Aplicar detección de outliers a las columnas cant_trx_m*
    cant_trx_cols <- df %>% select(starts_with("cant_trx_m"))
    outliers_list <- lapply(cant_trx_cols, detect_outliers_iqr)
    
    # Crear dataframe para gráfico de outliers
    outlier_counts_df <- data.frame(
      Variable = names(outliers_list),
      Outlier_Count = sapply(outliers_list, length)
    )
    
    list(
      data = df,
      var_missing = var_missing,
      var_comparacion = var_comparacion,
      var_imputed = var_imputed,
      imputed_data = imputed_data,
      outlier_counts = outlier_counts_df,
      missing_before = sum(is.na(df_impute[[var_missing]])),
      missing_after = sum(is.na(df_imputed_complete[[var_missing]]))
    )
  })
  
  # Resumen del dataset
  output$resumen_dataset <- renderUI({
    df <- datos_reactivos()
    tagList(
      p(strong("Número de observaciones:"), nrow(df)),
      p(strong("Número de variables:"), ncol(df)),
      p(strong("Variables numéricas:"), sum(sapply(df, is.numeric))),
      p(strong("Variables categóricas:"), sum(sapply(df, is.character))),
      p(strong("Total de valores faltantes:"), sum(is.na(df))),
      p(strong("Límite aplicado:"), input$obs_limit, "observaciones")
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
        labs(title = paste("Distribución de Valores Faltantes por Variable (", nrow(df), "obs.)"),
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
      head(20)
    
    ggplot(pattern_counts, aes(x = reorder(pattern, -n), y = n)) +
      geom_bar(stat = "identity", fill = "#00a65a", alpha = 0.8) +
      labs(title = paste("Patrones de Valores Faltantes (Top 20) -", nrow(df), "obs."),
           x = "Patrón (1 = faltante, 0 = presente)",
           y = "Frecuencia") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  # Estadísticas de Missing Values
  output$estadisticas_missing <- renderUI({
    df <- datos_reactivos()
    total_na <- sum(is.na(df))
    total_cells <- nrow(df) * ncol(df)
    percent_na <- round((total_na / total_cells) * 100, 2)
    
    variables_con_na <- sum(sapply(df, function(x) sum(is.na(x))) > 0)
    
    tagList(
      h4("Resumen General:"),
      p(strong("Número de observaciones:"), nrow(df)),
      p(strong("Límite aplicado:"), input$obs_limit),
      p(strong("Total de valores faltantes:"), total_na),
      p(strong("Porcentaje total de NA:"), paste0(percent_na, "%")),
      p(strong("Variables con NA:"), paste0(variables_con_na, " de ", ncol(df))),
      p(strong("Filas completas:"), sum(complete.cases(df)), "de", nrow(df)),
      hr(),
      h4("Variables con más NA:"),
      renderTable({
        na_summary <- data.frame(
          Variable = names(df),
          NA_Count = sapply(df, function(x) sum(is.na(x)))
        ) %>%
          filter(NA_Count > 0) %>%
          arrange(desc(NA_Count)) %>%
          head(5)
        
        na_summary
      }, width = "100%")
    )
  })
  
  # Mapa de calor de missing values - SIN UPSET PLOT
  output$grafico_missing_heatmap <- renderPlot({
    df <- datos_reactivos()
    nsets_selected <- input$sample_size
    
    if(input$heatmap_type == "bars") {
      # Gráfico de barras horizontales
      na_summary <- data.frame(
        Variable = names(df),
        Porcentaje_NA = round(sapply(df, function(x) sum(is.na(x))) / nrow(df) * 100, 2)
      ) %>%
        filter(Porcentaje_NA > 0) %>%
        arrange(Porcentaje_NA) %>%
        tail(nsets_selected)
      
      ggplot(na_summary, aes(x = reorder(Variable, Porcentaje_NA), y = Porcentaje_NA)) +
        geom_col(fill = "#ff6b6b", alpha = 0.8) +
        geom_text(aes(label = paste0(Porcentaje_NA, "%")), 
                  hjust = -0.1, size = 4, fontface = "bold") +
        coord_flip() +
        labs(title = paste("Porcentaje de Valores Faltantes por Variable -", nrow(df), "obs."),
             x = "Variables",
             y = "Porcentaje de Valores Faltantes (%)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_y_continuous(limits = c(0, 100))
      
    } else {
      # Matriz clásica
      missing_matrix <- as.data.frame(ifelse(is.na(df), 1, 0))
      na_counts <- colSums(missing_matrix)
      top_variables <- names(sort(na_counts, decreasing = TRUE))[1:min(nsets_selected, ncol(df))]
      missing_matrix_subset <- missing_matrix[, top_variables, drop = FALSE]
      
      if(ncol(missing_matrix_subset) > 1) {
        corrplot(cor(missing_matrix_subset), 
                 method = "color", 
                 type = "upper",
                 tl.col = "black",
                 tl.srt = 45,
                 title = paste("Correlación entre Variables con Valores Faltantes -", nrow(df), "obs."),
                 mar = c(0, 0, 2, 0))
      }
    }
  }, height = 600)
  
  # Interpretación del heatmap
  output$interpretacion_heatmap <- renderUI({
    df <- datos_reactivos()
    total_vars <- ncol(df)
    vars_con_na <- sum(sapply(df, function(x) sum(is.na(x))) > 0)
    
    tagList(
      h4("Cómo interpretar esta visualización:"),
      tags$ul(
        tags$li(strong("Barras Horizontales:"), "Muestra el porcentaje de valores faltantes por variable, ordenadas de mayor a menor."),
        tags$li(strong("Matriz Clásica:"), "Muestra la correlación entre la presencia de valores faltantes en diferentes variables.")
      ),
      hr(),
      p(strong("Resumen del dataset:"), 
        "De las", total_vars, "variables,", vars_con_na, "tienen valores faltantes."),
      p(strong("Observaciones analizadas:"), nrow(df), "(límite:", input$obs_limit, ")"),
      p("Usa los controles arriba para ajustar la visualización según tus necesidades.")
    )
  })
  
  # Boxplot para análisis de missing patterns - MODIFICADO
  output$boxplot_missing_analysis <- renderPlot({
    datos <- datos_avanzados()
    if(!is.null(datos)) {
      df <- datos$data
      var_comp <- datos$var_comparacion
      
      ggplot(df, aes(x = factor(missing_indicator), y = !!sym(var_comp), 
                     fill = factor(missing_indicator))) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste("Distribución de", var_comp, "según missing en", datos$var_missing),
             subtitle = paste("Basado en", nrow(df), "observaciones"),
             x = "Faltante (1=Sí, 0=No)",
             y = var_comp,
             fill = "Es Missing") +
        scale_fill_manual(values = c("0" = "#00a65a", "1" = "#ff6b6b")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      # Gráfico vacío con mensaje de error
      ggplot() + 
        annotate("text", x = 1, y = 1, 
                 label = "Seleccione variables válidas para el análisis", 
                 size = 6, fontface = "bold", color = "red") +
        theme_void()
    }
  })
  
  # Resultados del test t con validación
  output$test_resultados <- renderPrint({
    datos <- datos_avanzados()
    if(!is.null(datos)) {
      df <- datos$data
      var_comp <- datos$var_comparacion
      
      # Validar que hay suficientes observaciones en ambos grupos
      group_counts <- table(df$missing_indicator)
      
      if(any(group_counts < 2)) {
        cat("❌ ERROR: No hay suficientes observaciones en ambos grupos\n")
        cat("   para realizar el test t.\n\n")
        cat("Conteo por grupo:\n")
        print(group_counts)
        return()
      }
      
      # Realizar test t
      test_result <- tryCatch({
        t.test(as.formula(paste(var_comp, "~ missing_indicator")), data = df)
      }, error = function(e) {
        return(NULL)
      })
      
      if(is.null(test_result)) {
        cat("❌ ERROR: No se pudo realizar el test t.\n")
        cat("   Posiblemente no hay suficientes datos o hay problemas con las variables.\n")
        return()
      }
      
      cat("TEST T DE COMPARACIÓN DE MEDIAS\n")
      cat("================================\n\n")
      cat("Variable analizada:", datos$var_missing, "\n")
      cat("Variable de comparación:", var_comp, "\n")
      cat("Observaciones totales:", nrow(df), "\n\n")
      
      cat("RESULTADOS DEL TEST T:\n")
      cat("----------------------\n")
      cat("Estadístico t:", round(test_result$statistic, 4), "\n")
      cat("Valor p:", format.pval(test_result$p.value, digits = 4), "\n")
      cat("Grados de libertad:", round(test_result$parameter, 2), "\n\n")
      
      cat("INTERVALO DE CONFIANZA (95%):\n")
      cat("------------------------------\n")
      cat(round(test_result$conf.int[1], 4), "a", round(test_result$conf.int[2], 4), "\n\n")
      
      cat("INTERPRETACIÓN:\n")
      cat("---------------\n")
      if(test_result$p.value < 0.05) {
        cat("✅ Hay diferencia estadísticamente significativa (p < 0.05)\n")
        cat("   entre los grupos con y sin valores faltantes.\n")
      } else {
        cat("❌ No hay diferencia estadísticamente significativa (p >= 0.05)\n")
        cat("   entre los grupos con y sin valores faltantes.\n")
      }
    } else {
      cat("Seleccione variables válidas y haga clic en 'Ejecutar Análisis'\n")
    }
  })
  
  # Comparación antes/después de imputación MICE - NUEVO GRÁFICO
  output$comparacion_imputacion <- renderPlot({
    datos <- datos_avanzados()
    if(!is.null(datos)) {
      # Crear datos para comparación
      missing_counts <- data.frame(
        Imputation_Status = c("Antes de Imputación", "Después de Imputación"),
        Missing_Count = c(datos$missing_before, datos$missing_after)
      )
      
      ggplot(missing_counts, aes(x = Imputation_Status, y = Missing_Count, fill = Imputation_Status)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = Missing_Count), vjust = -0.5, size = 5, fontface = "bold") +
        labs(title = paste("Valores Faltantes en", datos$var_missing),
             subtitle = "Comparación Antes/Después de Imputación MICE",
             x = "Estado de Imputación",
             y = "Número de Valores Faltantes") +
        scale_fill_manual(values = c("Antes de Imputación" = "#ff6b6b", "Después de Imputación" = "#00a65a")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.position = "none")
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, 
                 label = "Seleccione variables válidas para el análisis", 
                 size = 6, fontface = "bold", color = "red") +
        theme_void()
    }
  })
  
  # Gráfico de outliers - NUEVO
  output$grafico_outliers <- renderPlot({
    datos <- datos_avanzados()
    if(!is.null(datos)) {
      ggplot(datos$outlier_counts, aes(x = Variable, y = Outlier_Count, fill = Variable)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = Outlier_Count), vjust = -0.5, size = 4, fontface = "bold") +
        labs(title = "Número de Outliers en Variables cant_trx_m*",
             subtitle = "Método IQR (1.5 * IQR)",
             x = "Variable",
             y = "Número de Outliers") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.position = "none")
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, 
                 label = "Seleccione variables válidas para el análisis", 
                 size = 6, fontface = "bold", color = "red") +
        theme_void()
    }
  })
  
  # Información de imputación MICE - MODIFICADO
  output$info_imputacion <- renderUI({
    datos <- datos_avanzados()
    if(!is.null(datos)) {
      df <- datos$data
      var_missing <- datos$var_missing
      var_imputed <- datos$var_imputed
      
      num_missing_before <- datos$missing_before
      num_missing_after <- datos$missing_after
      percent_reduction <- round((num_missing_before - num_missing_after) / num_missing_before * 100, 2)
      
      mean_imputed <- mean(df[[var_imputed]], na.rm = TRUE)
      mean_original <- mean(df[[var_missing]], na.rm = TRUE)
      
      # Información de outliers
      total_outliers <- sum(datos$outlier_counts$Outlier_Count)
      max_outliers_var <- datos$outlier_counts$Variable[which.max(datos$outlier_counts$Outlier_Count)]
      max_outliers_count <- max(datos$outlier_counts$Outlier_Count)
      
      tagList(
        h4("Resumen de Imputación MICE:"),
        p(strong("Variable imputada:"), var_missing),
        p(strong("Valores faltantes antes:"), num_missing_before),
        p(strong("Valores faltantes después:"), num_missing_after),
        p(strong("Reducción de missing values:"), paste0(percent_reduction, "%")),
        p(strong("Media original:"), round(mean_original, 4)),
        p(strong("Media después de imputación:"), round(mean_imputed, 4)),
        hr(),
        h4("Análisis de Outliers:"),
        p(strong("Total de outliers detectados:"), total_outliers),
        p(strong("Variable con más outliers:"), paste0(max_outliers_var, " (", max_outliers_count, " outliers)")),
        hr(),
        h4("Método de Imputación:"),
        p("Se utilizó MICE (Multiple Imputation by Chained Equations) con PMM (Predictive Mean Matching)."),
        p("Parámetros: m = 5 (5 datasets imputados), method = 'pmm', seed = 123")
      )
    } else {
      tags$div(
        style = "color: red; font-weight: bold;",
        "Seleccione variables válidas y haga clic en 'Ejecutar Análisis'"
      )
    }
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
  output$grafico_correlaciones <- renderPlot({
    df <- datos_reactivos()
    num_df <- df[sapply(df, is.numeric)]
    
    # Filtrar solo columnas numéricas válidas
    num_df <- num_df[, colSums(!is.na(num_df)) > 0]
    num_df <- num_df[, apply(num_df, 2, function(x) length(unique(na.omit(x))) > 1)]
    
    if(ncol(num_df) > 1) {
      corr_matrix <- cor(num_df, use = "complete.obs")
      
      corrplot(corr_matrix, 
               method = input$metodo_corr,
               type = input$tipo_corr,
               tl.col = "black", 
               tl.srt = 60,
               title = paste("Matriz de Correlaciones -", nrow(df), "observaciones"))
    } else {
      plot(1, type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, "No se pueden calcular correlaciones", cex = 1.2)
    }
  })
  
  # Tabla de datos completa - CON PAGINACIÓN
  output$tabla_datos <- renderTable({
    df <- datos_paginados()
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Ejecutar la aplicación
shinyApp(ui, server)