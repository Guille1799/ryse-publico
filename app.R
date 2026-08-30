# --- 1. Carga de Librerías ---
library(shiny)
library(tidyverse)
library(janitor)
library(scales)
library(htmltools)
library(ggcorrplot)
library(ranger)
library(markdown)
library(DT)
library(pdp)
library(iml)
library(cluster)
library(pROC)

# --- 2. Preparación de Datos y Nombres ---
kpi_names_full <- list( "win" = "Tasa de Victorias (win)", "gpm" = "Oro por Minuto (gpm)", "kda_ajustado" = "KDA (kda_ajustado)", "cs_pm" = "Súbditos por Minuto (cs_pm)", "vision_pm" = "Visión por Minuto (vision_pm)", "damage_dealt_to_objectives" = "Daño a Objetivos (damage_dealt_to_objectives)", "oci" = "Índice de Control de Objetivos (oci)" )
kpi_names_promedio <- list( "kda_promedio" = "KDA Promedio", "gpm_promedio" = "GPM Promedio", "vision_score_promedio" = "Visión Promedio", "cc_dealt_promedio" = "CC Infligido Promedio", "objetivos_promedio" = "Daño a Objetivos Promedio", "inhib_kills_promedio" = "Inhibidores Destruidos Promedio", "oci_promedio" = "OCI Promedio", "partidas_jugadas" = "Partidas Jugadas" )

cluster_descriptions_list <- list("TOP" = list("1" = "Agresivos de Combate", "2" = "De Utilidad y Control", "3" = "Enfocados a Objetivos"), "JUNGLE" = list("1" = "De Utilidad y Control de Mapa", "2" = "De Alto Impacto Económico (Carry)", "3" = "Agresivos de Combate (Gank-heavy)"), "MIDDLE" = list("1" = "Magos de Control y Asedio", "2" = "Asesinos y Roamers", "3" = "De Alto Impacto en Teamfights"), "BOTTOM" = list("1" = "Hypercarries de Juego Tardío", "2" = "Agresivos y de Dominancia en Línea", "3" = "De Utilidad y Apoyo"), "UTILITY" = list("1" = "De Control de Visión y 'Peel'", "2" = "Agresivos de 'Engage'", "3" = "Pasivos o de bajo impacto"))

# Cargamos la base de datos.
datos_raw <- read_csv("data/ryse_database.csv")

# Hacemos una limpieza inicial y calculamos el OCI.
df_limpio <- datos_raw %>%
  clean_names() %>%
  filter(tier %in% c("MASTER", "GRANDMASTER", "CHALLENGER"), individual_position != "Invalid", game_duration >= 900) %>%
  mutate(
    oci = (0.476 * tower_kills) + (0.397 * dragon_kills) + (0.127 * inhibitor_kills),
    display_name = paste0(game_name, " #", tag_line) 
  )

# Creamos el dataframe principal a nivel de partida.
df_kpis <- df_limpio %>%
  mutate(
    minutes = game_duration / 60,
    gpm = gold_earned / minutes,
    kda_ajustado = (kills + assists) / pmax(1, deaths),
    vision_pm = vision_score / minutes,
    cs_pm = total_minions_killed / minutes,
    individual_position = factor(individual_position, levels = c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY")),
    win = as.factor(win)
  ) %>%
  select(puuid, win, gpm, kda_ajustado, vision_pm, cs_pm, damage_dealt_to_objectives, oci, tier, individual_position, match_id, display_name)

# Calculamos la proporción de cada liga.
proporcion_ligas_total <- df_limpio %>% group_by(tier) %>% summarise(n_total = n(), .groups = "drop") %>% mutate(proporcion_total = n_total / sum(n_total))

# Creamos el dataframe agregado por jugador para el clustering.
df_agregado_con_tier <- df_limpio %>%
  group_by(puuid, individual_position, display_name) %>%
  summarise(
    kda_promedio = mean((kills + assists) / pmax(1, deaths), na.rm = TRUE), 
    gpm_promedio = mean(gold_earned / (game_duration / 60), na.rm = TRUE),
    vision_score_promedio = mean(vision_score, na.rm = TRUE),
    cc_dealt_promedio = mean(total_time_cc_dealt, na.rm = TRUE),
    objetivos_promedio = mean(damage_dealt_to_buildings, na.rm = TRUE),
    inhib_kills_promedio = mean(inhibitor_kills, na.rm = TRUE),
    oci_promedio = mean(oci, na.rm = TRUE),
    partidas_jugadas = n(),
    .groups = "drop"
  ) %>%
  filter(partidas_jugadas >= 5) %>%
  left_join(df_limpio %>% select(puuid, tier) %>% distinct(), by = "puuid")

# Nuevo dataframe para el análisis individualizado (>= 30 partidas)
df_jugadores_caso_estudio_base <- df_agregado_con_tier %>%
  filter(partidas_jugadas >= 30) %>%
  select(puuid, display_name, individual_position, tier, partidas_jugadas) %>%
  distinct()


# Función para predecir el cluster de un nuevo punto de datos
predict.kmeans <- function(km, data) {
  centers <- km$centers
  data_matrix <- as.matrix(data)
  
  ss_by_center <- rowSums(centers^2)
  ss_by_point <- rowSums(data_matrix^2)
  
  dist_mat <- outer(ss_by_point, ss_by_center, '+') - 2 * data_matrix %*% t(centers)
  
  max.col(-dist_mat)
}


# --- 3. Definición de la Interfaz de Usuario (UI) ---
ui <- fluidPage(
  titlePanel("Proyecto RYSE: Análisis de Rendimiento en League of Legends"),
  sidebarLayout(
    sidebarPanel(
      h3("Filtros Principales"),
      selectInput("rol_selector", "1. Selecciona un Rol:",
                  choices = c("TODOS", levels(df_kpis$individual_position)), selected = "TODOS"),
      conditionalPanel(
        condition = "input.rol_selector != 'TODOS'",
        selectInput("cluster_selector", "2. Selecciona un Perfil (Clúster):",
                    choices = c("TODOS", "1", "2", "3"))
      ),
      selectInput("tier_selector", "3. Selecciona un Rango:",
                  choices = c("TODOS", unique(df_kpis$tier)), selected = "TODOS")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Guía", br(), uiOutput("guia_ui")),
        tabPanel("Visión General", br(), fluidRow(column(4, uiOutput("winrate_box")), column(4, uiOutput("gpm_box")), column(4, uiOutput("kda_box"))), fluidRow(column(4, uiOutput("cspm_box")), column(4, uiOutput("visionpm_box")), column(4, uiOutput("oci_box"))), hr(), h4("Comparativa de Rendimiento por Rol"), p(tags$i("Nota: Este gráfico respeta el filtro de Rango, pero no el filtro de Rol. Muestra la media de la métrica seleccionada para TODOS los roles en el rango filtrado.")), selectInput("overview_kpi_selector", "Selecciona una métrica para comparar:", choices = setNames(names(kpi_names_full)[c(2:5,7)], kpi_names_full[c(2:5,7)])), plotOutput("overview_plot")),
        tabPanel("Perfiles de Jugador (Clustering)", br(), h4("Análisis de Arquetipos de Jugadores por Rol"), p("Usa el filtro de 'Rol' para explorar los diferentes estilos de juego. Se necesita seleccionar un rol específico."), hr(), plotOutput("plot_clusters"), hr(), plotOutput("plot_concentracion_relativa"), hr(), htmlOutput("text_clusters")),
        tabPanel("Análisis por Perfil", br(), p("Esta sección compara las métricas promedio de los 3 perfiles de jugador identificados para el rol seleccionado."), hr(),
                 tabsetPanel(
                   tabPanel("Rendimiento en Combate y Economía", plotOutput("profile_kpi_plot_1", height="600px")),
                   tabPanel("Rendimiento Estratégico y de Mapa", plotOutput("profile_kpi_plot_2", height="600px"))
                 )
        ),
        tabPanel("Perfil Ideal por Rol y Rango", br(), uiOutput("ideal_team_index_ui")),
        tabPanel("Análisis de Correlaciones", br(), plotOutput("cor_plot", height = "600px")),
        tabPanel("Factores Clave de Victoria", br(),
                 actionButton("run_analysis_fc", "Ejecutar Análisis de Factores Clave", icon = icon("play"), class = "btn-primary"),
                 hr(),
                 plotOutput("importance_plot"), hr(), uiOutput("model_summary"), hr(),
                 h4("Análisis de Impacto (Gráfico ALE)"),
                 p("Selecciona una de las variables más importantes para visualizar cómo afecta directamente a la probabilidad de victoria."),
                 uiOutput("ale_explanation_ui"),
                 uiOutput("pdp_selector_ui"),
                 plotOutput("pdp_plot")
        ),
        tabPanel("Consistencia del Jugador", br(),
                 uiOutput("consistency_explanation_ui"),
                 hr(),
                 selectInput("consistency_kpi", "Seleccionar Métrica de Rendimiento:", choices = c("GPM (gpm)" = "gpm_cv", "KDA (kda_ajustado)" = "kda_cv", "CS/min (cs_pm)" = "cs_cv")),
                 plotOutput("consistency_plot")),
        tabPanel("Informe Ejecutivo", br(), uiOutput("informe_automatico_ui")),
        tabPanel("Hallazgos Clave", br(), uiOutput("hallazgos_clave_ui")),
        tabPanel("Análisis Jugador", br(), uiOutput("player_analysis_ui"))
      )
    )
  )
)

# --- 4. Definición de la Lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  modelo_rf_reactivo <- reactiveVal(NULL)
  datos_modelo_reactivo <- reactiveVal(NULL)
  subtitle_modelo <- reactiveVal("Mostrando análisis general (filtros por defecto)")
  
  df_jugadores_caso_estudio_all <- reactive({
    df_jugadores_caso_estudio_base %>%
      select(puuid, display_name) %>%
      distinct()
  })
  
  perform_kmeans_clustering <- function(data, role_to_filter) {
    rol_data <- data %>% filter(individual_position == role_to_filter) %>% drop_na()
    
    kpi_cols <- names(kpi_names_promedio)
    rol_numeric_data <- rol_data %>% select(any_of(kpi_cols))
    
    rol_numeric_data_filtrado <- rol_numeric_data[, sapply(rol_numeric_data, var, na.rm = TRUE) > 0, drop = FALSE]
    
    if (ncol(rol_numeric_data_filtrado) < 3) return(NULL)
    
    rol_data_normalizada <- as.data.frame(scale(rol_numeric_data_filtrado))
    set.seed(123); modelo_kmeans <- kmeans(rol_data_normalizada, centers = 3, nstart = 25)
    
    rol_data_con_clusters <- rol_data %>% mutate(cluster = as.factor(modelo_kmeans$cluster))
    
    list(
      data_clusters = rol_data_con_clusters,
      modelo_kmeans = modelo_kmeans,
      centers_df = as.data.frame(modelo_kmeans$centers),
      scale_data = attr(rol_data_normalizada, "scaled:scale"), 
      center_data = attr(rol_data_normalizada, "scaled:center"),
      vars_usadas = colnames(rol_numeric_data_filtrado)
    )
  }
  
  clustering_data <- reactive({
    validate(need(input$rol_selector != "TODOS", "Por favor, selecciona un rol específico para generar los perfiles."))
    
    cluster_results <- perform_kmeans_clustering(df_agregado_con_tier, input$rol_selector)
    
    validate(need(!is.null(cluster_results), "No hay suficientes datos o varianza para realizar el clustering con el rol seleccionado."))
    
    cluster_results
  })
  
  datos_filtrados <- reactive({
    data <- df_kpis
    if (input$rol_selector != "TODOS") {
      data <- data %>% filter(individual_position == input$rol_selector)
      if (!is.null(input$cluster_selector) && input$cluster_selector != "TODOS") {
        req(clustering_data())
        jugadores_en_cluster <- clustering_data()$data_clusters %>%
          filter(cluster == input$cluster_selector) %>%
          pull(puuid)
        data <- data %>% filter(puuid %in% jugadores_en_cluster)
      }
    }
    if (input$tier_selector != "TODOS") {
      data <- data %>% filter(tier == input$tier_selector)
    }
    data %>% drop_na()
  })
  
  datos_consistencia <- reactive({
    datos_filtrados() %>%
      group_by(puuid) %>%
      summarise(n_matches = n(),
                winrate = mean(as.numeric(as.character(win)), na.rm = TRUE),
                gpm_mean = mean(gpm), gpm_sd = sd(gpm),
                kda_mean = mean(kda_ajustado), kda_sd = sd(kda_ajustado),
                cs_mean = mean(cs_pm), cs_sd = sd(cs_pm),
                .groups = "drop") %>%
      filter(n_matches >= 10) %>%
      mutate(gpm_cv = gpm_sd / gpm_mean,
             kda_cv = kda_sd / kda_mean,
             cs_cv = cs_sd / cs_mean)
  })
  
  observe({
    datos_iniciales <- df_kpis %>% drop_na()
    modelo_inicial <- ranger(
      formula = win ~ .,
      data = datos_iniciales %>% select(-tier, -individual_position, -puuid, -match_id, -display_name),
      importance = "impurity", probability = TRUE, num.trees = 300, keep.inbag = TRUE
    )
    modelo_rf_reactivo(modelo_inicial)
    datos_modelo_reactivo(datos_iniciales)
  })
  
  observeEvent(input$run_analysis_fc, {
    validate(need(nrow(datos_filtrados()) > 50, "No hay suficientes datos (>50 filas) para entrenar un modelo con estos filtros."))
    
    datos_para_modelo <- datos_filtrados()
    modelo_nuevo <- ranger(
      formula = win ~ .,
      data = datos_para_modelo %>% select(-tier, -individual_position, -puuid, -match_id, -display_name),
      importance = "impurity", probability = TRUE, num.trees = 300, keep.inbag = TRUE
    )
    modelo_rf_reactivo(modelo_nuevo)
    datos_modelo_reactivo(datos_para_modelo)
    subtitle_modelo(subtitle_text_flexible())
  })
  
  subtitle_text_flexible <- function(use_rol = TRUE, use_cluster = TRUE, use_tier = TRUE) {
    rol <- if(use_rol) input$rol_selector else "TODOS"
    perfil <- if(use_cluster && !is.null(input$cluster_selector) && rol != "TODOS") input$cluster_selector else "TODOS"
    rango <- if(use_tier) input$tier_selector else "TODOS"
    partes <- c()
    if (use_rol) partes <- c(partes, paste("Rol:", rol))
    if (use_cluster && rol != "TODOS") partes <- c(partes, paste("Perfil:", perfil))
    if (use_tier) partes <- c(partes, paste("Rango:", rango))
    if (length(partes) > 0) {
      return(paste("Filtros Aplicados |", paste(partes, collapse = " | ")))
    } else {
      return("Mostrando todos los datos (sin filtros)")
    }
  }
  
  calculo_perfiles_clave <- reactive({
    validate(
      need(input$rol_selector != "TODOS", "Por favor, selecciona un Rol para el informe."),
      need(input$tier_selector != "TODOS", "Por favor, selecciona un Rango para el informe.")
    )
    
    rol_actual <- input$rol_selector
    rango_seleccionado <- input$tier_selector
    data_cluster <- clustering_data()
    
    analisis_concentracion <- data_cluster$data_clusters %>%
      group_by(tier, cluster) %>% summarise(n_cluster = n(), .groups = "drop") %>%
      group_by(cluster) %>% mutate(proporcion_cluster = n_cluster / sum(n_cluster)) %>% ungroup() %>%
      left_join(proporcion_ligas_total, by = "tier") %>%
      mutate(indice_concentracion = proporcion_cluster / proporcion_total)
    
    perfil_popular_data <- analisis_concentracion %>%
      filter(tier == rango_seleccionado) %>%
      filter(indice_concentracion == max(indice_concentracion, na.rm = TRUE)) %>%
      slice(1)
    
    if (nrow(perfil_popular_data) == 0) return(NULL)
    
    pesos <- list(CARRY = c(rendimiento = 0.5, metrica_clave = 0.2, consistencia = 0.2, popularidad = 0.1), UTILIDAD = c(rendimiento = 0.2, metrica_clave = 0.4, consistencia = 0.3, popularidad = 0.1), HIBRIDO = c(rendimiento = 0.4, metrica_clave = 0.25, consistencia = 0.25, popularidad = 0.1))
    metricas_clave <- c(TOP="oci_promedio", JUNGLE="oci_promedio", MIDDLE="gpm_promedio", BOTTOM="kda_promedio", UTILITY="vision_score_promedio")
    arquetipos_rol <- c(TOP="HIBRIDO", JUNGLE="UTILIDAD", MIDDLE="CARRY", BOTTOM="CARRY", UTILITY="UTILIDAD")
    
    jugadores_contextualizados <- data_cluster$data_clusters %>% filter(tier == rango_seleccionado)
    if(nrow(jugadores_contextualizados) < 10) return(NULL)
    
    metricas_individuales <- df_kpis %>%
      filter(puuid %in% jugadores_contextualizados$puuid, individual_position == rol_actual) %>%
      group_by(puuid) %>%
      summarise(
        winrate = mean(as.numeric(as.character(win)), na.rm = TRUE),
        kda_cv = sd(kda_ajustado, na.rm = TRUE) / mean(kda_ajustado, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(jugadores_contextualizados %>% select(puuid, cluster, all_of(metricas_clave[[rol_actual]])), by = "puuid") %>%
      drop_na()
    
    if(nrow(metricas_individuales) < 10) return(NULL)
    
    p_score_rendimiento <- metricas_individuales %>% group_by(cluster) %>% summarise(mean_val = mean(winrate, na.rm = TRUE), .groups = "drop") %>% mutate(p_score = round(ecdf(metricas_individuales$winrate)(mean_val) * 100, 1))
    p_score_consistencia <- metricas_individuales %>% group_by(cluster) %>% summarise(mean_val = mean(kda_cv, na.rm = TRUE), .groups = "drop") %>% mutate(p_score = round(ecdf(-metricas_individuales$kda_cv)(-mean_val) * 100, 1))
    metrica_actual <- metricas_clave[[rol_actual]]
    p_score_metrica_clave <- metricas_individuales %>% group_by(cluster) %>% summarise(mean_val = mean(.data[[metrica_actual]], na.rm = TRUE), .groups = "drop") %>% mutate(p_score = round(ecdf(metricas_individuales[[metrica_actual]])(mean_val) * 100, 1))
    p_score_popularidad <- analisis_concentracion %>% filter(tier == rango_seleccionado) %>% mutate(p_score = round(cume_dist(indice_concentracion) * 100, 1))
    
    puntuacion_final <- p_score_rendimiento %>% select(cluster, p_score_win = p_score) %>%
      left_join(p_score_consistencia %>% select(cluster, p_score_con = p_score), by = "cluster") %>%
      left_join(p_score_metrica_clave %>% select(cluster, p_score_met = p_score), by = "cluster") %>%
      left_join(p_score_popularidad %>% select(cluster, p_score_pop = p_score), by = "cluster") %>%
      replace_na(list(p_score_win=50, p_score_con=50, p_score_met=50, p_score_pop=50)) %>%
      mutate(
        indice_idoneidad = (p_score_pop * pesos[[arquetipos_rol[rol_actual]]][["popularidad"]]) + (p_score_win * pesos[[arquetipos_rol[rol_actual]]][["rendimiento"]]) + (p_score_con * pesos[[arquetipos_rol[rol_actual]]][["consistencia"]]) + (p_score_met * pesos[[arquetipos_rol[rol_actual]]][["metrica_clave"]])
      ) %>%
      arrange(desc(indice_idoneidad))
    
    perfil_ideal_data <- puntuacion_final %>% slice(1)
    
    return(list(popular = perfil_popular_data, ideal = perfil_ideal_data))
  })
  
  output$guia_ui <- renderUI({
    tags$div(
      tags$h3(tags$b("Guía del Proyecto RYSE: Rendimiento y Salud en esports")),
      tags$p("Esta herramienta interactiva es un componente clave del ", tags$b("Trabajo de Fin de Máster 'Proyecto Rendimiento y Salud en esports (RYSE)'"), ", cuyo objetivo principal fue analizar el rendimiento competitivo de jugadores de alto nivel en ", tags$i("League of Legends"), ". El proyecto se enmarcó en el modelo de minería de datos ", tags$b("CRISP-DM"), ", el cual nos permitió abordar de forma sistemática el análisis, desde la comprensión del problema hasta la implementación de este dashboard. Hemos trabajado con datos públicos de jugadores de los rangos Maestro, Gran Maestro y Challenger, extraídos directamente de la API oficial de Riot Games."),
      tags$hr(),
      tags$h4(tags$b("1. Guía de Uso del Dashboard")),
      tags$p("Para explorar los hallazgos del proyecto, recomendamos seguir estos pasos:"),
      tags$ul(
        tags$li(tags$b("Filtros Principales:"), " En la barra lateral, comiencen seleccionando un rol específico. Esto activará el análisis de perfiles y los análisis contextualizados de las siguientes pestañas. También pueden filtrar por rango para ver las particularidades de cada liga."),
        tags$li(tags$b("Visión General:"), " Aquí se presenta un resumen de los indicadores clave de rendimiento (KPIs) para la selección de filtros actual."),
        tags$li(tags$b("Perfiles de Jugador (Clustering):"), " En esta pestaña, el dashboard les mostrará automáticamente los arquetipos de jugador identificados para el rol seleccionado. El análisis de concentración les permitirá ver si algún perfil es más popular en una liga de lo que se esperaría."),
        tags$li(tags$b("Análisis por Perfil:"), " Comparen directamente las estadísticas de cada arquetipo para entender sus fortalezas, debilidades y el impacto que tienen en el juego."),
        tags$li(tags$b("Perfil Ideal por Rol y Rango:"), " Esta es una de las funciones más avanzadas. Tras seleccionar un rango, la herramienta calculará un 'Índice de Idoneidad' personalizado para identificar qué perfil de jugador es el más robusto y efectivo para conseguir la victoria."),
        tags$li(tags$b("Análisis de Correlaciones:"), " Explora la relación entre las métricas de rendimiento y cómo se interrelacionan."),
        tags$li(tags$b("Factores Clave de Victoria:"), " Al hacer clic en 'Ejecutar Análisis', un modelo de ", tags$i("Machine Learning"), " se entrenará para identificar cuáles de los KPIs tienen mayor importancia predictiva para la victoria. El análisis de impacto muestra la relación real entre cada métrica y el éxito en la partida."),
        tags$li(tags$b("Consistencia del Jugador:"), " Analiza si la regularidad en el rendimiento de un jugador está ligada a un mayor éxito competitivo."),
        tags$li(tags$b("Informe Ejecutivo:"), " Este módulo genera un resumen redactado con los hallazgos más importantes de su análisis, comparando el perfil más popular con el estadísticamente más efectivo."),
        tags$li(tags$b("Análisis Jugador:"), " Una pestaña diseñada para el estudio de caso individualizado (jugadores con $\\ge 30$ partidas). Permite diagnosticar el arquetipo funcional y las métricas de un jugador específico.")
      ),
      tags$hr(),
      tags$h4(tags$b("2. Glosario de Métricas y Conceptos")),
      tags$p("A continuación se detallan los conceptos más importantes que se utilizan en la aplicación:"),
      tags$ul(
        tags$li(tags$b("Oro por Minuto (GPM):"), " Cantidad de oro que un jugador gana, en promedio, por minuto. Refleja su eficiencia económica."),
        tags$li(tags$b("KDA Ajustado:"), " Mide el rendimiento en combate con la fórmula ", tags$code("(Kills + Assists) / pmax(1, deaths)"), ". Se usa ", tags$code("pmax(1, deaths)"), " para evitar la división por cero sin penalizar a los jugadores con 0 muertes."),
        tags$li(tags$b("Súbditos por Minuto (CS/min):"), " Mide la eficiencia en la acumulación de recursos y experiencia a través de la eliminación de súbditos."),
        tags$li(tags$b("Visión por Minuto:"), " Refleja la capacidad del jugador para controlar el mapa y proporcionar información estratégica al equipo."),
        tags$li(tags$b("Índice de Control de Objetivos (OCI v3.0):"), " Métrica personalizada que pondera la participación en la destrucción de torres, dragones e inhibidores. Los pesos proceden de un GLM normalizado; barones y heraldos quedaron descartados por Lasso, Random Forest y XGBoost. Fórmula: $\\text{OCI} = (0.476 \\times \\text{Torres}) + (0.397 \\times \\text{Dragones}) + (0.127 \\times \\text{Inhibidores})$"),
        tags$li(tags$b("Coeficiente de Variación (CV):"), " Mide la irregularidad o volatilidad del rendimiento de un jugador. $\\text{CV} = \\sigma / \\mu$. Un CV bajo indica alta consistencia. Es la métrica clave para el componente 'Salud'."),
        tags$li(tags$b("Perfil / Clúster:"), " Grupo de jugadores con un estilo de juego similar, identificado automáticamente por el algoritmo de K-Means."),
        tags$li(tags$b("Índice de Concentración:"), " Mide si un perfil es más o menos común en una liga que lo esperado por azar (un valor > 1 indica sobrerrepresentación)."),
        tags$li(tags$b("Gráfico de Efectos Locales Acumulados (ALE):"), " Muestra el impacto real de una única variable sobre la probabilidad de victoria. A diferencia de otros métodos como los PDP, los gráficos ALE son robustos ante variables correlacionadas (multicolinealidad) ya que calculan el efecto analizando únicamente combinaciones de datos que ocurren en la realidad. Esto evita distorsiones y ofrece una interpretación más fiable.",
                tags$ul(
                  tags$li(tags$b("La 'Alfombra de Datos' (Rug Plot):"), " La serie de pequeñas líneas verticales en la parte inferior del gráfico ALE se conoce como 'rug plot'. Cada línea representa partidas reales de la base de datos, mostrando la distribución de los datos. Las zonas más densas u oscuras indican dónde el modelo tiene más información para aprender, haciendo que la curva en esa área sea más fiable.")
                )
        )
      ),
      tags$hr(),
      tags$h4(tags$b("3. Metodología del Dashboard")),
      tags$ul(
        tags$li(tags$b("Baremo por Percentiles (P-Score) en Índice de Idoneidad:"), " Hemos abandonado el sistema de puntos fijos. La puntuación de Rendimiento, Consistencia, Métrica Clave y Popularidad se calcula ahora mediante un sistema de percentiles (P-Score de 0 a 100). Esto compara el rendimiento de un perfil directamente con todos los demás perfiles de su rol/rango, elevando la precisión y la validez prescriptiva."),
        tags$li(tags$b("Ponderación Dinámica por Rol:"), " El Índice de Idoneidad aplica pesos estratégicos que varían según el rol para reflejar la prioridad de juego: ", tags$br(),tags$ul(tags$li(tags$b("Roles CARRY (Middle, Bottom):"), " [Rendimiento: 50%], [Métrica Clave: 20%], [Consistencia: 20%], [Popularidad: 10%]"), tags$li(tags$b("Roles UTILIDAD (Jungle, Utility):"), " [Métrica Clave: 40%], [Consistencia: 30%], [Rendimiento: 20%], [Popularidad: 10%]"), tags$li(tags$b("Rol HÍBRIDO (Top):"), " [Rendimiento: 40%], [Métrica Clave: 25%], [Consistencia: 25%], [Popularidad: 10%]"))
        )
      ),
      tags$hr(),
      tags$h4(tags$b("4. Futuras Líneas de Investigación")),
      tags$p("El trabajo de escalabilidad propone el ", tags$b("Análisis de Sinergia de Composiciones de Equipo"), ", utilizando el ", tags$code("match_id"), " para pasar de la unidad individual (el jugador y su perfil) a la unidad colectiva (la combinación de 5 perfiles) para identificar sinergias de victoria. También se propone la ", tags$b("Optimización Empírica de Fórmulas"), " (Grid Search) y la ", tags$b("Integración del Componente Salud (S de RYSE)"), " cruzando la Consistencia (CV) con datos de bienestar.")
    )
  })
  
  output$winrate_box <- renderUI({ winrate_val <- mean(as.numeric(as.character(datos_filtrados()$win))); tags$div(class = "well well-sm", tags$h3(style = "text-align: center;", kpi_names_full["win"]), tags$p(style = "text-align: center; font-size: 24px;", percent(winrate_val, accuracy = 0.1))) })
  output$gpm_box <- renderUI({ gpm_val <- mean(datos_filtrados()$gpm); tags$div(class = "well well-sm", tags$h3(style = "text-align: center;", kpi_names_full["gpm"]), tags$p(style = "text-align: center; font-size: 24px;", round(gpm_val, 1))) })
  output$kda_box <- renderUI({ kda_val <- mean(datos_filtrados()$kda_ajustado); tags$div(class = "well well-sm", tags$h3(style = "text-align: center;", kpi_names_full["kda_ajustado"]), tags$p(style = "text-align: center; font-size: 24px;", round(kda_val, 2))) })
  output$cspm_box <- renderUI({ cspm_val <- mean(datos_filtrados()$cs_pm); tags$div(class = "well well-sm", tags$h3(style = "text-align: center;", kpi_names_full["cs_pm"]), tags$p(style = "text-align: center; font-size: 24px;", round(cspm_val, 2))) })
  output$visionpm_box <- renderUI({ visionpm_val <- mean(datos_filtrados()$vision_pm); tags$div(class = "well well-sm", tags$h3(style = "text-align: center;", kpi_names_full["vision_pm"]), tags$p(style = "text-align: center; font-size: 24px;", round(visionpm_val, 2))) })
  output$oci_box <- renderUI({ oci_val <- mean(datos_filtrados()$oci); tags$div(class = "well well-sm", tags$h3(style = "text-align: center;", kpi_names_full["oci"]), tags$p(style = "text-align: center; font-size: 24px;", round(oci_val, 2))) })
  
  output$overview_plot <- renderPlot({
    data_para_plot <- df_kpis
    if (input$tier_selector != "TODOS") {
      data_para_plot <- data_para_plot %>% filter(tier == input$tier_selector)
    }
    kpi_seleccionado <- input$overview_kpi_selector
    kpi_label <- kpi_names_full[[kpi_seleccionado]]
    summary_data <- data_para_plot %>% group_by(individual_position) %>% summarise(mean_kpi = mean(.data[[kpi_seleccionado]], na.rm = TRUE))
    ggplot(summary_data, aes(x = individual_position, y = mean_kpi, fill = individual_position)) +
      geom_col() +
      geom_text(aes(label = round(mean_kpi, 2)), vjust = -0.5) +
      labs(title = paste("Promedio de", kpi_label, "por Rol"),
           subtitle = paste("Filtro de Rango Aplicado:", input$tier_selector),
           x = "Rol", y = paste("Valor Promedio de", kpi_label)) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
  })
  
  output$plot_clusters <- renderPlot({
    data <- clustering_data()
    centros_reales <- data$data_clusters %>% group_by(cluster) %>% summarise(gpm_promedio = mean(gpm_promedio), kda_promedio = mean(kda_promedio))
    ggplot(data$data_clusters, aes(x = gpm_promedio, y = kda_promedio, color = cluster)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_point(data = centros_reales, aes(x = gpm_promedio, y = kda_promedio, color = cluster), shape = 8, size = 6, stroke = 1.5, show.legend = FALSE) +
      labs(title = paste("Perfiles de Jugadores para el Rol de", input$rol_selector),
           subtitle = paste("Filtro Aplicado | Rol:", input$rol_selector),
           x = kpi_names_promedio[["gpm_promedio"]], y = kpi_names_promedio[["kda_promedio"]], color = "Perfil de Jugador (Clúster)") +
      theme_minimal(base_size = 16)
  })
  
  output$plot_concentracion_relativa <- renderPlot({
    data <- clustering_data()
    analisis_concentracion <- data$data_clusters %>% group_by(tier, cluster) %>% summarise(n_cluster = n(), .groups = "drop") %>% group_by(cluster) %>% mutate(proporcion_cluster = n_cluster / sum(n_cluster)) %>% ungroup() %>% left_join(proporcion_ligas_total, by = "tier") %>% mutate(indice_concentracion = proporcion_cluster / proporcion_total)
    ggplot(analisis_concentracion, aes(x = cluster, y = indice_concentracion, fill = tier)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = round(indice_concentracion, 2)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3.5) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      labs(title = paste("Concentración de Perfiles por Liga para el Rol de", input$rol_selector),
           subtitle = paste("Filtro Aplicado | Rol:", input$rol_selector),
           x = "Perfil de Jugador (Clúster)", y = "Índice de Concentración Relativa", fill = "Liga") +
      theme_minimal(base_size = 16)
  })
  
  output$text_clusters <- renderUI({
    data <- clustering_data()
    centros <- data$centers_df 
    
    analisis_concentracion <- data$data_clusters %>% group_by(tier, cluster) %>% summarise(n_cluster = n(), .groups = "drop") %>% group_by(cluster) %>% mutate(proporcion_cluster = n_cluster / sum(n_cluster)) %>% ungroup() %>% left_join(proporcion_ligas_total, by = "tier") %>% mutate(indice_concentracion = proporcion_cluster / proporcion_total)
    
    descripciones <- "### Interpretación de los Perfiles Identificados\n\n"
    for (i in 1:nrow(centros)) {
      descripcion_titulo <- cluster_descriptions_list[[input$rol_selector]][[as.character(i)]]
      descripciones <- paste0(descripciones, "#### Perfil ", i, ": Los '", descripcion_titulo, "'\n\n")
      descripciones <- paste0(descripciones, "**Características Principales (valores Z-score):**\n")
      
      for (col in colnames(centros)) {
        descripciones <- paste0(descripciones, "* **", kpi_names_promedio[[col]], "**: ", round(centros[i, col], 2), "\n")
      }
      
      concentracion_cluster_actual <- analisis_concentracion %>% filter(cluster == i)
      if (nrow(concentracion_cluster_actual) > 0) {
        descripciones <- paste0(descripciones, "\n**Distribución por Liga:**\n")
        for (j in 1:nrow(concentracion_cluster_actual)) {
          descripciones <- paste0(descripciones, "* **", concentracion_cluster_actual$tier[j], "**: Índice de concentración de ", round(concentracion_cluster_actual$indice_concentracion[j], 2), ".\n")
        }
      }
      descripciones <- paste0(descripciones, "\n\n")
    }
    HTML(markdown(descripciones))
  })
  
  output$profile_kpi_plot_1 <- renderPlot({
    validate(need(input$rol_selector != "TODOS", "Por favor, selecciona un rol específico en el filtro principal."))
    data_con_clusters <- clustering_data()$data_clusters
    if (input$tier_selector != "TODOS") {
      data_con_clusters <- data_con_clusters %>% filter(tier == input$tier_selector)
    }
    validate(need(nrow(data_con_clusters) > 0, "Datos insuficientes para el Rol y Rango seleccionados."))
    
    cluster_descriptions_df <- data.frame(cluster = as.factor(1:3), profile_name = unlist(list("TOP" = list("1" = "Agresivos", "2" = "De Utilidad", "3" = "Objetivos"), "JUNGLE" = list("1" = "Utilidad/Control", "2" = "Carry Económico", "3" = "Agresivo/Ganks"), "MIDDLE" = list("1" = "Control/Asedio", "2" = "Asesino/Roamer", "3" = "Impacto en TFs"), "BOTTOM" = list("1" = "Hypercarry", "2" = "Dominante en Línea", "3" = "De Utilidad"), "UTILITY" = list("1" = "Control/Peel", "2" = "Agresivo/Engage", "3" = "Pasivo"))[[input$rol_selector]]))
    
    kpis_a_mostrar <- c("kda_promedio", "gpm_promedio", "objetivos_promedio", "cc_dealt_promedio")
    summary_data <- data_con_clusters %>%
      left_join(cluster_descriptions_df, by = "cluster") %>%
      group_by(profile_name) %>%
      summarise(across(all_of(kpis_a_mostrar), mean, na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(cols = -profile_name, names_to = "kpi", values_to = "valor") %>%
      mutate(kpi_label = as.character(kpi_names_promedio[kpi]))
    
    ggplot(summary_data, aes(x = profile_name, y = valor, fill = profile_name)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(valor, 2)), vjust = -0.5) +
      facet_wrap(~ kpi_label, scales = "free_y", ncol = 2) +
      labs(title = paste("Métricas de Combate y Economía para Perfiles de", input$rol_selector), subtitle = subtitle_text_flexible(use_cluster = FALSE), x = "Nombre del Perfil de Jugador", y = "Valor Promedio") +
      theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  output$profile_kpi_plot_2 <- renderPlot({
    validate(need(input$rol_selector != "TODOS", "Por favor, selecciona un rol específico en el filtro principal."))
    data_con_clusters <- clustering_data()$data_clusters
    if (input$tier_selector != "TODOS") {
      data_con_clusters <- data_con_clusters %>% filter(tier == input$tier_selector)
    }
    validate(need(nrow(data_con_clusters) > 0, "Datos insuficientes para el Rol y Rango seleccionados."))
    
    cluster_descriptions_df <- data.frame(cluster = as.factor(1:3), profile_name = unlist(list("TOP" = list("1" = "Agresivos", "2" = "De Utilidad", "3" = "Objetivos"), "JUNGLE" = list("1" = "Utilidad/Control", "2" = "Carry Económico", "3" = "Agresivo/Ganks"), "MIDDLE" = list("1" = "Control/Asedio", "2" = "Asesino/Roamer", "3" = "Impacto en TFs"), "BOTTOM" = list("1" = "Hypercarry", "2" = "Dominante en Línea", "3" = "De Utilidad"), "UTILITY" = list("1" = "Control/Peel", "2" = "Agresivo/Engage", "3" = "Pasivo"))[[input$rol_selector]]))
    
    kpis_a_mostrar <- c("vision_score_promedio", "oci_promedio", "inhib_kills_promedio", "partidas_jugadas")
    summary_data <- data_con_clusters %>%
      left_join(cluster_descriptions_df, by = "cluster") %>%
      group_by(profile_name) %>%
      summarise(across(all_of(kpis_a_mostrar), mean, na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(cols = -profile_name, names_to = "kpi", values_to = "valor") %>%
      mutate(kpi_label = as.character(kpi_names_promedio[kpi]))
    
    ggplot(summary_data, aes(x = profile_name, y = valor, fill = profile_name)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(valor, 2)), vjust = -0.5) +
      facet_wrap(~ kpi_label, scales = "free_y", ncol = 2) +
      labs(title = paste("Métricas de Visión y Macro para Perfiles de", input$rol_selector), subtitle = subtitle_text_flexible(use_cluster = FALSE), x = "Nombre del Perfil de Jugador", y = "Valor Promedio") +
      theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  output$ideal_team_index_ui <- renderUI({
    validate(need(input$tier_selector != "TODOS", "Por favor, selecciona un Rango específico en el panel de filtros para calcular el Perfil Ideal."))
    
    rango_seleccionado <- input$tier_selector
    pesos <- list(CARRY = c(rendimiento = 0.5, metrica_clave = 0.2, consistencia = 0.2, popularidad = 0.1), UTILIDAD = c(rendimiento = 0.2, metrica_clave = 0.4, consistencia = 0.3, popularidad = 0.1), HIBRIDO = c(rendimiento = 0.4, metrica_clave = 0.25, consistencia = 0.25, popularidad = 0.1))
    metricas_clave <- c(TOP="oci_promedio", JUNGLE="oci_promedio", MIDDLE="gpm_promedio", BOTTOM="kda_promedio", UTILITY="vision_score_promedio")
    arquetipos_rol <- c(TOP="HIBRIDO", JUNGLE="UTILIDAD", MIDDLE="CARRY", BOTTOM="CARRY", UTILITY="UTILIDAD")
    
    ideal_team <- list()
    
    for(rol_actual in levels(df_kpis$individual_position)) {
      
      cluster_results <- perform_kmeans_clustering(df_agregado_con_tier, rol_actual)
      if(is.null(cluster_results)) { ideal_team[[rol_actual]] <- "Datos insuficientes para clustering"; next }
      
      rol_data_con_clusters <- cluster_results$data_clusters
      
      jugadores_contextualizados <- rol_data_con_clusters %>% filter(tier == rango_seleccionado)
      if(nrow(jugadores_contextualizados) < 10) { ideal_team[[rol_actual]] <- "Datos insuficientes en este rango"; next }
      
      metricas_individuales <- df_kpis %>%
        filter(puuid %in% jugadores_contextualizados$puuid, individual_position == rol_actual) %>%
        group_by(puuid) %>%
        summarise(
          winrate = mean(as.numeric(as.character(win)), na.rm = TRUE),
          kda_cv = sd(kda_ajustado, na.rm = TRUE) / mean(kda_ajustado, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(jugadores_contextualizados %>% select(puuid, cluster, all_of(metricas_clave[[rol_actual]])), by = "puuid") %>%
        drop_na()
      
      if(nrow(metricas_individuales) < 10) { ideal_team[[rol_actual]] <- "Datos insuficientes post-procesado"; next }
      
      p_score_rendimiento <- metricas_individuales %>% group_by(cluster) %>% summarise(mean_val = mean(winrate, na.rm = TRUE), .groups = "drop") %>% mutate(p_score = round(ecdf(metricas_individuales$winrate)(mean_val) * 100, 1))
      p_score_consistencia <- metricas_individuales %>% group_by(cluster) %>% summarise(mean_val = mean(kda_cv, na.rm = TRUE), .groups = "drop") %>% mutate(p_score = round(ecdf(-metricas_individuales$kda_cv)(-mean_val) * 100, 1))
      metrica_actual <- metricas_clave[[rol_actual]]
      p_score_metrica_clave <- metricas_individuales %>% group_by(cluster) %>% summarise(mean_val = mean(.data[[metrica_actual]], na.rm = TRUE), .groups = "drop") %>% mutate(p_score = round(ecdf(metricas_individuales[[metrica_actual]])(mean_val) * 100, 1))
      
      analisis_concentracion <- jugadores_contextualizados %>%
        group_by(cluster) %>% summarise(n = n(), .groups = "drop") %>%
        mutate(proporcion_en_rango = n / sum(n)) %>%
        left_join(proporcion_ligas_total %>% filter(tier == rango_seleccionado) %>% select(proporcion_total), by = character()) %>%
        mutate(indice_concentracion = proporcion_en_rango / proporcion_total)
      p_score_popularidad <- analisis_concentracion %>% mutate(p_score = round(cume_dist(indice_concentracion) * 100, 1))
      
      puntuacion_final <- p_score_rendimiento %>% select(cluster, p_score_win = p_score) %>%
        left_join(p_score_consistencia %>% select(cluster, p_score_con = p_score), by = "cluster") %>%
        left_join(p_score_metrica_clave %>% select(cluster, p_score_met = p_score), by = "cluster") %>%
        left_join(p_score_popularidad %>% select(cluster, p_score_pop = p_score), by = "cluster") %>%
        replace_na(list(p_score_win=50, p_score_con=50, p_score_met=50, p_score_pop=50)) %>%
        mutate(
          indice_idoneidad = (p_score_pop * pesos[[arquetipos_rol[rol_actual]]][["popularidad"]]) + (p_score_win * pesos[[arquetipos_rol[rol_actual]]][["rendimiento"]]) + (p_score_con * pesos[[arquetipos_rol[rol_actual]]][["consistencia"]]) + (p_score_met * pesos[[arquetipos_rol[rol_actual]]][["metrica_clave"]])
        ) %>%
        arrange(desc(indice_idoneidad))
      
      perfil_ganador <- puntuacion_final$cluster[1]
      ideal_team[[rol_actual]] <- paste("Perfil", perfil_ganador, "-", cluster_descriptions_list[[rol_actual]][[as.character(perfil_ganador)]])
    }
    
    tags$div(
      tags$h3(paste("Perfil Ideal por Rol para el Rango:", rango_seleccionado)),
      tags$p("Esta alineación se basa en un 'Índice de Idoneidad' multicriterio que evalúa los perfiles de cada rol."),
      tags$hr(),
      tags$ul(
        tags$li(tags$strong("TOP: "), ideal_team[["TOP"]]),
        tags$li(tags$strong("JUNGLE: "), ideal_team[["JUNGLE"]]),
        tags$li(tags$strong("MIDDLE: "), ideal_team[["MIDDLE"]]),
        tags$li(tags$strong("BOTTOM: "), ideal_team[["BOTTOM"]]),
        tags$li(tags$strong("UTILITY: "), ideal_team[["UTILITY"]])
      ),
      tags$hr(),
      tags$h4("¿Cómo se ha calculado esto? La Metodología del 'Índice de Idoneidad' con Baremo por Percentiles (P-Score)"),
      tags$p("Para asegurar una selección robusta y precisa, el perfil ideal se elige mediante un índice compuesto que evalúa cada perfil en cuatro áreas clave. Todos los cálculos se realizan de forma contextual, usando solo datos del rango seleccionado."),
      tags$ul(
        tags$li(tags$strong("1. Criterios de Evaluación:"), " Se miden 4 áreas: Rendimiento (Winrate), Consistencia (inverso del CV de KDA), Popularidad (Índice de Concentración) y Dominancia en una Métrica Clave para el rol (ej. OCI para Jungla)."),
        tags$li(tags$strong("2. Sistema de Puntuación (P-Score):"), " Se abandona el sistema de puntos fijos. Ahora, para cada criterio, el rendimiento promedio de un perfil se compara con la distribución de todos los jugadores de ese rol/rango para obtener su percentil (P-Score de 0 a 100). Esto proporciona una puntuación única y granular."),
        tags$li(tags$strong("3. Ponderación Dinámica por Rol:"), " Los 4 P-Scores se combinan usando pesos diferentes según el arquetipo de cada rol, para reflejar qué es más importante para cada posición:")
      ),
      tags$ul(
        tags$li(tags$b("Roles CARRY (Middle, Bottom):"), " [Rendimiento: 50%], [Métrica Clave: 20%], [Consistencia: 20%], [Popularidad: 10%]"),
        tags$li(tags$b("Roles UTILIDAD (Jungle, Utility):"), " [Métrica Clave: 40%], [Consistencia: 30%], [Rendimiento: 20%], [Popularidad: 10%]"),
        tags$li(tags$b("Rol HÍBRIDO (Top):"), " [Rendimiento: 40%], [Métrica Clave: 25%], [Consistencia: 25%], [Popularidad: 10%]")
      ),
      tags$p(tags$b("El perfil con la puntuación final más alta en este 'Índice de Idoneidad' es el seleccionado como ideal."))
    )
  })
  
  output$cor_plot <- renderPlot({
    cor_data <- datos_filtrados() %>% select(any_of(names(kpi_names_full)))
    colnames(cor_data) <- as.character(kpi_names_full[names(cor_data)])
    corr_matrix <- round(cor(sapply(cor_data, as.numeric), use = "complete.obs"), 2)
    ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 4) +
      labs(title = "Matriz de Correlación de Métricas Clave", subtitle = subtitle_text_flexible())
  })
  
  output$importance_plot <- renderPlot({
    modelo <- modelo_rf_reactivo()
    validate(need(!is.null(modelo), "El modelo no está disponible. Pulsa 'Ejecutar Análisis' o revisa los filtros."))
    importancia_df <- data.frame(Variable = names(modelo$variable.importance), Importancia = modelo$variable.importance) %>%
      mutate(Variable_label = as.character(kpi_names_full[Variable]))
    ggplot(importancia_df, aes(x = reorder(Variable_label, Importancia), y = Importancia)) +
      geom_col(fill = "#00ba38") +
      geom_text(aes(label = round(Importancia, 0)), hjust = -0.2, size = 3.5) +
      coord_flip() +
      labs(title = "Importancia de Variables para Predecir la Victoria",
           subtitle = subtitle_modelo(),
           x = "Variable de Rendimiento", y = "Importancia (Reducción de Impureza Gini)") +
      theme_minimal(base_size = 16)
  })
  
  output$model_summary <- renderUI({
    modelo <- modelo_rf_reactivo()
    datos_del_modelo <- datos_modelo_reactivo()
    validate(need(!is.null(modelo) && !is.null(datos_del_modelo), ""))
    
    prediction_error <- if (!is.null(modelo$prediction.error)) round(modelo$prediction.error * 100, 2) else NA
    error_tag <- if (!is.na(prediction_error)) paste(prediction_error, "% (Error OOB)") else "No disponible"
    
    true_labels <- datos_del_modelo$win
    oob_predictions <- modelo$predictions[, "1"]
    
    auc_valor <- "No calculable"
    if(length(levels(true_labels)) == 2) {
      roc_obj <- roc(response = true_labels, predictor = oob_predictions, quiet = TRUE)
      auc_valor <- round(auc(roc_obj), 4)
    }
    
    tags$div(
      tags$h4("Interpretación Detallada del Modelo"),
      tags$p(tags$strong("Métricas de Rendimiento del Modelo (Random Forest):")),
      tags$ul(
        tags$li(tags$b("AUC (Área bajo la Curva ROC):"), tags$span(style="color:blue; font-weight:bold;", auc_valor), " - (Calculado con OOB) Capacidad del modelo para distinguir entre victoria y derrota. Más alto es mejor (1.0 es perfecto)."),
        tags$li(tags$b("Error de Predicción:"), tags$span(style="color:red; font-weight:bold;", error_tag), " - Porcentaje de predicciones incorrectas sobre nuevos datos. Más bajo es mejor.")
      ),
      tags$hr(),
      
      if(length(modelo$variable.importance) > 2) {
        importancia <- sort(modelo$variable.importance, decreasing = TRUE)
        top_vars_raw <- names(importancia)[1:3]
        top_vars_clean <- as.character(kpi_names_full[top_vars_raw])
        tags$div(
          tags$h4("Jerarquía de Importancia de Variables"),
          tags$p("Para la selección de filtros actual, la jerarquía de variables más predictivas para la victoria es:"),
          tags$ol(
            tags$li(tags$b(top_vars_clean[1]), ": Es el factor más determinante."),
            tags$li(tags$b(top_vars_clean[2]), ": Actúa como un segundo predictor clave."),
            tags$li(tags$b(top_vars_clean[3]), ": Muestra una influencia significativa.")
          ),
          tags$p("Esta jerarquía proporciona una guía clara sobre qué aspectos del juego se deben priorizar para maximizar las probabilidades de ganar.")
        )
      } else {
        tags$p("")
      }
    )
  })
  
  output$ale_explanation_ui <- renderUI({ 
    tags$div(
      tags$h5(tags$b("Interpretación del Gráfico ALE")),
      tags$ul(
        tags$li(tags$b("Eje Y (Vertical):"), " Efecto sobre la Probabilidad de Victoria. Muestra cuánto cambia la probabilidad de victoria para un valor de la métrica, tomando la media como el punto de referencia (0%)."),
        tags$li(tags$b("Curva:"), " Indica la relación real. Si la curva asciende, un valor más alto en la métrica se asocia con una mayor probabilidad de victoria. Si desciende, con una menor probabilidad. El valor cero (la línea horizontal) es el promedio. "),
        tags$li(tags$b("Alfombra de Datos (Rug Plot):"), " Las líneas en la parte inferior representan la densidad de los datos. La curva es más fiable en las zonas con más líneas. El ALE usa estas distribuciones reales para evitar sesgos por multicolinealidad.")
      )
    )
  })
  
  output$pdp_selector_ui <- renderUI({
    modelo <- modelo_rf_reactivo()
    validate(need(!is.null(modelo), ""))
    top_vars_raw <- names(sort(modelo$variable.importance, decreasing = TRUE))[1:3]
    selectInput("pdp_variable_selector", "Selecciona una variable para analizar su impacto:",
                choices = setNames(top_vars_raw, kpi_names_full[top_vars_raw]))
  })
  
  output$pdp_plot <- renderPlot({
    modelo <- modelo_rf_reactivo()
    datos_para_iml <- datos_modelo_reactivo()
    validate(need(!is.null(modelo) && !is.null(datos_para_iml), ""))
    validate(need(!is.null(input$pdp_variable_selector), "Seleccionando variable..."))
    
    X <- datos_para_iml %>% select(all_of(modelo$forest$independent.variable.names))
    y <- datos_para_iml$win
    
    pred_func <- function(model, newdata) {
      predict(model, newdata)$predictions[, "1"]
    }
    
    predictor <- Predictor$new(model = modelo, data = X, y = y, predict.fun = pred_func)
    
    ale_effect <- FeatureEffect$new(predictor = predictor, feature = input$pdp_variable_selector, method = "ale")
    
    plot(ale_effect) +
      scale_y_continuous(name = "Efecto sobre la Probabilidad de Victoria", labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = paste("Impacto de", kpi_names_full[[input$pdp_variable_selector]], "en la Victoria (Gráfico ALE)"),
        subtitle = "El gráfico muestra el cambio en la probabilidad de victoria respecto a la media.",
        x = kpi_names_full[[input$pdp_variable_selector]]
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$consistency_explanation_ui <- renderUI({
    tags$div(
      tags$h4("¿Cómo interpretar este gráfico?"),
      tags$p("Este análisis explora la relación entre el éxito de un jugador (su tasa de victorias) y su consistencia (la regularidad de su rendimiento)."),
      tags$ul(
        tags$li(tags$b("Eje Y (Vertical):"), " Tasa de Victorias. Cuanto más alto, más éxito tiene el jugador."),
        tags$li(tags$b("Eje X (Horizontal):"), " Mide la Irregularidad (Coeficiente de Variación). Valores bajos (izquierda) indican un rendimiento consistente. Valores altos (derecha) indican un rendimiento volátil."),
        tags$li(tags$b("Línea de Tendencia (Roja):"), " Muestra la relación general:",
                tags$ul(
                  tags$li(tags$strong("Si desciende:"), " Los jugadores más consistentes tienden a ganar más."),
                  tags$li(tags$strong("Si asciende:"), " Los jugadores más irregulares (con partidas extremas) tienden a ganar más."),
                  tags$li(tags$strong("Si es plana:"), " La consistencia no parece ser un factor decisivo.")
                )
        )
      )
    )
  })
  
  output$consistency_plot <- renderPlot({
    validate(need(nrow(datos_consistencia()) > 10, "No hay suficientes jugadores con más de 10 partidas para los filtros seleccionados."))
    consistency_kpi_labels <- c("gpm_cv" = "GPM (gpm)", "kda_cv" = "KDA (kda_ajustado)", "cs_cv" = "CS/min (cs_pm)")
    selected_kpi_label <- consistency_kpi_labels[input$consistency_kpi]
    ggplot(datos_consistencia(), aes_string(x = input$consistency_kpi, y = "winrate")) +
      geom_point(alpha = 0.5, color = "darkblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Relación entre Tasa de Victorias y Consistencia del Rendimiento", subtitle = subtitle_text_flexible(use_cluster=FALSE),
           x = paste("Coeficiente de Variación (Irregularidad) de", selected_kpi_label), y = "Tasa de Victorias del Jugador") +
      theme_minimal(base_size = 16)
  })
  
  output$informe_automatico_ui <- renderUI({
    resultados <- calculo_perfiles_clave()
    validate(need(!is.null(resultados), "Datos insuficientes para generar un informe con los filtros seleccionados."))
    
    perfil_popular_data <- resultados$popular
    perfil_ideal_data <- resultados$ideal
    
    nombre_perfil_popular <- cluster_descriptions_list[[input$rol_selector]][[as.character(perfil_popular_data$cluster)]]
    nombre_perfil_ideal <- cluster_descriptions_list[[input$rol_selector]][[as.character(perfil_ideal_data$cluster)]]
    
    conclusion_html <- if (perfil_popular_data$cluster == perfil_ideal_data$cluster) {
      tags$div(
        tags$h4("Conclusión: Metajuego Optimizado"),
        tags$p("El análisis revela que el perfil más popular (el 'meta') coincide con el perfil estadísticamente más efectivo según el 'Índice de Idoneidad'. Esto sugiere que la comunidad de jugadores de este nivel ha identificado y adoptado correctamente el arquetipo de juego más óptimo para este rol y rango.")
      )
    } else {
      tags$div(
        tags$h4("Conclusión: Posible Ineficiencia en el Metajuego"),
        tags$p("Se ha detectado una discrepancia clave: el perfil más popular no es el que nuestro análisis multicriterio identifica como el más efectivo. Mientras que el 'meta' se inclina por el perfil de ", tags$b(paste0("'", nombre_perfil_popular, "'")), ", los datos sugieren que el arquetipo de ", tags$b(paste0("'", nombre_perfil_ideal, "'")), " es estadísticamente superior en su contribución a la victoria."),
        tags$p("Este hallazgo sugiere una posible ineficiencia en el metajuego actual, presentando una oportunidad estratégica para aquellos jugadores que adopten el perfil óptimo pero menos popular.")
      )
    }
    
    tags$div(
      tags$h3(paste("Informe Ejecutivo para el Rol de", input$rol_selector, "en", input$tier_selector)),
      tags$hr(),
      tags$h4("1. El Metajuego: ¿Qué Perfil es el Más Popular?"),
      tags$p("Para el rol de ", tags$b(input$rol_selector), ", el arquetipo de jugador con mayor concentración en el rango ", tags$b(input$tier_selector), " es el perfil de ", tags$b(paste0("'", nombre_perfil_popular, "' (Perfil ", perfil_popular_data$cluster, ")."))),
      tags$br(),
      tags$h4("2. El Óptimo: ¿Qué Perfil es el Más Efectivo?"),
      tags$p("Tras aplicar el 'Índice de Idoneidad' con P-Score, que pondera el rendimiento, la consistencia y el dominio de métricas clave, el análisis identifica al perfil de ", tags$b(paste0("'", nombre_perfil_ideal, "' (Perfil ", perfil_ideal_data$cluster, ")")), " como el más robusto y efectivo para conseguir la victoria."),
      tags$br(),
      conclusion_html
    )
  })
  
  output$hallazgos_clave_ui <- renderUI({
    tags$div(
      tags$h3("Hallazgos Clave del Análisis"),
      tags$hr(),
      tags$h4("1. El Rendimiento de Élite se Basa en Arquetipos Funcionales, no en un Estilo Único"),
      tags$p("El principal descubrimiento del proyecto es la confirmación empírica de que no existe un único camino hacia el alto rendimiento. Mediante clustering K-Means, hemos identificado y validado la existencia de, al menos, ", tags$b("3 arquetipos de jugador (perfiles) distintos y medibles para cada uno de los 5 roles."), " Este hallazgo refuta la idea de un 'estilo de juego óptimo' universal, demostrando que el éxito en el ELO alto se sustenta en la especialización en diferentes estrategias funcionales, desde 'hypercarries' de alto impacto económico hasta jugadores de control y utilidad."),
      tags$hr(),
      tags$h4("2. El 'Metajuego' Popular No Siempre es el Estadísticamente Más Efectivo"),
      tags$p("Nuestro 'Índice de Idoneidad' —una métrica multicriterio que pondera rendimiento, consistencia, popularidad y dominio de métricas clave— revela discrepancias significativas entre el perfil más jugado (el 'meta') y el más efectivo. Por ejemplo, para el rol de ", tags$b("TOP en Master,"), " a pesar de que el perfil más popular es el ", tags$b("'Agresivos de Combate'"), ", nuestro análisis identifica al arquetipo ", tags$b("'Enfocados a Objetivos'"), " como el estadísticamente superior para conseguir la victoria. Este tipo de ineficiencias en el metajuego representan oportunidades estratégicas que pueden ser explotadas por jugadores y equipos para obtener una ventaja competitiva."),
      tags$hr(),
      tags$h4("3. Para Roles de 'Carry', la Consistencia Supera a la Explosividad Ocasional"),
      tags$p("El análisis de la variabilidad del rendimiento mediante el Coeficiente de Variación (CV) sugiere que, especialmente para roles de daño sostenido como el ", tags$b("ADC (Bottom)"), ", la consistencia es un factor más determinante para el éxito a largo plazo. Un bajo CV en métricas como el KDA —lo que implica evitar partidas muy malas— está más correlacionado con una alta tasa de victorias que la capacidad de tener partidas 'explosivas' de forma esporádica. Esto indica que, en el nivel más alto, la fiabilidad y la mitigación de errores son más valiosas que la búsqueda de jugadas de alto riesgo."),
      tags$hr(),
      tags$h4("4. El Perfil Ideal es un Concepto Dinámico que Evoluciona con el Nivel Competitivo"),
      tags$p("El análisis demuestra que el arquetipo ideal para un rol no es estático, sino que se adapta al ecosistema competitivo de cada rango. El perfil de Jungla que es óptimo en Master (donde la agresividad temprana puede ser más decisiva) no es necesariamente el mismo que en Challenger (donde el control de mapa y la eficiencia de macrojuego son primordiales). Esto valida que el 'meta' es un ecosistema dinámico que evoluciona a medida que aumentan la habilidad y la coordinación de los jugadores."),
      tags$hr(),
      tags$h4("5. Descubrimiento Metodológico: La Interpretabilidad Robusta es Clave en el Análisis de eSports"),
      tags$p("Quizás el hallazgo más significativo de este TFM es de naturaleza metodológica. Durante el análisis de los factores clave de victoria, se demostró que las técnicas de interpretabilidad de modelos de Machine Learning estándar, como los ", tags$b("Gráficos de Dependencia Parcial (PDP)"), ", pueden producir resultados engañosos y contraintuitivos en dominios con alta correlación entre variables, como los eSports."),
      tags$p("Nuestro diagnóstico reveló que la relación real entre métricas como el KDA y la victoria era positiva, pero los PDP mostraban lo contrario. Este artefacto se debió a que el método generaba escenarios de jugador irreales. La solución fue implementar una técnica más avanzada y robusta, los ", tags$b("Gráficos de Efectos Locales Acumulados (ALE)"), ", que sí respetan la estructura de correlación de los datos."),
      tags$p(tags$b("Este descubrimiento subraya una conclusión crítica para la analítica de eSports:"), " la elección de una herramienta de interpretabilidad adecuada es tan importante como la elección del propio modelo predictivo para evitar conclusiones erróneas y garantizar la validez de los insights estratégicos.")
    )
  })
  
  output$player_analysis_ui <- renderUI({
    jugadores_df <- df_jugadores_caso_estudio_all()
    
    tags$div(
      tags$h3("Estudio de Caso: Análisis Individualizado de Jugador (≥ 30 Partidas)"),
      tags$hr(),
      tags$p("Selecciona un Jugador (ID de Invocador) de la base de datos para ver su rendimiento, consistencia y arquetipo estratégico. Solo se muestran jugadores con ", tags$b("30 o más partidas"), " para garantizar la fiabilidad del perfil. Los filtros de Rol y Rango en la barra lateral NO afectan esta selección, el análisis es contextual al jugador seleccionado."),
      
      fluidRow(
        column(12,
               selectInput("player_selector", "Seleccionar Jugador (ID de Invocador):",
                           choices = setNames(c("", jugadores_df$puuid), c("Selecciona un Jugador", paste0(jugadores_df$display_name, " (", substr(jugadores_df$puuid, 1, 8), "...)"))),
                           selected = NULL)
        )
      ),
      
      uiOutput("player_profile_summary")
    )
  })
  
  output$player_profile_summary <- renderUI({
    req(input$player_selector != "")
    
    player_puuid <- input$player_selector
    
    player_data_all_roles <- df_agregado_con_tier %>%
      filter(puuid == player_puuid) %>%
      arrange(desc(partidas_jugadas))
    
    validate(
      need(nrow(player_data_all_roles) > 0, "No se encontró el perfil agregado del jugador."),
      need(player_data_all_roles$partidas_jugadas[1] >= 30, "El jugador seleccionado no cumple el mínimo de 30 partidas en el rol dominante.")
    )
    
    player_data_agg <- player_data_all_roles %>% slice(1)
    rol_jugador_dominante <- as.character(player_data_agg$individual_position)
    rango_jugador <- as.character(player_data_agg$tier)
    
    roles_jugados_html <- player_data_all_roles %>%
      mutate(rol_label = paste0(individual_position, " (", partidas_jugadas, " partidas)")) %>%
      pull(rol_label) %>%
      paste(collapse = ", ")
    
    cluster_model_data <- perform_kmeans_clustering(df_agregado_con_tier, rol_jugador_dominante)
    validate(need(!is.null(cluster_model_data), paste("Datos insuficientes para el clustering en el rol dominante:", rol_jugador_dominante)))
    
    vars_usadas_clustering <- cluster_model_data$vars_usadas
    player_numeric_data <- player_data_agg %>% 
      select(all_of(vars_usadas_clustering))
    
    player_numeric_data_ordered <- player_numeric_data[, names(cluster_model_data$center_data), drop = FALSE]
    
    player_data_normalized <- scale(player_numeric_data_ordered, 
                                    center = cluster_model_data$center_data, 
                                    scale = cluster_model_data$scale_data)
    
    player_cluster_number <- predict.kmeans(cluster_model_data$modelo_kmeans, player_data_normalized)[1]
    nombre_perfil_jugador <- cluster_descriptions_list[[rol_jugador_dominante]][[as.character(player_cluster_number)]]
    
    player_data_raw <- df_kpis %>%
      filter(puuid == player_puuid, individual_position == rol_jugador_dominante)
    
    player_consistency <- player_data_raw %>%
      summarise(
        winrate = mean(as.numeric(as.character(win)), na.rm = TRUE),
        kda_cv = sd(kda_ajustado, na.rm = TRUE) / mean(kda_ajustado, na.rm = TRUE),
        gpm_cv = sd(gpm, na.rm = TRUE) / mean(gpm, na.rm = TRUE),
        oci_mean = mean(oci, na.rm = TRUE)
      )
    
    tags$div(
      tags$h4(tags$b(paste("Perfil de Rendimiento para Jugador:", player_data_agg$display_name[1]))),
      tags$hr(),
      fluidRow(
        column(4, tags$p(tags$strong("Rango Actual:"), rango_jugador)),
        column(8, tags$p(tags$strong("Roles Jugados (≥ 5):"), roles_jugados_html))
      ),
      tags$p(tags$strong("Rol Dominante para Análisis:"), tags$span(style="color:blue; font-weight:bold;", rol_jugador_dominante)),
      tags$hr(),
      tags$h4("Diagnóstico Estratégico"),
      tags$p("El análisis de K-Means clasifica al jugador en el siguiente arquetipo, comparándolo con la población de alto ELO de su rol dominante:"),
      tags$ul(
        tags$li(tags$b("Arquetipo Funcional:"), tags$span(style="color:blue; font-weight:bold;", paste("Perfil", player_cluster_number, "-", nombre_perfil_jugador)))
      ),
      tags$hr(),
      tags$h4("Métricas de Rendimiento y Consistencia (Rol Dominante)"),
      fluidRow(
        column(4, tags$div(class = "well well-sm", tags$h5(style = "text-align: center;", "Tasa de Victorias (Winrate)"), tags$p(style = "text-align: center; font-size: 20px;", scales::percent(player_consistency$winrate[1], accuracy = 0.1)))),
        column(4, tags$div(class = "well well-sm", tags$h5(style = "text-align: center;", "Irregularidad (CV de KDA)"), tags$p(style = "text-align: center; font-size: 20px;", round(player_consistency$kda_cv[1], 2)))),
        column(4, tags$div(class = "well well-sm", tags$h5(style = "text-align: center;", "OCI Promedio"), tags$p(style = "text-align: center; font-size: 20px;", round(player_consistency$oci_mean[1], 2))))
      ),
      tags$hr(),
      tags$h4("Comparativa de Rendimiento (Z-Score)"),
      plotOutput("player_comparison_plot")
    )
  })
  
  output$player_comparison_plot <- renderPlot({
    req(input$player_selector != "")
    
    player_puuid <- input$player_selector
    
    player_data_all_roles <- df_agregado_con_tier %>%
      filter(puuid == player_puuid) %>%
      arrange(desc(partidas_jugadas))
    player_data_agg <- player_data_all_roles %>% slice(1)
    rol_jugador_dominante <- as.character(player_data_agg$individual_position)
    
    cluster_model_data <- perform_kmeans_clustering(df_agregado_con_tier, rol_jugador_dominante)
    validate(need(!is.null(cluster_model_data), paste("Datos insuficientes para el clustering en el rol:", rol_jugador_dominante)))
    
    vars_usadas_clustering <- cluster_model_data$vars_usadas
    player_numeric_data <- player_data_agg %>% 
      select(all_of(vars_usadas_clustering))
    
    player_numeric_data_ordered <- player_numeric_data[, names(cluster_model_data$center_data), drop = FALSE]
    
    player_data_normalized <- as.data.frame(scale(player_numeric_data_ordered, 
                                                  center = cluster_model_data$center_data, 
                                                  scale = cluster_model_data$scale_data))
    
    player_cluster_number <- predict.kmeans(cluster_model_data$modelo_kmeans, player_data_normalized)[1] 
    
    plot_data_centers <- cluster_model_data$centers_df %>%
      mutate(cluster = as.factor(1:3), type = "Perfil (Media del Clúster)") %>%
      pivot_longer(cols = -c(cluster, type), names_to = "kpi", values_to = "z_score")
    
    plot_data_player <- player_data_normalized %>%
      mutate(cluster = as.factor(player_cluster_number), type = "Jugador Seleccionado") %>%
      pivot_longer(cols = -c(cluster, type), names_to = "kpi", values_to = "z_score")
    
    plot_data_filtered <- plot_data_centers %>%
      filter(cluster == player_cluster_number)
    
    plot_data_combined <- bind_rows(plot_data_filtered, plot_data_player) %>%
      mutate(kpi_label = as.character(kpi_names_promedio[kpi]))
    
    ggplot(plot_data_combined, aes(x = kpi_label, y = z_score, fill = type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = round(z_score, 2)), position = position_dodge(width = 0.9), vjust = -0.2, size = 3.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      coord_flip() +
      labs(title = paste("Comparativa de Arquetipos (Z-Score) para", rol_jugador_dominante),
           subtitle = paste0("Jugador ", player_data_agg$display_name, " vs. Media de su Perfil Asignado (Perfil ", player_cluster_number, ")"),
           x = "", y = "Valor Estandarizado (Z-Score)", fill = "Referencia") +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "bottom", 
            legend.title = element_blank())
  })
}

# --- 5. Ejecución de la Aplicación ---
shinyApp(ui = ui, server = server)