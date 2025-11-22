library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggwordcloud) # Para nuvem de palavras
library(treemapify)  # Para o treemap

# ==============================================================================
# 1. CONFIGURAÇÕES GERAIS E DICIONÁRIOS
# ==============================================================================

tabela_partidos <- tibble(
  NM_PARTIDO = c(
    "PARTIDO SOCIAL DEMOCRÁTICO", "PARTIDO DA MULHER BRASILEIRA", "PARTIDO SOCIALISTA BRASILEIRO",
    "UNIÃO BRASIL", "SOLIDARIEDADE", "REPUBLICANOS", "PROGRESSISTAS", "MOBILIZAÇÃO NACIONAL",
    "PARTIDO NOVO", "PODEMOS", "PARTIDO DEMOCRÁTICO TRABALHISTA", "PARTIDO DOS TRABALHADORES",
    "PARTIDO SOCIALISMO E LIBERDADE", "PARTIDO LIBERAL", "MOVIMENTO DEMOCRÁTICO BRASILEIRO",
    "PARTIDO RENOVAÇÃO DEMOCRÁTICA", "REDE SUSTENTABILIDADE", "AGIR",
    "PARTIDO DA SOCIAL DEMOCRACIA BRASILEIRA", "PARTIDO COMUNISTA DO BRASIL", "PARTIDO VERDE",
    "PARTIDO RENOVADOR TRABALHISTA BRASILEIRO", "CIDADANIA", "DEMOCRACIA CRISTÃ",
    "PARTIDO SOCIALISTA DOS TRABALHADORES UNIFICADO", "PARTIDO DA CAUSA OPERÁRIA"
  ),
  SG_PARTIDO_SIGLA = c(
    "PSD", "PMB", "PSB", "UNIÃO", "SOLIDARIEDADE", "REPUBLICANOS", "PP", "MOBILIZA",
    "NOVO", "PODE", "PDT", "PT", "PSOL", "PL", "MDB", "PRD", "REDE", "AGIR",
    "PSDB", "PCdoB", "PV", "PRTB", "CIDADANIA", "DC", "PSTU", "PCO"
  )
)

cores_partidos <- c(
  "PT" = "#FF0000", "PL" = "#0000FF", "MDB" = "#00A550", "PSDB" = "#00BFFF",
  "PSD" = "#FFD700", "PODE" = "#00CED1", "REPUBLICANOS" = "#008080",
  "PP" = "#4169E1", "PSOL" = "#FF4500", "NOVO" = "#FFA500", "UNIÃO" = "#0033A0",
  "PSB" = "#E30613", "PDT" = "#E2001A", "PCdoB" = "#ED1C24", "REDE" = "#009999",
  "PV" = "#008000", "SOLIDARIEDADE" = "#FF8C00", "CIDADANIA" = "#FF7F50",
  "AGIR" = "#9370DB", "MOBILIZA" = "#B22222", "AVANTE" = "#FFB347",
  "PSTU" = "#B22222", "PCB" = "#A52A2A", "PRTB" = "#006400", "DC" = "#6495ED",
  "PCO" = "#FF0000", "PMB" = "#FF69B4", "UP" = "#800000", "PRD" = "#191970"
)

# ==============================================================================
# 2. CARREGAMENTO SEGURO DOS DADOS
# ==============================================================================

carregar_dados <- function() {
  tryCatch({
    message("Iniciando carregamento dos dados...")
    
    shp_zonas <- "ctba_tre_zona_eleitoral_a.shp"
    shp_locais <- "ctba_tre_local_votacao_p.shp"
    csv_path <- "Dados_curitiba.csv"
    
    if (!file.exists(shp_zonas) || !file.exists(shp_locais)) stop("Arquivos SHP não encontrados.")
    
    # --- SHAPEFILES ---
    z <- st_read(shp_zonas, quiet = TRUE) %>% st_make_valid() %>% st_transform(crs = 4326)
    l <- st_read(shp_locais, quiet = TRUE) %>% st_transform(crs = 4326)
    
    # Renomear colunas se necessário
    if("zona_eleit" %in% names(z)) z <- rename(z, zona_real = zona_eleit)
    if("zona_eleit" %in% names(l)) l <- rename(l, zona_real = zona_eleit, local_nome = nome_local)
    
    # IDs Limpos
    z$id_interno <- 1:nrow(z)
    z$zona_real <- as.character(readr::parse_number(as.character(z$zona_real)))
    l$zona_real <- as.character(readr::parse_number(as.character(l$zona_real)))
    
    # Criar ID único para escola para o clique (usando índice da linha)
    l$id_escola_click <- 1:nrow(l)
    
    # --- CSV ---
    if (file.exists(csv_path)) {
      dados_votos <- read_csv(csv_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
      dados_votos <- dados_votos %>%
        mutate(
          NM_PARTIDO = str_to_upper(str_trim(NM_PARTIDO)),
          NR_ZONA = as.character(readr::parse_number(as.character(NR_ZONA))) 
        ) %>%
        left_join(tabela_partidos, by = "NM_PARTIDO")
    } else {
      dados_votos <- NULL
    }
    
    list(zonas = z, locais = l, votos = dados_votos, erro = FALSE)
    
  }, error = function(e) {
    message("ERRO CRÍTICO: ", e$message)
    # Dados Fake de Emergência
    z_fake <- st_as_sf(data.frame(id_interno = 1, zona_real = "1", geometry = st_sfc(st_polygon(list(rbind(c(-49.3,-25.4), c(-49.25,-25.4), c(-49.25,-25.45), c(-49.3,-25.45), c(-49.3,-25.4)))))), crs = 4326)
    l_fake <- st_as_sf(data.frame(zona_real = "1", local_nome = "Erro", id_escola_click = 1, geometry = st_sfc(st_point(c(-49.27,-25.42)))), crs = 4326)
    list(zonas = z_fake, locais = l_fake, votos = NULL, erro = TRUE)
  })
}

dados <- carregar_dados()
zonas <- dados$zonas
locais <- dados$locais
df_votos <- dados$votos

# ==============================================================================
# 3. INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; font-family: 'Segoe UI', sans-serif; }
      #mapa { height: 70vh !important; border: 1px solid #ccc; border-radius: 5px; }
      .painel-lateral { height: 70vh; overflow-y: auto; background: white; padding: 15px; border-radius: 5px; border: 1px solid #ccc; box-shadow: 0 2px 5px rgba(0,0,0,0.05); }
      .painel-inferior { min-height: 30vh; background: white; margin-top: 15px; padding: 20px; border-radius: 5px; border: 1px solid #ccc; box-shadow: 0 2px 5px rgba(0,0,0,0.05); }
      .metric-box { background: #f1f3f5; border-radius: 8px; padding: 10px; text-align: center; margin-bottom: 10px; }
      .metric-val { font-size: 1.2rem; font-weight: bold; color: #2c3e50; }
    "))
  ),
  
  div(style="padding: 15px;",
    h3("Eleições Curitiba 2024", style="margin-top:0; padding-bottom: 10px; border-bottom: 1px solid #ddd;"),
    
    fluidRow(
      column(8, leafletOutput("mapa")),
      column(4, 
             div(class="painel-lateral",
                 uiOutput("header_lateral"),
                 hr(),
                 tabsetPanel(id = "tabs_lateral",
                   tabPanel("Prefeito", plotlyOutput("plot_prefeito", height = "350px")),
                   tabPanel("Vereador", plotlyOutput("plot_vereador", height = "350px")),
                   tabPanel("Nuvem (Ver)", plotOutput("plot_nuvem", height = "350px")),
                   tabPanel("Treemap", plotOutput("plot_treemap", height = "350px"))
                 )
             )
      )
    ),
    
    fluidRow(
      column(12,
             div(class="painel-inferior",
                 tabsetPanel(id = "tabs_inferior",
                   tabPanel("Comparecimento", 
                            br(),
                            fluidRow(
                              column(3, uiOutput("stats_comparecimento")),
                              column(9, plotlyOutput("plot_abstencao", height = "200px"))
                            )
                   ),
                   tabPanel("Heatmap (Zonas x Partidos)",
                            br(),
                            plotOutput("plot_heatmap", height = "400px")
                   )
                 )
             )
      )
    )
  )
)

# ==============================================================================
# 4. SERVIDOR
# ==============================================================================
server <- function(input, output, session) {
  
  zona_ativa <- reactiveVal(NULL)
  
  # --- 4.1 FILTRAGEM DE DADOS ---
  dados_filtrados <- reactive({
    req(df_votos)
    if(is.null(zona_ativa())) df_votos else df_votos %>% filter(NR_ZONA == zona_ativa())
  })
  
  # --- 4.2 MAPA ---
  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = zonas,
        layerId = ~id_interno,
        fillColor = "#3498db", fillOpacity = 0.3,
        color = "white", weight = 1,
        label = ~paste("Zona Eleitoral", zona_real),
        highlightOptions = highlightOptions(weight = 3, color = "#2c3e50", fillOpacity = 0.5, bringToFront = TRUE)
      ) %>%
      setView(lng = -49.2715, lat = -25.44, zoom = 11)
  })
  
  # --- 4.3 CLIQUE NA ZONA (Filtra os gráficos) ---
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    req(click$id)
    
    id_clicado <- as.numeric(click$id)
    poly_focado <- zonas %>% filter(id_interno == id_clicado)
    
    if(nrow(poly_focado) > 0) {
      num_zona <- poly_focado$zona_real[1]
      zona_ativa(num_zona)
      
      bb <- as.numeric(st_bbox(poly_focado))
      pontos_focados <- locais %>% filter(zona_real == num_zona)
      
      leafletProxy("mapa") %>%
        clearMarkers() %>% clearGroup("selecao") %>%
        addPolygons(data = poly_focado, group = "selecao", fillColor = "transparent", color = "#e74c3c", weight = 4, opacity = 1) %>%
        addCircleMarkers(
          data = pontos_focados,
          layerId = ~id_escola_click, # ID importante para o clique na escola
          radius = 6, color = "#e74c3c", fillColor = "#e74c3c", fillOpacity = 0.8, stroke = FALSE,
          label = ~local_nome
        )
      
      if(!any(is.na(bb))) leafletProxy("mapa") %>% flyToBounds(lng1=bb[1], lat1=bb[2], lng2=bb[3], lat2=bb[4])
    }
  })
  
  # --- 4.4 CLIQUE NA ESCOLA (Mostra Modal com Detalhes) ---
  observeEvent(input$mapa_marker_click, {
    click <- input$mapa_marker_click
    req(click$id)
    
    # Busca informações da escola clicada
    escola <- locais %>% filter(id_escola_click == click$id)
    
    if(nrow(escola) > 0) {
      showModal(modalDialog(
        title = icon("school"),
        h3(escola$local_nome),
        hr(),
        p(strong("Zona Eleitoral:"), escola$zona_real),
        p(strong("Endereço:"), ifelse("endereco" %in% names(escola), escola$endereco, "Não informado")),
        p(strong("Bairro:"), ifelse("bairro" %in% names(escola), escola$bairro, "Não informado")),
        p(strong("Seções:"), ifelse("num_secoes" %in% names(escola), escola$num_secoes, "?")),
        p(strong("Total de Eleitores:"), ifelse("num_eleito" %in% names(escola), escola$num_eleito, "?")),
        size = "m",
        easyClose = TRUE,
        footer = modalButton("Fechar")
      ))
    }
  })
  
  # Botão Voltar
  output$header_lateral <- renderUI({
    z <- zona_ativa()
    if(is.null(z)) h3("Curitiba (Geral)") else tagList(h3(paste("Zona", z)), actionButton("bt_limpar", "Ver Cidade Inteira"))
  })
  
  observeEvent(input$bt_limpar, {
    zona_ativa(NULL)
    leafletProxy("mapa") %>% clearMarkers() %>% clearGroup("selecao") %>% setView(lng = -49.2715, lat = -25.44, zoom = 11)
  })
  
  # --- 4.5 GRÁFICOS BÁSICOS (PREFEITO/VEREADOR) ---
  plot_barras <- function(cargo) {
    req(dados_filtrados())
    df <- dados_filtrados() %>%
      filter(DS_CARGO_PERGUNTA == cargo, !NM_VOTAVEL %in% c("Branco", "Nulo")) %>%
      group_by(NM_VOTAVEL, SG_PARTIDO_SIGLA) %>%
      summarise(votos = sum(QT_VOTOS, na.rm=T), .groups="drop") %>%
      arrange(desc(votos)) %>% head(10)
    
    if(nrow(df)==0) return(NULL)
    
    plot_ly(df, x = ~votos, y = ~reorder(NM_VOTAVEL, votos), type = 'bar', orientation = 'h',
            text = ~paste(votos), textposition = 'auto',
            marker = list(color = ~cores_partidos[SG_PARTIDO_SIGLA])) %>%
      layout(yaxis = list(title=""), xaxis = list(title="Votos"), margin = list(l=10, r=10, t=10, b=10))
  }
  
  output$plot_prefeito <- renderPlotly({ plot_barras("Prefeito") })
  output$plot_vereador <- renderPlotly({ plot_barras("Vereador") })
  
  # --- 4.6 NUVEM DE PALAVRAS (VEREADOR) ---
  output$plot_nuvem <- renderPlot({
    req(dados_filtrados())
    
    df_nuvem <- dados_filtrados() %>%
      filter(DS_CARGO_PERGUNTA == "Vereador", !NM_VOTAVEL %in% c("Branco", "Nulo")) %>%
      group_by(NM_VOTAVEL, SG_PARTIDO_SIGLA) %>%
      summarise(total_votos = sum(QT_VOTOS, na.rm=T), .groups="drop") %>%
      arrange(desc(total_votos)) %>%
      head(70) # Top 70 para não poluir
    
    ggplot(df_nuvem, aes(label = NM_VOTAVEL, size = total_votos, color = SG_PARTIDO_SIGLA)) +
      geom_text_wordcloud(area_corr = 1, rm_outside = TRUE) +
      scale_size_area(max_size = 18) +
      scale_color_manual(values = cores_partidos) +
      theme_minimal()
  })
  
  # --- 4.7 TREEMAP (PARTIDOS VEREADOR) ---
  output$plot_treemap <- renderPlot({
    req(dados_filtrados())
    
    df_tree <- dados_filtrados() %>%
      filter(DS_CARGO_PERGUNTA == "Vereador", !is.na(SG_PARTIDO_SIGLA)) %>%
      group_by(SG_PARTIDO_SIGLA) %>%
      summarise(total_votos = sum(QT_VOTOS, na.rm=T), .groups="drop")
    
    ggplot(df_tree, aes(area = total_votos, fill = SG_PARTIDO_SIGLA, label = SG_PARTIDO_SIGLA)) +
      geom_treemap() +
      geom_treemap_text(color = "white", place = "centre", fontface = "bold", size = 14) +
      scale_fill_manual(values = cores_partidos) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Proporção de Votos por Partido (Vereador)")
  })
  
  # --- 4.8 COMPARECIMENTO ---
  output$stats_comparecimento <- renderUI({
    req(dados_filtrados())
    d <- dados_filtrados() %>% distinct(NR_ZONA, NR_SECAO, .keep_all = TRUE)
    aptos <- sum(d$QT_APTOS, na.rm=T)
    abst <- sum(d$QT_ABSTENCOES, na.rm=T)
    perc <- if(aptos>0) round((abst/aptos)*100, 1) else 0
    
    tagList(
      div(class="metric-box", "Eleitorado Apto", div(class="metric-val", format(aptos, big.mark="."))),
      div(class="metric-box", "Taxa de Abstenção", div(class="metric-val", style="color:red", paste0(perc, "%")))
    )
  })
  
  output$plot_abstencao <- renderPlotly({
    req(dados_filtrados())
    d <- dados_filtrados() %>% distinct(NR_ZONA, NR_SECAO, .keep_all = TRUE)
    comp <- sum(d$QT_COMPARECIMENTO, na.rm=T)
    abst <- sum(d$QT_ABSTENCOES, na.rm=T)
    if(comp+abst == 0) return(NULL)
    
    plot_ly(labels = c("Comparecimento", "Abstenção"), values = c(comp, abst), type='pie',
            marker = list(colors = c("#2ecc71", "#e74c3c")), textinfo='label+percent', hole=0.5) %>%
      layout(showlegend=FALSE, margin=list(t=0, b=0, l=0, r=0))
  })
  
  # --- 4.9 HEATMAP (GLOBAL) ---
  # Este gráfico mostra todas as zonas, por isso usamos df_votos (dataset completo)
  output$plot_heatmap <- renderPlot({
    req(df_votos)
    
    # Preparar dados: % de votos de cada partido dentro de cada zona
    df_heat <- df_votos %>%
      filter(DS_CARGO_PERGUNTA == "Vereador") %>%
      group_by(NR_ZONA, SG_PARTIDO_SIGLA) %>%
      summarise(votos = sum(QT_VOTOS, na.rm=T), .groups="drop") %>%
      group_by(NR_ZONA) %>%
      mutate(percentual = (votos / sum(votos)) * 100)
    
    ggplot(df_heat, aes(x = as.factor(NR_ZONA), y = SG_PARTIDO_SIGLA, fill = percentual)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = c("white", "yellow", "red", "black"), values=c(0, 0.1, 0.3, 1)) +
      labs(title = "Concentração de Votos por Zona e Partido",
           x = "Zona Eleitoral", y = "Partido", fill = "% Votos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  })
}

shinyApp(ui, server)