library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# ==============================================================================
# 1. CARREGAMENTO SEGURO DOS DADOS
# ==============================================================================
shp_zonas_path <- "ctba_tre_zona_eleitoral_a.shp"
shp_locais_path <- "ctba_tre_local_votacao_p.shp"

carregar_dados <- function() {
  tryCatch({
    if (!file.exists(shp_zonas_path) || !file.exists(shp_locais_path)) {
      stop("Arquivos shapefile não encontrados.")
    }
    
    # Lê os arquivos e transforma para Latitude/Longitude (WGS84)
    z <- st_read(shp_zonas_path, quiet = TRUE) %>% st_transform(crs = 4326)
    l <- st_read(shp_locais_path, quiet = TRUE) %>% st_transform(crs = 4326)
    
    # PADRONIZAÇÃO DE NOMES
    if("zona_eleit" %in% names(z)) z <- rename(z, zona_real = zona_eleit)
    if("zona_eleit" %in% names(l)) l <- rename(l, zona_real = zona_eleit, local_nome = nome_local)
    
    # CRIAÇÃO DE ID INTERNO (SOLUÇÃO ANTI-TRAVAMENTO)
    # Criamos um índice numérico simples (1, 2, 3...) para o clique funcionar sempre
    z$id_interno <- 1:nrow(z)
    
    # Garante que a zona real seja texto para exibição
    z$zona_real <- as.character(z$zona_real)
    l$zona_real <- as.character(l$zona_real)
    
    return(list(zonas = z, locais = l, erro = FALSE))
    
  }, error = function(e) {
    message("ERRO: ", e$message)
    # Dados falsos apenas para o app abrir se não tiver os arquivos
    z_fake <- st_as_sf(data.frame(id_interno = 1:3, zona_real = c("177", "178", "179"), geometry = st_sfc(st_polygon(list(rbind(c(-49.3,-25.4), c(-49.25,-25.4), c(-49.25,-25.45), c(-49.3,-25.45), c(-49.3,-25.4)))), st_polygon(list(rbind(c(-49.25,-25.4), c(-49.2,-25.4), c(-49.2,-25.45), c(-49.25,-25.45), c(-49.25,-25.4)))), st_polygon(list(rbind(c(-49.25,-25.45), c(-49.2,-25.45), c(-49.2,-25.5), c(-49.25,-25.5), c(-49.25,-25.45)))))), crs = 4326)
    l_fake <- st_as_sf(data.frame(zona_real = c("177", "177", "178"), local_nome = c("Colégio A", "Colégio B", "Colégio C"), geometry = st_sfc(st_point(c(-49.28,-25.42)), st_point(c(-49.27,-25.43)), st_point(c(-49.22,-25.42)))), crs = 4326)
    return(list(zonas = z_fake, locais = l_fake, erro = TRUE))
  })
}

d <- carregar_dados()
zonas <- d$zonas
locais <- d$locais

# ==============================================================================
# 2. INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  # CSS para remover margens e deixar tela cheia
  tags$head(
    tags$style(HTML("
      body, html { margin: 0; padding: 0; height: 100%; overflow: hidden; }
      #mapa { height: 100vh !important; width: 100%; }
      
      /* Painel Lateral Flutuante Estilizado */
      .sidebar-panel {
        position: absolute; top: 0; right: 0; bottom: 0;
        width: 350px; background: rgba(255, 255, 255, 0.95);
        border-left: 1px solid #ccc; padding: 20px;
        box-shadow: -2px 0 10px rgba(0,0,0,0.1);
        z-index: 1000; overflow-y: auto;
        transform: translateX(0); transition: transform 0.3s ease;
      }
      .sidebar-hidden { transform: translateX(100%); }
      .card-metric { background: #f8f9fa; padding: 15px; border-radius: 6px; margin-bottom: 10px; border-left: 4px solid #34495e; }
    "))
  ),
  
  leafletOutput("mapa"),
  
  # Painel da Direita
  div(id = "sidebar", class = "sidebar-panel",
      uiOutput("conteudo_lateral")
  )
)

# ==============================================================================
# 3. SERVIDOR (LÓGICA)
# ==============================================================================
server <- function(input, output, session) {
  
  # Variável para controlar estado
  estado_atual <- reactiveVal(NULL) # NULL = Visão Geral
  
  # 1. RENDERIZAÇÃO INICIAL DO MAPA (BÁSICO)
  output$mapa <- renderLeaflet({
    # Opções para 'travar' o mapa e deixá-lo clean
    opt <- leafletOptions(zoomControl = FALSE, minZoom = 10, maxZoom = 16)
    
    leaflet(options = opt) %>%
      # MAPA BASE CLEAN (Sem vegetação, tons de cinza e branco)
      addProviderTiles(providers$CartoDB.Positron) %>% 
      
      # Polígonos das Zonas (Camada Base)
      addPolygons(
        data = zonas,
        layerId = ~id_interno, # Usamos o ID numérico seguro
        fillColor = "#2c3e50", # Azul escuro sóbrio
        fillOpacity = 0.1,     # Bem transparente para ver as ruas
        color = "#2c3e50",     # Borda
        weight = 1,
        label = ~paste("Zona Eleitoral", zona_real),
        highlightOptions = highlightOptions(weight = 3, color = "black", fillOpacity = 0.2, bringToFront = TRUE)
      ) %>%
      setView(lng = -49.2715, lat = -25.44, zoom = 11)
  })
  
  # 2. LÓGICA DO CLIQUE (BLINDADA)
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    
    # Verificação de segurança
    if (is.null(click$id)) return()
    
    # Pega o ID Interno (Numérico) clicado
    id_clicado <- as.numeric(click$id)
    
    # Seleciona a zona baseada nesse índice (infalível)
    zona_focada <- zonas %>% filter(id_interno == id_clicado)
    
    # Se achou a zona, processa
    if (nrow(zona_focada) > 0) {
      
      # Pega o número real da zona para filtrar os locais
      num_zona_real <- zona_focada$zona_real[1]
      pontos_focados <- locais %>% filter(zona_real == num_zona_real)
      
      # Atualiza estado
      estado_atual(num_zona_real)
      
      # Calcula BBOX de forma segura (convertendo para números puros)
      bb <- as.numeric(st_bbox(zona_focada))
      
      # ATUALIZA O MAPA (Sem recarregar tudo)
      leafletProxy("mapa") %>%
        clearMarkers() %>%             # Limpa pontos anteriores
        clearGroup("destaque") %>%     # Limpa destaque anterior
        
        # Adiciona a Zona em Destaque (Cria uma cópia por cima)
        addPolygons(
          data = zona_focada,
          group = "destaque",          # Grupo separado para facilitar limpeza
          fillColor = "transparent",   # Fundo transparente para focar nos pontos
          color = "#e74c3c",           # Borda Vermelha de destaque
          weight = 3,
          opacity = 1,
          fillOpacity = 0
        ) %>%
        
        # Adiciona os Pontos (Seções)
        addCircleMarkers(
          data = pontos_focados,
          radius = 4,
          color = "#e74c3c", # Vermelho
          fillColor = "#e74c3c",
          fillOpacity = 0.8,
          stroke = FALSE,
          label = ~local_nome,
          popup = ~paste("Local:", local_nome, "<br>Seções:", ifelse("num_secoes" %in% names(pontos_focados), num_secoes, "?"))
        ) %>%
        
        # Zoom Suave (Fly) usando as coordenadas numéricas
        flyToBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
    }
  })
  
  # 3. BOTÃO VOLTAR (RESET)
  observeEvent(input$bt_reset, {
    estado_atual(NULL)
    
    leafletProxy("mapa") %>%
      clearMarkers() %>%
      clearGroup("destaque") %>% # Remove o polígono vermelho e os pontos
      setView(lng = -49.2715, lat = -25.44, zoom = 11) # Volta pra visão geral
  })
  
  # 4. CONTEÚDO DO PAINEL LATERAL
  output$conteudo_lateral <- renderUI({
    zona <- estado_atual()
    
    if (is.null(zona)) {
      # --- Visão Geral ---
      tagList(
        h2("Mapa Eleitoral"),
        h4("Curitiba - PR"),
        hr(),
        p("Selecione uma zona no mapa para expandir e visualizar as seções."),
        div(class = "card-metric",
            h5("Total de Zonas"),
            h3(nrow(zonas))
        ),
        div(class = "card-metric",
            h5("Total de Locais"),
            h3(nrow(locais))
        )
      )
    } else {
      # --- Visão da Zona ---
      locais_zona <- locais %>% filter(zona_real == zona)
      
      tagList(
        actionButton("bt_reset", "Voltar para o Mapa Geral", icon = icon("arrow-left"), style = "width: 100%; margin-bottom: 20px;"),
        h2(paste("Zona", zona)),
        hr(),
        div(class = "card-metric",
            h5("Locais de Votação"),
            h3(nrow(locais_zona))
        ),
        h4("Comparativo por Seção"),
        p("O espaço abaixo está reservado para os gráficos comparativos desta zona."),
        # Placeholder para gráficos futuros
        div(style = "height: 200px; background: #eee; border-radius: 5px; display: flex; align-items: center; justify-content: center; color: #777;",
            "Gráfico: Votos por Local")
      )
    }
  })
}

shinyApp(ui, server)