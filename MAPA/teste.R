library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(treemapify)
library(shinyWidgets)
library(DT)

# ==============================================================================
# 1. CONFIGURAÇÕES E CORES
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
# 2. CARREGAMENTO E TRATAMENTO DE DADOS
# ==============================================================================

# Função auxiliar para normalizar texto
normalizar_texto <- function(texto) {
  texto %>%
    as.character() %>%
    str_to_upper() %>%
    iconv(to = "ASCII//TRANSLIT") %>% 
    str_replace_all("[^A-Z0-9 ]", "") %>% 
    str_squish()
}

carregar_dados <- function() {
  tryCatch({
    message(">>> INICIANDO CARREGAMENTO <<<")
    
    shp_zonas <- "ctba_tre_zona_eleitoral_a.shp"
    shp_locais <- "ctba_tre_local_votacao_p.shp"
    csv_votos <- "Dados_curitiba.csv"
    csv_locais <- "locais.csv"
    
    if (!file.exists(shp_zonas)) stop("Arquivo SHP ZONAS não encontrado.")
    if (!file.exists(shp_locais)) stop("Arquivo SHP LOCAIS não encontrado.")
    if (!file.exists(csv_locais)) stop("Arquivo locais.csv não encontrado.")
    
    clean_id <- function(x) {
      as.character(readr::parse_number(as.character(x)))
    }
    
    # --- A. SHAPEFILES ---
    z <- st_read(shp_zonas, quiet = TRUE) %>% st_make_valid() %>% st_transform(crs = 4326)
    l <- st_read(shp_locais, quiet = TRUE) %>% st_transform(crs = 4326)
    
    if("zona_eleit" %in% names(z)) z <- rename(z, zona_real = zona_eleit)
    z$id_interno <- 1:nrow(z)
    z$zona_real <- clean_id(z$zona_real)
    
    if("zona_eleit" %in% names(l)) l <- rename(l, zona_real = zona_eleit)
    if("nome_local" %in% names(l)) l <- rename(l, local_nome = nome_local)
    if(!"local_nome" %in% names(l)) l$local_nome <- "LOCAL SEM NOME"
    
    l$zona_real <- clean_id(l$zona_real)
    l$local_nome <- str_to_upper(str_trim(l$local_nome))
    l$id_unico_mapa <- as.character(1:nrow(l))
    l$NOME_NORM <- normalizar_texto(l$local_nome)
    
    cols_l <- names(l)
    col_id_local <- cols_l[grepl("local|cod", cols_l, ignore.case = TRUE) & !grepl("nome|end|zona|norm", cols_l, ignore.case = TRUE)][1]
    l$cod_local_oficial <- if(!is.na(col_id_local)) clean_id(l[[col_id_local]]) else NA_character_
    
    # --- B. CSV LOCAIS ---
    dados_locais_info <- read_csv(csv_locais, locale = locale(encoding = "WINDOWS-1252"), show_col_types = FALSE)
    
    nms <- names(dados_locais_info)
    if(!"NR_ZONA" %in% nms) dados_locais_info <- rename(dados_locais_info, NR_ZONA = !!nms[grepl("zona", nms, ignore.case=T)][1])
    if(!"NR_SECAO" %in% nms) dados_locais_info <- rename(dados_locais_info, NR_SECAO = !!nms[grepl("secao|sec", nms, ignore.case=T)][1])
    if(!"NM_LOCAL_VOTACAO" %in% nms) dados_locais_info <- rename(dados_locais_info, NM_LOCAL_VOTACAO = !!nms[grepl("nome|nm_loc", nms, ignore.case=T)][1])
    
    dados_locais_info <- dados_locais_info %>%
      mutate(
        NR_ZONA = clean_id(NR_ZONA),
        NR_SECAO = clean_id(NR_SECAO),
        NM_LOCAL_VOTACAO_NORM = normalizar_texto(NM_LOCAL_VOTACAO)
      ) %>%
      distinct(NR_ZONA, NR_SECAO, .keep_all = TRUE)
    
    if("NR_LOCAL_VOTACAO" %in% names(dados_locais_info)) {
      dados_locais_info$NR_LOCAL_VOTACAO <- clean_id(dados_locais_info$NR_LOCAL_VOTACAO)
    }
    
    # --- C. CSV VOTOS ---
    dados_votos <- NULL
    if (file.exists(csv_votos)) {
      dados_votos <- read_csv(csv_votos, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
      dados_votos <- dados_votos %>%
        mutate(
          NM_PARTIDO = str_to_upper(str_trim(NM_PARTIDO)),
          NR_ZONA = clean_id(NR_ZONA),
          NR_SECAO = clean_id(NR_SECAO)
        ) %>%
        left_join(tabela_partidos, by = "NM_PARTIDO") %>%
        left_join(dados_locais_info, by = c("NR_ZONA", "NR_SECAO"))
    }
    
    list(zonas = z, locais = l, votos = dados_votos, erro = FALSE)
    
  }, error = function(e) {
    message("ERRO: ", e$message)
    z_fake <- st_as_sf(data.frame(id_interno=1, zona_real="1", geometry=st_sfc(st_polygon(list(rbind(c(-49.3,-25.4), c(-49.25,-25.4), c(-49.25,-25.45), c(-49.3,-25.45), c(-49.3,-25.4)))))), crs=4326)
    list(zonas = z_fake, locais = NULL, votos = NULL, erro = TRUE)
  })
}

d <- carregar_dados()
zonas <- d$zonas
locais <- d$locais
df_votos <- d$votos

# ==============================================================================
# 3. UI
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap"),
    tags$style(HTML("
      body { font-family: 'Poppins', sans-serif; background-color: #f4f7f6; }
      h1, h2, h3, h4, h5 { font-weight: 600; color: #2c3e50; }
      .card-style { background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); padding: 15px; margin-bottom: 15px; border: 1px solid #eee; }
      #mapa { height: 65vh !important; border-radius: 8px; }
      .cargo-switch { text-align: center; margin-bottom: 10px; }
      .modal-lg { max-width: 900px; }
      /* Ajuste para o botão de análise se destacar */
      .btn-analise { background-color: #3498db; color: white; border: none; font-weight: 600; width: 100%; }
      .btn-analise:hover { background-color: #2980b9; color: white; }
    "))
  ),
  
  div(class = "container-fluid", style="padding: 20px;",
      
      fluidRow(
        column(8, h3("Mapa Eleitoral Detalhado - Curitiba", style="margin:0;")),
        column(4, 
               div(class = "cargo-switch",
                   prettyRadioButtons(
                     inputId = "cargo_input", label = NULL,
                     choices = c("Prefeito", "Vereador"), selected = "Prefeito",
                     shape = "curve", animation = "pulse", status = "primary", inline = TRUE, bigger = TRUE
                   )
               )
        )
      ),
      hr(),
      
      fluidRow(
        column(8, div(class="card-style", leafletOutput("mapa"))),
        column(4,
               div(class="card-style", style="height: 68vh; overflow-y: auto;",
                   uiOutput("titulo_lateral"),
                   tabsetPanel(id = "tabs_lateral",
                               tabPanel("Candidatos", br(), plotlyOutput("plot_candidatos", height = "400px")),
                               tabPanel("Partidos", br(), plotOutput("plot_forca_partidaria", height = "400px")),
                               tabPanel("Treemap", br(), plotOutput("plot_treemap", height = "400px"))
                   )
               )
        )
      ),
      
      fluidRow(
        column(8, 
               div(class="card-style",
                   # Controles e BOTÃO NOVO
                   fluidRow(
                     column(3, h5("Distribuição de Votos", style="margin-top:5px;")),
                     column(3, actionButton("bt_concentracao", "🔍 Análise de Concentração", class="btn-analise", icon = icon("chart-pie"))), # Botão Novo
                     column(3, selectizeInput("candidato_dist", label = NULL, choices = NULL, options = list(placeholder = 'Candidato...'))),
                     column(3, textInput("search_local", label = NULL, placeholder = "Filtrar local..."))
                   ),
                   plotlyOutput("plot_distribuicao", height = "300px")
               )
        ),
        column(4, div(class="card-style", h5("Abstenção Geral"), uiOutput("stats_abstencao_texto"), plotlyOutput("plot_abstencao_bar", height = "150px")))
      )
  )
)

# ==============================================================================
# 4. SERVIDOR
# ==============================================================================
server <- function(input, output, session) {
  
  zona_ativa <- reactiveVal(NULL)
  
  # FILTROS
  dados_base <- reactive({
    req(df_votos, input$cargo_input)
    d <- df_votos %>% filter(DS_CARGO_PERGUNTA == input$cargo_input, !NM_VOTAVEL %in% c("Branco", "Nulo"))
    if(input$cargo_input == "Vereador") d <- d %>% filter(DS_TIPO_VOTAVEL == "Nominal")
    d
  })
  
  observe({
    req(dados_base())
    candidatos <- sort(unique(dados_base()$NM_VOTAVEL))
    updateSelectizeInput(session, "candidato_dist", choices = candidatos, server = TRUE)
  })
  
  dados_grafico <- reactive({
    req(dados_base())
    if(is.null(zona_ativa())) dados_base() else dados_base() %>% filter(NR_ZONA == zona_ativa())
  })
  
  vencedores_zona <- reactive({
    req(dados_base())
    dados_base() %>%
      group_by(NR_ZONA, SG_PARTIDO_SIGLA) %>% summarise(total = sum(QT_VOTOS, na.rm=T), .groups="drop") %>%
      group_by(NR_ZONA) %>% slice_max(total, n=1, with_ties=FALSE) %>% select(NR_ZONA, partido_vencedor = SG_PARTIDO_SIGLA)
  })
  
  # MAPA
  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -49.2715, lat = -25.44, zoom = 11)
  })
  
  observe({
    req(zonas, vencedores_zona())
    zonas_pintadas <- zonas %>% left_join(vencedores_zona(), by = c("zona_real" = "NR_ZONA"))
    pal <- colorFactor(palette = cores_partidos, domain = names(cores_partidos), na.color = "#cccccc")
    leafletProxy("mapa", data = zonas_pintadas) %>% clearShapes() %>%
      addPolygons(
        layerId = ~id_interno, fillColor = ~pal(partido_vencedor), fillOpacity = 0.6, color = "white", weight = 1,
        label = ~paste0("Zona ", zona_real, ": ", partido_vencedor),
        highlightOptions = highlightOptions(weight = 3, color = "white", fillOpacity = 0.8, bringToFront = FALSE)
      )
  })
  
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    req(click$id)
    poly <- zonas %>% filter(id_interno == as.numeric(click$id))
    if(nrow(poly) > 0) {
      num_zona <- poly$zona_real[1]
      if(!is.na(num_zona)) {
        zona_ativa(num_zona)
        bb <- as.numeric(st_bbox(poly))
        pontos <- locais %>% filter(zona_real == num_zona)
        leafletProxy("mapa") %>% clearMarkers() %>% clearGroup("highlight") %>%
          addPolygons(data=poly, group="highlight", fillColor="transparent", color="black", weight=4, opacity=1) %>%
          addCircleMarkers(data = pontos, layerId = ~id_unico_mapa, radius = 5, color = "#2c3e50", fillColor = "white", fillOpacity = 1, weight=2, label = ~local_nome)
        if(!any(is.na(bb))) leafletProxy("mapa") %>% flyToBounds(lng1=bb[1], lat1=bb[2], lng2=bb[3], lat2=bb[4])
      }
    }
  })
  
  observeEvent(input$mapa_marker_click, {
    click <- input$mapa_marker_click
    req(click$id, dados_base())
    id_clicado <- as.character(click$id)
    meta_escola <- locais %>% filter(id_unico_mapa == id_clicado)
    if(nrow(meta_escola) > 0) {
      nome_norm_shp <- meta_escola$NOME_NORM[1]
      zona_shp <- meta_escola$zona_real[1]
      cod_oficial_shp <- meta_escola$cod_local_oficial[1]
      df_escola <- data.frame()
      
      if(!is.na(cod_oficial_shp) && "NR_LOCAL_VOTACAO" %in% names(dados_base())) {
        df_temp <- dados_base() %>% filter(NR_LOCAL_VOTACAO == cod_oficial_shp)
        if(nrow(df_temp)>0) df_escola <- df_temp
      }
      if(nrow(df_escola) == 0 && "NOME_NORM" %in% names(dados_base())) {
        df_temp <- dados_base() %>% filter(NR_ZONA == zona_shp, NOME_NORM == nome_norm_shp)
        if(nrow(df_temp)>0) df_escola <- df_temp
      }
      
      nome_display <- meta_escola$local_nome[1]
      endereco_display <- "Endereço não vinculado"
      if(nrow(df_escola) > 0) {
        nome_display <- df_escola$NM_LOCAL_VOTACAO[1]
        if("DS_ENDERECO" %in% names(df_escola)) endereco_display <- paste(df_escola$DS_ENDERECO[1], "-", df_escola$NM_BAIRRO[1])
      }
      
      df_tabela_modal <- data.frame()
      if(nrow(df_escola) > 0) {
        df_tabela_modal <- df_escola %>% group_by(NM_VOTAVEL, SG_PARTIDO_SIGLA) %>%
          summarise(Votos = sum(QT_VOTOS), .groups="drop") %>% arrange(desc(Votos)) %>%
          mutate(`%` = round(Votos/sum(Votos)*100, 2), Posição = row_number()) %>%
          select(Posição, Candidato=NM_VOTAVEL, Partido=SG_PARTIDO_SIGLA, Votos, `%`)
      }
      
      output$plot_modal <- renderPlotly({
        if(nrow(df_tabela_modal) == 0) return(plotly_empty() %>% layout(title = "Dados não encontrados"))
        top <- head(df_tabela_modal, 10)
        plot_ly(top, x = ~Votos, y = ~reorder(Candidato, Votos), type='bar', orientation='h',
                text=~paste0(`%`,"%"), textposition='auto', marker=list(color=~cores_partidos[Partido])) %>%
          layout(yaxis=list(title=""), xaxis=list(title="Votos"))
      })
      output$tabela_modal_render <- renderDT({
        datatable(df_tabela_modal, options = list(pageLength=5, scrollY="300px"), rownames=FALSE, selection="none")
      })
      titulo <- if(nrow(df_escola) == 0) paste(nome_display, "(Sem vínculo)") else nome_display
      showModal(modalDialog(title = icon("school"), h3(titulo, style="margin-top:0"), p(endereco_display),
                            hr(), h4("Top 10"), plotlyOutput("plot_modal", height = "250px"),
                            hr(), h4("Lista Completa"), DTOutput("tabela_modal_render"), size = "l", easyClose = TRUE, footer = modalButton("Fechar")))
    }
  })
  
  # --- BOTÃO DE ANÁLISE DE CONCENTRAÇÃO (NOVA FUNÇÃO) ---
  observeEvent(input$bt_concentracao, {
    req(dados_base())
    showModal(modalDialog(title="Calculando...", "Processando dados eleitorais, aguarde um momento...", easyClose=F, footer=NULL))
    
    # 1. Totais por Candidato
    totais <- dados_base() %>%
      group_by(NM_VOTAVEL, SG_PARTIDO_SIGLA) %>%
      summarise(Total_Geral = sum(QT_VOTOS), .groups="drop") %>%
      filter(Total_Geral > 2000) # Filtro > 2000 votos
    
    # 2. Votos por Zona e Cálculo
    if(nrow(totais) > 0) {
      concentracao <- dados_base() %>%
        filter(NM_VOTAVEL %in% totais$NM_VOTAVEL) %>%
        group_by(NM_VOTAVEL, NR_ZONA) %>%
        summarise(Votos_Zona = sum(QT_VOTOS), .groups="drop") %>%
        left_join(totais, by="NM_VOTAVEL") %>%
        mutate(Perc = Votos_Zona / Total_Geral) %>%
        filter(Perc > 0.5) %>% # Critério > 50%
        arrange(desc(Perc)) %>%
        mutate(Concentracao = paste0(round(Perc*100, 1), "%")) %>%
        # CORREÇÃO: Usar SG_PARTIDO_SIGLA em vez de SG_PARTIDO_SIGLA.x
        select(Candidato = NM_VOTAVEL, Partido = SG_PARTIDO_SIGLA, `Zona Dominante` = NR_ZONA, `Votos na Zona` = Votos_Zona, `Total Geral` = Total_Geral, `% na Zona` = Concentracao)
    } else {
      concentracao <- data.frame()
    }
    
    removeModal()
    
    showModal(modalDialog(
      title = icon("chart-pie"),
      h3("Candidatos com Alta Concentração Regional"),
      p("Lista de candidatos com mais de 2.000 votos totais que obtiveram mais de 50% da sua votação em uma única Zona Eleitoral."),
      hr(),
      renderDT({
        datatable(concentracao, options = list(pageLength = 10, scrollX = TRUE))
      }),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  # GRÁFICOS
  output$titulo_lateral <- renderUI({
    if(is.null(zona_ativa())) h4("Curitiba - Geral") else tagList(h4(paste("Zona", zona_ativa())), actionButton("bt_reset", "Ver Geral", class="btn-xs btn-default"))
  })
  observeEvent(input$bt_reset, {
    zona_ativa(NULL)
    leafletProxy("mapa") %>% clearMarkers() %>% clearGroup("highlight") %>% setView(lng = -49.2715, lat = -25.44, zoom = 11)
  })
  output$plot_candidatos <- renderPlotly({
    req(dados_grafico())
    df <- dados_grafico() %>% group_by(NM_VOTAVEL, SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS), .groups="drop") %>% mutate(p=v/sum(v)*100) %>% arrange(desc(v)) %>% head(10)
    plot_ly(df, x=~v, y=~reorder(NM_VOTAVEL, v), type='bar', orientation='h', text=~paste0(round(p,1),"%"), textposition='auto', marker=list(color=~cores_partidos[SG_PARTIDO_SIGLA])) %>% layout(yaxis=list(title=""), xaxis=list(title="Votos"), margin=list(l=10,t=10,b=10))
  })
  output$plot_forca_partidaria <- renderPlot({
    req(dados_grafico())
    df <- dados_grafico() %>% group_by(SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS)) %>% mutate(p=v/sum(v)*100) %>% arrange(desc(v)) %>% head(15)
    ggplot(df, aes(x=reorder(SG_PARTIDO_SIGLA, v), y=v, fill=SG_PARTIDO_SIGLA)) + geom_col() + geom_text(aes(label=paste0(round(p,1),"%")), hjust=-0.1, family="Poppins") + coord_flip() + scale_fill_manual(values=cores_partidos) + theme_minimal(base_family="Poppins") + theme(legend.position="none") + labs(x="", y="Votos")
  })
  output$plot_treemap <- renderPlot({
    req(dados_grafico())
    df <- dados_grafico() %>% group_by(SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS))
    ggplot(df, aes(area=v, fill=SG_PARTIDO_SIGLA, label=SG_PARTIDO_SIGLA)) + geom_treemap() + geom_treemap_text(color="white", place="centre", family="Poppins", fontface="bold") + scale_fill_manual(values=cores_partidos) + theme_void(base_family="Poppins") + theme(legend.position="none")
  })
  output$plot_distribuicao <- renderPlotly({
    req(input$candidato_dist, dados_base())
    candidato <- input$candidato_dist
    zona <- zona_ativa()
    filtro_texto <- input$search_local
    df_cand <- dados_base() %>% filter(NM_VOTAVEL == candidato)
    if(nrow(df_cand) == 0) return(plotly_empty() %>% layout(title = "Sem votos"))
    if(is.null(zona)) {
      df_plot <- df_cand %>% group_by(NR_ZONA) %>% summarise(v = sum(QT_VOTOS), .groups="drop") %>% arrange(desc(v))
      plot_ly(df_plot, x = ~v, y = ~reorder(paste("Zona", NR_ZONA), v), type = 'bar', orientation='h', marker = list(color = "#3498db")) %>% layout(xaxis = list(title = "Votos"), yaxis = list(title = ""), title = paste("Votação por Zona:", candidato))
    } else {
      df_plot <- df_cand %>% filter(NR_ZONA == zona) %>% group_by(NM_LOCAL_VOTACAO) %>% summarise(v = sum(QT_VOTOS), .groups="drop") %>% arrange(desc(v))
      if(!is.null(filtro_texto) && filtro_texto != "") df_plot <- df_plot %>% filter(grepl(filtro_texto, NM_LOCAL_VOTACAO, ignore.case = TRUE))
      df_plot$NM_LOCAL_VOTACAO[is.na(df_plot$NM_LOCAL_VOTACAO)] <- "Local N/I"
      plot_ly(df_plot, x = ~v, y = ~reorder(NM_LOCAL_VOTACAO, v), type = 'bar', orientation = 'h', text = ~paste(NM_LOCAL_VOTACAO, ":", v, "votos"), hoverinfo = "text", marker = list(color = "#e67e22")) %>% layout(xaxis = list(title = "Votos"), yaxis = list(title = "", showticklabels = FALSE), title = paste("Votação na Zona", zona), margin = list(l=0, r=0, t=30, b=0))
    }
  })
  output$stats_abstencao_texto <- renderUI({
    req(dados_grafico())
    d <- dados_grafico() %>% distinct(NR_ZONA, NR_SECAO, .keep_all=T)
    if(nrow(d)==0) return(NULL)
    abs_v <- sum(d$QT_ABSTENCOES, na.rm=T); tot <- sum(d$QT_APTOS, na.rm=T)
    h4(paste0("Abstenção: ", round((abs_v/tot)*100,1), "%"), style="color:#e74c3c; text-align:center;")
  })
  output$plot_abstencao_bar <- renderPlotly({
    req(dados_grafico())
    d <- dados_grafico() %>% distinct(NR_ZONA, NR_SECAO, .keep_all=T)
    if(nrow(d)==0) return(plotly_empty())
    df <- data.frame(T=c("Votos","Abstenção"), Q=c(sum(d$QT_COMPARECIMENTO, na.rm=T), sum(d$QT_ABSTENCOES, na.rm=T)))
    df$P <- df$Q/sum(df$Q)
    plot_ly(df, x=~Q, y=~T, type='bar', orientation='h', marker=list(color=c("#27ae60","#c0392b")), text=~paste0(round(P*100,1),"%"), textposition='auto') %>% layout(xaxis=list(showticklabels=F, title=""), yaxis=list(title=""), margin=list(l=80,t=0,b=0))
  })
}

shinyApp(ui, server)