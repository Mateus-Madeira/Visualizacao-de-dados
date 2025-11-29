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

fmt_num <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)

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
  "AGIR"          = "#5C6BC0",
  "AVANTE"        = "#FFB300",
  "CIDADANIA"     = "#8D6E63",
  "DC"            = "#546E7A",
  "MDB"           = "#388E3C",
  "MOBILIZA"      = "#8E24AA",
  "NOVO"          = "#F57C00",
  "PCB"           = "#AD1457",
  "PCdoB"         = "#C62828",
  "PCO"           = "#B71C1C",
  "PDT"           = "#7E57C2",
  "PL"            = "#2c3e50",
  "PMB"           = "#F48FB1",
  "PODE"          = "#0288D1",
  "PP"            = "#0097A7",
  "PRD"           = "#455A64",
  "PRTB"          = "#00695C",
  "PSB"           = "#F06292",
  "PSD"           = "#1976D2",
  "PSDB"          = "#039BE5",
  "PSOL"          = "#FBC02D",
  "PSTU"          = "#D84315",
  "PT"            = "#D32F2F",
  "PV"            = "#43A047",
  "REDE"          = "#26A69A",
  "REPUBLICANOS"  = "#00796B",
  "SOLIDARIEDADE" = "#FB8C00",
  "UNIÃO"         = "#303F9F",
  "UP"            = "#880E4F"
)
# ==============================================================================
# 2. CARREGAMENTO DE DADOS
# ==============================================================================

normalizar_texto <- function(texto) {
  texto %>%
    as.character() %>%
    str_to_upper() %>%
    str_remove_all("^\\d+[ª°º]?\\s*ZONA\\s*[-]?\\s*") %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>% 
    iconv(to = "ASCII//TRANSLIT") %>% 
    str_remove_all("\\b(EM|CE|EE|CMEI|UE|ER|E M|C E|E E|E M EF|E E F M)\\b") %>%
    str_remove_all("\\b(ESCOLA|COLEGIO|MUNICIPAL|ESTADUAL|CENTRO|EDUCACAO|INFANTIL|ENSINO|FUNDAMENTAL|MEDIO|PROFESSOR|PROFESSORA|DOM|DOUTOR|PREFEITO|ALMIRANTE|GENERAL|MARECHAL|CORONEL|PADRE|IRMA|SANTO|SANTA|SAO)\\b") %>%
    str_squish()
}

carregar_dados <- function() {
  tryCatch({
    message(">>> INICIANDO CARREGAMENTO <<<")
    shp_zonas <- "ctba_tre_zona_eleitoral_a.shp"
    shp_locais <- "ctba_tre_local_votacao_p.shp"
    csv_votos <- "Dados_curitiba.csv"
    csv_locais <- "locais.csv"
    
    if (!file.exists(shp_zonas) || !file.exists(shp_locais) || !file.exists(csv_locais)) stop("Arquivos ausentes.")
    
    clean_id <- function(x) as.character(readr::parse_number(as.character(x)))
    
    # --- SHAPEFILES ---
    z <- st_read(shp_zonas, quiet=T) %>% st_make_valid() %>% st_transform(4326)
    l <- st_read(shp_locais, quiet=T) %>% st_transform(4326)
    
    if("zona_eleit" %in% names(z)) z <- rename(z, zona_real = zona_eleit)
    z$id_interno <- 1:nrow(z); z$zona_real <- clean_id(z$zona_real)
    
    if("zona_eleit" %in% names(l)) l <- rename(l, zona_real = zona_eleit)
    if("nome_local" %in% names(l)) l <- rename(l, local_nome = nome_local)
    if(!"local_nome" %in% names(l)) l$local_nome <- "LOCAL SEM NOME"
    
    l$zona_real <- clean_id(l$zona_real)
    l$local_nome <- str_to_upper(str_trim(l$local_nome))
    l$id_unico_mapa <- as.character(1:nrow(l))
    l$NOME_NORM <- normalizar_texto(l$local_nome)
    
    cols_l <- names(l); col_id <- cols_l[grepl("local|cod", cols_l, ignore.case=T) & !grepl("nome|end|zona|norm", cols_l, ignore.case=T)][1]
    l$cod_local_oficial <- if(!is.na(col_id)) clean_id(l[[col_id]]) else NA_character_
    
    # --- CSV LOCAIS ---
    dl <- read_csv(csv_locais, locale = locale(encoding = "WINDOWS-1252"), show_col_types = FALSE)
    nms <- names(dl)
    if(!"NR_ZONA" %in% nms) dl <- rename(dl, NR_ZONA = !!nms[grepl("zona", nms, ignore.case=T)][1])
    if(!"NR_SECAO" %in% nms) dl <- rename(dl, NR_SECAO = !!nms[grepl("secao|sec", nms, ignore.case=T)][1])
    if(!"NM_LOCAL_VOTACAO" %in% nms) dl <- rename(dl, NM_LOCAL_VOTACAO = !!nms[grepl("nome|nm_loc", nms, ignore.case=T)][1])
    
    # Endereço
    if(!"DS_ENDERECO" %in% nms) {
      ce <- nms[grepl("endereco|logradouro", nms, ignore.case=T)][1]
      if(!is.na(ce)) dl <- rename(dl, DS_ENDERECO = !!ce) else dl$DS_ENDERECO <- "Endereço N/D"
    }
    if(!"NM_BAIRRO" %in% nms) {
      cb <- nms[grepl("bairro", nms, ignore.case=T)][1]
      if(!is.na(cb)) dl <- rename(dl, NM_BAIRRO = !!cb) else dl$NM_BAIRRO <- ""
    }
    
    dl <- dl %>% mutate(NR_ZONA=clean_id(NR_ZONA), NR_SECAO=clean_id(NR_SECAO), 
                        NM_LOCAL_VOTACAO_NORM=normalizar_texto(NM_LOCAL_VOTACAO)) %>%
      distinct(NR_ZONA, NR_SECAO, .keep_all=T)
    
    if("NR_LOCAL_VOTACAO" %in% names(dl)) dl$NR_LOCAL_VOTACAO <- clean_id(dl$NR_LOCAL_VOTACAO)
    
    # --- CSV VOTOS ---
    dv <- NULL
    if (file.exists(csv_votos)) {
      dv <- read_csv(csv_votos, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
      dv <- dv %>% mutate(NM_PARTIDO=str_to_upper(str_trim(NM_PARTIDO)), NR_ZONA=clean_id(NR_ZONA), NR_SECAO=clean_id(NR_SECAO)) %>%
        left_join(tabela_partidos, by = "NM_PARTIDO") %>% 
        left_join(dl, by = c("NR_ZONA", "NR_SECAO"))
    }
    list(zonas = z, locais = l, votos = dv, erro = FALSE)
  }, error = function(e) {
    message("ERRO: ", e$message)
    list(zonas = st_as_sf(data.frame(id_interno=1, zona_real="1", geometry=st_sfc(st_polygon(list(rbind(c(-49.3,-25.4), c(-49.25,-25.4), c(-49.3,-25.45), c(-49.3,-25.4)))))), crs=4326), locais=NULL, votos=NULL, erro=TRUE)
  })
}

d <- carregar_dados()
zonas <- d$zonas; locais <- d$locais; df_votos <- d$votos

# ==============================================================================
# 3. UI
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap"),
    tags$style(HTML("
      body { font-family: 'Poppins', sans-serif; background-color: #f4f7f6; }
      h1, h2, h3, h4, h5 { font-weight: 600; color: #2c3e50; }
      .card-style { background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); padding: 15px; margin-bottom: 15px; border: 1px solid #eee; position: relative; }
      #mapa { height: 65vh !important; border-radius: 8px; }
      .cargo-switch { text-align: center; margin-bottom: 10px; }
      .btn-expand { position: absolute; top: 10px; right: 10px; color: #95a5a6; cursor: pointer; z-index: 100; background: transparent; border: none; }
      .btn-expand:hover { color: #2c3e50; }
      .leaflet-control-legend { background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 5px rgba(0,0,0,0.2); max-height: 300px; overflow-y: auto; font-size: 11px; line-height: 1.6; width: 160px; }
      .leaflet-control-legend i { width: 12px; height: 12px; float: left; margin-right: 8px; margin-top: 2px; border-radius: 2px; opacity: 0.9; }
      .control-panel { background-color: #f8f9fa; border-radius: 6px; padding: 15px; margin-bottom: 15px; border-left: 4px solid #3498db; }
      .btn-destaque { background-color: #2c3e50 !important; color: white !important; font-weight: bold; width: 100%; height: 100%; min-height: 80px; border-radius: 6px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s ease; display: flex; align-items: center; justify-content: center; flex-direction: column; border: none;}
      .btn-destaque:hover { transform: translateY(-2px); box-shadow: 0 6px 8px rgba(0,0,0,0.15); background-color: #1a252f !important; }
    "))
  ),
  
  div(class="container-fluid", style="padding: 20px;",
      fluidRow(
        column(8, h3("Mapa Eleitoral Detalhado - Curitiba", style="margin:0;")),
        column(4, div(class="cargo-switch", prettyRadioButtons("cargo_input", NULL, c("Prefeito", "Vereador"), selected="Vereador", shape="curve", animation="pulse", status="primary", inline=T, bigger=T)))
      ), hr(),
      
      fluidRow(
        column(8, div(class="card-style", leafletOutput("mapa"))),
        column(4, div(class="card-style", style="height: 68vh; overflow-y: auto;",
                      uiOutput("titulo_lateral"),
                      tabsetPanel(id="tabs_lateral",
                                  tabPanel("Candidatos", br(), 
                                           textInput("busca_cand_sidebar", NULL, placeholder="Buscar Candidato (Nome)..."),
                                           actionButton("exp_cand", "", icon=icon("expand"), class="btn-expand"),
                                           # AQUI FOI ALTERADO: uiOutput em vez de plotlyOutput fixo
                                           uiOutput("ui_plot_candidatos")),
                                  tabPanel("Partidos", br(), 
                                           actionButton("exp_part", "", icon=icon("expand"), class="btn-expand"),
                                           plotlyOutput("plot_forca_partidaria", height="400px")),
                                  tabPanel("Mosaico", br(), 
                                           actionButton("exp_tree", "", icon=icon("expand"), class="btn-expand"),
                                           plotlyOutput("plot_treemap", height="400px"))
                      )))
      ),
      
      fluidRow(
        column(8, div(class="card-style",
                      div(class="control-panel",
                          fluidRow(
                            column(8, h4("📊 Distribuição de Votos", style="margin-top:0; color:#2980b9; font-weight:700;"),
                                   fluidRow(column(6, selectizeInput("candidato_dist", "Visualizar Candidato:", choices=NULL, width="100%")),
                                            column(6, textInput("search_local", "Filtrar Local de Votação:", placeholder="Ex: Escola Municipal...", width="100%")))),
                            column(4, div(style="height: 100%; padding-left: 10px; border-left: 1px solid #ddd;",
                                          actionButton("bt_concentracao", label = HTML("Possíveis Redutos<br><small style='font-weight:normal'>(Conc. > 50% em uma Zona)</small>"), class="btn-destaque")))
                          )
                      ),
                      div(style="position: relative;", actionButton("exp_dist", "", icon=icon("expand"), class="btn-expand"), plotlyOutput("plot_distribuicao", height="300px"))
        )),
        column(4, div(class="card-style", h5("Abstenção Geral"), uiOutput("stats_abstencao_texto"), plotlyOutput("plot_abstencao_bar", height="150px")))
      )
  )
)

# ==============================================================================
# 4. SERVIDOR
# ==============================================================================
server <- function(input, output, session) {
  zona_ativa <- reactiveVal(NULL)
  empty_plot <- function(title="Sem dados") plot_ly(type="scatter", mode="markers") %>% layout(title=list(text=title, y=0.5), xaxis=list(visible=F), yaxis=list(visible=F))
  
  dados_base <- reactive({
    req(df_votos, input$cargo_input)
    d <- df_votos %>% filter(DS_CARGO_PERGUNTA == input$cargo_input, !NM_VOTAVEL %in% c("Branco", "Nulo"))
    if(input$cargo_input == "Vereador") d <- d %>% filter(DS_TIPO_VOTAVEL == "Nominal")
    d %>% mutate(NM_DISPLAY = paste0(NM_VOTAVEL, " (", SG_PARTIDO_SIGLA, ")"))
  })
  
  observe({ req(dados_base()); updateSelectizeInput(session, "candidato_dist", choices = sort(unique(dados_base()$NM_VOTAVEL)), server = TRUE) })
  
  dados_grafico <- reactive({
    req(dados_base()); if(is.null(zona_ativa())) dados_base() else dados_base() %>% filter(NR_ZONA == zona_ativa())
  })
  
  vencedores_zona <- reactive({
    req(dados_base())
    dados_base() %>% group_by(NR_ZONA, SG_PARTIDO_SIGLA) %>% summarise(total=sum(QT_VOTOS, na.rm=T), .groups="drop") %>%
      group_by(NR_ZONA) %>% slice_max(total, n=1, with_ties=F) %>% select(NR_ZONA, partido_vencedor_zona=SG_PARTIDO_SIGLA)
  })
  
  vencedores_escola_mapa <- reactive({
    req(dados_base())
    d <- dados_base()
    if("NM_LOCAL_VOTACAO_NORM" %in% names(d)) {
      d %>% filter(!is.na(NM_LOCAL_VOTACAO_NORM)) %>%
        group_by(NR_ZONA, NM_LOCAL_VOTACAO_NORM, SG_PARTIDO_SIGLA) %>%
        summarise(total=sum(QT_VOTOS, na.rm=T), .groups="drop") %>%
        group_by(NR_ZONA, NM_LOCAL_VOTACAO_NORM) %>% slice_max(total, n=1, with_ties=F) %>%
        select(NR_ZONA, NOME_NORM=NM_LOCAL_VOTACAO_NORM, partido_vencedor=SG_PARTIDO_SIGLA)
    } else NULL
  })
  
  output$mapa <- renderLeaflet({
    pal <- colorFactor(palette = cores_partidos, domain = names(cores_partidos), na.color = "#cccccc")
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -49.2715, lat = -25.44, zoom = 11) %>%
      addLegend("bottomleft", pal = pal, values = names(cores_partidos), title = "Partidos", opacity = 1, className = "leaflet-control-legend")
  })
  
  observe({
    req(zonas, vencedores_zona())
    zp <- zonas %>% left_join(vencedores_zona(), by = c("zona_real" = "NR_ZONA"))
    pal <- colorFactor(palette = cores_partidos, domain = names(cores_partidos), na.color = "#cccccc")
    
    proxy <- leafletProxy("mapa", data = zp) %>% clearShapes() %>%
      addPolygons(layerId=~id_interno, fillColor=~pal(partido_vencedor_zona), fillOpacity=0.5, color="white", weight=1,
                  label=~paste0("Zona ", zona_real, ": ", partido_vencedor_zona), highlightOptions=highlightOptions(weight=3, color="white", fillOpacity=0.8))
    
    if(!is.null(zona_ativa())) {
      num_zona <- zona_ativa()
      pontos <- locais %>% filter(zona_real == num_zona)
      if(nrow(pontos) > 0) {
        pontos$partido_vencedor <- NA
        if(!is.null(vencedores_escola_mapa())) {
          wins <- vencedores_escola_mapa() %>% filter(NR_ZONA == num_zona)
          for(i in 1:nrow(pontos)) {
            p_norm <- pontos$NOME_NORM[i]
            m <- wins[wins$NOME_NORM == p_norm, ]
            if(nrow(m) == 0) m <- wins[grepl(p_norm, wins$NOME_NORM, fixed=TRUE), ]
            if(nrow(m) == 0 && nchar(p_norm) > 4) m <- wins[grepl(substr(p_norm, 1, 5), wins$NOME_NORM, fixed=TRUE), ]
            if(nrow(m) > 0) pontos$partido_vencedor[i] <- m$partido_vencedor[1]
          }
        }
        proxy %>% clearMarkers() %>%
          addCircleMarkers(data = pontos, layerId = ~id_unico_mapa, radius = 6, color = "white", weight = 1, opacity = 1,
                           fillColor = ~pal(partido_vencedor), fillOpacity = 1,
                           label = ~paste0(local_nome, " (Venceu: ", ifelse(is.na(partido_vencedor), "-", partido_vencedor), ")"))
      }
    } else { leafletProxy("mapa") %>% clearMarkers() }
  })
  
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click; req(click$id)
    poly <- zonas %>% filter(id_interno == as.numeric(click$id))
    if(nrow(poly) > 0) {
      zona_ativa(poly$zona_real[1]); bb <- as.numeric(st_bbox(poly))
      if(!any(is.na(bb))) leafletProxy("mapa") %>% flyToBounds(lng1=bb[1], lat1=bb[2], lng2=bb[3], lat2=bb[4])
    }
  })
  
  observeEvent(input$mapa_marker_click, {
    click <- input$mapa_marker_click; req(click$id, dados_base())
    id_clicado <- as.character(click$id); meta <- locais %>% filter(id_unico_mapa == id_clicado)
    if(nrow(meta) > 0) {
      nm_norm <- meta$NOME_NORM[1]; zn <- meta$zona_real[1]; cod_shp <- meta$cod_local_oficial[1]
      df_e <- data.frame()
      if(!is.na(cod_shp) && "NR_LOCAL_VOTACAO" %in% names(dados_base())) {
        df_temp <- dados_base() %>% filter(NR_LOCAL_VOTACAO == cod_shp)
        if(nrow(df_temp) > 0) df_e <- df_temp
      }
      if(nrow(df_e) == 0) {
        df_zona <- dados_base() %>% filter(NR_ZONA == zn)
        if(nrow(df_zona) > 0) {
          df_match <- df_zona %>% filter(NM_LOCAL_VOTACAO_NORM == nm_norm)
          if(nrow(df_match) == 0) df_match <- df_zona %>% filter(grepl(nm_norm, NM_LOCAL_VOTACAO_NORM, fixed=TRUE))
          if(nrow(df_match) > 0) df_e <- df_match
        }
      }
      
      nome_display <- meta$local_nome[1]; end_display <- "Endereço não vinculado"
      df_tab <- data.frame()
      if(nrow(df_e) > 0) {
        nome_display <- df_e$NM_LOCAL_VOTACAO[1]
        if("DS_ENDERECO" %in% names(df_e)) {
          end <- df_e$DS_ENDERECO[1]; if(!is.na(end) && end != "Endereço N/D") end_display <- paste(end, "-", df_e$NM_BAIRRO[1])
        }
        df_tab <- df_e %>% group_by(NM_DISPLAY, SG_PARTIDO_SIGLA) %>% summarise(Votos=sum(QT_VOTOS), .groups="drop") %>%
          arrange(desc(Votos)) %>% mutate(`%`=round(Votos/sum(Votos)*100,2), Pos=row_number()) %>% select(Pos, Candidato=NM_DISPLAY, Partido=SG_PARTIDO_SIGLA, Votos, `%`)
      }
      
      output$plot_modal <- renderPlotly({
        if(nrow(df_tab)==0) return(empty_plot("Dados não encontrados"))
        top <- head(df_tab, 10)
        plot_ly(top, x=~Votos, y=~reorder(Candidato, Votos), type='bar', orientation='h',
                text=~paste0(fmt_num(Votos), " (", `%`, "%)"), textposition='auto', marker=list(color=~cores_partidos[Partido])) %>%
          layout(yaxis=list(title=""), xaxis=list(title="Votos", tickformat=".0f"))
      })
      output$tabela_render <- renderDT({ datatable(df_tab, options=list(pageLength=5, scrollY="300px"), rownames=F, selection="none") })
      titulo <- if(nrow(df_tab) == 0) paste(nome_display, "(Sem Vínculo)") else nome_display
      showModal(modalDialog(title=icon("school"), h3(titulo, style="margin:0"), p(end_display), hr(), 
                            h4("Top 10"), plotlyOutput("plot_modal", height="250px"), 
                            hr(), h4("Completo"), DTOutput("tabela_render"), size="l", easyClose=T, footer=modalButton("Fechar")))
    }
  })
  
  modal_expand <- function(id, t) showModal(modalDialog(title=t, plotlyOutput(id, height="80vh"), size="l", easyClose=T, footer=modalButton("Fechar")))
  observeEvent(input$exp_cand, modal_expand("plot_candidatos_modal", "Candidatos"))
  observeEvent(input$exp_part, modal_expand("plot_forca_modal", "Partidos"))
  observeEvent(input$exp_tree, modal_expand("plot_treemap_modal", "Treemap"))
  observeEvent(input$exp_dist, modal_expand("plot_dist_modal", "Distribuição"))
  
  output$plot_candidatos_modal <- renderPlotly({
    req(dados_grafico()); df <- dados_grafico() %>% group_by(NM_DISPLAY, SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS), .groups="drop") %>% mutate(p=v/sum(v)*100) %>% arrange(desc(v)) %>% head(20)
    plot_ly(df, x=~v, y=~reorder(NM_DISPLAY, v), type='bar', orientation='h', text=~paste0(fmt_num(v), " (", round(p,1), "%)"), textposition='auto', marker=list(color=~cores_partidos[SG_PARTIDO_SIGLA])) %>% layout(yaxis=list(title=""), xaxis=list(title="Votos", tickformat=".0f"))
  })
  output$plot_forca_modal <- renderPlotly({
    req(dados_grafico()); df <- dados_grafico() %>% group_by(SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS)) %>% mutate(p=v/sum(v)*100) %>% arrange(desc(v))
    plot_ly(df, x=~v, y=~reorder(SG_PARTIDO_SIGLA, v), type='bar', orientation='h', text=~paste0(fmt_num(v), " (", round(p,1), "%)"), textposition='auto', marker=list(color=~cores_partidos[SG_PARTIDO_SIGLA])) %>% layout(yaxis=list(title=""), xaxis=list(title="Votos", tickformat=".0f"))
  })
  output$plot_treemap_modal <- renderPlotly({
    req(dados_grafico()); df <- dados_grafico() %>% group_by(SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS))
    plot_ly(type="treemap", labels=df$SG_PARTIDO_SIGLA, parents=NA, values=df$v, textinfo="label+value+percent parent", hoverinfo="label+value+percent parent", marker=list(colors=cores_partidos[df$SG_PARTIDO_SIGLA]))
  })
  
  plot_dist_logica <- function(cand, zona, txt) {
    df <- dados_base() %>% filter(NM_VOTAVEL == cand)
    if(nrow(df)==0) return(empty_plot("Sem votos"))
    labels_visiveis <- is.null(zona)
    p <- if(is.null(zona)) {
      df %>% group_by(NR_ZONA) %>% summarise(v=sum(QT_VOTOS)) %>% arrange(desc(v)) %>% mutate(Label=paste("Zona", NR_ZONA))
    } else {
      temp <- df %>% filter(NR_ZONA==zona) %>% group_by(NM_LOCAL_VOTACAO) %>% summarise(v=sum(QT_VOTOS)) %>% arrange(desc(v))
      if(!is.null(txt)&&txt!="") temp <- temp %>% filter(grepl(txt, NM_LOCAL_VOTACAO, ignore.case=T))
      temp$NM_LOCAL_VOTACAO[is.na(temp$NM_LOCAL_VOTACAO)] <- "N/I"
      temp %>% mutate(Label=NM_LOCAL_VOTACAO)
    }
    if(nrow(p)==0) return(empty_plot("Filtro vazio"))
    plot_ly(p, x=~v, y=~reorder(Label, v), type='bar', orientation='h',
            text=~paste0("<b>", fmt_num(v), "</b>"), textposition='auto',
            marker=list(color=~v, colorscale=list(c(0, 1), c("#85c1e9", "#2e86c1")), showscale=FALSE),
            hoverinfo="text", hovertext=~paste0("<b>", Label, "</b><br>Votos: ", fmt_num(v))) %>%
      layout(xaxis=list(title="Votos", tickformat=".0f", gridcolor="#ecf0f1"), yaxis=list(title="", showticklabels = labels_visiveis), plot_bgcolor="rgba(0,0,0,0)")
  }
  
  output$plot_dist_modal <- renderPlotly({ req(input$candidato_dist); plot_dist_logica(input$candidato_dist, zona_ativa(), input$search_local) })
  output$plot_distribuicao <- renderPlotly({ req(input$candidato_dist); plot_dist_logica(input$candidato_dist, zona_ativa(), input$search_local) %>% layout(margin=list(l=10, r=10, t=10, b=30)) })
  
  # --- LOGICA NOVA DE CANDIDATOS (RENDERUI PARA ALTURA DINAMICA) ---
  dados_candidatos_processados <- reactive({
    req(dados_grafico())
    df <- dados_grafico() %>% 
      group_by(NM_DISPLAY, SG_PARTIDO_SIGLA) %>% 
      summarise(v=sum(QT_VOTOS), .groups="drop") %>% 
      mutate(p=v/sum(v)*100) %>% 
      arrange(desc(v))
    
    busca <- input$busca_cand_sidebar
    if(!is.null(busca) && busca != "") {
      df <- df %>% filter(grepl(busca, NM_DISPLAY, ignore.case = TRUE))
    }
    df
  })
  
  output$ui_plot_candidatos <- renderUI({
    df <- dados_candidatos_processados()
    # Altura dinâmica: 25px por barra, mínimo de 400px
    altura_plot <- max(400, nrow(df) * 25)
    plotlyOutput("plot_candidatos_render", height = paste0(altura_plot, "px"))
  })
  
  output$plot_candidatos_render <- renderPlotly({
    df <- dados_candidatos_processados()
    if(nrow(df)==0) return(empty_plot("Nenhum encontrado"))
    
    # Tooltip customizado (Hover)
    hover_txt <- paste0("<b>", df$NM_DISPLAY, "</b><br>",
                        "Partido: ", df$SG_PARTIDO_SIGLA, "<br>",
                        "Votos: ", fmt_num(df$v), " (", round(df$p, 2), "%)")
    
    plot_ly(df, x=~v, y=~reorder(NM_DISPLAY, v), type='bar', orientation='h',
            # hoverinfo='text' e hovertext definem o balãozinho
            hoverinfo="text", hovertext=hover_txt,
            marker=list(color=~cores_partidos[SG_PARTIDO_SIGLA])) %>% 
      layout(yaxis=list(title=""), 
             xaxis=list(title="Votos", tickformat=".0f"), 
             margin=list(l=10,t=10,b=10))
  })
  
  output$plot_forca_partidaria <- renderPlotly({
    req(dados_grafico()); df <- dados_grafico() %>% group_by(SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS)) %>% mutate(p=v/sum(v)*100) %>% arrange(desc(v))
    if(nrow(df)==0) return(empty_plot())
    plot_ly(df, x=~v, y=~reorder(SG_PARTIDO_SIGLA, v), type='bar', orientation='h', text=~paste0(fmt_num(v), " (", round(p,1), "%)"), textposition='auto', marker=list(color=~cores_partidos[SG_PARTIDO_SIGLA])) %>% layout(yaxis=list(title=""), xaxis=list(title="Votos", tickformat=".0f"))
  })
  
  output$plot_treemap <- renderPlotly({
    req(dados_grafico()); df <- dados_grafico() %>% group_by(SG_PARTIDO_SIGLA) %>% summarise(v=sum(QT_VOTOS))
    if(nrow(df)==0) return(empty_plot())
    plot_ly(type="treemap", labels=df$SG_PARTIDO_SIGLA, parents=NA, values=df$v, textinfo="label+value+percent parent", hoverinfo="label+value+percent parent", marker=list(colors=cores_partidos[df$SG_PARTIDO_SIGLA])) %>% layout(margin=list(t=0,l=0,r=0,b=0))
  })
  
  output$stats_abstencao_texto <- renderUI({
    req(dados_grafico()); d <- dados_grafico() %>% distinct(NR_ZONA, NR_SECAO, .keep_all=T)
    if(nrow(d)==0) return(NULL); tot <- sum(d$QT_APTOS, na.rm=T); abs_v <- sum(d$QT_ABSTENCOES, na.rm=T)
    h4(paste0("Abstenção: ", round(abs_v/tot*100,1), "% (", fmt_num(abs_v), ")"), style="color:#e74c3c; text-align:center;")
  })
  output$plot_abstencao_bar <- renderPlotly({
    req(dados_grafico()); d <- dados_grafico() %>% distinct(NR_ZONA, NR_SECAO, .keep_all=T)
    if(nrow(d)==0) return(empty_plot())
    df <- data.frame(T=c("Votos","Abstenção"), Q=c(sum(d$QT_COMPARECIMENTO, na.rm=T), sum(d$QT_ABSTENCOES, na.rm=T))); df$P <- df$Q/sum(df$Q)
    plot_ly(df, x=~Q, y=~T, type='bar', orientation='h', marker=list(color=c("#27ae60","#c0392b")), text=~paste0(fmt_num(Q), " (", round(P*100,1), "%)"), textposition='auto', hoverinfo="none") %>% layout(xaxis=list(showticklabels=F, title=""), yaxis=list(title=""), margin=list(l=80,t=0,b=0))
  })
  
  observeEvent(input$bt_concentracao, {
    req(dados_base()); showModal(modalDialog("Calculando redutos eleitorais...", easyClose=F, footer=NULL))
    tot <- dados_base() %>% group_by(NM_DISPLAY, SG_PARTIDO_SIGLA) %>% summarise(Total=sum(QT_VOTOS), .groups="drop") %>% filter(Total>2000)
    if(nrow(tot)>0) {
      res <- dados_base() %>% filter(NM_DISPLAY %in% tot$NM_DISPLAY) %>% group_by(NM_DISPLAY, SG_PARTIDO_SIGLA, NR_ZONA) %>% summarise(Votos_Z=sum(QT_VOTOS), .groups="drop") %>% 
        left_join(tot, by=c("NM_DISPLAY","SG_PARTIDO_SIGLA")) %>% mutate(P=Votos_Z/Total) %>% filter(P>0.5) %>% arrange(desc(P)) %>% 
        mutate(Conc=paste0(round(P*100,1),"%")) %>% select(Candidato=NM_DISPLAY, Partido=SG_PARTIDO_SIGLA, Zona=NR_ZONA, `Votos na Zona`=Votos_Z, `Total Geral`=Total, `Concentração`=Conc)
    } else { res <- data.frame() }
    removeModal()
    showModal(modalDialog(title=HTML("<i class='fa fa-map-marked-alt'></i> Redutos Eleitorais Detectados"), h4("Candidatos com mais de 50% dos votos concentrados em uma única Zona"), renderDT({ datatable(res, options=list(pageLength=10, scrollX=T)) }), size="l", easyClose=T, footer=modalButton("Fechar")))
  })
  
  output$titulo_lateral <- renderUI({ if(is.null(zona_ativa())) h4("Curitiba - Geral") else tagList(h4(paste("Zona", zona_ativa())), actionButton("bt_reset", "Ver Geral", class="btn-xs btn-default")) })
  observeEvent(input$bt_reset, { zona_ativa(NULL); leafletProxy("mapa") %>% clearMarkers() %>% clearGroup("highlight") %>% setView(-49.2715, -25.44, 11) })
}

shinyApp(ui, server)