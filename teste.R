# --- Bibliotecas ---
library(wordcloud2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(patchwork)
library(purrr)
library(circlize)
library(showtext)

# --- Fonte e cores ---
paleta_cores <- list(preto="#000000", amarelo="#FFDD00", verde="#8DC63F", roxo="#92278F", vermelho="#ED1C24")
font_add(family="Axiforma", regular="Axiforma-Black.ttf")
showtext_auto()

# --- Dados ---
dados_curitiba <- read_csv("https://raw.githubusercontent.com/Mateus-Madeira/Visualizacao-de-dados/main/Dados_curitiba.csv",
                           locale = locale(encoding = "UTF-8"))
dados_secao <- dados_vereador %>%
  group_by(NR_ZONA, NR_SECAO) %>%
  summarise(comparecimento=first(QT_COMPARECIMENTO),
            abstencao=first(QT_ABSTENCOES),
            .groups="drop")

prop_geral <- sum(dados_secao$comparecimento) / 
  (sum(dados_secao$comparecimento) + sum(dados_secao$abstencao))

# --- Gráfico geral ---
p_geral <- dados_secao %>%
  summarise(comparecimento=sum(comparecimento), abstencao=sum(abstencao)) %>%
  tidyr::pivot_longer(cols=c(comparecimento, abstencao)) %>%
  ggplot(aes(y="Total Curitiba", x=value, fill=name)) +
  geom_col(position="fill") +
  geom_text(aes(label=scales::percent(value/sum(value), accuracy=0.1)),
            position=position_fill(vjust=0.5),
            color="white", fontface="bold", family="Axiforma") +
  scale_fill_manual(values=c(comparecimento=paleta_cores$verde,
                             abstencao=paleta_cores$vermelho)) +
  coord_cartesian(xlim=c(0,1)) +
  theme_minimal(base_family="Axiforma") +
  theme(
    panel.background=element_rect(fill="transparent", color=NA),
    plot.background=element_rect(fill="transparent", color=NA),
    legend.position="none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

plots_zonas <- purrr::map(zonas, ~{
  dz <- dados_secao %>%
    filter(NR_ZONA==.x) %>%
    summarise(comparecimento=sum(comparecimento),
              abstencao=sum(abstencao)) %>%
    tidyr::pivot_longer(cols=c(comparecimento, abstencao))
  
  ggplot(dz, aes(y=paste("Zona",.x), x=value, fill=name)) +
    geom_col(position="fill") +
    geom_text(aes(label=scales::percent(value/sum(value), accuracy=0.1)),
              position=position_fill(vjust=0.5),
              color="white", size=3.5, family="Axiforma") +
    geom_vline(xintercept=prop_geral, linetype="dashed", color="black") +
    scale_fill_manual(values=c(comparecimento=paleta_cores$verde,
                               abstencao=paleta_cores$vermelho)) +
    coord_cartesian(xlim=c(0,1)) +
    theme_minimal(base_family="Axiforma") +
    theme(
      panel.background=element_rect(fill="transparent", color=NA),
      plot.background=element_rect(fill="transparent", color=NA),
      legend.position="none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
})
wrap_plots(c(list(p_geral), plots_zonas), ncol=1) +
  plot_annotation(
    title="Comparecimento vs Abstenções em Curitiba",
    subtitle="Geral e por zona (linha tracejada = média geral)",
    theme=theme(
      plot.background=element_rect(fill="#a6b8eb", color=NA),
      plot.title=element_text(face="bold", size=14, family="Axiforma")
    )
  )
# --- Gráfico 2: Top 5 Prefeito ---
cores_cand <- c("CRISTINA GRAEML"="#8B0000","EDUARDO PIMENTEL"="#cccc00","LUCIANO DUCCI"="#FF4500",
                "LUIZÃO GOULART"="#FF8C00","NEY LEPREVOST"="#4682B4","Brancos/Nulos"="#A9A9A9")

top5 <- dados_prefeito %>%
  filter(!NM_VOTAVEL %in% c("Branco","Nulo"), !is.na(NM_VOTAVEL)) %>%
  group_by(NM_VOTAVEL) %>%
  summarise(votos=sum(QT_VOTOS), .groups="drop") %>%
  arrange(desc(votos)) %>%
  slice_head(n=5)

ggplot(top5, aes(x=reorder(NM_VOTAVEL,votos), y=votos, fill=NM_VOTAVEL)) +
  geom_col(show.legend=FALSE) + coord_flip() +
  geom_text(aes(label=scales::number(votos, big.mark=".", decimal.mark=",")),
            hjust=1.1, color="white", family="Axiforma") +
  scale_fill_manual(values=cores_cand) +
  theme_minimal() +
  theme(plot.background=element_rect(fill="#a6b8eb", color=NA))

# --- Gráfico 3: Heatmap Vereadores ---
dados_vereador %>%
  filter(!is.na(NM_PARTIDO), NM_PARTIDO!="#NULO#") %>%
  group_by(NR_ZONA, NM_PARTIDO) %>%
  summarise(votos=sum(QT_VOTOS), .groups="drop") %>%
  ggplot(aes(x=factor(NR_ZONA), y=NM_PARTIDO, fill=votos)) +
  geom_tile() +
  scale_fill_gradient(low="#dbeabc", high="#0038d7") +
  theme_minimal() +
  theme(plot.background=element_rect(fill="#a6b8eb", color=NA))

# --- Gráfico 4: Nuvem de Palavras Vereadores ---
votos_candidatos <- dados_vereador %>% 
  filter(!is.na(NM_VOTAVEL), 
         NM_VOTAVEL != "Branco",
         NM_VOTAVEL != "Nulo") %>% 
  group_by(NM_VOTAVEL) %>% 
  summarise(
    total_votos = sum(QT_VOTOS,
                      na.rm = TRUE),
    .groups = "drop") %>% 
  arrange(desc(total_votos)) 
votos_candidatos <- votos_candidatos %>% 
  mutate(cor = case_when( 
    total_votos >= 10000 ~ "#0038d7",
    total_votos >= 5000 ~ "#5b8700",
    total_votos >= 2000 ~ "black",
    TRUE ~ "grey" )) 
SG_PARTIDO_SIGLA = c( "PSD", "PMB", "PSB", "UNIÃO", "SOLIDARIEDADE", "REPUBLICANOS", "PP", "MOBILIZA", "NOVO",
                      "PODE", "PDT", "PT", "PSOL", "PL", "MDB", "PRD","REDE", "AGIR", "PSDB", "PCdoB", "PV", 
                      "PRTB", "CIDADANIA", "DC", "PSTU", "PCO" )
votos_filtrados <- votos_candidatos[
  !votos_candidatos$NM_VOTAVEL %in% SG_PARTIDO_SIGLA, 
  ]
wordcloud2(
  votos_filtrados[, c("NM_VOTAVEL", "total_votos")],
  size = 0.2, 
  color = votos_filtrados$cor,
  backgroundColor = "#a6b8eb", 
  fontFamily = "Axiforma")

# --- Gráfico 5: Cordas Prefeito ---
dados_mod <- dados_prefeito %>%
  filter(!is.na(NR_ZONA)) %>%
  mutate(cand=ifelse(NM_VOTAVEL %in% c("Branco","Nulo"), "Brancos/Nulos", NM_VOTAVEL))
top_cand <- dados_mod %>%
  filter(cand!="Brancos/Nulos") %>%
  group_by(cand) %>%
  summarise(votos=sum(QT_VOTOS)) %>%
  slice_max(votos, n=5) %>% pull(cand)
catg <- c(top_cand,"Brancos/Nulos")
dados_cordas <- dados_mod %>%
  filter(cand %in% catg) %>%
  group_by(NR_ZONA, cand) %>%
  summarise(votos=sum(QT_VOTOS)) %>%
  rename(from=NR_ZONA, to=cand, value=votos)
cores_zonas <- setNames(rep("#D3D3D3", length(unique(dados_cordas$from))), unique(dados_cordas$from))
par(bg="#a6b8eb")
circos.clear()
chordDiagram(dados_cordas, grid.col=c(cores_zonas, cores_cand),
             col=cores_cand[as.character(dados_cordas$to)],
             directional=0, annotationTrack="grid",
             preAllocateTracks=list(track.height=0.4))
circos.track(track.index=1, panel.fun=function(x,y){
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing="clockwise", niceFacing=TRUE, adj=c(0,0.5), cex=0.8, family="Axiforma")
}, bg.border=NA)
title("Fluxo de Votos por Zona - Prefeito de Curitiba", family="Axiforma")

# --- Gráfico 6: Top 10 Partidos (Vereadores) ---
dados_vereador %>%
  filter(!is.na(NM_PARTIDO), NM_PARTIDO != "#NULO#") %>%
  group_by(NM_PARTIDO) %>%
  summarise(votos = sum(QT_VOTOS)) %>%
  arrange(desc(votos)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(NM_PARTIDO, votos), y = votos, fill = votos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = scales::number(votos, big.mark = ".", decimal.mark = ",")),
            hjust = 1.1, color = "white", family = "Axiforma") +
  scale_fill_gradient(low = "#5b8700", high = "#0038d7") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#a6b8eb", color = NA))
