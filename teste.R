# --- Carregando as bibliotecas ---
library(wordcloud2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr) 
library(patchwork)
library(purrr)
library(circlize)
library(showtext)

# --- Definição da nova paleta de cores ---
paleta_cores <- list(
  preto    = "#000000",
  amarelo  = "#FFDD00",
  verde    = "#8DC63F",
  roxo     = "#92278F",
  vermelho = "#ED1C24"
)
font_add(family = "Axiforma", regular = "Axiforma-Black.ttf")
showtext_auto()

# --- Leitura e preparação dos dados ---
dados_curitiba <- read_csv("Dados_curitiba.csv", locale = locale(encoding = "UTF-8"))

dados_vereador <- dados_curitiba %>%
  filter(DS_CARGO_PERGUNTA == "Vereador")
dados_prefeito <- dados_curitiba %>%
  filter(DS_CARGO_PERGUNTA == "Prefeito")

dados_secao <- dados_vereador %>%
  group_by(NR_ZONA, NR_SECAO) %>%
  summarise(
    aptos = first(QT_APTOS),
    comparecimento = first(QT_COMPARECIMENTO),
    abstencao = first(QT_ABSTENCOES),  
    .groups = "drop"
  )

# --- GRÁFICO 1: Comparecimento vs Abstenções ---
proporcao_comparecimento_geral <- dados_secao %>%
  summarise(
    total_comparecimento = sum(comparecimento),
    total_geral = sum(comparecimento) + sum(abstencao)
  ) %>%
  mutate(proporcao_comparecimento = total_comparecimento / total_geral) %>%
  pull(proporcao_comparecimento)

# --- Gráfico Geral (Total Curitiba) ---
p_geral <- dados_secao %>%
  summarise(
    comparecimento = sum(comparecimento),
    abstencao = sum(abstencao)
  ) %>%
  pivot_longer(cols = c(comparecimento, abstencao), names_to = "tipo", values_to = "qtd") %>%
  ggplot(aes(y = "Total Curitiba", x = qtd, fill = tipo)) +
  geom_col(position = "fill", width = 0.95) +
  geom_text(
    aes(label = scales::percent(qtd / sum(qtd), accuracy = 0.1)),
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 5,
    family = "Axiforma"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("comparecimento" = paleta_cores$verde, "abstencao" = paleta_cores$vermelho),
    labels = c("comparecimento" = "Comparecimento", "abstencao" = "Abstenção")
  ) +
  labs(y = NULL, x = NULL, fill = "Tipo") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10, face = "bold", family = "Axiforma"),
    axis.text.x = element_text(family = "Axiforma"),
    legend.text = element_text(family = "Axiforma"),
    legend.title = element_text(family = "Axiforma")
  )

# --- Gráficos por Zona Eleitoral ---
zonas_unicas <- unique(dados_secao$NR_ZONA)

lista_plots_zonas <- map(zonas_unicas, ~{
  
  dados_zona_atual <- dados_secao %>%
    filter(NR_ZONA == .x) %>%
    summarise(
      comparecimento = sum(comparecimento),
      abstencao = sum(abstencao)
    ) %>%
    pivot_longer(cols = c(comparecimento, abstencao), names_to = "tipo", values_to = "qtd")
  
  ggplot(dados_zona_atual, aes(y = paste("Zona", .x), x = qtd, fill = tipo)) +
    geom_col(position = "fill", width = 0.95) +
    geom_text(
      aes(label = scales::percent(qtd / sum(qtd), accuracy = 0.1)),
      position = position_fill(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 3.5,
      family = "Axiforma"
    ) +
    geom_vline(
      xintercept = proporcao_comparecimento_geral,
      linetype = "dashed",
      color = "black",
      size = 1
    ) +
    scale_x_continuous(labels = scales::percent) +
    scale_fill_manual(
      values = c("comparecimento" = paleta_cores$verde, "abstencao" = paleta_cores$vermelho),
      labels = c("comparecimento" = "Comparecimento", "abstencao" = "Abstenção")
    ) +
    labs(y = NULL, x = NULL) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10, family = "Axiforma"),
      axis.text.x = element_text(family = "Axiforma"),
      plot.title = element_text(family = "Axiforma", face = "bold"),
      legend.text = element_text(family = "Axiforma"),
      legend.title = element_text(family = "Axiforma")
    )
})

# --- Junção Final do Gráfico 1 (com fundo alterado) ---
todos_os_plots <- c(list(p_geral), lista_plots_zonas)

grafico_final <- wrap_plots(todos_os_plots, ncol = 1, guides = 'collect') +
  plot_annotation(
    title = 'Comparecimento vs Abstenções em Curitiba',
    subtitle = 'Análise geral e por zona eleitoral (linha tracejada indica a média geral)',
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", family = "Axiforma"),
      plot.subtitle = element_text(size = 12, family = "Axiforma"),
      # --- ALTERAÇÃO 1: COR DE FUNDO DO GRÁFICO 1 ---
      plot.background = element_rect(fill = "#a6b8eb", color = NA)
    )
  ) &
  # Aplica a cor a todos os sub-gráficos e legenda
  theme(
    legend.position = 'right',
    plot.background = element_rect(fill = "#a6b8eb", color = NA),
    panel.background = element_rect(fill = "#a6b8eb", color = NA),
    legend.background = element_rect(fill = "#a6b8eb", color = NA)
  )

grafico_final

# --- GRÁFICO 2: Top 5 Candidatos (Prefeito) ---
dados_prefeito %>%
  filter(!is.na(NM_VOTAVEL), NM_VOTAVEL != "Branco", NM_VOTAVEL != "Nulo") %>%
  group_by(NM_VOTAVEL) %>%
  summarise(total_votos = sum(QT_VOTOS, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_votos)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(NM_VOTAVEL, total_votos), y = total_votos, fill = total_votos)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = scales::number(total_votos, big.mark = ".", decimal.mark = ",")),
    color = "white",
    fontface = "bold",
    hjust = 1.1,
    family = "Axiforma"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#5b8700",  
    high = "#0038d7"
  ) +
  # --- LINHA ADICIONADA PARA FORMATAR O EIXO ---
  scale_y_continuous(labels = scales::number) +
  # ----------------------------------------------
labs(
  title = "Top 5 candidatos mais votados (Prefeito)",
  x = "Candidato",
  y = "Total de Votos"
) +
  theme_minimal() +
  theme(
    text = element_text(family = "Axiforma"),
    plot.title = element_text(face = "bold", size = 12),
    plot.margin = unit(c(1, 1, 1, 1.5), "cm"),
    plot.background = element_rect(fill = "#a6b8eb", color = NA),
    panel.background = element_rect(fill = "#a6b8eb", color = NA)
  )

# --- GRÁFICO 3: Heatmap (com fundo alterado) ---
dados_vereador %>%
  filter(!is.na(NM_PARTIDO), NM_PARTIDO != "#NULO#") %>%
  group_by(NR_ZONA, NM_PARTIDO) %>%
  summarise(total_votos = sum(QT_VOTOS, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = as.factor(NR_ZONA), y = NM_PARTIDO, fill = total_votos)) +
  geom_tile() +
  scale_fill_gradient(low = "#dbeabc", high = "#0038d7") +
  labs(title = "Distribuição de votos Vereadores por Zona Eleitoral e Partido",
       x = "Zona Eleitoral", y = NULL, fill = "Votos") +
  theme_minimal() +
  theme(
    text = element_text(family = "Axiforma"),
    plot.margin = unit(c(1, 1, 1, 1.5), "cm"),
    # --- ALTERAÇÃO 3: COR DE FUNDO DO GRÁFICO 3 ---
    plot.background = element_rect(fill = "#a6b8eb", color = NA),
    panel.background = element_rect(fill = "#a6b8eb", color = NA),
    legend.background = element_rect(fill = "#a6b8eb", color = NA)
  )

# --- GRÁFICO 4: Nuvem de Palavras (com fundo alterado) ---
votos_candidatos <- dados_vereador %>%
  filter(!is.na(NM_VOTAVEL), NM_VOTAVEL != "Branco", NM_VOTAVEL != "Nulo") %>%
  group_by(NM_VOTAVEL) %>%
  summarise(total_votos = sum(QT_VOTOS, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_votos))

votos_candidatos <- votos_candidatos %>%
  mutate(cor = case_when(
    total_votos >= 10000 ~ "#0038d7",
    total_votos >= 5000  ~ "#5b8700",
    total_votos >= 2000  ~ "black",
    TRUE                ~ "grey"
  ))

SG_PARTIDO_SIGLA = c(
  "PSD", "PMB", "PSB", "UNIÃO", "SOLIDARIEDADE", "REPUBLICANOS", "PP", "MOBILIZA",
  "NOVO", "PODE", "PDT", "PT", "PSOL", "PL", "MDB", "PRD", "REDE", "AGIR",
  "PSDB", "PCdoB", "PV", "PRTB", "CIDADANIA", "DC", "PSTU", "PCO"
)
votos_filtrados <- votos_candidatos[!votos_candidatos$NM_VOTAVEL %in% SG_PARTIDO_SIGLA, ]

font_add(family = "Axiforma", regular = "Axiforma-Heavy.ttf")
showtext_auto()

wordcloud2(votos_filtrados[, c("NM_VOTAVEL", "total_votos")],
           size = 0.2,
           color = votos_filtrados$cor,
           # --- ALTERAÇÃO 4: COR DE FUNDO DO GRÁFICO 4 ---
           backgroundColor = "#a6b8eb",
           fontFamily = "Axiforma")

# --- GRÁFICO 5: Gráfico de Cordas (com fundo alterado) ---
# Preparação dos dados
dados_modificados <- dados_prefeito %>%
  filter(!is.na(NR_ZONA)) %>%
  mutate(NM_VOTAVEL_AJUSTADO = case_when(
    NM_VOTAVEL %in% c("Branco", "Nulo") ~ "Brancos/Nulos",
    TRUE ~ NM_VOTAVEL
  ))

top_candidatos <- dados_modificados %>%
  filter(!is.na(NM_VOTAVEL_AJUSTADO), NM_VOTAVEL_AJUSTADO != "Brancos/Nulos") %>%
  group_by(NM_VOTAVEL_AJUSTADO) %>%
  summarise(total_votos_candidato = sum(QT_VOTOS, na.rm = TRUE)) %>%
  arrange(desc(total_votos_candidato)) %>%
  slice_head(n = 5) %>%
  pull(NM_VOTAVEL_AJUSTADO)

categorias_grafico <- c(top_candidatos, "Brancos/Nulos")

dados_grafico_cordas <- dados_modificados %>%
  filter(NM_VOTAVEL_AJUSTADO %in% categorias_grafico) %>%
  group_by(NR_ZONA, NM_VOTAVEL_AJUSTADO) %>%
  summarise(votos = sum(QT_VOTOS, na.rm = TRUE), .groups = "drop") %>%
  rename(from = NR_ZONA, to = NM_VOTAVEL_AJUSTADO, value = votos) %>%
  mutate(from = as.factor(from), to = as.factor(to))

# Definição de cores para o gráfico
cores_candidatos <- c(
  "CRISTINA GRAEML" = "#8B0000",
  "EDUARDO PIMENTEL" = "#cccc00",
  "LUCIANO DUCCI" = "#FF4500",
  "LUIZÃO GOULART" = "#FF8C00",
  "NEY LEPREVOST" = "#4682B4",
  "Brancos/Nulos" = "#A9A9A9"
)

zonas_no_grafico <- unique(dados_grafico_cordas$from)
cores_zonas <- rep("#D3D3D3", length(zonas_no_grafico))
names(cores_zonas) <- zonas_no_grafico
grid_cores <- c(cores_zonas, cores_candidatos)

# --- ALTERAÇÃO 5: COR DE FUNDO DO GRÁFICO 5 ---
# Define a cor de fundo para a janela de plotagem ANTES de criar o gráfico
par(bg = "#a6b8eb")
# ---------------------------------------------

# Geração do gráfico
circos.clear()
chordDiagram(
  x = dados_grafico_cordas,
  grid.col = grid_cores,
  col = cores_candidatos[as.character(dados_grafico_cordas$to)],
  directional = 0,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = 0.4)
)

circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      x = CELL_META$xcenter,
      y = CELL_META$ylim[1],
      labels = CELL_META$sector.index,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.8,
      family = "Axiforma"
    )
  },
  bg.border = NA
)

title(
  "Fluxo de Votos por Zona para Prefeito de Curitiba",
  family = "Axiforma"
)

circos.clear()
