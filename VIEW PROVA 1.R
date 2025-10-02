# =================================================================
# PASSO 0: CARREGAR PACOTES E LER O ARQUIVO CORRETAMENTE
# =================================================================
library(tidyverse)
library(stringr)
library(ggwordcloud) # Carregar o novo pacote
library(treemapify)
library(wordcloud2)


dados_curitiba <- read_csv("Dados_curitiba.csv", locale = locale(encoding = "UTF-8"))

#dicionário partidos
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

# =================================================================
# PASSO 2: PREPARAR OS DADOS (sem alteração)
# =================================================================
votos_vereadores <- dados_curitiba %>% 
  filter(
    !NM_VOTAVEL %in% c("Branco", "Nulo"),
    DS_CARGO_PERGUNTA == "Vereador"   # 🔑 garante só vereador
  ) %>% 
  mutate(NM_PARTIDO = str_to_upper(str_trim(NM_PARTIDO))) %>% 
  group_by(NM_VOTAVEL, NM_PARTIDO) %>% 
  summarise(total_votos = sum(QT_VOTOS), .groups = "drop") %>% 
  left_join(tabela_partidos, by = "NM_PARTIDO") %>% 
  arrange(desc(total_votos))



# =================================================================
# PASSO 3: PALETA DE CORES (sem alteração)
# =================================================================
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

# =================================================================
# PASSO 4: GERAR A NUVEM DE PALAVRAS COM GGPLOT2
# =================================================================
votos_top <- votos_vereadores   # pega top 77

ggplot(
  votos_top,
  aes(label = NM_VOTAVEL, size = total_votos, color = SG_PARTIDO_SIGLA)
) +
  geom_text_wordcloud(area_corr = 1, rm_outside = TRUE) +
  scale_size_area(max_size = 15) +
  scale_color_manual(values = cores_partidos) +
  theme_minimal() +
  labs(color = "Partido")
#geom_text_wordcloud(area_corr = 0.95, rm_outside = TRUE)
#scale_size_area(max_size = 15)



# =================================================================
# PASSO 5: AGREGAR VOTOS POR PARTIDO
# =================================================================
 # Carregar o pacote para o gráfico

votos_por_partido <- votos_vereadores %>%
  # Garantir que temos siglas válidas para agrupar
  filter(!is.na(SG_PARTIDO_SIGLA)) %>%
  
  # Agrupar pela sigla do partido
  group_by(SG_PARTIDO_SIGLA) %>%
  
  # Somar os votos de todos os candidatos de cada partido
  summarise(total_votos_partido = sum(total_votos)) %>%
  
  # Opcional: ordenar para visualização em tabelas
  arrange(desc(total_votos_partido))

# Inspecionar a nova tabela agregada
print(head(votos_por_partido))


# =================================================================
# PASSO 6: GERAR O GRÁFICO DE MOSAICO (TREEMAP)
# =================================================================

ggplot(
  data = votos_por_partido, 
  aes(
    area = total_votos_partido,  # A área de cada retângulo é o total de votos
    fill = SG_PARTIDO_SIGLA,     # A cor de cada retângulo é o partido
    label = SG_PARTIDO_SIGLA     # O texto dentro de cada retângulo é a sigla
  )) +
  geom_treemap() +               # Cria os retângulos do treemap
  geom_treemap_text(             # Adiciona o texto (siglas) dentro dos retângulos
    color = "white",             # Cor do texto
    place = "centre",            # Posição do texto
    fontface = "bold",           # Deixa o texto em negrito
    size = 14                    # Tamanho base da fonte
  ) +
  scale_fill_manual(values = cores_partidos) + # Usa a MESMA paleta de cores da nuvem!
  theme_minimal() +
  theme(legend.position = "none") + # Remove a legenda, pois os nomes já estão no gráfico
  labs(
    title = "Distribuição de Votos para Vereador por Partido",
    subtitle = "Curitiba - O tamanho de cada área é proporcional ao total de votos do partido",
    caption = "Fonte: Dados do TSE"
  )


# Você precisará agregar seus dados por partido e por zona
dados_heatmap <- dados_curitiba %>%
  filter(DS_CARGO_PERGUNTA == "Vereador") %>%
  group_by(NM_PARTIDO, NR_ZONA) %>%
  summarise(votos = sum(QT_VOTOS)) %>%
  group_by(NR_ZONA) %>%
  mutate(percentual_votos = (votos / sum(votos)) * 100)

# Use os mesmos 'dados_heatmap' da alternativa anterior
ggplot(dados_heatmap, aes(x = as.factor(NR_ZONA), y = votos)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  facet_wrap(~ NM_PARTIDO, ncol = 3) + # Magia acontece aqui!
  labs(title = "Distribuição de Votos por Zona para Cada Partido",
       x = "Zona Eleitoral", y = "Total de Votos") +
  theme_bw()

# O seu código original para criar o gráfico
ggplot(dados_heatmap, aes(x = as.factor(NR_ZONA), y = NM_PARTIDO, fill = percentual_votos)) +
  geom_tile(color = "white") + # geom_tile cria o heatmap
  
  # --- SUBSTITUA A LINHA DE COR ANTIGA POR ESTA ---
  # Define a escala de cores indo de branco, para amarelo, para preto
  scale_fill_gradientn(colors = c("yellow", "black")) +
  
  labs(title = "Concentração de Votos por Partido e Zona Eleitoral",
       x = "Zona Eleitoral", y = "Partido", fill = "% de Votos na Zona") +
  theme_minimal()


