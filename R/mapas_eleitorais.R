
# TODOS OS DADOS FORAM BAIXADOS DO REPOSITÓRIO DE DADOS ELEITORAIS DO TSE.

# PODEM SER ACESSADO AQUI: "https://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais"

# ELABORAÇÃO DOS MAPAS ELEITORAIS POR ZONAS ELEITORAIS EM SÃO PAULO

# NO RESULTADO FINAL ACABOU QUE NÃO USEI TODOS OS PACOTES CARREGADOS

#BIBLIOTECAS
library(gridExtra)
library(ggthemes)
library(tidyverse)
library(ggplot2)
library(sf)
library(rgdal)
library(janitor)
library(maptools)
library(raster)
library(electionsBR)
library(grid)
library(magick)
library(purrr)

# 1 TURNO SÃO PAULO - PREFEITO

# IMPORTAÇÃO E TRATAMENTO DOS DADOS


# Dados

sp_20 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2020_SP.csv", sep = ";")


sp_16 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2016_SP.csv", sep = ";")



sp_12 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2012_SP.txt",
                  sep = ";",
                  header = FALSE)



sp_08 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2008_SP.txt",
                  sep = ";",
                  header = FALSE)




#### FUNCAO PARA TRATAR DADOS 2020 E 2016 ####

limpa_dados <- function(banco_dado){
  dado <- banco_dado %>% filter(DS_CARGO == "Prefeito") %>%
    clean_names() %>%
  filter(nm_municipio == "SÃO PAULO") %>%
    dplyr::select(ano_eleicao,
                  nr_turno,
                  nr_zona,
                  nm_municipio,
                  ds_cargo,
                  sg_partido,
                  nm_urna_candidato,
                  qt_votos_nominais) %>%
    arrange(desc(nr_zona)) %>%
    group_by(nr_zona, nr_turno) %>%
    mutate(percentual = qt_votos_nominais/sum(qt_votos_nominais)*100)

}


#### FUNCAO PARA TRATAR DADOS 2012 E 2008 ####


limpa_dados2 <- function(dados_eleitorais){
  dados_eleitorais <- dados_eleitorais %>%
    dplyr::rename(ano_eleicao = V3,
                  nm_municipio = V9,
                  nr_turno = V4,
                  nm_urna_candidato = V15,
                  DS_CARGO = V16,
                  SG_PARTIDO = V24,
                  QT_VOTOS_NOMINAIS = V29,
                  nr_zona = V10) %>%
    dplyr::filter(DS_CARGO == "PREFEITO" & nm_municipio == "SÃO PAULO") %>%
    dplyr::select(ano_eleicao,
                  nr_zona,
                  nm_municipio,
                  nm_urna_candidato,
                  DS_CARGO,
                  nr_turno,
                  SG_PARTIDO,
                  QT_VOTOS_NOMINAIS) %>%
    janitor::clean_names() %>%
    dplyr::arrange(desc(nr_zona)) %>%
    dplyr::group_by(nr_zona, nr_turno) %>%
    dplyr::mutate(percentual = qt_votos_nominais/sum(qt_votos_nominais)*100)

}



# TRATANDO E JUNTANDO OS DADOS


base1 <- list(sp_20, sp_16) %>%
  map(limpa_dados) %>%
  reduce(full_join)


base2 <- list(sp_12, sp_08) %>%
  map(limpa_dados2) %>%
  reduce(full_join)


sp_prefeito <- full_join(base1, base2)




#### SHAPE DO MUNICIPIO DE SP - ZONA ELEITORAIS ####

zonas_elec_sp <- st_read("C:\\Users\\mcaio\\Desktop\\Nova pasta\\ZONAS_FINAL.shp") %>%
  dplyr::rename(nr_zona = ZEFINAL)

sp_prefeito <- left_join(sp_prefeito, zonas_elec_sp)


#### GRAFICOS DO PRIMEIRO TURNO (1-TURNO) DA ELEIÇÃO PARA PREFEITURA EM 2020 NA CIDADE DE SP ####


# GRAFICO PSDB 2020


sp_prefeito %>% filter(ano_eleicao == "2020" & sg_partido == "PSDB" & nr_turno == "1") %>%
  ggplot(aes(geometry = geometry, fill = percentual))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "(%) Votos por Zona Eleitoral ", reverse = TRUE))+
  scale_fill_distiller(palette = "Blues", direction = 1)+
  ggtitle("(%) de Votação do PSDB por Zona Eleitoral na Eleição pra Prefeito de 2020")+
  annotate("text", x=-46.77, y=-23.63, label="Campo Limpo",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="Butantã",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.67, y=-23.58, label="Jardim Paulista",
           size=3.45, family = "ITCOfficinaSans LT Book")




#GRAFICO PT 2020


sp_prefeito %>% filter(ano_eleicao == "2020" & sg_partido == "PT" & nr_turno == "1") %>%
  ggplot(aes(geometry = geometry, fill = percentual))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "(%) VOTOS POR ZONA ELEITORAL ",
                             label.position = "left", reverse = TRUE))+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  ggtitle("(%) de Votação do PT por Zona Eleitoral na Eleição pra Prefeito de 2020")+
  annotate("text", x=-46.77, y=-23.63, label="CAMPO LIMPO",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="BUTANTÃ",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.7, y=-23.85, label="PARELHEIROS",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.67, y=-23.746, label="GRAJAÚ",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.4, y=-23.605, label="CIDADE TIRADENTES",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "Black")



# GRAFICO PT 2016


sp_prefeito %>% filter(ano_eleicao == "2016" & sg_partido == "PT" & nr_turno == "1") %>%
  ggplot(aes(geometry = geometry, fill = percentual))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "(%) VOTOS POR ZONA ELEITORAL ",
                             label.position = "left", reverse = TRUE))+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  ggtitle("(%) de Votação do PT por Zona Eleitoral na Eleição pra Prefeito de 2016")+
  annotate("text", x=-46.77, y=-23.63, label="CAMPO LIMPO",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="BUTANTÃ",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.7, y=-23.85, label="PARELHEIROS",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.67, y=-23.746, label="GRAJAÚ",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.4, y=-23.605, label="CIDADE TIRADENTES",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "Black")



# GRAFICO PT 2012


sp_prefeito %>% filter(ano_eleicao == "2012" & sg_partido == "PT" & nr_turno == "1") %>%
  ggplot(aes(geometry = geometry, fill = percentual))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "(%) VOTOS POR ZONA ELEITORAL ",
                             label.position = "left", reverse = TRUE))+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  ggtitle("(%) de Votação do PT por Zona Eleitoral na Eleição pra Prefeito de 2012")+
  annotate("text", x=-46.77, y=-23.63, label="CAMPO LIMPO",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="BUTANTÃ",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.7, y=-23.85, label="PARELHEIROS",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.67, y=-23.746, label="GRAJAÚ",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.4, y=-23.605, label="CIDADE TIRADENTES",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "Black")




# GRAFICO PT 2008

sp_prefeito %>% filter(ano_eleicao == "2008" & sg_partido == "PT" & nr_turno == "1") %>%
  ggplot(aes(geometry = geometry, fill = percentual))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(vjust = 48.5))+
  guides(fill = guide_legend(title = "(%) VOTOS POR ZONA ELEITORAL ",
                             label.position = "left", reverse = TRUE))+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  ggtitle("(%) de Votação do PT por Zona Eleitoral na Eleição Pra Prefeito de 2008")+
  annotate("text", x=-46.77, y=-23.63, label="CAMPO LIMPO",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="BUTANTÃ",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.7, y=-23.85, label="PARELHEIROS",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.67, y=-23.746, label="GRAJAÚ",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.4, y=-23.605, label="CIDADE TIRADENTES",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "Black")+
  labs(caption = "AUTOR: CAIO MARTINS")


#### VOTOS VALIDOS ####

votos_validos <- sp_prefeito %>% filter(nr_turno == "1") %>%
  group_by(ano_eleicao, sg_partido) %>%
  summarise(total_votos_validos = sum(qt_votos_nominais)) %>%
  mutate(percentual_valido = total_votos_validos/sum(total_votos_validos)*100) %>%
  mutate(percentual_valido = round(percentual_valido, digits = 2))



#-------------------------------------------------------------------------------------------------

# REFAZER GRAFICOS DOS VOTOS VALIDOS. TÁ MUITO FEIO


# GRAFICO VOTOS VALIDOS PT


#votos_validos %>% filter(sg_partido == "PT") %>%
#  ggplot(aes(x = factor(ano_eleicao), percentual_valido, group = 1))+
#  geom_point()+
#  geom_line(size = 0.8)+
#  geom_label(aes(label = percentual_valido))+
#  labs(x = "Ano", y = "(%) de votos válidos")+
#  theme_economist()+
#  ggtitle("Percentual de Votos Válidos do PT nas Eleições de Prefeito da Cidade de São Paulo")



#GRAFICO PSDB


#votos_validos %>% filter(sg_partido == "PSDB") %>%
#  ggplot(aes(x = factor(ano_eleicao), percentual_valido, group = 1))+
#  geom_point()+
#  geom_line(size = 0.8)+
#  geom_label(aes(label = percentual_valido))+
#  labs(x = "Ano", y = "(%) de votos válidos")+
#  theme_economist()+
#  ggtitle("Percentual de Votos Válidos do PSDB nas Eleições de Prefeito da Cidade de São Paulo")


#-------------------------------------------------------------------------------------------------



#### PARTIDO VENCEDOR POR ZONA ELEITORAL PRIMEIRO TURNO ####

partido_vencedor <- sp_prefeito %>%
  filter(nr_turno == "1") %>%
  group_by(ano_eleicao, nr_zona) %>%
  slice(which.max(qt_votos_nominais)) %>%
  dplyr::select(ano_eleicao,
                nr_zona,
                qt_votos_nominais,
                sg_partido,
                nm_urna_candidato,
                Shape_Leng,
                Shape_Area,
                geometry,
                OBJECTID,
                FIRST_sede,
                FIRST_NOME) %>%
  mutate(partido_vencedor = sg_partido)


#mapa do partido vencedor por zona eleitoral


partido_vencedor %>% filter(ano_eleicao == "2020") %>%
  ggplot(aes(geometry = geometry, fill = partido_vencedor)) +
  geom_sf() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "Partido", reverse = FALSE))+
  scale_fill_manual(values = c("#009999","#ff6633", "#0040ff", "#e60000"))+
  ggtitle("Partidos que ganharam as zonas eleitorais")

#_________________________________________________________________________________________________________________


#### SEGUNDO TURNO (2 TURNO) SÃO PAULO ####


# GRAFICOS DO SEGUNDO TURNO DA ELEIÇÃO PARA PREFEITURA EM 2020 NA CIDADE DE SP


# GRAFICO PSDB 2020


sp_prefeito %>% filter(ano_eleicao == "2020" & sg_partido == "PSDB" & nr_turno == "2") %>%
  ggplot(aes(geometry = geometry, fill = percentual))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "(%) Votos por Zona Eleitoral", reverse = TRUE))+
  scale_fill_distiller(palette = "Blues", direction = 1)+
  ggtitle("(%) de Votação do PSDB por Zona Eleitoral no 2º turno")+
  annotate("text", x=-46.77, y=-23.63, label="Campo Limpo",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="Butantã",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.67, y=-23.58, label="Jardim Paulista",
           size=3.45, family = "ITCOfficinaSans LT Book")


# GRAFICO PSOL 2020


sp_prefeito %>% filter(ano_eleicao == "2020" & sg_partido == "PSOL" & nr_turno == "2") %>%
  ggplot(aes(geometry = geometry, fill = percentual))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "(%) VOTOS POR ZONA ELEITORAL ",
                             label.position = "left", reverse = TRUE))+
  scale_fill_distiller(palette = "Yellow", direction = 1)+
  ggtitle("(%) de Votação do PSOL por Zona Eleitoral na Eleição pra Prefeito de 2020")+
  annotate("text", x=-46.77, y=-23.63, label="CAMPO LIMPO",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="BUTANTÃ",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.7, y=-23.85, label="PARELHEIROS",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.67, y=-23.746, label="GRAJAÚ",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "White")+
  annotate("text", x=-46.4, y=-23.605, label="CIDADE TIRADENTES",
           size=3.45, family = "ITCOfficinaSans LT Book", colour = "Black")


# PARTIDO VENCEDOR POR ZONA ELEITORAL NO 2 TURNO

partido_vencedor_2 <- sp_prefeito %>%
  filter(nr_turno == "2") %>%
  group_by(ano_eleicao, nr_zona) %>%
  slice(which.max(qt_votos_nominais)) %>%
  dplyr::select(ano_eleicao,
                nr_zona,
                qt_votos_nominais,
                sg_partido,
                nm_urna_candidato,
                Shape_Leng,
                Shape_Area,
                geometry,
                OBJECTID,
                FIRST_sede,
                FIRST_NOME) %>%
  mutate(partido_vencedor = sg_partido)


# MAPA DO PARTIDO VENCEDOR POR ZONA ELEITORAL NO 2 TURNO


partido_vencedor_2 %>% filter(ano_eleicao == "2020") %>%
  ggplot(aes(geometry = geometry, fill = partido_vencedor)) +
  geom_sf() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "Partido", reverse = FALSE))+
  scale_fill_manual(values = c("#009999","#ff6633", "#0040ff", "#e60000"))+
  ggtitle("Partidos que ganharam as zonas eleitorais")



