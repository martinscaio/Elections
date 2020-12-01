#ELABORAÇÃO DOS MAPAS ELEITORAIS POR ZONAS ELEITORAIS

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


# IMPORTAÇÃO E TRATAMENTO DOS DADOS

sp_20 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2020_SP.csv", sep = ";") %>%
  filter(DS_CARGO == "Prefeito") %>%
  clean_names()

sp_20 <- sp_20 %>%
  filter(nm_municipio == "SÃO PAULO" & nr_turno == "1") %>% #até o momento não aconteceu o segundo turno
  dplyr::select(ano_eleicao,
                nr_turno,
                nr_zona,
                nm_municipio,
                ds_cargo,
                sg_partido,
                nm_urna_candidato,
                qt_votos_nominais) %>%
  arrange(desc(nr_zona)) %>%
  group_by(nr_zona) %>%
  mutate(perc = qt_votos_nominais/sum(qt_votos_nominais)*100)


sp_16 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2016_SP.csv", sep = ";") %>%
  filter(DS_CARGO == "Prefeito") %>%
  clean_names()

sp_16 <- sp_16 %>%
  filter(nm_municipio == "SÃO PAULO" & nr_turno == 1)%>%
  dplyr::select(ano_eleicao,
                nr_turno,
                nr_zona,
                nm_municipio,
                ds_cargo,sg_partido,
                nm_urna_candidato,
                qt_votos_nominais) %>%
  dplyr::arrange(desc(nr_zona)) %>%
  group_by(nr_zona) %>%
  mutate(perc = qt_votos_nominais/sum(qt_votos_nominais)*100)

sp_12 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2012_SP.txt",
                  sep = ";",
                  header = FALSE) %>%
  rename(ano_eleicao = V3,
         nm_municipio = V9,
         nr_turno = V4,
         nm_urna_candidato = V15,
         DS_CARGO = V16,
         SG_PARTIDO = V24,
         QT_VOTOS_NOMINAIS = V29,
         nr_zona = V10) %>%
  filter(DS_CARGO == "PREFEITO" & nm_municipio == "SÃO PAULO" & nr_turno == "1") %>%
  dplyr::select(ano_eleicao,
                nr_zona,
                nm_municipio,
                nm_urna_candidato,
                DS_CARGO,
                nr_turno,
                SG_PARTIDO,
                QT_VOTOS_NOMINAIS) %>%
  clean_names() %>%
  dplyr::arrange(desc(nr_zona)) %>%
  group_by(nr_zona) %>%
  mutate(perc = qt_votos_nominais/sum(qt_votos_nominais)*100)


sp_08 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\votacao_candidato_munzona_2008_SP.txt",
                  sep = ";",
                  header = FALSE) %>%
  rename(ano_eleicao = V3,
         nm_municipio = V9,
         nr_turno = V4,
         nm_urna_candidato = V15,
         DS_CARGO = V16,
         SG_PARTIDO = V24,
         QT_VOTOS_NOMINAIS = V29,
         nr_zona = V10) %>%
  filter(DS_CARGO == "PREFEITO" & nm_municipio == "SÃO PAULO" & nr_turno == "1") %>%
  dplyr::select(ano_eleicao,
                nr_zona, nm_municipio,
                nm_urna_candidato,
                nr_turno,
                DS_CARGO,
                SG_PARTIDO,
                QT_VOTOS_NOMINAIS) %>%
  clean_names() %>%
  dplyr::arrange(desc(nr_zona)) %>%
  group_by(nr_zona) %>%
  mutate(perc = qt_votos_nominais/sum(qt_votos_nominais)*100)


sp_prefeito <- full_join(sp_08, sp_12)

sp_prefeito <- full_join(sp_prefeito, sp_16)

sp_prefeito <- full_join(sp_prefeito, sp_20)

# SHAPE DO MU SP DAS ZONA ELEITORAIS

zonas_elec_sp <- st_read("C:\\Users\\mcaio\\Desktop\\Nova pasta\\ZONAS_FINAL.shp") %>%
  dplyr::rename(nr_zona = ZEFINAL)

sp_prefeito <- left_join(sp_prefeito, zonas_elec_sp)


# GRAFICO PSDB 2020
sp_prefeito %>% filter(ano_eleicao == "2020" & sg_partido == "PSDB") %>%
  ggplot(aes(geometry = geometry, fill = perc))+
  geom_sf()+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")+
  guides(fill = guide_legend(title = "(%) Votos por Zona Eleitoral ", reverse = TRUE))+
  scale_fill_distiller(palette = "Blues", direction = 1)+
  ggtitle("(%) de Votação do PSDB por Zona Eleitoral")+
  annotate("text", x=-46.77, y=-23.63, label="Campo Limpo",
           size=3.45, family = "ITCOfficinaSans LT Book") +
  annotate("text", x=-46.72, y=-23.595, label="Butantã",
           size=3.45, family = "ITCOfficinaSans LT Book")+
  annotate("text", x=-46.67, y=-23.58, label="Jardim Paulista",
           size=3.45, family = "ITCOfficinaSans LT Book")


#GRAFICO PT 2020
sp_prefeito %>% filter(ano_eleicao == "2020" & sg_partido == "PT") %>%
  ggplot(aes(geometry = geometry, fill = perc))+
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
sp_prefeito %>% filter(ano_eleicao == "2016" & sg_partido == "PT") %>%
  ggplot(aes(geometry = geometry, fill = perc))+
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
sp_prefeito %>% filter(ano_eleicao == "2012" & sg_partido == "PT") %>%
  ggplot(aes(geometry = geometry, fill = perc))+
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

#grafico PT 2008
sp_prefeito %>% filter(ano_eleicao == "2008" & sg_partido == "PT") %>%
  ggplot(aes(geometry = geometry, fill = perc))+
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







teeeeste<- sp_prefeito %>% filter(ano_eleicao == "2020" & nr_zona == "371") %>%
  mutate(percentual = (qt_votos_nominais/sum(qt_votos_nominais)*100))

sp_prefeito %>% filter(ano_eleicao == "2020" & nr_zona == "371" & sg_partido == "PT")

sum(teeeeste$percentual)

aaaaah <- teeeeste %>% dplyr::select(nm_urna_candidato, sg_partido, percentual, qt_votos_nominais)

vaaai <- aaaaah %>% filter(sg_partido == "PT") %>%
  group_by(sg_partido, percentual) %>%
  mutate(sum(aaaaah$percentual))

sum(vaaai$n)

grid.arran

PT_08 + PT_16

grid.arrange(pt_16, pt_20, nrow = 2)


votos_validos <- sp_prefeito %>% filter(nr_turno == "1") %>%
  group_by(ano_eleicao, sg_partido) %>%
  summarise(total_votos_validos = sum(qt_votos_nominais)) %>%
  mutate(percentual_valido = total_votos_validos/sum(total_votos_validos)*100) %>%
  mutate(percentual_valido = round(percentual_valido, digits = 2))

# GRAFICO VOTOS VALIDOS PT
votos_validos %>% filter(sg_partido == "PT") %>%
  ggplot(aes(x = factor(ano_eleicao), percentual_valido, group = 1))+
  geom_point()+
  geom_line(size = 0.8)+
  geom_label(aes(label = percentual_valido))+
  labs(x = "Ano", y = "(%) de votos válidos")+
  theme_economist()+
  ggtitle("Percentual de Votos Válidos do PT nas Eleições de Prefeito da Cidade de São Paulo")

#GRAFICO PSDB
votos_validos %>% filter(sg_partido == "PSDB") %>%
  ggplot(aes(x = factor(ano_eleicao), percentual_valido, group = 1))+
  geom_point()+
  geom_line(size = 0.8)+
  geom_label(aes(label = percentual_valido))+
  labs(x = "Ano", y = "(%) de votos válidos")+
  theme_economist()+
  ggtitle("Percentual de Votos Válidos do PSDB nas Eleições de Prefeito da Cidade de São Paulo")




img1 <- image_read("C:\\Users\\mcaio\\Desktop\\Nova pasta\\pt_08_prefeito.png")
img2 <- image_read("C:\\Users\\mcaio\\Desktop\\Nova pasta\\pt_12_prefeito.png")
img3 <- image_read("C:\\Users\\mcaio\\Desktop\\Nova pasta\\pt_16_prefeito.png")

frames <- image_morph(c(img1, img2, img3), frames = 25)
teste <- image_animate(frames, fps = 10)

teste

#partido vencedor por zona eleitoral

partido_vencedor <- sp_prefeito %>% group_by(ano_eleicao, nr_zona) %>%
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



# teste gifs

img1 <- image_read("....\\pt_08_prefeito.png")
img2 <- image_read("....\\pt_12_prefeito.png")
img3 <- image_read("....\\pt_16_prefeito.png")

frames <- image_morph(c(img1, img2, img3), frames = 25)
teste <- image_animate(frames, fps = 10)

teste


