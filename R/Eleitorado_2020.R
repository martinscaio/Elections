

# EXPLORAÇÃO INICIAL DOS DADOS SOBRE O ELEITORADO BRASILEIRO DE 2020

# ESTÁ INCOMPLETO. FALTA ATUALIZAR E ARRUMAR OS GRÁFICOS. DEIXÁ-LOS BONITOS


library(tidyverse)
library(ggplot2)
library(skimr)
library(ggthemes)


eleitorado_2020 <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\perfil_eleitorado_2020.csv",
                            sep = ";")

eleitorado_2020$DT_GERACAO <- NULL
eleitorado_2020$HH_GERACAO <- NULL
eleitorado_2020$CD_GENERO <- NULL
eleitorado_2020$ANO_ELEICAO <- NULL
eleitorado_2020$CD_MUN_SIT_BIOMETRIA <- NULL
eleitorado_2020$CD_GRAU_ESCOLARIDADE <- NULL
eleitorado_2020$CD_FAIXA_ETARIA <- NULL
eleitorado_2020$CD_ESTADO_CIVIL <- NULL

glimpse(eleitorado_2020)
any[is.na(eleitorado_2020)]

skim(eleitorado_2020)


#escolaridade no estado de sp
eleitorado_2020 %>% filter(SG_UF == "SP") %>%
  group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(soma = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(perc = soma/sum(soma)*100) %>%
  ggplot(aes(DS_GRAU_ESCOLARIDADE, perc))+
  geom_col(position = "dodge")+
  theme_economist()

#grau escolaridade pa?s
eleitorado_2020 %>% group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(N_Eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = round(N_Eleitores/sum(N_Eleitores)*100,1)) %>%
  ggplot(aes(DS_GRAU_ESCOLARIDADE, percentual))+
  geom_col(position = "dodge", fill = "#75a3a3")+
  geom_label(aes(label = percentual, vjust = -0.5), colour = "Black")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))+
  ylab(label = "PERCENTUAL")+
  xlab(label = "ESCOLARIDADE")+
  labs(title = "Percentual de Escolaridade do Eleitorado 2020", caption = "Autor: Caio Martins\n Fonte: TSE")

#genero
eleitorado_2020 %>% group_by(DS_GENERO) %>%
  summarise(N_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = N_eleitores/sum(N_eleitores)*100) %>%
  ggplot(aes(DS_GENERO, percentual))+
  geom_col(position = "dodge")+
  geom_text(aes(label = percentual))+
  theme_economist()

#eleitores totais
eleitorado_2020 %>% summarise(total = sum(QT_ELEITORES_PERFIL))


#estado civil
eleitorado_2020 %>% group_by(DS_ESTADO_CIVIL) %>%
  summarise(N_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = N_eleitores/sum(N_eleitores)*100) %>%
  ggplot(aes(DS_ESTADO_CIVIL, percentual))+
  geom_col(position = "dodge")+
  geom_text(aes(label = percentual))+
  theme_economist()

#faixa etaria
eleitorado_2020 %>% group_by(DS_FAIXA_ETARIA) %>%
  summarise(N_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = N_eleitores/sum(N_eleitores)*100) %>%
  ggplot(aes(DS_FAIXA_ETARIA, percentual))+
  geom_col(position = "dodge")+
  geom_text(aes(label = percentual))+
  theme_economist()

eleitorado_2020 %>% filter(NM_MUNICIPIO == "SÃO PAULO") %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(N_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = round(N_eleitores/sum(N_eleitores)*100,4)) %>%
  ggplot(aes(DS_FAIXA_ETARIA, percentual))+
  geom_col(position = "dodge", fill = "#3399ff")+
  geom_label(aes(label = percentual, vjust = -0.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))+
  ylab(label = "PERCENTUAL")+
  xlab(label = "Faixa Etária")+
  labs(title = "Percentual de Faixa Etária do Eleitorado 2020", caption = "Autor: Caio Martins\n Fonte: TSE")


eleitorado_2020 %>% filter(NM_MUNICIPIO == "SÃO PAULO" & "RIO DE JANEIRO")

capitais <- data.frame(NM_MUNICIPIO = c("SÃO PAULO",
                                        "RIO DE JANEIRO",
                                        "BELO HORIZONTE",
                                        "CURITIBA",
                                        "CUIABÁ",
                                        "SALVADOR",
                                        "MANAUS",
                                        "BELÉM",
                                        "RIO BRANCO",
                                        "MACEI?",
                                        "MACAP?",
                                        "FORTALEZA",
                                        "BRAS?LIA",
                                        "VIT?RIA",
                                        "GOI?NIA",
                                        "S?O LU?S",
                                        "CAMPO GRANDE",
                                        "JO?O PESSOA",
                                        "RECIFE",
                                        "TERESINA",
                                        "NATAL",
                                        "PORTO ALEGRE",
                                        "PORTO VELHO",
                                        "BOA VISTA",
                                        "FLORIAN?POLIS",
                                        "ARACAJU",
                                        "PALMAS"))

class(capitais$Municipios)
teste <- left_join(capitais, eleitorado_2020, by = "NM_MUNICIPIO")

teste %>% group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(total_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = total_eleitores/sum(total_eleitores)*100) %>%
  ggplot()+
  geom_segment(aes(x = total_eleitores, xend = total_eleitores, y = NM_MUNICIPIO),
               size = 5,
               color = '#a7a9ac')

teste %>% filter(DS_GRAU_ESCOLARIDADE == NA)

teste %>% filter(NM_MUNICIPIO == "S?O PAULO") %>%
  group_by(DS_GENERO) %>%
  summarise(n_eleitores = sum(QT_ELEITORES_PERFIL,na.rm = TRUE)) %>%
  mutate(percentual = n_eleitores/sum(n_eleitores)*100)


teste <- teste %>%
  summarise(N_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  mutate(percentual = N_Eleitores/sum(N_Eleitores)*100)

vai %>% filter(NM_MUNICIPIO == "S?O PAULO")

vai %>% group_by(DS_GENERO) %>%
  ggplot()+
  geom_segment(aes(x = 0, xend = percentual, y = NM_MUNICIPIO, yend = NM_MUNICIPIO),
               size = 5)+
  geom_point(aes(total_eleitores, NM_MUNICIPIO, color = DS_GENERO, fill = DS_GENERO),size = 5)
scale_y_continuous(labels = percent_format())


vai <- teste %>% select(DS_GENERO, DS_GRAU_ESCOLARIDADE, QT_ELEITORES_PERFIL, NM_MUNICIPIO) %>%
  group_by(NM_MUNICIPIO, DS_GENERO) %>%
  summarise(total_eleitores = sum(QT_ELEITORES_PERFIL,na.rm = TRUE)) %>%
  mutate(percentual = (total_eleitores/sum(total_eleitores))*100)

vai<- teste %>% group_by(DS_GENERO, NM_MUNICIPIO) %>%
  summarise(N_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  mutate(percentual = (N_Eleitores/sum(N_Eleitores))*100) %>%
  ggplot()+
  geom_segment(aes(x = percentual, xend = percentual, y = NM_MUNICIPIO, yend = NM_MUNICIPIO),
               size = 5)+
  geom_point(aes(N_Eleitores, NM_MUNICIPIO, color = DS_GENERO, fill = DS_GENERO),size = 5)
scale_x_continuous(labels = comma_format(big.mark))


teste %>% group_by(DS_GRAU_ESCOLARIDADE, NM_MUNICIPIO) %>%
  mutate(percentual = QT_ELEITORES_PERFIL/sum(QT_ELEITORES_PERFIL)*100) %>%
  ggplot()+
  geom_segment(aes(x = percentual, xend = percentual, y = NM_MUNICIPIO, yend = NM_MUNICIPIO),
               size = 5)+
  geom_point(aes(percentual, NM_MUNICIPIO, color = DS_GRAU_ESCOLARIDADE, fill = DS_GRAU_ESCOLARIDADE),size = 5)














UFs_2020 <- eleitorado_2020 %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(SG_UF, DS_FAIXA_ETARIA) %>%
  summarise(soma = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_FAIXA_ETARIA, values_from = soma)

cidades_2020 <- eleitorado_2020 %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA) %>%
  summarise(soma = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_FAIXA_ETARIA, values_from = soma)

eleitorado_2020_n <- eleitorado_2020 %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, DS_GENERO) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_GENERO, values_from = total) %>%
  replace(is.na(.), 0) %>%
  janitor::clean_names() %>%
  mutate(total = feminino + masculino + nao_informado,
         fem_perc = round((feminino / total) * 100),
         mas_perc = round((masculino / total) * 100),
         diferenca = feminino - masculino,
         mais_mulher = feminino > masculino) %>%
  arrange(desc(fem_perc))

