

# EXPLORAÇÃO INICIAL DOS DADOS SOBRE O ELEITORADO BRASILEIRO DE 2020

# ESTÁ INCOMPLETO. FALTA ATUALIZAR E ARRUMAR OS GRÁFICOS. DEIXÁ-LOS BONITOS


library(tidyverse)
library(ggplot2)
library(skimr)
library(ggthemes)
library(data.table)
library(ggalt)


drop <- c("DT_GERACAO", "HH_GERACAO", "CD_GENERO", "ANO_ELEICAO",
          "CD_MUN_SIT_BIOMETRIA", "CD_GRAU_ESCOLARIDADE", "CD_FAIXA_ETARIA", "CD_ESTADO_CIVIL")



eleitorado_2020 <- fread("C:\\Users\\mcaio\\Desktop\\Nova pasta\\perfil_eleitorado_2020.csv",
                         sep = ";", drop = drop)

eleitorado_2020 <- eleitorado_2020 %>%
  mutate(DS_GRAU_ESCOLARIDADE = case_when(DS_GRAU_ESCOLARIDADE == "ENSINO FUNDAMENTAL INCOMPLETO" ~ "FUND INCOMPLETO",
                                          DS_GRAU_ESCOLARIDADE == "ENSINO FUNDAMENTAL COMPLETO" ~ "FUND COMPLETO",
                                          DS_GRAU_ESCOLARIDADE == "ENSINO MÉDIO INCOMPLETO" ~ "MÉDIO INCOMPLETO",
                                          DS_GRAU_ESCOLARIDADE == "ENSINO MÉDIO COMPLETO" ~ "MÉDIO COMPLETO", TRUE ~ DS_GRAU_ESCOLARIDADE))


sp_2020 <- eleitorado_2020 %>% filter(SG_UF == "SP")

glimpse(eleitorado_2020)
any(is.na(eleitorado_2020))

skim(eleitorado_2020)

eleitorado_2020 %>% select(DS_GRAU_ESCOLARIDADE)


# ESCOLARIDADE BRASIL

eleitorado_2020 %>%
  filter(!DS_GRAU_ESCOLARIDADE == "NÃO INFORMADO") %>%
  group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(soma = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(perc = soma/sum(soma)*100) %>%
  ggplot(aes(DS_GRAU_ESCOLARIDADE, perc))+
  geom_col(position = "dodge")+
  theme_economist()+
  ylab("Percentual (%)")+
  xlab("")+
  ggtitle("Escolaridade do eleitorado no BRASIL")


# GRAU ESCOLARIDADE NO PAIS

eleitorado_2020 %>%
  filter(!DS_GRAU_ESCOLARIDADE == "NÃO INFORMADO") %>%
  group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(N_Eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = round(N_Eleitores/sum(N_Eleitores)*100,1)) %>%
  ggplot(aes(DS_GRAU_ESCOLARIDADE, percentual))+
  geom_col(position = "dodge", fill = "#75a3a3")+
  geom_text(aes(label = percentual, vjust = -0.5), colour = "Black")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab(label = "PERCENTUAL (%)")+
  xlab(label = "")+
  labs(title = "Percentual de Escolaridade do Eleitorado 2020", caption = "Autor: Caio Martins\n Fonte: TSE")




# ESCOLARIDADE NO ESTADO DE SÃO PAULO

sp_2020 %>%
  filter(!DS_GRAU_ESCOLARIDADE == "NÃO INFORMADO") %>%
  group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(soma = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(perc = round(soma/sum(soma)*100,0)) %>%
  ggplot(aes(DS_GRAU_ESCOLARIDADE, perc))+
  geom_col(aes(position = "dodge"), fill = "#66c2ff")+
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5),size = 6, color = "#001a1a")+
  theme_bw(base_family="Calibri")+
  theme(panel.border=element_blank())+
  theme(panel.grid.major.x  = element_blank())+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, color = "#333300"))+
  theme(plot.title=element_text(face="bold"))+
  theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))+
  theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  ylab("PERCENTUAL (%)")+
  xlab("")+
  ggtitle("Percentual(%) da Escolaridade do Eleitorado no Estado de São Paulo")



# PERCENTUAL GENERO EM SAO PAULO


sp_2020 %>% filter(!DS_GENERO == "NÃO INFORMADO") %>%
  group_by(DS_GENERO) %>%
  summarise(N_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = round(N_eleitores/sum(N_eleitores)*100,0)) %>%
  ggplot(aes(DS_GENERO, percentual))+
  geom_col(position = "dodge")+
  geom_text(aes(label = percentual), position = position_stack(vjust = 0.5),size = 6, color = "#001a1a")+
  theme_bw(base_family="Calibri")+
  theme(panel.border=element_blank())+
  theme(panel.grid.major.x  = element_blank())+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, color = "#333300"))+
  theme(plot.title=element_text(face="bold"))+
  theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))+
  theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("PERCENTUAL (%)")+
  xlab("")+
  ggtitle("Percentual(%) de Gênero do Eleitorado no Estado de São Paulo")


# ELEITORES TOTAIS


eleitorado_2020 %>% summarise(total = sum(QT_ELEITORES_PERFIL))



# ESTADO CIVIL EM SAO PAULO


sp_2020 %>%
  filter(!DS_ESTADO_CIVIL == "NÃO INFORMADO") %>%
  group_by(DS_ESTADO_CIVIL) %>%
  summarise(N_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = round(N_eleitores/sum(N_eleitores)*100,0)) %>%
  ggplot(aes(DS_ESTADO_CIVIL, percentual))+
  geom_col(position = "dodge")+
  geom_text(aes(label = percentual), position = position_stack(vjust = 0.5),size = 6, color = "#001a1a")+
  theme_bw(base_family="Calibri")+
  theme(panel.border=element_blank())+
  theme(panel.grid.major.x  = element_blank())+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, color = "#333300"))+
  theme(plot.title=element_text(face="bold"))+
  theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))+
  theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("PERCENTUAL (%)")+
  xlab("")+
  ggtitle("Percentual(%) do Estado Civil do Eleitorado no Estado de São Paulo")



# FAIXA ETARIA EM SAO PAULO


sp_2020 %>% filter(!DS_ESTADO_CIVIL == "Inválido") %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(N_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(percentual = round(N_eleitores/sum(N_eleitores)*100,2)) %>%
  ggplot(aes(DS_FAIXA_ETARIA, percentual))+
  geom_col(position = "dodge")+
  geom_text(aes(label = percentual), position = position_stack(vjust = 0.5), size = 6, color = "#001a1a")+
  theme_bw(base_family="Calibri")+
  theme(panel.border=element_blank())+
  theme(panel.grid.major.x  = element_blank())+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 9, color = "#333300"))+
  theme(plot.title=element_text(face="bold"))+
  theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))+
  theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  ylab("PERCENTUAL (%)")+
  xlab("")+
  ggtitle("Percentual(%) do Estado Civil do Eleitorado no Estado de São Paulo")



#-------------------------------------------------------------------------------------------------

# CAPITAIS DO PAIS


dados_arr <- eleitorado_2020 %>%
  group_by(SG_UF, DS_GENERO) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_GENERO, values_from = total) %>%
  replace(is.na(.), 0) %>%
  mutate(total = FEMININO + MASCULINO + `NÃO INFORMADO`,
         mperc = (MASCULINO/sum(total)*100),
         fperc = (FEMININO/sum(total)*100),
         SG_UF = factor(SG_UF, levels = c("AC",
                                          "AL",
                                          "AM",
                                          "AP",
                                          "BA",
                                          "CE",
                                          "ES",
                                          "GO",
                                          "MA",
                                          "MG",
                                          "MS",
                                          "MT",
                                          "PA",
                                          "PB",
                                          "PE",
                                          "PI",
                                          "PR",
                                          "RJ",
                                          "RN",
                                          "RO",
                                          "RR",
                                          "RS",
                                          "SC",
                                          "SE",
                                          "SP",
                                          "TO")))

dados_arr %>% ggplot()+
  geom_segment(aes(x = mperc, xend = fperc, y = fct_rev(fct_inorder(SG_UF)), yend = SG_UF),
                         size = 1.5)+
  geom_dumbbell(aes(x = mperc, xend = fperc, y = SG_UF),
                size=2.2, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "#0171CE", colour_xend = "#DE4433")+
  geom_text(aes(x=mperc, y=SG_UF, label=round(mperc,1)),
            color="Black", size=2.75, vjust=2.3, family="Calibri")+
  geom_text(aes(x=fperc, y=SG_UF, label=round(fperc,1)),
            color="Black", size=2.75, vjust=2.3, family="Calibri")+
  geom_text(data=filter(dados_arr, SG_UF=="AC"),
            aes(x=mperc, y=SG_UF, label="MASCULINO (%)"),
            color="#9fb059", size=5, vjust=-1.2, fontface="bold", family="Calibri")+
  geom_text(data=filter(dados_arr, SG_UF=="AC"),
            aes(x=fperc, y=SG_UF, label="FEMININO (%)"),
            color="#9fb059", size=5, vjust=-1.2, fontface="bold", family="Calibri")+
  theme_bw()+
  xlab(NULL)+
  ylab(NULL)+
  labs(x=NULL, y=NULL, title="Gênero no Eleitorado Brasileiro",
       subtitle="Percentual(%) de Genero nos Estados Brasileiros com base no Eleitorado",
       caption="Source: Os dados foram extraídos do Tribunal Superior Eleitoral(TSE)")+
  theme_bw(base_family="Calibri")+
  theme(panel.border=element_blank())+
  theme(panel.grid.major.x  = element_blank())+
  theme(panel.grid.minor.x = element_blank())+
  theme(axis.text.y = element_text(size = 12, color = "Black", face = "bold"))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_blank())+
  theme(plot.title=element_text(face="bold"))+
  theme(plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12), hjust = 0.5))+
  theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_discrete(expand=c(0.075,0))



