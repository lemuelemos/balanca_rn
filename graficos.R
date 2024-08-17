library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggimprensa)
library(readxl)


EXP_2024 <- read_delim("dados/EXP_2024.csv", 
                       delim = ";", 
                       escape_double = FALSE, 
                       trim_ws = TRUE) %>% 
  mutate(TIPO = "EXP") %>% 
  mutate(CO_MES = as.numeric(CO_MES)) %>% 
  filter(SG_UF_NCM == "RN")


IMP_2024 <- read_delim("dados/IMP_2024.csv", 
                       delim = ";", 
                       escape_double = FALSE, 
                       trim_ws = TRUE) %>% 
  mutate(TIPO = "IMP") %>% 
  mutate(CO_MES = as.numeric(CO_MES)) %>% 
  filter(SG_UF_NCM == "RN")


NCM <- read_delim("dados/NCM.csv", delim = ";", 
                  escape_double = FALSE, 
                  locale = locale(encoding = "WINDOWS-1252"), 
                  trim_ws = TRUE)

setores <- read_excel("dados/TABELAS_AUXILIARES.xlsx", 
                                 sheet = "4")

bind_rows(EXP_2024,
          IMP_2024) %>% 
  mutate(Competência = as.Date(paste0(CO_ANO,
                                      "-",
                                      CO_MES ,
                                      "-01"))) %>% 
  arrange(CO_ANO,CO_MES) %>% 
  left_join(
    NCM %>% 
      select(CO_NCM,NO_NCM_POR)
  ) %>% 
  summarise(VL_FOB = sum(VL_FOB),.by = c("Competência","TIPO")) %>% 
  tidyr::pivot_wider(names_from = TIPO,values_from = VL_FOB) %>% 
  mutate(Saldo = EXP - IMP) %>% 
  tidyr::pivot_longer(cols = c("EXP","IMP"),names_to = "Movimento",values_to = "Valor") %>% 
  ggplot(aes(x = Competência, y = Saldo)) +
  labs(
    title = "Balança Comercial do Rio Grande do Norte",
    subtitle = "Evolução das Exportações/Importações no primeiro semestre de 2024",
    caption = "Fonte: Elaboração própria com dados da Secretaria de Comércio Exterior - SECEX",
  ) +
  geom_bar(aes(x = Competência, y = Valor, fill = Movimento),
           stat = "identity", position = position_dodge()) +
  geom_line(color = "black",size = 1.2, linetype = "dashed") +
  geom_label(aes(label = paste0(scales::comma(Saldo / 1000000, accuracy = 0.1), " M"),nudge_x = 0.3), 
             size = 4,
             label.size = 1,
             color = '#1D4E89',
             border_color = "#1D4E89") +
  scale_x_date(date_labels = "%b-%Y") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_manual(values = c("EXP" = "#1D4E89", "IMP" = "#c6160d")) +
  tema_g1()


bind_rows(EXP_2024,
          IMP_2024) %>% 
  mutate(Competência = as.Date(paste0(CO_ANO,
                                      "-",
                                      CO_MES ,
                                      "-01"))) %>% 
  arrange(CO_ANO,CO_MES) %>% 
  left_join(setores %>% 
              select(CO_NCM,
                     NO_NCM_POR,
                     NO_ISIC_CLASSE,
                     NO_ISIC_GRUPO,
                     NO_ISIC_DIVISAO,
                     NO_ISIC_SECAO)) %>% 
  summarise(L_FOB = round(sum(VL_FOB)/1000000,2),.by = c("NO_ISIC_SECAO","TIPO")) %>% 
  arrange(TIPO,-L_FOB) %>% 
  mutate(L_FOB = case_when(TIPO == "IMP" ~ -L_FOB,
                           .default = L_FOB)) %>% 
  mutate(NO_ISIC_SECAO = paste0(TIPO," - ",NO_ISIC_SECAO)) %>% 
  select(NO_ISIC_SECAO,L_FOB) %>% 
  mutate(
    NO_ISIC_SECAO = case_when(NO_ISIC_SECAO == "EXP - Indústria de Transformação" ~ "EXP - Indústria \nde Transformação",
                              NO_ISIC_SECAO == "EXP - Indústria Extrativa" ~ "EXP - Indústria\n Extrativa",
                              NO_ISIC_SECAO == "IMP - Indústria de Transformação" ~ "IMP - Indústria de\n Transformação",
                              NO_ISIC_SECAO == "IMP - Indústria Extrativa" ~ "IMP - Indústria\n Extrativa",
                              NO_ISIC_SECAO == "IMP - Outros Produtos" ~ "IMP - Outros\n Produtos",
                              NO_ISIC_SECAO == "EXP - Outros Produtos" ~ "EXP - Outros\n Produtos",
                              .default = NO_ISIC_SECAO)
  ) %>% 
  waterfalls::waterfall(calc_total = TRUE,
                        fill_by_sign = F,
                        fill_colours = c("#1D4E89",
                                         "#1D4E89",
                                         "#1D4E89",
                                         "#1D4E89",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d"
                                         ),
                        total_rect_color = "gray",
                        total_rect_text_color = "black") +
  labs(
    title = "Balança Comercial do Rio Grande do Norte",
    subtitle = "Composição da Balança Comercial Potiguar - Setores Econômicos",
    caption = "Fonte: Elaboração própria com dados da Secretaria de Comércio Exterior - SECEX",
  ) +
  tema_g1()


bind_rows(EXP_2024,
          IMP_2024) %>% 
  mutate(Competência = as.Date(paste0(CO_ANO,
                                      "-",
                                      CO_MES ,
                                      "-01"))) %>% 
  arrange(CO_ANO,CO_MES) %>% 
  left_join(setores %>% 
              select(CO_NCM,
                     NO_NCM_POR,
                     NO_ISIC_CLASSE,
                     NO_ISIC_GRUPO,
                     NO_ISIC_DIVISAO,
                     NO_ISIC_SECAO)) %>% 
  summarise(L_FOB = round(sum(VL_FOB)/1000000,2),.by = c("NO_ISIC_CLASSE","TIPO")) %>% 
  arrange(TIPO,-L_FOB) %>% 
  group_by(TIPO) %>% 
  mutate(`Part(%)` = round(L_FOB/sum(L_FOB),2)) %>% 
  ungroup() %>% 
  mutate(NO_ISIC_CLASSE = case_when(`Part(%)` < 0.02 ~ "Outros",
                                    .default = NO_ISIC_CLASSE)) %>% 
  mutate(
    NO_ISIC_CLASSE = case_when(
      NO_ISIC_CLASSE == "Fabricação de produtos petrolíferos refinados" ~ "Petróleo/Gás",
      NO_ISIC_CLASSE == "Cultivo de hortaliças e melões, raízes e tubérculos" ~ "Fruticultura/Agricultura",
      NO_ISIC_CLASSE == "Fabricação de componentes eletrônicos e placas" ~ "Componentes Eletrônicos",
      NO_ISIC_CLASSE == "Cultivo de cereais (exceto arroz), leguminosas e oleaginosas" ~ "Fruticultura/Agricultura",
      NO_ISIC_CLASSE == "Fabricação de motores elétricos, geradores, transformadores e aparelhos de distribuição e controle de energia elétrica" ~ "Geração de Energia",
      NO_ISIC_CLASSE == "Fabricação de plásticos e borracha sintética em formas primárias" ~ "Plásticos",
      NO_ISIC_CLASSE == "Fabricação de rolamentos, engrenagens, engrenagens e elementos de acionamento" ~ "Componentes Mecânicos",
      NO_ISIC_CLASSE == "Fabricação de outro equipamento elétrico" ~ "Equipamentos Elétricos",
      NO_ISIC_CLASSE == "Fabricação de produtos lácteos" ~ "Latícinios",
      .default = NO_ISIC_CLASSE
    )
  ) %>%
  summarise(L_FOB = sum(L_FOB),.by = c("NO_ISIC_CLASSE","TIPO")) %>%
  mutate(L_FOB = case_when(TIPO == "IMP" ~ -L_FOB,
                           .default = L_FOB)) %>% 
  mutate(NO_ISIC_CLASSE = paste0(TIPO," - ",NO_ISIC_CLASSE)) %>% 
  select(NO_ISIC_CLASSE,L_FOB) %>% 
  waterfalls::waterfall(calc_total = TRUE,
                        fill_by_sign = F,
                        fill_colours = c("#1D4E89",
                                         "#1D4E89",
                                         "#1D4E89",
                                         "#1D4E89",
                                         "#1D4E89",
                                         "#1D4E89",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d",
                                         "#c6160d"
                        ),
                        total_rect_color = "gray",
                        total_rect_text_color = "black") +
  labs(
    title = "Balança Comercial do Rio Grande do Norte",
    subtitle = "Composição da Balança Comercial Potiguar - Principais Produtos",
    caption = "Fonte: Elaboração própria com dados da Secretaria de Comércio Exterior - SECEX",
  ) +
  tema_g1() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) 



cores_G1 <- c("Fabricação de açúcar" = "#0F9D58",  # Vermelho
              "Fruticultura/Agricultura" = "#1A73E8",  # Azul
              "Outros" = "#FFC107",  # Amarelo
              "Petróleo/Gás" = "#D71920",  # Verde
              "Tecelagem de têxteis" = "#9E9E9E")

bind_rows(EXP_2024,
          IMP_2024) %>% 
  mutate(Competência = as.Date(paste0(CO_ANO,
                                      "-",
                                      CO_MES ,
                                      "-01"))) %>% 
  arrange(CO_ANO,CO_MES) %>% 
  left_join(setores %>% 
              select(CO_NCM,
                     NO_NCM_POR,
                     NO_ISIC_CLASSE,
                     NO_ISIC_GRUPO,
                     NO_ISIC_DIVISAO,
                     NO_ISIC_SECAO)) %>% 
  summarise(L_FOB = round(sum(VL_FOB)/1000000,2),.by = c("Competência","NO_ISIC_CLASSE","TIPO")) %>% 
  arrange(TIPO,-L_FOB) %>% 
  group_by(Competência,TIPO) %>% 
  mutate(`Part(%)` = round(L_FOB/sum(L_FOB),2)) %>% 
  ungroup() %>% 
  mutate(NO_ISIC_CLASSE = case_when(`Part(%)` < 0.1 ~ "Outros",
                                    .default = NO_ISIC_CLASSE)) %>% 
  mutate(
    NO_ISIC_CLASSE = case_when(
      NO_ISIC_CLASSE == "Fabricação de produtos petrolíferos refinados" ~ "Petróleo/Gás",
      NO_ISIC_CLASSE == "Cultivo de hortaliças e melões, raízes e tubérculos" ~ "Fruticultura/Agricultura",
      NO_ISIC_CLASSE == "Fabricação de componentes eletrônicos e placas" ~ "Componentes Eletrônicos",
      NO_ISIC_CLASSE == "Cultivo de cereais (exceto arroz), leguminosas e oleaginosas" ~ "Fruticultura/Agricultura",
      NO_ISIC_CLASSE == "Fabricação de motores elétricos, geradores, transformadores e aparelhos de distribuição e controle de energia elétrica" ~ "Geração de Energia",
      NO_ISIC_CLASSE == "Fabricação de plásticos e borracha sintética em formas primárias" ~ "Plásticos",
      NO_ISIC_CLASSE == "Fabricação de rolamentos, engrenagens, engrenagens e elementos de acionamento" ~ "Componentes Mecânicos",
      NO_ISIC_CLASSE == "Fabricação de outro equipamento elétrico" ~ "Equipamentos Elétricos",
      NO_ISIC_CLASSE == "Fabricação de produtos lácteos" ~ "Latícinios",
      .default = NO_ISIC_CLASSE
    )
  ) %>%
  summarise(L_FOB = sum(L_FOB),.by = c("Competência","NO_ISIC_CLASSE","TIPO")) %>%
  group_by(Competência,TIPO) %>% 
  mutate(Percentual = round(L_FOB/sum(L_FOB),2)*100) %>% 
  mutate(L_FOB = case_when(TIPO == "IMP" ~ -L_FOB,
                           .default = L_FOB)) %>% 
  filter(TIPO == "EXP") %>% 
  rename(Produto = NO_ISIC_CLASSE) %>% 
  ggplot(aes(x = Competência, y = Percentual, fill = Produto)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = cores_G1) +
  labs(
    title = "Balança Comercial do Rio Grande do Norte",
    subtitle = "Participaçao dos principais produtos da pauta de exportação - Principais Produtos (%)",
    caption = "Fonte: Elaboração própria com dados da Secretaria de Comércio Exterior - SECEX",
  ) +
  tema_g1()


cores_G1 <- c("Componentes Eletrônicos" = "#1A73E8",  # Vermelho
              "Fruticultura/Agricultura" = "#9E9E9E",  # Azul
              "Geração de Energia" = "#FFC107",  # Amarelo
              "Petróleo/Gás" = "#D71920",  # Verde
              "Outros" = "#0F9D58")

bind_rows(EXP_2024,
          IMP_2024) %>% 
  mutate(Competência = as.Date(paste0(CO_ANO,
                                      "-",
                                      CO_MES ,
                                      "-01"))) %>% 
  arrange(CO_ANO,CO_MES) %>% 
  left_join(setores %>% 
              select(CO_NCM,
                     NO_NCM_POR,
                     NO_ISIC_CLASSE,
                     NO_ISIC_GRUPO,
                     NO_ISIC_DIVISAO,
                     NO_ISIC_SECAO)) %>% 
  summarise(L_FOB = round(sum(VL_FOB)/1000000,2),.by = c("Competência","NO_ISIC_CLASSE","TIPO")) %>% 
  arrange(TIPO,-L_FOB) %>% 
  group_by(Competência,TIPO) %>% 
  mutate(`Part(%)` = round(L_FOB/sum(L_FOB),2)) %>% 
  ungroup() %>% 
  mutate(NO_ISIC_CLASSE = case_when(`Part(%)` < 0.1 ~ "Outros",
                                    .default = NO_ISIC_CLASSE)) %>% 
  mutate(
    NO_ISIC_CLASSE = case_when(
      NO_ISIC_CLASSE == "Fabricação de produtos petrolíferos refinados" ~ "Petróleo/Gás",
      NO_ISIC_CLASSE == "Cultivo de hortaliças e melões, raízes e tubérculos" ~ "Fruticultura/Agricultura",
      NO_ISIC_CLASSE == "Fabricação de componentes eletrônicos e placas" ~ "Componentes Eletrônicos",
      NO_ISIC_CLASSE == "Cultivo de cereais (exceto arroz), leguminosas e oleaginosas" ~ "Fruticultura/Agricultura",
      NO_ISIC_CLASSE == "Fabricação de motores elétricos, geradores, transformadores e aparelhos de distribuição e controle de energia elétrica" ~ "Geração de Energia",
      NO_ISIC_CLASSE == "Fabricação de plásticos e borracha sintética em formas primárias" ~ "Plásticos",
      NO_ISIC_CLASSE == "Fabricação de rolamentos, engrenagens, engrenagens e elementos de acionamento" ~ "Componentes Mecânicos",
      NO_ISIC_CLASSE == "Fabricação de outro equipamento elétrico" ~ "Equipamentos Elétricos",
      NO_ISIC_CLASSE == "Fabricação de produtos lácteos" ~ "Latícinios",
      .default = NO_ISIC_CLASSE
    )
  ) %>%
  summarise(L_FOB = sum(L_FOB),.by = c("Competência","NO_ISIC_CLASSE","TIPO")) %>%
  group_by(Competência,TIPO) %>% 
  mutate(Percentual = round(L_FOB/sum(L_FOB),2)*100) %>% 
  mutate(L_FOB = case_when(TIPO == "IMP" ~ -L_FOB,
                           .default = L_FOB)) %>% 
  filter(TIPO == "IMP") %>% 
  rename(Produto = NO_ISIC_CLASSE) %>% 
  ggplot(aes(x = Competência, y = Percentual, fill = Produto)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = cores_G1) +
  labs(
    title = "Balança Comercial do Rio Grande do Norte",
    subtitle = "Participaçao dos principais produtos da pauta de importação - Principais Produtos (%)",,
    caption = "Fonte: Elaboração própria com dados da Secretaria de Comércio Exterior - SECEX",
  ) +
  tema_g1()
