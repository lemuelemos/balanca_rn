options(timeout = 1000)

library(dplyr)
library(readr)
library(ggplot2)
library(ggimprensa)

download.file("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_COMPLETA.zip",
              destfile = "./dados/exp_imp/EXP_COMPLETA.zip")

download.file("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_COMPLETA.zip",
              destfile = "./dados/exp_imp/IMP_COMPLETA.zip")



unzip("./dados/exp_imp/EXP_COMPLETA.zip", exdir = "./dados/exp_imp/")


exportacoes <- read_delim("dados/exp_imp/EXP_COMPLETA.csv", 
                       delim = ";", 
                       escape_double = FALSE, 
                       trim_ws = TRUE) %>% 
  mutate(TIPO = "EXP") %>% 
  mutate(CO_MES = as.numeric(CO_MES)) %>% 
  filter(SG_UF_NCM == "RN")



unzip("./dados/exp_imp/IMP_COMPLETA.zip", exdir = "./dados/exp_imp/")


importacoes <- read_delim("dados/exp_imp/IMP_COMPLETA.csv", 
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



bind_rows(exportacoes,
          importacoes) %>% 
  filter(CO_ANO >= 2014) %>% 
  left_join(
    NCM %>% 
      select(CO_NCM,NO_NCM_POR)
  ) %>% 
  left_join(
    setores %>% 
      select(CO_NCM,NO_ISIC_SECAO,NO_ISIC_DIVISAO) %>% 
      distinct_all()
  ) %>% 
  arrange(CO_ANO,TIPO) %>% 
  summarise(Total = sum(VL_FOB,na.rm = T),.by = c("CO_ANO","TIPO","NO_ISIC_SECAO","NO_ISIC_DIVISAO")) %>% 
  tidyr::pivot_wider(names_from = TIPO,values_from = Total,values_fill = 0) %>% 
  mutate(Saldo = EXP-IMP) %>% 
  ggplot(aes(x = CO_ANO, y = Saldo)) +
  labs(
    title = "Balança Comercial do Rio Grande do Norte",
    subtitle = "Evolução das Exportações/Importações no primeiro semestre de 2024",
    caption = "Fonte: Elaboração própria com dados da Secretaria de Comércio Exterior - SECEX",
  ) +
  geom_line(color = "black",size = 1.2, linetype = "dashed") +
  geom_label(aes(label = paste0(scales::comma(Saldo / 1000000, accuracy = 0.1), " M"),nudge_x = 0.3), 
             size = 4,
             label.size = 1,
             color = '#1D4E89',
             border_color = "#1D4E89") +
  # scale_x_date(date_labels = "%b-%Y") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_manual(values = c("EXP" = "#1D4E89", "IMP" = "#c6160d")) +
  tema_g1()


