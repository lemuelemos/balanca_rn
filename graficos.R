library(readr)
library(dplyr)

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
  ggplot() +
  labs(
    title = "Balança Comercial do Rio Grande do Norte",
    subtitle = "Evolução das Exportações/Importações no primeiro semestre de 2024",
    caption = "Fonte: {palmerpenguins} traduzido no pacote {dados}"
  ) +
  geom_line(aes(Competência,Saldo)) +
  geom_col(aes(Competência,EXP)) +
  scale_x_date(date_labels = "%b-%Y") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  tema_g1()






