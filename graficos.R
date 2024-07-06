library(readr)

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
  mutate(Competência = format(as.Date(paste0(CO_ANO,"-" , "-01")),"%B %Y"))
