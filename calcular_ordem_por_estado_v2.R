##############################
## Packages
##############################

if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}

##############################
### Funções auxiliares
##############################

#' beggining.of.month:
#' Função que faz a leitura de um string em formato de data ("%Y-%m-%d"), e altera o dia para o primeiro dia do mês
#' @x: character. Data a ser transformada para o primeiro dia do mês da data correspondente.
#' Exemplo: "2022-12-27" é convertido para "2022-12-01"
#' 
beginning.of.month <- function(x) {
  substr(x,9,10) <- "01"
  return(x)
}

#' end.of.epiweek:
#' Função que converte uma data no último dia da semana epidemiológica ao qual esta data pertence
#' @x: Date. Data a ser convertida
#' @end: Número de dias a serem contados a partir do primeiro da semana epidemiológica

end.of.epiweek <- function(x, end = 6) {
  offset <- (end - 4) %% 7
  num.x <- as.numeric(x)
  return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
}


###############
### Script
###############

# Data de referência: primeiro dia do ano
first.day <- as.Date("2021-01-01")

# Faz a leitura dos arquivos presentes em "dados/" e escolhe a data da base mais recente
data_base <- list.files("dados/") %>% 
  grep("^dados_.*.csv", ., value = T) %>%
  substr(7,16) %>%
  as.Date() %>%
  max(na.rm = T)

if(is.infinite(data_base)) {
  print("Data em '^dados_.*.csv' não encontrada. Tentando ^limpo_dados_.*.csv")
  data_base <- list.files("dados/") %>% 
    grep("^limpo_dados_.*.csv", ., value = T) %>%
    substr(13,22) %>%
    as.Date() %>%
    max(na.rm = T)
}

# Horário e data de início do processamento dos dados
ini = Sys.time()

############
#### Calcular a cobertura de doses por estado por ordem de aplicação
############

# Escolhe todos os arquivos sem separação (sem numeração no título)
files <- grep("(^[A-Z][A-Z]_PNI)|(^SP_1)",grep("PNI_clean",list.files("output/"), value = TRUE), value = TRUE)

# Tabela de cobertura de doses por mês
da_month <- data.frame()

# Tabela de cobertura de doses por semana
da_week <- data.frame()

# Tabela de registro de variáveis com valor = NA
log_table = data.frame()

# Iniciar loop para todos os arquivos selecionados
for(i in files) {
  
  # Salva a sigla do estado, de acordo com o nome do arquivo
  state = substr(i,1,2)
  print(state)
  
  if(state != "SP") {
  # Ler tabela
  df <- data.frame(fread(paste0("output/",i),
                         select = c("id","data","doses","vacina","idade"),
                         colClasses = c("id" = "factor",
                                        "data" = "Date",
                                        "vacina" = "factor",
                                        "doses" = "factor",
                                        "idade" = "numeric")))
  } else {
  
    sp_files = grep("SP_*._PNI_clean.csv",grep("PNI_clean",list.files("output/"), value = TRUE), value = TRUE)
    
    df <- data.frame()
    
    for (j in sp_files) {
    
    print(paste0("Loading: ", j))  
      
    df_sp <- data.frame(fread(paste0("output/",j),
                           select = c("id","data","doses","vacina","idade"),
                           colClasses = c("id" = "factor",
                                          "data" = "Date",
                                          "vacina" = "factor",
                                          "doses" = "factor",
                                          "idade" = "numeric")))  
    
    df <- bind_rows(df, df_sp)
    
    }
    rm(df_sp);gc()
  }
  
  # Converte id em numérico
  df$id <- as.numeric(df$id)
  
  df <- df %>% 
    filter(!(idade < 6 & data < as.Date("2022-07-13"))) %>% # Filtra vacinação em menores de 5 anos antes da aprovação pela anvisa
    arrange(id, data) %>%
    group_by(id) %>%
    mutate(ordem = 1:n()) %>%
    ungroup() %>%
    mutate(firstJ = (ordem == 1 & vacina == 88 |
                     ordem == 1 & doses == "D")) %>%
    group_by(id) %>%
    mutate(janssen = any(firstJ == TRUE)) %>%
    ungroup() %>%
    select(-firstJ) %>%
    mutate(janssen =  factor(janssen, levels = c(TRUE, FALSE), labels = c("J","NJ"))) %>%
    mutate(agegroup = factor(cut(idade, 
                             breaks = c(0,5,12,18,seq(30,90,10),Inf),
                             include.lowest = T, 
                             right = F,
                             labels = F)))
  
    # Calcular linhas com NA para dose, agegroup e Janssen
  drop_na_dose <- sum(is.na(df$doses))
  drop_na_agegroup <- sum(is.na(df$agegroup))
  drop_na_janssen <- sum(is.na(df$janssen))
  
  # Calcula a frequência de 1ª Dose por data, faixa etária e tipo de primeira vacina (Janssen/Não-Janssen)
  D1 = df %>% filter(ordem == 1) %>%
    rename(date = data) %>%
    count(date, agegroup, janssen) %>%
   complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"), agegroup, janssen,
            fill = list(n = 0)) %>%
    mutate(dose = "a")
  
  # Calcula a frequência de 2ª Dose por data, faixa etária e tipo de primeira vacina (Janssen/Não-Janssen)
  D2 = df %>% filter(ordem == 2) %>%
    rename(date = data) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "b")
  
  # Calcula a frequência de 3ª Dose por data, faixa etária e tipo de primeira vacina (Janssen/Não-Janssen)
  D3 = df %>% filter(ordem == 3) %>%
    rename(date = data) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "c")
  
  # Calcula a frequência de 4ª Dose por data, faixa etária e tipo de primeira vacina (Janssen/Não-Janssen)
  D4 = df %>% filter(ordem == 4) %>%
    rename(date = data) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "d")
  
  # Calcula a frequência de 5ª Dose por data, faixa etária e tipo de primeira vacina (Janssen/Não-Janssen)
  D5 = df %>% filter(ordem == 5) %>%
    rename(date = data) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "e")
  
  # Une todas as tabelas  
  df_doses <- bind_rows(D1,D2,D3,D4,D5) %>%
    mutate(dose = factor(dose))
  
  # Limpa cache
  rm(df, D1, D2, D3, D4, D5);gc()
  
  # Calcula a cobertura de doses por mês
  
  df_month <- df_doses %>%
    mutate(month = as.Date(beginning.of.month(as.character(date)))) %>%
    group_by(month, agegroup, dose, janssen) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    mutate(dose_vaccine = paste0(dose,janssen)) %>%
    select(-dose,-janssen) %>%
    spread(key = dose_vaccine, value = total) %>%
    complete(month = seq.Date(min(month), as.Date(beginning.of.month(as.character(data_base))), by="month"), agegroup,
             fill = list(aJ = 0, bJ = 0, cJ = 0, dJ = 0, eJ = 0,
                         aNJ = 0, bNJ = 0, cNJ = 0, dNJ = 0, eNJ = 0)) %>%
    mutate(month = as.Date(month)) %>%
    distinct() %>%
    mutate(aJ = aJ - bJ,
           bJ = bJ - cJ,
           cJ = cJ - dJ,
           dJ = dJ - eJ,
           
           aNJ = aNJ - bNJ,
           bNJ = bNJ - cNJ,
           cNJ = cNJ - dNJ,
           dNJ = dNJ - eNJ) %>%
    
    group_by(agegroup) %>%
    
    mutate(acNJ = cumsum(aNJ),
           bcNJ = cumsum(bNJ),
           ccNJ = cumsum(cNJ),
           dcNJ = cumsum(dNJ),
           ecNJ = cumsum(eNJ),
           
           acJ = cumsum(aJ),
           bcJ = cumsum(bJ),
           ccJ = cumsum(cJ),
           dcJ = cumsum(dJ),
           ecJ = cumsum(eJ)) %>%
    
    gather(key = "dose", value = "n", -month, -agegroup)  %>%
    mutate(first_dose = factor(!grepl("N", dose), 
                               levels = c(T,F), 
                               labels = c("Janssen","Other")),
           dose = gsub("J","",dose),
           dose = gsub("N","",dose),
           dose = factor(dose,
                         levels = c("a", "ac", "b", "bc", "c", "cc", "d", "dc", "e", "ec"),
                         labels = c("D1", "D1cum", "D2", "D2cum", "D3", "D3cum", "D4", "D4cum", "D5", "D5cum")),
           UF = state)
  
  # Calcula a cobertura de doses por semana epidemiológica
  
  df_week <- df_doses %>%
    mutate(week = end.of.epiweek(date)) %>%
    group_by(week, agegroup, dose, janssen) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    mutate(dose_vaccine = paste0(dose,janssen)) %>%
    select(-dose,-janssen) %>%
    spread(key = dose_vaccine, value = total) %>%
    complete(week = seq.Date(min(week), end.of.epiweek(as.Date(data_base)), by="week"), agegroup,
             fill = list(aJ = 0, bJ = 0, cJ = 0, dJ = 0, eJ = 0,
                         aNJ = 0, bNJ = 0, cNJ = 0, dNJ = 0, eNJ = 0)) %>%
    mutate(week = as.Date(week)) %>%
    distinct() %>%
    mutate(aJ = aJ - bJ,
           bJ = bJ - cJ,
           cJ = cJ - dJ,
           dJ = dJ - eJ,
           
           aNJ = aNJ - bNJ,
           bNJ = bNJ - cNJ,
           cNJ = cNJ - dNJ,
           dNJ = dNJ - eNJ) %>%
    
    group_by(agegroup) %>%
    
    mutate(acNJ = cumsum(aNJ),
           bcNJ = cumsum(bNJ),
           ccNJ = cumsum(cNJ),
           dcNJ = cumsum(dNJ),
           ecNJ = cumsum(eNJ),
           
           acJ = cumsum(aJ),
           bcJ = cumsum(bJ),
           ccJ = cumsum(cJ),
           dcJ = cumsum(dJ),
           ecJ = cumsum(eJ)) %>%
    
    gather(key = "dose", value = "n", -week, -agegroup)  %>%
    mutate(first_dose = factor(!grepl("N", dose), 
                               levels = c(T,F), 
                               labels = c("Janssen","Other")),
           dose = gsub("J","",dose),
           dose = gsub("N","",dose),
           dose = factor(dose,
                         levels = c("a", "ac", "b", "bc", "c", "cc", "d", "dc", "e", "ec"),
                         labels = c("D1", "D1cum", "D2", "D2cum", "D3", "D3cum", "D4", "D4cum", "D5", "D5cum")),
           UF = state)
  
  rm(df_doses); gc()
  
  # Une tabelas de dados de todos os estados pelo estado processado no loop atual (agrupamento por mês)
  da_month <- bind_rows(da_month, df_month)
  
  # Une tabelas de dados de todos os estados pelo estado processado no loop atual (agrupamento por semana epidemiológica)
  da_week <- bind_rows(da_week, df_week)  
  
  
  log_table_temp <- data.frame(drop_na_dose = drop_na_dose,
                               drop_na_agegroup = drop_na_agegroup,
                               drop_na_janssen = drop_na_janssen,
                               state = state)
  
  log_table = bind_rows(log_table, log_table_temp)
}

# Define a data do mês para o primeiro dia do próximo mês. remove valores NA
da_month <- da_month %>%
  mutate(month = as.Date(beginning.of.month(as.character(month + 32)))) %>%
  drop_na(month, agegroup)

# Remove valores NA para tabela de dados agrupados por semana epidemiológica
da_week <- da_week %>%
  drop_na(week, agegroup)

# Salva arquivos de saída

fwrite(da_month, file = "output/doses_cobertura_proporcao_mes_ordem.csv")
fwrite(da_week, file = "output/doses_cobertura_proporcao_semana_ordem.csv")

# Salva log

data_base_title = format(as.Date(Sys.time()), format = "%Y_%m_%d")
filename = paste0("output/log/log_ordem_",data_base_title,".csv")
write.csv(log_table, file = filename)

################
### Plots
###############

# Plot de dados agrupados por mês e faixa etária

gage_month <- da_month %>% 
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 1:11,
                           labels = c("0 a 4",
                                      "5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = month, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                       labels = c("5ª dose",
                                  "4ª dose",
                                  "3ª dose",
                                  "2ª dose",
                                  "1ª dose"))

ggsave(gage_month, file = "figuras/aplicacao_doses_mes_ordem.png", width = 24, height = 12)

# Plot de dados agrupados por semana epidemiológica e faixa etária

gage_week <- da_week %>% 
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 1:11,
                           labels = c("0 a 4",
                                      "5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = week, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  theme_set(theme_gray(base_size = 30)) +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                       labels = c("5ª dose",
                                  "4ª dose",
                                  "3ª dose",
                                  "2ª dose",
                                  "1ª dose"))

ggsave(gage_week, file = "figuras/aplicacao_doses_semana_ordem.png", width = 24, height = 12)

# Plot de dados agrupados por mês e UF

guf_month <- da_month %>%
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 1:11,
                           labels = c("0 a 4",
                                      "5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = month, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  theme_set(theme_gray(base_size = 30)) +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                       labels = c("5ª dose",
                                  "4ª dose",
                                  "3ª dose",
                                  "2ª dose",
                                  "1ª dose"))

ggsave(guf_month, file = "figuras/aplicacao_doses_uf_mes_ordem.png", width = 24, height = 12)

# Plot de dados agrupados por semana epidemiológica e UF

guf_week <- da_week %>%
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 1:11,
                           labels = c("0 a 4",
                                      "5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = week, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  theme_set(theme_gray(base_size = 30)) +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                       labels = c("5ª dose",
                                  "4ª dose",
                                  "3ª dose",
                                  "2ª dose",
                                  "1ª dose"))

ggsave(guf_week, file = "figuras/aplicacao_doses_uf_semana_ordem.png", width = 24, height = 12)

# Retorna o tempo total de execução do script
fin = Sys.time()
fin - ini
