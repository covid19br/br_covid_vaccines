##############################
## Packages
##############################

if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}

##############################
### Auxiliary functions
##############################

#' beggining.of.month:
#' Função que faz a leitura de um string em formato de data ("%Y-%m-%d"), e altera o dia para o primeiro dia do mês
#' @x: character. Data a ser transformada para o primeiro dia do mês da data correspondente.
#' Exemplo: "2022-12-27" é convertido para "2022-12-01"

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

#' Operador leNAF
#' Operador <= (less or equal). Caso um dos valores seja NA, retorna FALSE

`%leNAF%` <- function(A,B){
  ifelse(is.na(A) | is.na(B), FALSE, A <= B)
}

#'which_order:
#'
#' @x: Date. Coluna com quatro valores de datas a serem ordenadas
#' 
#' Função que avalia quatro colunas e retonar um string com a ordem (decrescente) de cada coluna.
#' Valores NA são ignorados no string de saída.
#' 
#' Esta função é utilizada para avaliar a sequência de quatro datas de doses de aplicação,
#' e retornar qual a ordem de colunas em que as datas estejam ordenadas de mais antiga para a mais recente.
#'
#' Exemplo: x = c("2022-11-03","2022-11-01","2022-11-02","2022-11-04")
#' which_order(x) retorna "3124")
#' 
#' #' Exemplo: x = c("2022-11-03","2022-11-01",NA,"2022-11-04")
#' which_order(x) retorna "213")

which_order <- function(x) {
  y = c(x[1],x[2],x[3],x[4])
  return(substr(paste0(order(y),collapse = ""), start = 1, stop = 4 - sum(is.na(y))))
}

###############
### Script
###############

# Define paleta de cor

pal = c(wes_palette(n = 5, name = "Zissou1", type = "discrete"),
        wes_palette(n = 5, name = "Rushmore1", type = "discrete"))

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

# Salvar data da base em formato para titulo de arquivo
data_base_title <- format(as.Date(data_base), format = "%Y_%m_%d")

# Arquivo de output
output_folder = "output/"

#### Calcular a cobertura de doses por estado

# Horário e data de início do processamento dos dados
ini = Sys.time()

# Faz a leitura de todos os arquivos em formato wide na pasta /output
all_files <- list.files("output/wide")

# Escolhe todos os arquivos sem separação (sem numeração no título e que não seja SP)
files <- all_files[!grepl("[1-9].csv", all_files) & !grepl("SP", all_files)]

# Tabela de cobertura de doses por mês
da_month <- data.frame()

# Tabela de cobertura de doses por semana
da_week <- data.frame()

# Iniciar loop para todos os arquivos selecionados

for(i in files) {
  
  # Salva a sigla do estado, de acordo com o nome do arquivo
  state = substr(i,22,23)
  print(state)
  
  # Ler tabela em formato wide
  # Esta tabela carrega apenas as datas de D1, D2, Reforço (R) e Dose Janssen (D)
  
  df2 <- data.frame(fread(paste0("output/wide/",i),
                         select = c("data_D1",
                                    "data_D2",
                                    "data_R",
                                    "data_D",
                                    "agegroup"),
                         colClasses = c("data_D1" = "Date",
                                        "data_D2" = "Date",
                                        "data_R" = "Date",
                                        "data_D" = "Date")))
  
  # Converte valores vazios em NA
  df2[df2==""] <- NA
  
  # Converte coluna de idade em fator
  df2$agegroup <- factor(df2$agegroup, levels = c(1:11))
  
  agegroup1 <- sum(df2$agegroup==1, na.rm = T)
  
  before_filter_date <- nrow(df2)
  
  # Calcular a diferença de tempo em dias entre cada tipo de dose e a data de referência
  df2 <- df2 %>% mutate(dif1 = as.numeric(data_D1 - first.day),
                       dif2 = as.numeric(data_D2 - first.day),
                       difR = as.numeric(data_R - first.day),
                       difU = as.numeric(data_D - first.day)) %>%
                filter(!(difR %leNAF% dif2)) %>%
                filter(!(dif2 %leNAF% dif1))

  after_filter_date <- nrow(df2)
  
  # Calcular a sequência de datas da dose
  df2$next_order = apply(df2[,c("dif1","dif2","difU","difR")], 1, which_order)
  
  # Datas para D1 e D
  
  # Reseta a classificação de doses para D
  df2$dose <- NA
  
  # Todo id com D1 como primeira dose recebe D1
  df2$dose[df2$next_order %in% c(1,12,13,14,
                                 123,124,132,134,142,143,
                                 1234,1243,1342,1324,1423,1432)] <- "D1"
  
  # Todo id com D como primeira dose recebe D
  df2$dose[df2$next_order %in% c(3,31,32,34,
                                 312,314,321,324,341,342,
                                 3124,3142,3214,3241,3421) ] <- "D"
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  
  # Registra as datas de acordo com a data de D1
  df2$data <- df2$data_D1
  
  # Sobrepõe e registra das datas de acordo com a data de D quando esta for a primeira dose
  df2$data[which(df2$dose == "D")] <- df2$data_D[which(df2$dose == "D")]
  
  # Calcular linhas com NA para dose e agegroup
  drop_na_dose <- sum(is.na(df2$dose))
  drop_na_agegroup <- sum(is.na(df2$agegroup))
  
  # Criar um primeiro data.frame com as datas de D1 e D
  # Calcula a frequência de D1 e D por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  
  df_d1 <- df2 %>% 
            drop_na(dose) %>% 
            count(data, agegroup, dose) %>%
            rename(date = data) %>%
            complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
                     agegroup, dose,
                     fill = list(n = 0)) %>%
            rename(data = date)
  
  # Datas para D2
  
  # Registra quais ids possuem D2 na segunda posição
  d2_code_a <- c(12,123,124,1234,1243)
  
  # Registra quais ids possuem D na segunda posição, porém a primeira dose não é Janssen
  d2_code_b <- c(13, 132, 134, 1342, 1324)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 196
  df2$data[df2$next_order %in% d2_code_a] <- df2$data_D2[df2$next_order %in% d2_code_a]
  
  # Registra as datas de acordo com o critério da linha 199
  df2$data[which(df2$next_order %in% d2_code_b)] <- df2$data_D[which(df2$next_order %in% d2_code_b)]
  
  # Criar um segundo data.frame com as datas da segunda dose (D2 ou D como segunda dose)
  # Calcula a frequência de segunda dose por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  
  df_d2a <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "D2") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Registra quais ids possuem D2 na segunda posição, porém D1 não está presente no registro
  d2_code_c <- c(2,23,234,24,243)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 225
  df2$data[df2$next_order %in% d2_code_c] <- df2$data_D2[df2$next_order %in% d2_code_c]
  
  # Criar um terceiro data.frame com as datas de ids com D2, porém D1 ausente (D2*)
  # Calcula a frequência de D2* por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  
  df_d2b <- df2 %>% 
            drop_na(data) %>% 
            count(data, agegroup) %>% 
            mutate(dose = "D2f") %>%
            rename(date = data) %>%
            complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
                     agegroup, dose,
                     fill = list(n = 0)) %>%
            rename(data = date)
  

  # Datas para Reforço (R)
  
  # Registra quais ids possuem R na terceira posição e D1 presente
  dr_code_a <- c(124,1243)
  
  # Registra quais ids possuem R na terceira posição e D1 ausente
  dr_code_b <- c(24, 243)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 252
  df2$data[df2$next_order %in% dr_code_a] <- df2$data_R[df2$next_order %in% dr_code_a]
  
  # Registra as datas de acordo com o critério da linha 255
  df2$data[which(df2$next_order %in% dr_code_b)] <- df2$data_R[which(df2$next_order %in% dr_code_b)]

  # Criar um quarto data.frame com as datas de ids com R na terceira posição (com D1 ausente ou presente)
  # Calcula a frequência de R por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  
  df_Ra <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Registra quais ids possuem D na terceira posição (entendido como dose de reforço) e D1 presente
  dr_code_c <- c(123,1234) #143 (D1-R-D) e 1432 (D1-R-D-D2) ignorados
  
  # Registra quais ids possuem D na segunda posição (entendido como dose de reforço) e D1 ausente
  dr_code_d <- c(23,234)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 281
  df2$data[df2$next_order %in% dr_code_c] <- df2$data_D[df2$next_order %in% dr_code_c]
  
  # Registra as datas de acordo com o critério da linha 284
  df2$data[which(df2$next_order %in% dr_code_d)] <- df2$data_D[which(df2$next_order %in% dr_code_d)]
  
  # Criar um quinto data.frame com as datas de ids com D como dose de reforço
  # Calcula a frequência de R por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero  
  
  df_Rb <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Limpar cache
  rm(df2);gc()
  
  # Unir todas as tabelas
  
  df_doses <- full_join(df_d1, 
                   df_d2a, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never") %>%
              full_join(df_d2b, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never")  %>%
              full_join(df_Ra, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never")  %>%
              full_join(df_Rb, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never")
            
  # Calcular a cobertura de dose da população por mês
  
  df_month <- df_doses %>%
              mutate(month = as.Date(beginning.of.month(as.character(data)))) %>%
              group_by(month, agegroup, dose) %>% 
              summarise(total = sum(n, na.rm = T)) %>%
              ungroup() %>%
              spread(key = dose, value = total) %>%
              complete(month = seq.Date(as.Date("2021-01-01"), as.Date(beginning.of.month(as.character(data_base))), by="month"), agegroup,
                       fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
              distinct() %>%
              mutate(D1 = D1 - D2,
                     D2 = D2 + D2f - R) %>%
              group_by(agegroup) %>%
              mutate(D1cum = cumsum(D1),
                     D2cum = cumsum(D2),
                     Rcum = cumsum(R),
                     Dcum = cumsum(D)) %>%
              gather(key = "dose", value = "n", -month, -agegroup)  %>%
              mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
              mutate(UF = state)
  
  # Calcular a cobertura de dose da população por semana epidemiológica
  
  df_week <- df_doses %>%
    mutate(week = end.of.epiweek(data)) %>%
    group_by(week, agegroup, dose) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    spread(key = dose, value = total) %>%
    complete(week = seq.Date(end.of.epiweek(as.Date("2021-01-17")), end.of.epiweek(data_base), by= "week"), agegroup,
             fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
    distinct() %>%
    mutate(D1 = D1 - D2,
           D2 = D2 + D2f - R) %>%
    group_by(agegroup) %>%
    mutate(D1cum = cumsum(D1),
           D2cum = cumsum(D2),
           Rcum = cumsum(R),
           Dcum = cumsum(D)) %>%
    select(-D2f) %>%
    gather(key = "dose", value = "n", -week, -agegroup) %>%
    mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
    mutate(UF = state) %>%
    arrange(week, dose, agegroup)
  
  # Unir linhas
  
  da_month <- bind_rows(da_month, df_month)  
  da_week <- bind_rows(da_week, df_week)  
  
  # Salvar dados 
  # Obs: os dados são reescritos para cada novo estado processado. Isto permite reiniciar o código
  # a partir de um determinado ponto caso ele seja interrompido antes de processar os dados de todos os estados
  
  fwrite(da_month, file = "output/doses_cobertura_proporcao_mes.csv")
  fwrite(da_week, file = "output/doses_cobertura_proporcao_semana.csv")
  
  # Salvar log
  
  log_table <- data.frame(before_filter_date = before_filter_date,
                        after_filter_date = after_filter_date,
                        drop_na_dose = drop_na_dose,
                        drop_na_agegroup = drop_na_agegroup,
                        agegroup1  = agegroup1,
                        state = state,
                        indice = 0)
  
  filename <- paste0("log_cobertura_", data_base_title,".csv")
  
  if(any(grepl(paste0("log_cobertura_", data_base_title), list.files(paste0(output_folder,"log/"))))) {
    
    # Acrescenta o log para o arquivo anterior
    
    log_table_todos <- read.csv(paste0(output_folder,"log/",filename), row.names = 1)
    log_table_todos <- bind_rows(log_table_todos, log_table)
    write.csv(log_table_todos, file = paste0(output_folder, "log/", filename))
    
  } else {
    
    # Cria um arquivo novo
    write.csv(log_table, file = paste0(output_folder, "log/", filename))
    
  }
  
}

### Calcular cobertura para SP (estado com split)

# Faz a leitura dos arquivos em formato wide separados (splitted) na pasta /output
files <- list.files("output/wide")[grepl("[1-9].csv", list.files("output/wide"))]

# Tabela de cobertura de doses por mês para arquivos separados
df_month_split <- data.frame()

# Tabela de cobertura de doses por semana para arquivos separados
df_week_split <- data.frame()

indice = 0

# Iniciar loop para os arquivos selecionados
for(i in files) {

  # Salva a sigla do estado, de acordo com o nome do arquivo
  state = substr(i,22,23)
  print(substr(i,22,25))
  
  indice = indice + 1
  
  # Ler tabela em formato wide
  # Esta tabela carrega apenas as datas de D1, D2, Reforço (R) e Dose Janssen (D)
  
  df2 <- data.frame(fread(paste0("output/wide/",i),
                          select = c("data_D1",
                                     "data_D2",
                                     "data_R",
                                     "data_D",
                                     "agegroup"),
                          colClasses = c("data_D1" = "Date",
                                         "data_D2" = "Date",
                                         "data_R" = "Date",
                                         "data_D" = "Date")))

  # Converte valores vazios em NA
  df2[df2==""] <- NA
  
  # Converte coluna de idade em fator
  df2$agegroup <- factor(df2$agegroup, levels = c(1:11))

  agegroup1 <- sum(df2$agegroup==1, na.rm = T)
  
  # Calcular a diferença de tempo em dias entre cada tipo de dose e a data de referência
  before_filter_date <- nrow(df2)
  
  df2 <- df2 %>% mutate(dif1 = as.numeric(data_D1 - first.day),
                        dif2 = as.numeric(data_D2 - first.day),
                        difR = as.numeric(data_R - first.day),
                        difU = as.numeric(data_D - first.day)) %>%
    filter(!(difR %leNAF% dif2)) %>%
    filter(!(dif2 %leNAF% dif1))

  after_filter_date <- nrow(df2)
  
  # Calcular a sequência de datas da dose
  # Divide a tabela em 10 blocos, para processamento de cada bloco separadamente
  cuts <- seq(1,nrow(df2),length.out = 10)
  cuts[1] <- 0
  
  # Calcula a ordem de aplicação de doses para um mesmo id por bloco, e então acrescenta o resultado ao vetor final
  next_order = c()
  for(j in 1:(length(cuts)-1)) {
    
    limits = as.integer(cuts[j:(j+1)]+c(1,0))
    df_temp <- df2[limits[1]:limits[2],]
    next_temp <- apply(df_temp[,c("dif1","dif2","difU","difR")], 1, which_order)
    next_order = c(next_order, next_temp)
    rm(next_temp, df_temp)
  }
  
  df2$next_order = next_order

  # Calcular linhas com NA para dose e agegroup
  drop_na_dose <- sum(is.na(df2$dose))
  drop_na_agegroup <- sum(is.na(df2$agegroup))
  
  # Datas para D1 e D
  
  # Reseta a classificação de doses
  df2$dose <- NA
  
  # Todo id com D1 como primeira dose recebe D1
  df2$dose[df2$next_order %in% c(1,12,13,14,
                                 123,124,132,134,142,143,
                                 1234,1243,1342,1324,1423,1432)] <- "D1"
  
  # Todo id com D como primeira dose recebe D
  df2$dose[df2$next_order %in% c(3,31,32,34,
                                 312,314,321,324,341,342,
                                 3124,3142,3214,3241,3421) ] <- "D"
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  
  # Registra as datas de acordo com a data de D1
  df2$data <- df2$data_D1
  
  # Sobrepõe e registra das datas de acordo com a data de D quando esta for a primeira dose
  df2$data[which(df2$dose == "D")] <- df2$data_D[which(df2$dose == "D")]

  # Criar um primeiro data.frame com as datas de D1 e D
  # Calcula a frequência de D1 e D por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  
  df_d1 <- df2 %>% 
    drop_na(dose) %>% 
    count(data, agegroup, dose) %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  # Datas para D2
  
  # Registra quais ids possuem D2 na segunda posição
  d2_code_a <- c(12,123,124,1234,1243)
  
  # Registra quais ids possuem D na segunda posição, porém a primeira dose não é Janssen
  d2_code_b <- c(13, 132, 134, 1342, 1324)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 491
  df2$data[df2$next_order %in% d2_code_a] <- df2$data_D2[df2$next_order %in% d2_code_a]
  
  # Registra as datas de acordo com o critério da linha 494
  df2$data[which(df2$next_order %in% d2_code_b)] <- df2$data_D[which(df2$next_order %in% d2_code_b)]
  
  # Criar um segundo data.frame com as datas da segunda dose (D2 ou D como segunda dose)
  # Calcula a frequência de segunda dose por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  df_d2a <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "D2") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  # Registra quais ids possuem D2 na segunda posição, porém D1 não está presente no registro
  d2_code_c <- c(2,23,234,24,243)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 519
  df2$data[df2$next_order %in% d2_code_c] <- df2$data_D2[df2$next_order %in% d2_code_c]
  
  # Criar um terceiro data.frame com as datas de ids com D2, porém D1 ausente (D2*)
  # Calcula a frequência de D2* por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  
  df_d2b <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% 
    mutate(dose = "D2f") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  # Datas para Reforço (R)
  
  # Registra quais ids possuem R na terceira posição e D1 presente
  dr_code_a <- c(124,1243)
  
  # Registra quais ids possuem R na terceira posição e D1 ausente
  dr_code_b <- c(24, 243)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 545
  df2$data[df2$next_order %in% dr_code_a] <- df2$data_R[df2$next_order %in% dr_code_a]
  
  # Registra as datas de acordo com o critério da linha 548
  df2$data[which(df2$next_order %in% dr_code_b)] <- df2$data_R[which(df2$next_order %in% dr_code_b)]
  
  # Criar um quarto data.frame com as datas de ids com R na terceira posição (com D1 ausente ou presente)
  # Calcula a frequência de R por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero
  
  df_Ra <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  # Registra quais ids possuem D na terceira posição (entendido como dose de reforço) e D1 presente
  dr_code_c <- c(123,1234) #143 (D1-R-D) e 1432 (D1-R-D-D2) ignorados
  
  # Registra quais ids possuem D na segunda posição (entendido como dose de reforço) e D1 ausente
  dr_code_d <- c(23,234)
  
  # Reseta a data de aplicação de doses para NA
  df2$data <- NA
  class(df2$data) <- "Date"
  
  # Registra as datas de acordo com o critério da linha 574
  df2$data[df2$next_order %in% dr_code_c] <- df2$data_D[df2$next_order %in% dr_code_c]
  
  # Registra as datas de acordo com o critério da linha 577
  df2$data[which(df2$next_order %in% dr_code_d)] <- df2$data_D[which(df2$next_order %in% dr_code_d)]
  
  # Criar um quinto data.frame com as datas de ids com D como dose de reforço
  # Calcula a frequência de R por data e faixa etária (agegroup).
  # Completa os valores faltantes com zero  
  
  df_Rb <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Limpar cache
  rm(df2);gc()
  
  # Unir todas as tabelas
  
  df_doses <- full_join(df_d1, 
                        df_d2a, 
                        by = c("data","agegroup","dose","n"), 
                        na_matches = "never") %>%
    full_join(df_d2b, 
              by = c("data","agegroup","dose","n"), 
              na_matches = "never")  %>%
    full_join(df_Ra, 
              by = c("data","agegroup","dose","n"), 
              na_matches = "never")  %>%
    full_join(df_Rb, 
              by = c("data","agegroup","dose","n"), 
              na_matches = "never")
  
  # Calcular a cobertura de dose da população por mês

  df_month <- df_doses %>%
    mutate(month = as.Date(beginning.of.month(as.character(data)))) %>%
    group_by(month, agegroup, dose) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    spread(key = dose, value = total) %>%
    complete(month = seq.Date(as.Date("2021-01-01"), as.Date(beginning.of.month(as.character(data_base))), by="month"), agegroup,
             fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
    distinct() %>%
    mutate(D1 = D1 - D2,
           D2 = D2 + D2f - R) %>%
    group_by(agegroup) %>%
    mutate(D1cum = cumsum(D1),
           D2cum = cumsum(D2),
           Rcum = cumsum(R),
           Dcum = cumsum(D)) %>%
    gather(key = "dose", value = "n", -month, -agegroup)  %>%
    mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
    mutate(UF = state)
  
  # Calcular a cobertura de dose da população por semana epidemiológica

  df_week <- df_doses %>%
    mutate(week = end.of.epiweek(data)) %>%
    group_by(week, agegroup, dose) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    spread(key = dose, value = total) %>%
    complete(week = seq.Date(end.of.epiweek(as.Date("2021-01-17")), end.of.epiweek(data_base), by= "week"), agegroup,
             fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
    distinct() %>%
    mutate(D1 = D1 - D2,
           D2 = D2 + D2f - R) %>%
    group_by(agegroup) %>%
    mutate(D1cum = cumsum(D1),
           D2cum = cumsum(D2),
           Rcum = cumsum(R),
           Dcum = cumsum(D)) %>%
    select(-D2f) %>%
    gather(key = "dose", value = "n", -week, -agegroup) %>%
    mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
    mutate(UF = state) %>%
    arrange(week, dose, agegroup)
  
  # ACrescentar linhas de dados processados de cada arquivo
  df_month_split <- bind_rows(df_month_split, df_month)  
  df_week_split <- bind_rows(df_week_split, df_week)  
  
  # Salvar log
  
  log_table <- data.frame(before_filter_date = before_filter_date,
                          after_filter_date = after_filter_date,
                          drop_na_dose = drop_na_dose,
                          drop_na_agegroup = drop_na_agegroup,
                          agegroup1  = agegroup1,
                          state = state,
                          indice = indice)
  
  filename <- paste0("log_cobertura_", data_base_title,".csv")
  
  if(any(grepl(paste0("log_cobertura_", data_base_title), list.files(paste0(output_folder,"log/"))))) {
    
    # Acrescenta o log para o arquivo anterior
    
    log_table_todos <- read.csv(paste0(output_folder,"log/",filename), row.names = 1)
    log_table_todos <- bind_rows(log_table_todos, log_table)
    write.csv(log_table_todos, file = paste0(output_folder, "log/", filename))
    
  } else {
    
    # Cria um arquivo novo
    write.csv(log_table, file = paste0(output_folder, "log/", filename))
    
  }
  
}

  # Recalcular a frequência de doses por mês, faixa etária (agegroup), dose, e estado (UF) dos arquivos separados
df_month <- df_month_split %>%
  group_by(month, agegroup, dose,UF) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  complete(month = seq.Date(as.Date("2021-01-01"), as.Date(beginning.of.month(as.character(data_base))), by="month"), 
           agegroup, dose, UF,
           fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0, D1cum = 0, D2cum = 0, Rcum = 0, Dcum = 0))

  # Recalcular a frequência de doses por semana epidemiológica, faixa etária (agegroup), dose, e estado (UF) dos arquivos separados
df_week <- df_week_split %>%
  group_by(week, agegroup, dose,UF) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  complete(week = seq.Date(end.of.epiweek(as.Date("2021-01-17")), end.of.epiweek(data_base), by="week"), 
         agegroup, dose, UF,
         fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0, D1cum = 0, D2cum = 0, Rcum = 0, Dcum = 0))

##### Unir todas as bases (estados com e sem split)

da_month <- bind_rows(da_month, df_month) %>% distinct()
da_week <- bind_rows(da_week, df_week) %>% distinct()

# Define a data do mês para o primeiro dia do próximo mês e filtra dados para faixa etária < 5 anos (agegroup  = 1)

da_month <- da_month %>%
  mutate(month = beginning.of.month(as.character(month + 32))) %>%
  drop_na(month, agegroup) %>%
  filter(agegroup != 1)

# Filtra dados para faixa etária < 5 anos (agegroup  = 1)
da_week <- da_week %>%
  drop_na(week, agegroup) %>%
  filter(agegroup != 1)

# Salvar arquivos de saída

fwrite(da_month, file = "output/doses_cobertura_proporcao_mes.csv")
fwrite(da_week, file = "output/doses_cobertura_proporcao_semana.csv")

################
### Plots
###############

### Preparar os dados para plotar gráficos

data = da_month %>%
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(month = as.Date(month)) %>%
  group_by(month, agegroup, dose) %>%
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
                       ordered = TRUE),
         agegroup = factor(agegroup, levels = 2:11,
                          labels = c("5 a 11",
                                    "12 a 17",
                                    "18 a 29",
                                    "30 a 39",
                                    "40 a 49",
                                    "50 a 59",
                                    "60 a 69",
                                    "70 a 79",
                                    "80 a 89",
                                    "90+")))

# Gráfico de cobertura de doses por mês e faixa etária

g1 <- ggplot(data, aes(x = month, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g1, file = "figuras/aplicacao_doses_mes.png", width = 12, height = 8)

###
data2 = da_month %>% 
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(month = as.Date(month)) %>%
  group_by(month, UF, dose) %>% 
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
                       ordered = TRUE))

# Gráfico de cobertura de doses por mês e UF

g2 <- ggplot(data2, aes(x = month, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g2, file = "figuras/aplicacao_doses_uf_mes.png", width = 18, height = 12)

# Gráfico de cobertura de doses por semana epidemiológica e faixa etária

data3 = da_week %>%
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(week = as.Date(week)) %>%
  group_by(week, agegroup, dose) %>% summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
                       ordered = TRUE),
         agegroup = factor(agegroup, levels = 2:11,
                           labels = c("5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+")))

g3 <- ggplot(data3, aes(x = week, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g3, file = "figuras/aplicacao_doses_semana.png", width = 12, height = 8)

# Gráfico de cobertura de doses por semana epidemiológica e UF

data4 = da_week %>%
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(week = as.Date(week)) %>%
  group_by(week, UF, dose) %>% 
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
                       ordered = TRUE))

g4 <- ggplot(data4, aes(x = week, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g4, file = "figuras/aplicacao_doses_uf_semana.png", width = 18, height = 12)

# Retorna o tempo total de execução do script

fin = Sys.time()
fin - ini