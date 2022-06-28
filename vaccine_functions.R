suppressPackageStartupMessages({
if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(scales)){install.packages("scales"); library(scales)}
if(!require(optparse)){install.packages("scales"); library(optparse)}
})

#' Dose nomes:
#' Classificador de doses de acordo com o campo vacina_descricao_dose do SI-PNI
#' @x: character. Descrição da dose

doses_nomes <- function(x){
  
  if(grepl("Reforço",x,ignore.case = T)){
    return("R")
  }
  
  if(grepl("1",x)){
    return("D1")
  }
  
  if(grepl("2",x)){
    return("D2")
  }
  
  if(grepl("3",x)){
    return("3")
  }
  
  if(grepl("4",x)){
    return("4")
  }
  
  if(grepl("5",x)){
    return("5")
  }
  
  if(grepl("^Dose$",x,ignore.case = T)){
    return("D")
  }
  
  if(grepl("Adicional",x,ignore.case = T)){
    return("DA")
  }
  
  if(grepl("Única",x,ignore.case = T)){
    return("D")
  }
  
  if(grepl("Inicial",x,ignore.case = T)){
    return("D")
  }
  
  return(NA)
}

#' prepare_table:
#' Lê os arquivos brutos de dados do PNI-SI, seleciona apenas as colunas relevantes
#' e realiza a limpeza e reorganização dos dados.
#' @estado: character. Sigla do estado do Brasil para leitura.
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' @data_base: character. Data da base de dados, no formato %Y-%m-%
#' @split: logical. Adequa o processamento de dados caso os arquivos a serem processados por Estado estão divididos em múltiplos arquivos ou não

prepare_table <- function(estado,
                          input_folder = "dados/",
                          output_folder = "output/",
                          data_base = "2022-02-02",
                          split = F) {

    if(split){
    
    #  Padrão para o nome do arquivo a ser lido
    pattern <- paste0("split_sorted_limpo_dados_",data_base,"_",estado)
    
    # Arquivos na pasta de entrada que correspondem ao padrão
    arquivos <- list.files(input_folder,pattern)
    
    # O objeto indice cria um loop para os múltiplos arquivos de um mesmo estado a serem processados
    indice = 0
    
    for(arquivo in arquivos){
    
    # Leitura do arquivo  
      todas_vacinas <- fread(paste0(input_folder,arquivo), 
                             select = c("paciente_id", "paciente_dataNascimento", "vacina_dataAplicacao", 
                                        "vacina_descricao_dose", "vacina_codigo"),
                             colClasses = c("paciente_id" = "factor",
                                            "paciente_dataNascimento" = "Date",
                                            "vacina_dataAplicacao" = "Date",
                                            "vacina_descricao_dose" = "character",
                                            "vacina_codigo" = "integer"), 
                             encoding = "UTF-8") %>%
        data.frame()
      
      print(paste0(estado, " ", indice, " of ", length(arquivos), ": succesfully loaded. Preparing data..."))
      
      # Atualiza o valor do índice
      indice = indice + 1
      
      # Padroniza os códigos das vacinas
      # Atenção: Se for utilizado no banco de dados um novo código para vacina (campo vacina_codigo), esta parte do código de verá ser atualizada
      
      todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 89] <- 85
      todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 98] <- 86
      todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 99] <- 87
      
      # Remove registros duplicados
      todas_vacinas <- todas_vacinas %>% 
        distinct(paciente_id,vacina_descricao_dose,.keep_all = TRUE) %>% 
        distinct(paciente_id,vacina_dataAplicacao,.keep_all = TRUE)
  
      # Classifica as doses e converte em fator
      todas_vacinas$dose <- as.factor(sapply(todas_vacinas$vacina_descricao_dose,doses_nomes))
      
      # Salva registro de tipos de doses por estado e a classificação correspondente
      
      data_base_title <- format(as.Date(data_base), format = "%Y_%m_%d")
      
      filename <- paste0("doses_", data_base_title,".csv")
      
      contagem_dose <- todas_vacinas %>% 
        count(vacina_descricao_dose, dose) %>% 
        mutate(UF = estado)
      
      # Salva o registro de doses em um arquivo .csv para a data de extração.
      # Caso o arquivo não exista ainda, cria um arquivo novo.
      # Caso o arquivo já exista, agrega os dados em relação ao arquivo anterior.
      
      if(any(grepl(data_base_title, list.files(paste0(output_folder,"dose_types/"))))) {
        
        contagem_dose_todos <- read.csv(paste0(output_folder,"dose_types/",filename))
        contagem_dose_todos <- bind_rows(contagem_dose_todos, contagem_dose)
        contagem_dose_todos <- contagem_dose_todos %>% 
                                  group_by(vacina_descricao_dose, dose, UF) %>%
                                  summarise(n = sum(n))
      
        # Salva o arquivo com o registro de doses e sua classificação
        write.csv(contagem_dose_todos, file = paste0(output_folder,"dose_types/",filename))
        
      } else {
        
        # Salva o arquivo com o registro de doses e sua classificação
        write.csv(contagem_dose, file = paste0(output_folder,"dose_types/",filename))
      }
      
      # Limpeza de dados
      # Filtra apenas registros entre a data da base e a data de início da campanha de vacinação
      todas_vacinas <- todas_vacinas %>% 
        filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao >= as.Date("2021-01-17"))
      
      # Remove arquivos com informações faltantes (NA)
      todas_vacinas <- todas_vacinas %>% drop_na()
      
      # Filtra pacientes com data de nascimento registrada antes de 1900
      todas_vacinas <- todas_vacinas %>% filter(paciente_dataNascimento > "1900-01-01")
      
      ##
      print("Preparing data... part 2")
      
      ##########################
      
      # Filtra apenas registros com os códigos das 4 vacinas
      # Atenção: Se for utilizado no banco de dados um novo código para vacina (campo vacina_codigo), esta parte do código de verá ser atualizada
      
      todas_vacinas <- todas_vacinas %>% 
        filter(vacina_codigo %in% c(85,86,87,88)) %>% 
        select(-vacina_descricao_dose)
      
      # Atualiza o nome das colunas para tornar o código mais limpo
      colnames(todas_vacinas) <- c("id", "nasc", "data","vacina","doses")
      
      # Filtra registros com mais de 6 doses por id_individuo
      
      todas_vacinas <- todas_vacinas %>% 
                          group_by(id) %>% 
                          mutate(n = n()) %>% 
                          ungroup() %>% 
                          filter(n < 6)
      
      # Filtra se tiver mais de um tipo de dose por id_individuo
      
      ## Encontra ids com mais deu um tipo de dose por id_individuo
      remove_ids <- todas_vacinas %>% 
        group_by(id, doses) %>% 
        mutate(m = n()) %>% 
        ungroup() %>%
        filter(m > 1) %>%
        select(id)
      
      ## Caso encontre id_individuos com esta condição, remove do banco de dados
      if(nrow(remove_ids)>0) {
        todas_vacinas = todas_vacinas %>% 
          filter(!(id %in% remove_ids$id)) %>% 
          mutate(id = droplevels(id))
      }
      
      ## Remove objeto com identificação dos id_individuos com repetição do doses, e limpa memória ram
      rm(remove_ids);gc()
      
      # Calcula a idade no momento da primeira dose
      
      todas_vacinas <- todas_vacinas %>% 
        group_by(id) %>% 
        mutate(nasc = min(nasc, na.rm = T)) %>%
        ungroup() %>%
        mutate(idade = as.numeric(data - nasc) %/% 365.25) %>% 
        select(-nasc)
      
      # Salva dados
      fwrite(todas_vacinas, file = paste0(output_folder, estado,"_",indice, "_PNI_clean.csv"))
    }
  } else {
    
    # Esta parte do script é semelhante ao anterior, porém realiza o processamento dos dados dos estados com apenas um arquivo

    # Leitura do arquivo      
    todas_vacinas <- fread(paste0(input_folder,"limpo_dados_",data_base,"_",estado,".csv"), 
                           select = c("paciente_id", "paciente_dataNascimento", "vacina_dataAplicacao", 
                                      "vacina_descricao_dose", "vacina_codigo"),
                           colClasses = c("paciente_id" = "factor",
                                          "paciente_dataNascimento" = "Date",
                                          "vacina_dataAplicacao" = "Date",
                                          "vacina_descricao_dose" = "character",
                                          "vacina_codigo" = "integer"), 
                           encoding = "UTF-8") %>%
                           data.frame()
    
    print(paste0(estado, " data succesfully loaded. Preparing data... 1"))
    
    # Padroniza os códigos das vacinas
    # Atenção: Se for utilizado no banco de dados um novo código para vacina (campo vacina_codigo), esta parte do código de verá ser atualizada
    todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 89] <- 85
    todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 98] <- 86
    todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 99] <- 87
    
    # Remove registros duplicados
    todas_vacinas <- todas_vacinas %>% 
      distinct(paciente_id,vacina_descricao_dose,.keep_all = TRUE) %>% 
      distinct(paciente_id,vacina_dataAplicacao,.keep_all = TRUE)
    
    # Classifica as doses e converte em fator
    todas_vacinas$dose <- as.factor(sapply(todas_vacinas$vacina_descricao_dose,doses_nomes))
    
    # Salva registro de tipos de doses por estado e a classificação correspondente
    data_base_title <- format(as.Date(data_base), format = "%Y_%m_%d")
    
    filename <- paste0("doses_", data_base_title,".csv")
    
    contagem_dose <- todas_vacinas %>% 
      count(vacina_descricao_dose, dose) %>% 
      mutate(UF = estado)
    
    # Salva o registro de doses em um arquivo .csv para a data de extração.
    # Caso o arquivo não exista ainda, cria um arquivo novo.
    # Caso o arquivo já exista, agrega os dados em relação ao arquivo anterior.
    
    if(any(grepl(data_base_title, list.files(paste0(output_folder,"dose_types/"))))) {
      
      contagem_dose_todos <- read.csv(paste0(output_folder,"dose_types/",filename))
      contagem_dose_todos <- bind_rows(contagem_dose_todos, contagem_dose)
      contagem_dose_todos <- contagem_dose_todos %>% 
        group_by(vacina_descricao_dose, dose, UF) %>%
        summarise(n = sum(n))

      # Salva o arquivo com o registro de doses e sua classificação      
      write.csv(contagem_dose_todos, file = paste0(output_folder,"dose_types/",filename))
      
    } else {
      
      # Salva o arquivo com o registro de doses e sua classificação
      write.csv(contagem_dose, file = paste0(output_folder,"dose_types/",filename))
    }
    
    # Limpeza de dados
    # Filtra apenas registros entre a data da base e a data de início da campanha de vacinação
    todas_vacinas <- todas_vacinas %>% 
      filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao >= as.Date("2021-01-17"))
    
    # Remove arquivos com informações faltantes (NA)
    todas_vacinas <- todas_vacinas %>% drop_na()
    
    # Filtra pacientes com data de nascimento registrada antes de 1900
    todas_vacinas <- todas_vacinas %>% filter(paciente_dataNascimento > "1900-01-01")
    
    ##
    print(paste0("Preparing data... 2 (", estado, ")"))
    
    # Filtra apenas registros com os códigos das 4 vacinas
    # Atenção: Se for utilizado no banco de dados um novo código para vacina (campo vacina_codigo), esta parte do código de verá ser atualizada
    
    todas_vacinas <- todas_vacinas %>% 
                      filter(vacina_codigo %in% c(85,86,87,88)) %>% 
                      select(-vacina_descricao_dose)
    
    # Atualiza o nome das colunas para tornar o código mais limpo
    colnames(todas_vacinas) <- c("id", "nasc", "data","vacina","doses")
    
    # Filtra registros com mais de 6 doses por id_individuo
    todas_vacinas <- todas_vacinas %>% 
                        group_by(id) %>%
                        mutate(n = n()) %>%
                        ungroup() %>%
                        filter(n < 6)
    
    # Filtra se tiver mais de um tipo de dose por id_individuo
    
    ## Encontra ids com mais deu um tipo de dose por id_individuo      
    remove_ids <- todas_vacinas %>% 
      group_by(id, doses) %>% 
      mutate(m = n()) %>% 
      ungroup() %>%
      filter(m > 1) %>%
      select(id)
    
    ## Caso encontre id_individuos com esta condição, remove do banco de dados
    if(nrow(remove_ids)>0) {
      todas_vacinas = todas_vacinas %>% 
        filter(!(id %in% remove_ids$id)) %>% 
        mutate(id = droplevels(id))
    }
  
  ## Remove objeto com identificação dos id_individuos com repetição do doses, e limpa memória ram
  rm(remove_ids);gc()
  
  # Calcula a idade no momento da primeira dose
  todas_vacinas <- todas_vacinas%>% 
      group_by(id) %>% 
      mutate(nasc = min(nasc, na.rm = T)) %>%
      ungroup() %>%
      mutate(idade = as.numeric(data - nasc) %/% 365.25) %>% 
      select(-nasc)
  
  # Salva dados  
  filename = paste0(output_folder, estado, "_PNI_clean.csv")
  print(paste0("Salvando: ", filename))
  fwrite(todas_vacinas, file = filename)
  }
}

#' prepara_historico:
#' Prepara e salva duas tabela com historico entre as doses.
#' A primeira tabela indica a frequencia de individuos em função 
#' do tempo de vacina de acordo com vacina, grupo etário e 
#' tempo entre data da primeira dose e data da base de dados.
#' A segunda tabela indica a frequencia de individuos de cada faixa etaria
#' que já receberam a segunda dose.
#' 
#' @estado: character. Sigla do estado do Brasil
#' @data_base: Date. Data da base de dados.
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' @split: logical. Adequa o processamento de dados caso os arquivos a serem processados por Estado estão divididos em múltiplos arquivos ou não
#' 
#' Details: a função salva as tabelas de output em um arquivo .csv
#' Para o arquivo doses_aplicadas.csv, as faixas etárias são divididas de duas formas, em duas colunas:
#' 
#' ag_child: 0-4 anos, 5-11 anos, 12-17 anos, 18-29 anos, 
#'                 30-39 anos, 40-49 anos, 50-59 anos, 60-69 anos,
#'                 70-79 anos, 80-89 anos, 90 anos ou mais
#'  Esta divisão leva em conta as divisões específicas para faixas etárias de crianças
#'                                
#' ag_10:             0-9 anos, 10-19 anos, 20-29 anos,
#'                 30-39 anos, 40-49 anos, 50-59 anos, 60-69 anos,
#'                 70-79 anos, 80-89 anos, 90 anos ou mais         
#'

prepara_historico <- function(estado = "SP", 
                              data_base = as.Date("2022-01-15"),
                              input_folder = "output/",
                              output_folder = "output/",
                              split = FALSE) {
  
  # Caso data_base seja objeto de texto (character), converte o objeto data_base em Date
  if(class(data_base) == "character") data_base <- as.Date(data_base)
  
  # Roda o script para os múltiplos arquivos de um mesmo estado, caso esteja separado
  
  if(split) {
  
  #  Padrão para o nome dos arquivos a serem lidos
  splited_files <- grep("[1-9]_PNI_clean.csv", list.files(input_folder), value = T)
  
  # Arquivos na pasta de entrada que correspondem ao padrão
  files <- grep(estado, splited_files, value = T)
  
  # Executa o script para cada arquivo separadamente
  for(j in 1:length(files)) {
  
  # Imprime na tela o arquivo sendo processado  
  print(files[j])
  
  # Leitura do arquivo
  todas_vacinas <- fread(paste0(input_folder,files[j]), 
                          colClasses = c("id" = "factor",
                                         "data" = "character", "vacina" = "integer","n" = "integer",
                                         "doses" = "factor","idade" = "integer"), encoding = "UTF-8")
  
  # Converte a coluna 'data' em Date. A leitura é feito como character, para otimizar o tempo de processamento
  todas_vacinas$data <- as.Date(todas_vacinas$data)
  
  #
  todas_vacinas <- todas_vacinas %>% 
    select(-n) %>%
    filter(data > "2021-01-01") %>%             # Limpeza de datas de aplicação anteriores a 2021
    filter(data <= data_base) %>%               # Limpeza de datas de aplicação posteriores à data da base
    drop_na(vacina, idade, data, doses) %>%     # Remove colunas com informações incompletas (NA)
    
    # Classificação das vacinas de acordo com os códigos
    mutate(vacina = factor(vacina, levels = c(85,86,87,88), labels = c("AZ","Coronavac","Pfizer","Janssen")),
           
    # Classificação da faixa etária considerando intervalo para crianças       
           agegroup = factor(cut(idade, 
                                 breaks = c(0,5,12,18,seq(30,90,10),Inf),
                                 include.lowest = T, 
                                 right = F,
                                 labels = F)),
    
    # Classificação da faixa etária (ag_10), intervalos de 10 em 10 anos
           agegroup_10 = factor(cut(idade, 
                                    breaks = c(seq(0,90,10),Inf),
                                    include.lowest = T, 
                                    right = F,
                                    labels = F)))
  
  # Tabela com classificação da faixa etária considerando intervalo para crianças
  tabela_child <- todas_vacinas %>% 
    rename(date = data) %>%
    count(vacina, agegroup,date,doses, .drop = FALSE) %>% 
    mutate(type = "ag_child") %>%
    drop_na(vacina, agegroup, date, doses) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"),
             vacina, agegroup, doses, type,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Tabela com classificação da faixa etária com intervalos de 10 em 10 anos
  tabela_10 <- todas_vacinas %>%
    rename(date = data) %>%
    count(vacina, agegroup_10, date, doses, .drop = FALSE) %>% 
    mutate(type = "ag_10") %>%
    drop_na(vacina, agegroup_10, date, doses) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"),
             vacina, agegroup_10, doses, type,
             fill = list(n = 0)) %>%
    rename(data = date) %>%
    rename(agegroup = agegroup_10)
  
  # Junção das duas tabelas em uma única

  tabela_final <- bind_rows(tabela_child, tabela_10) %>% 
    spread(key = type, value = n) %>% 
    arrange(data, agegroup, vacina, doses)
  
  # Nome do arquivo de saída
  filename = paste0(output_folder,"doses_aplicadas/doses_aplicadas_",estado,"_",j,".csv")
  
  # Salva arquivo de saída
  print(paste0("Salvando: ",filename))
  fwrite(tabela_final, file= filename)

  ########
  ### Preparar tabela em formato wide
  ########
  
  # Salva idade de acordo com id_individuo
  tabela_id_idade <- todas_vacinas %>% 
                        select(id, agegroup) %>% 
                        distinct()
  
  # Divide a tabela em intervalos de 3*10^6 linhas, para processamento de cada bloco separadamente
  cut_points <- as.integer(c(seq(1,  nrow(todas_vacinas), 3*10^6), nrow(todas_vacinas)))
  
  tabela_wide_split = tibble()
  
  # Converte cada bloco em formato wide, e então une cada bloco em uma única tabela  
  for(i in 1:(length(cut_points)-1)){
      
      print(c(i,j,cut_points[i]))
      
      tabela_wide_split_temp <- todas_vacinas[cut_points[i]:(cut_points[i+1]-1),] %>%
        select(-agegroup, -idade) %>%
        pivot_wider(id_cols = id,
                    names_from = doses,
                    values_from = c(data, vacina),
                    values_fn = first,
                    values_fill = NA)
      
      tabela_wide_split <- bind_rows(tabela_wide_split, tabela_wide_split_temp)
    }
  
  # Une a tabela em formato wide com a classificação da faixa etária de cada grupo  
  tabela_wide_split = tabela_wide_split %>%
                        right_join(tabela_id_idade, by = "id", na_matches = "never") %>% 
                        select(-id)
  
  # Remove tabela 'todas_vacinas' e limpa a memória ram
  rm(todas_vacinas);gc()
  
  # Salva o arquivo de saída
  filename = paste0(output_folder,"wide/wide_doses_aplicadas_",estado,"_",j,".csv")
  
  print(paste0("Salvando: ",filename))
  fwrite(tabela_wide_split, file = filename)
  
   } #j
    
  } else {
  # Processa os dados para estados sem divisão do banco em diversos arquivos
  
  # Leitura do arquivo  
  todas_vacinas <- fread(paste0(input_folder,estado, "_PNI_clean.csv"), 
                          colClasses = c("id" = "factor",
                                         "data" = "character", "vacina" = "integer","n" = "integer",
                                         "doses" = "factor","idade" = "integer"), encoding = "UTF-8")
  
  # Converte a coluna 'data' em Date. A leitura é feito como character, para otimizar o tempo de processamento  
  todas_vacinas$data <- as.Date(todas_vacinas$data)

  # Limpeza e reclassificação dos dados
  
  todas_vacinas <- todas_vacinas %>% 
    select(-n) %>%
    filter(data > "2021-01-01") %>%           # Limpeza de datas de aplicação anteriores a 2021
    filter(data <= data_base) %>%             # Limpeza de datas de aplicação posteriores à data da base
    drop_na(vacina, idade, data, doses) %>%   # Remove colunas com informações incompletas (NA)
    
    # Classificação das vacinas de acordo com os códigos
    mutate(vacina = factor(vacina, levels = c(85,86,87,88), 
                           labels = c("AZ","Coronavac","Pfizer","Janssen")),
           
    # Classificação da faixa etária considerando intervalo para crianças              
    agegroup = factor(cut(idade, 
                   breaks = c(0,5,12,18,seq(30,90,10),Inf),
                   include.lowest = T, 
                   right = F,
                   labels = F)),
    
    # Classificação da faixa etária (ag_10), intervalos de 10 em 10 anos
    agegroup_10 = factor(cut(idade, 
                    c(seq(0,90,10),Inf),
                    include.lowest = T, 
                    right = F,
                    labels = F)))
  
  # Tabela com classificação da faixa etária considerando intervalo para crianças
  tabela_child <- todas_vacinas %>% 
                    rename(date = data) %>%
                    count(vacina, agegroup,date,doses, .drop = FALSE) %>% 
                    mutate(type = "ag_child") %>%
                    drop_na(vacina, agegroup, date, doses) %>%
                    complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"),
                      vacina, agegroup, doses, type,
                             fill = list(n = 0)) %>%
                    rename(data = date)

  # Tabela com classificação da faixa etária com intervalos de 10 em 10 anos
  tabela_10 <- todas_vacinas %>%
                  rename(date = data) %>%
                  count(vacina, agegroup_10, date, doses, .drop = FALSE) %>% 
                  mutate(type = "ag_10") %>%
                  drop_na(vacina, agegroup_10, date, doses) %>%
                  complete(date = seq.Date(as.Date("2021-01-17"), as.Date(data_base), by="day"),
                           vacina, agegroup_10, doses, type,
                           fill = list(n = 0)) %>%
                rename(data = date) %>%
                rename(agegroup = agegroup_10)
  
  # Junção das duas tabelas em uma única
  tabela_final <- bind_rows(tabela_child, tabela_10) %>% 
            spread(key = type, value = n) %>% 
            arrange(data, agegroup, vacina, doses)
  
  # Nome do arquivo de saída
  filename = paste0(output_folder,"doses_aplicadas/doses_aplicadas_",estado,".csv")
  
  # Salva arquivo de saída
  print(paste0("Salvando: ",filename))
  fwrite(tabela_final, file = filename)
  
  ########
  ### Preparar tabela em formato wide
  ########
  
  # Salva idade de acordo com id_individuo
  tabela_id_idade <- todas_vacinas %>% 
                        select(id, agegroup) %>% 
                        distinct()
  
  # Converte em formato wide
  tabela_wide = todas_vacinas %>%
    select(-agegroup, -idade) %>%
    pivot_wider(id_cols = id,
                names_from = doses,
                values_from = c(data, vacina),
                values_fn = first,
                values_fill = NA) %>%
    
  # Une a tabela em formato wide com a classificação da faixa etária de cada grupo    
    left_join(tabela_id_idade, by = "id",
              na_matches = "never") %>%
    select(-id)
  
  # Remove tabela 'todas_vacinas' e limpa a memória ram
  rm(todas_vacinas);gc()
  
  # Salva o arquivo de saída
  filename = paste0(output_folder,"wide/wide_doses_aplicadas_",estado,".csv")
  
  print(paste0("Salvando: ",filename))
  fwrite(tabela_wide, file = filename)
  }
}

#' join_historico: 
#' Faz a união das tabelas que foram separadas para estados com bancos de dados grandes
#' 
#' @estado: character. Sigla do estado do Brasil
#' @data_base: Date. Data da base de dados.
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' 
#' @return: a função não retorna nenhum output para o environment. 
#' Porém, salvar três arquivos diferentes:
#' * doses_aplicadas_{estado}.csv: tabela com histórico de doses aplicadas por data, vacina, 
#' grupo etário e doses
#' * wide_doses_aplicadas_{estado}.csv: tabela com vacina, grupo etário e datas de aplicação
#' de cada dose por indivíduo (id)
#' * tempo_d2_reforco_{estado}.csv: tabela com vacina, grupo etário e datas de aplicação
#' de cada dose por indivíduo (id), apenas para aqueles que receberam a segunda dose mas
#' não receberam a dose de reforço 

join_historico <- function(estado = "SP",
                           input_folder = "output/",
                           output_folder = "output/") {

  ### Cria um conjunto de dados com as seguintes variáveis (tabela de formato long):
  ### vacina, agegroup, data, doses, n
  
  # Encontrar aquivos separados
  splited_files <- grep("^doses_aplicadas_.+[1-9].csv", list.files(paste0(input_folder,"doses_aplicadas/")), value = T)
  files <- grep(estado, splited_files, value = T)
  
  # Agregar dados em uma única tabela
  doses_aplicadas <- tibble()
  
  for(j in files) {
    print(paste0("Reading: ",j))
    df = tibble(fread(paste0(input_folder,"doses_aplicadas/",j)))
    df[is.na(df)] <- NA
    doses_aplicadas <- rbind(doses_aplicadas, df)
  }
  
  # Soma os valores de acordo com os grupos
  tabela <- doses_aplicadas %>%
    group_by(vacina, agegroup, data, doses, .drop = FALSE) %>%
    summarise(ag_10    = sum(ag_10, na.rm = T),
              ag_child = sum(ag_child, na.rm = T))
  
  # Salvar dados agregados
  filename = paste0(output_folder,"doses_aplicadas/doses_aplicadas_",estado,".csv")
  print(paste0("Saving: ",filename))
  fwrite(tabela, file = filename)
  
  ### Crie um conjunto de dados com as seguintes variáveis (tabela de formato wide):
  ### data_D1, data_D2, data_R, data_DU, vacina_D1, vacina_D2, vacina_R, vacina_DU, agegroup
  
  # Localiza arquivos separados
  splited_files <- grep("wide_doses_aplicadas_.+[1-9].csv", list.files(paste0(input_folder,"wide/")), value = T)
  files <- grep(estado, splited_files, value = T)
  
  # Agregar dados em uma única tabela
  wide_doses <- tibble()
  for(j in files) {
    print(paste0("Reading: ",j))
    df = tibble(fread(paste0(input_folder,"wide/",j)))
    df[is.na(df)] <- NA
    wide_doses <- rbind(wide_doses, df)
  }
  
  # Salvar dados agregados
  filename = paste0(output_folder,"wide/wide_doses_aplicadas_",estado,".csv")
  print(paste0("Saving: ",filename))
  fwrite(wide_doses, file = filename)
  
  print(paste0(estado," done."))
}

################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--command",
                help = ("Comando a ser feito: prepara_dado | prepara_cobertura"),
                metavar = "command"),
    make_option("--split", default = "FALSE",
                help = ("Booleano. Dados são quebrados em partes?"),
                metavar = "split"),
    make_option("--dataBase",
                help = ("Data da base de dados, formato 'yyyy-mm-dd'"),
                metavar = "dataBase"),
    make_option("--estado",
                help = ("Sigla do estado"),
                metavar = "estado"),
    make_option("--input_folder", default = "dados/",
                help = ("Pasta de dados de entrada"),
                metavar = "input_folder"),
    make_option("--output_folder", default = "output/",
                help = ("Pasta de dados de saída"),
                metavar = "output_folder")
  )
  parser_object <- OptionParser(usage = "Rscript %prog --command comando --estado UF --dataBase yyyy-mm-dd --split TRUE|FALSE \n",
                                option_list = option_list,
                                description = "Script para processar banco de dados de vacinção do SI-PNI.")

  ## TO TEST INTERACTIVELY the command-line arguments
  #input <- " --command prepara_dado --estado AC --dataBase 2022-01-26"
  #command.args <- strsplit(input, " ")[[1]]
  #opt <- parse_args(parser_object, args = command.args, positional_arguments = TRUE)
  ## SKIP opt line below

  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE),
                    positional_arguments = TRUE)
  ## aliases
  command <- opt$options$command
  estado <- opt$options$estado
  split <- opt$options$split
  dataBase <- opt$options$dataBase
  input_folder <- opt$options$input_folder
  output_folder <- opt$options$output_folder

  if (length(command) == 0 || length(estado) == 0 || length(dataBase) == 0) {
    print("Argumento de entrada faltando! Saindo...")
    quit(save = "no", status = 1)
  }

  # quit on error when run non-interactively #small change because this is killing my local sessions T_T
  if (!interactive()) options(error = function() quit(save = "no", status = 1))

  ### roda comando
  if (command == "prepara_dado") {
    prepare_table(estado, data_base = dataBase, split = split,
                  input_folder = input_folder, output_folder = output_folder)
  } else if (command == "prepara_cobertura") {
    prepara_historico(estado, data_base = dataBase, split = split,
                  input_folder = output_folder, output_folder = output_folder)
  } else if (command == "prepara_cobertura_split") {
    prepara_historico(estado, data_base = dataBase, split = split,
                      input_folder = output_folder, output_folder = output_folder)
    join_historico(estado,
                   input_folder = output_folder, output_folder = output_folder)
  } else {
    print(paste("Comando", command, "não encontrado."))
  }
}

