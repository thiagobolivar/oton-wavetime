
## Função para calcular o leadtime em na unidade de tempo desejada
## start - timestamp do inicio do leadetime
## end - timestamp do fim do leadtime
calc_leadtime = function (start, end, days_off) {
  
  start_date <- ymd_hms(start)
  end_date <-  ymd_hms(end)
  
  # Calcular o intervalo de tempo entre as datas de início e fim
  check_interval <- interval(start_date, end_date)
  
  # Converter o intervalo de tempo para horas e retornar o resultado
  check_time <- as.period(check_interval, unit = "days")
  
  is_same_date <- as.Date(start_date) == as.Date(end_date)
    
  ## Se o período é menor que 1 dia, retorna o interval em horas
  if (is_same_date == TRUE){
     
    check_time  <- as.period(check_interval, unit = "hours")
    
    lead_time_work_hours <- as.numeric(check_time, "hours")
    
    
  } else{
    
    # Calcular o intervalo de tempo entre as datas de início e fim
    work_days <- seq(as.Date(start_date), as.Date(end_date), by = "day")
    
    # Remover feriados da lista de dias úteis
    work_days <- work_days[!work_days %in% as.Date(days_off$date)]
    
    # Remover finais de semana de dias úteis
    work_days <- work_days[!weekdays(work_days) %in% c("Saturday", "Sunday")]
    
    # Calcular o número total de horas de trabalho considerando 8 horas por dia
    # não considera o último dia
    lead_time_work_hours <- length(work_days - 1) * 8
    
  }
  
  lead_time_work_hours <- ifelse(lead_time_work_hours == 0, 1, lead_time_work_hours);
  
  ## Retorna o valor inteiro arredondado para cima
  return (lead_time_work_hours)
}



## Calcula o lead time em dias. Considerando que um dia é 8 horas de trabalho
calc_leadtime_in_days = function(start, end, days_off) {
  
  lead_time_in_days <- calc_leadtime(start, end, days_off)
  
  lead_time_in_days <- round(lead_time_in_days/8)
  
  lead_time_in_days <- ifelse(lead_time_in_days == 0, 1, lead_time_in_days);
  
  return (lead_time_in_days)
  
}


## Função para calcular o score de um card
# leadtime - leadtime em horas
# unit - unidade de medida mínima de um score em horas. (se unit = 4 então 1 ponto representa 4 horas)
calc_score = function(leadtime, min_unit){

  score <- round(leadtime/min_unit)
  
  score <- replace(score, score==0,  1)
  
  return ( score )
}



## Função para selecionar as variáveis utilizadas para a análise
get_fields <- function(jiraDs){
  
  fieldsdf <- jiraDs$issues %>% select(key, fields) %>%  mutate(created = fields$created,
                                  customer = fields$customfield_12315,
                                  
                                  priority = fields$priority$name,
                                  issuetype = fields$issuetype$name,
                                  projectkey = fields$project$key,
                                  category = fields$customfield_12314,
                                  summary = fields$summary,
                                  description = fields$description,
                                  resolutiondate = fields$resolutiondate,
                                  status = fields$status$name)%>%
    select(key, created, customer, priority, 
           issuetype, projectkey, category, summary, description,
           resolutiondate, status) 
  
  return(fieldsdf)
}

get_issue_changelog <- function(jira_ds){
  
  histories_df <- jira_ds$issues %>% select(key, changelog) %>% 
    mutate(histories = changelog$histories,
           total = changelog$total) %>% select(key, histories, total)
  n <- nrow(histories_df)
  i <- 0
  
  change_log_df <- NULL
  
  for  (i in 1:n) {
    key <- histories_df[i,]$key
    histories <- histories_df[i,]$histories
    j <- 0
    for (j in 1:histories_df[i,]$total){
      l <- histories[[1]][j,]
      l_author <- l$author$name
      l_created <- l$created
      l_items <- l$items
      total_items <- length(l_items)
      
      c <- 0
      for (c in 1: total_items){
        from <-  l_items[[1]][c,]$from
        fromString <- l_items[[1]][c,]$fromString
        to <- l_items[[1]][c,]$to
        toString <- l_items[[1]][c,]$toString
        fieldtype <-  l_items[[1]][c,]$fieldtype
        field <-  l_items[[1]][c,]$field
        
        arow <- tibble(key = c(key), developer = c(l_author),  initial_state_time = c(l_created),
                       to = c(to), initial_state = c(toString), 
                       from = c(from), fromString = (fromString), fieldtype = c(fieldtype), field = c(field))
        
        change_log_df <- bind_rows(change_log_df, arow)
      }
    }
  }
  
  return(change_log_df)
    
}

#Função que converte a estrutura do JIRA em um DataFrame com o leadtime caclulado
get_issue_life_cycle <- function(jira_ds, holidays, initial_state_ids, test_state_ids) {
  
  
  partial_df <- get_fields(jira_ds)
  
  
  change_log_df <- get_issue_changelog(jira_ds)

  
  ##Pega a demanda que inicia o leadtime em initial_state_ids
  initial_state_df <- change_log_df %>% filter((to %in% initial_state_ids) & !(from  %in% initial_state_ids) ) 
  
  ##Remove as Keys duplicadas pela alteração do mesmo estado por diferentes pessoas
  ## Esse tipo de dado não é importante para a análise
  initial_state_df <- initial_state_df %>% remove_duplicate_key()

  
  ## Pega o nome do tester no momento em que a demanda é passada para teste em test_state_ids
  teste_state_df <- change_log_df %>% 
      filter((to %in% test_state_ids)  & !(from  %in% test_state_ids)) %>% 
      rename(tester = developer) %>% distinct(key, tester)
  
  ##Remove as Keys duplicadas pela alteração do mesmo estado por diferentes pessoas
  ## Esse tipo de dado não é importante para a análise
  teste_state_df <- teste_state_df %>% remove_duplicate_key()

  final_df <- initial_state_df  %>% 
    inner_join(partial_df, by="key")  %>%
    inner_join(teste_state_df, by="key") %>%
    mutate(lead_time_hours = mapply(calc_leadtime, initial_state_time, resolutiondate, MoreArgs = list(days_off = holidays)),
           lead_time_days = mapply(calc_leadtime_in_days, initial_state_time, resolutiondate, MoreArgs = list(days_off = holidays))) %>%
    mutate(score = mapply(calc_score, lead_time_hours, 4))
 
  
  return (final_df)
  
}

##Remove as Keys duplicadas pela alteração do mesmo estado por diferentes pessoas
## Esse tipo de dado não é importante para a análise
remove_duplicate_key <- function(df){
  
  ## remove as keys dublicadas 
  remove_keys <- df %>% count(key) %>% filter(n > 1)
  remove_keys <- remove_keys$key
  df <- df %>% filter(!(key %in% remove_keys))
  
  return (df)
}


##Função para embaralhar uma string para não apresentar nomes de empresas e pessoas
shuffle <- function(s) {
  s_no_spaces <- gsub("\\s", "", s)
  original_length <- str_length(s_no_spaces)
  summarized_length <- ceiling(original_length / 2)
  s_summarized <- str_sub(s_no_spaces, 1, summarized_length)
  
  s_summarized_shuffled <- strsplit(s_summarized, "")[[1]]
  set.seed(123) 
  s_summarized_shuffled <- sample(s_summarized_shuffled)
  s_summarized_shuffled <- paste(s_summarized_shuffled, collapse = "")
  return(s_summarized_shuffled)
}





