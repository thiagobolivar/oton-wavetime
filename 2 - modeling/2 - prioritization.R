library("readxl")
library("xgboost")

##############################################################################
#                     TIME DISPONÍVEL E ATIVIDADES EM BACKLOG
##############################################################################

## Nesta versão a premissa do modelo é que o Desenvolvedor e o Tester irão trabalhar 
## em conjunto no período fechado para concluir o leadtime
## Uma melhoria do modelo será calcular o leadtime do desenvolvedor e o leadtime do teste
## para otimizar o tempo do tester e do desenvolvedor nas tarefas. 

backlog_df <- read.csv("data/cc-2022-backlog.csv") ## Backlog de atividades

developer_df <- read.csv("data/developers.csv") ## Desenvolvedores disponíveis na celula

tester_df <- read.csv("data/testers.csv") ## Testadores disponíveis na celula

##############################################################################
#                         Critérios de Priorização
##############################################################################
criteria.priority <-  read.csv("data/priority_weights.csv") ## Pesos das Prioridades

criteria.customer <-  read.csv("data/customer_weights.csv") ## Pesos dos Clientes
  
criteria.issuetype <-  read.csv("data/issuetype_weights.csv") ## Pesos dos tipos de atividade 
  
##############################################################################
#                    Priorização das demandas com AHP GUASSIANO        
##############################################################################

nrow(backlog_df)
nrow(developer_df)
nrow(tester_df)

set.seed(123)

backlog_prior_df <- calculate_priority_ranking(data = backlog_df, 
                                            criteria.priority = criteria.priority,
                                            criteria.customer =  criteria.customer,
                                            criteria.issuetype = criteria.issuetype)

issue_combo_ds <- create_team_combo(backlog_prior_df, developer_df, tester_df)
issue_combo_ds$lead_time_hours <- NA

issue_combo_dtm_ds <- process_corpus_dictionary(issue_combo_ds)

issue_combo_dummies_ds <- process_dummies_dictionary(issue_combo_ds)

issue_combo_dummies_ds <- issue_combo_dummies_ds[,3:ncol(issue_combo_dummies_ds)]

validation_X <- cbind(issue_combo_dummies_ds, issue_combo_dtm_ds)

ip_xgboost_model_v1 <- xgb.load("models/ip_xgboost_model_v1.model")

validation_Y_Pred <- predict(ip_xgboost_model_v1, newdata = as.matrix(validation_X))

total_hours <- 120 ## Sprint com 120 horas 

issue_combo_ds$lead_time_hours <- validation_Y_Pred

issue_combo_ds <- issue_combo_ds %>% filter(lead_time_hours > 0)

ranked_issues_df <- issue_combo_ds %>% distinct(key, ranking)

resource_time_left <- new.env()

##Define o máximo de 120 horas para cada recurso
for ( idev in 1:nrow(developer_df)){
  resource_time_left[[developer_df[idev,1]]] <- 120
}
for ( itest in 1:nrow(tester_df)){
  resource_time_left[[tester_df[itest,1]]] <- 120
}

has_more_time <- function(resource, leadtime){
  return(resource_time_left[[resource]] >= leadtime)
}

remove_time_left <- function(resource, leadtime){
  resource_time_left[[resource]] <- resource_time_left[[resource]] - leadtime
  if (resource_time_left[[resource]] < 0){
    resource_time_left[[resource]] = 0
  }
}

tester_used_list <- c()
developer_used_list <- c()


total_tester <- nrow(tester_df)
total_developer <- nrow(developer_df)

suggested_issues <- NULL

not_distributed <- NULL

for (ii in 1:nrow(ranked_issues_df)) {
  
  a_ranked_issue <- ranked_issues_df[ii,]
  
  cat("Priorizou a demanda ", a_ranked_issue$key, "\n")
  
  ##Se a lista se esgotar, todos podem participar de novas rodadas
  if (length(tester_used_list) == total_tester){
    tester_used_list <- c()
  }
  
  ##Se a lista se esgotar, todos podem participar de novas rodadas
  if (length(developer_used_list) == total_developer){
    developer_used_list <- c()
  }
  

  issues_selected <- issue_combo_ds %>% 
    filter(key == a_ranked_issue$key)  %>%
    filter(!(developer %in% developer_used_list) & !(tester %in% tester_used_list)) %>%
    arrange(desc(ranking)) %>% 
    group_by(key) %>%
    slice_min(lead_time_hours) %>%
    ungroup() %>% slice(1)
      
  cat("Selecionou o par ", issues_selected$developer, issues_selected$tester, "\n")
        
  if (nrow(issues_selected) > 0) {
    ## Se o par dev e tester ainda tiverem capacidade, são adicionados na lista
    if (has_more_time(issues_selected$developer, issues_selected$lead_time_hours) & 
        has_more_time(issues_selected$tester, issues_selected$lead_time_hours)){
      
      suggested_issues <- rbind(suggested_issues, issues_selected)
      
      ## desconta do tempo máximo disponível
      remove_time_left(issues_selected$developer, issues_selected$lead_time_hours)
      remove_time_left(issues_selected$tester, issues_selected$lead_time_hours)
      
      ##Adiciona na lista de usados para garantir que todos participem até que a lista seja zerada
      tester_used_list <- c(tester_used_list, issues_selected$tester)
      developer_used_list <- c(developer_used_list, issues_selected$developer)
    }else{
      
      not_distributed <- rbind(not_distributed, issues_selected)
      
    }
  }

}

issus_need_split <-not_distributed %>% filter(lead_time_hours > 120)

issus_need_split <- issus_need_split %>% select(key, lead_time_hours)

suggested_issues <- suggested_issues %>% select(key, developer, tester, lead_time_hours)
