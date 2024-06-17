library("tidyverse")
library("jsonlite")
library("readxl")
library("lubridate")
library("stringr")
library("ggplot2")
library("tidyr")
library("rvest")
library("memoise")
library("openxlsx")

##Datasource base a ser tratado
datasource <- fromJSON("data/ciclo-jira-conta-corrente.json")

##Feriados locais e nacionais
holidays <-  read.csv("data/holidays.csv")

set.seed(123)

##Análise inicial dos dados 
issue_with_changelog <- get_issue_changelog(datasource)

##Demonstração de irregularidade no andamento das issues no JIRA
issue_lifecycle<- c("11215", "13321", "13315", "11607", "11317", "13317")


##Lista apenas os controle de estado do ciclo de vida da Issua
issue_with_changelog <- issue_with_changelog %>% filter(field == "status") %>% 
  select(key, to, initial_state) %>% rename(lifecycle = initial_state) %>% 
  filter(to %in% issue_lifecycle) %>% mutate(lifecycle = ifelse(lifecycle == "For Technical Analysis", "Para Análise", lifecycle),
                                             lifecycle = ifelse(lifecycle == "For Test", "Para Teste", lifecycle),
                                             lifecycle = ifelse(lifecycle == "Ready for Development", "Para Desenvolvimento", lifecycle),
                                             lifecycle = ifelse(lifecycle == "For Technical Analysis (Pilot)", "Para Análise", lifecycle),
                                             lifecycle = ifelse(lifecycle == "For Test (Pilot)", "Para Teste", lifecycle),
                                             lifecycle = ifelse(lifecycle == "Ready for Development (Pilot)", "Para Desenvolvimento", lifecycle))


issue_with_changelog <- issue_with_changelog %>% select(key, lifecycle) %>% group_by(lifecycle) 


ggplot(issue_with_changelog, aes(x = lifecycle, fill = lifecycle)) +
  geom_bar(color = "black") +
  stat_count(aes(label = ..count..), geom = "text", color = "black", vjust = -0.5) +
  scale_fill_viridis_d() +
  labs(title = "Histograma do Ciclo de Vida das Issues",
       x = "Ciclo de Vida",
       y = "Issues",
       fill = "Ciclo de Vida")



set.seed(123)
## Cria um dataframe tidy a partir de um JSON JIRA
## datasource - json original do JIRA
## holiday - dataset de feriados locais e nacionais
## initial_state_ids - códigos do estado inicial que compoe o leadtime de uma issue
## test_state_ids - códigos que representa a fase de teste do leadtime
issues <- get_issue_life_cycle(datasource, holidays, c(11608, 13316), c(12222, 13322)) 


## Removendo os cards cancelados e descartados
issues <- issues %>% filter(!(status %in% c("Canceled", "Canceled (Pilot)", "Discarded", "Discarded (Pilot)") ))

## Removendo épicos e cards sem descrição
issues <- issues %>% filter(!(issuetype %in% c("Épico")) & !is.na(description))

##Embaralhar nome dos clientes para não ser exposto
issues$customer <- mapply(shuffle, issues$customer)

##Embaralhar nome do developer para não ser exposto
issues$developer <- mapply(shuffle, issues$developer)

##Embaralhar nome do Tester para não ser exposto
issues$tester <-  mapply(shuffle, issues$tester)

summary(issues)
glimpse(issues)

write_csv(issues, "data/treated/cc-2022-treated.csv")

## Data Augmentation
## Os dados são mantidos com a mesma característica porém o dataset é
## embaralhado com técnica de embaralhamento de n-gramas com sinonimos no summary e description
## É criado um ID diferente para cada issue que pode ser identificado por ser um valor > 90000
## Os demais valores são mantidos iguais
## O dataframe está sendo aumentado em 10x 
set.seed(123)

issues_df <- read.csv("data/treated/cc-2022-treated.csv")


##Teste amostral da função shuffle_dataframe - 2 linhas, não replica (n = 1)
sample_test <- as.data.frame(issues_df[1:2,])
sample_test <- shuffle_dataframe(sample_test, 2)

## Aumenta dem 10x a base de dados
augmentation_ds <- shuffle_dataframe(issues_df, 10)

##Cria a mesma coluna do user_story no dataset original para fazer o rbind
issues_df$user_story <- paste(issues_df$summary, issues_df$description)

## Adiciona a base original de treino
augmentation_ds <- rbind(augmentation_ds, issues_df)

## Remove keys duplicadas
augmentation_dup_keys <- augmentation_ds %>% select(key) %>% count(key) %>% filter(n > 1) %>% select(key)
augmentation_ds <- augmentation_ds %>% filter(!(key %in% augmentation_dup_keys))

write_csv(augmentation_ds, "data/treated/cc-2022-augmentation.csv")




