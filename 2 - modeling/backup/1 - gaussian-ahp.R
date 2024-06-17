library("tidyverse")
library("readxl")


issues <- read.csv("data/cc-issues.csv")


decision_matrix <- issues %>% select(key, customer, priority, issue_type) %>%
  inner_join(priorities, by = "priority") %>% inner_join(issue_types, by = "issue_type") %>%
  inner_join(customers, by = "customer") %>% select(key, priority_weight, issue_type_weight, customer_weight) %>%
  rename( priority = priority_weight, issue_type = issue_type_weight, customer = customer_weight) 

total_priority <- sum(decision_matrix$priority)
total_customer <- sum(decision_matrix$customer)
total_issue_type <- sum(decision_matrix$issue_type)

### Normalização da matriz de decisão
decision_matrix <- decision_matrix %>% mutate(customer = customer/total_customer,
                                              priority = priority/total_priority,
                                              issue_type = issue_type/total_issue_type)

## Extraindo estatística descritiva (média e desvio padrão) dos critérios customer, priority e issue type
descriptive <- decision_matrix %>% summarise(mean_customer = mean(customer),
                         mean_priority = mean(priority),
                         mean_issue_type = mean(issue_type),
                         sd_customer = sd(customer),
                         sd_priority = sd(priority),
                         sd_issue_type = sd(issue_type))

## Definindo o fator gaussiano de cada critério
gaussian_factor <- NULL 
gaussian_factor <- bind_rows(gaussian_factor, tibble(criterion = c("customer"), factor = c(descriptive$sd_customer/ descriptive$mean_customer)))
gaussian_factor <- bind_rows(gaussian_factor, tibble(criterion = c("priority"), factor = c(descriptive$sd_priority/ descriptive$mean_priority)))
gaussian_factor <- bind_rows(gaussian_factor, tibble(criterion = c("issue_type"), factor = c(descriptive$sd_issue_type/ descriptive$mean_issue_type)))

total_factor <- gaussian_factor %>% summarise(gausf_total = sum(factor))

## Normalizando o fator gaussiano
gaussian_factor <- gaussian_factor %>% mutate(factor = factor/total_factor$gausf_total)


## Ponderação da matriz do decisão
weighted_matrix <- decision_matrix %>% mutate(customer = customer*gaussian_factor[gaussian_factor$criterion == 'customer',]$factor,
                                              priority = priority*gaussian_factor[gaussian_factor$criterion == 'priority',]$factor,
                                              issue_type = issue_type*gaussian_factor[gaussian_factor$criterion == 'issue_type',]$factor)

## Obtenção do Ranking
weighted_matrix$ranking <- rowSums(weighted_matrix[,c("customer", "priority", "issue_type")])

weighted_matrix <- weighted_matrix %>% select(key, ranking)

prioritized_issues <- issues %>% inner_join(weighted_matrix, by="key") %>% 
  select(ranking, created, key, customer, priority, issue_type, summary, description, developer, dev_level, tester, tester_level, duedate, score) %>%
  arrange(desc(ranking))

write.csv(prioritized_issues, "data/cc-issues-prioritized.csv")
