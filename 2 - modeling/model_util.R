## Função que utiliza o método AHP Causiano para a priorização das demandas
calculate_priority_ranking <- function(data, criteria.priority, criteria.customer, criteria.issuetype){

  #Construção da Matriz de Decisão
  decision_matrix <- data %>% select(key, customer, priority, issuetype) %>%
    inner_join(criteria.priority, by = "priority") %>% inner_join(criteria.issuetype, by = "issuetype") %>%
    inner_join(criteria.customer, by = "customer") %>% select(key, priority_weight, issuetype_weight, customer_weight) %>%
    rename( priority = priority_weight, issue_type = issuetype_weight, customer = customer_weight) 
  
  
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
  
  prioritized_issues <- data %>% inner_join(weighted_matrix, by="key") %>% arrange(desc(ranking))
  
  
  return (prioritized_issues)
  
}


process_dummies_dictionary <- function(data){
  
  terms_dummies <- read.csv("models/dictionary/terms_dummies.csv", header = FALSE)$V1
  
  dummies_ds <- parse_dummies(data)
  
  c1 <- terms_dummies
  c2 <- colnames(dummies_ds)
  
  diff_columns_has_dict <- setdiff(c1, c2)
  
  for (acolumn in diff_columns_has_dict) {
    dummies_ds <- mutate(dummies_ds, !!acolumn := 0)
  }
  
  diff_columns_has_no_dict <- setdiff(c2, c1)

  dummies_ds <- dummies_ds %>% select(-one_of(diff_columns_has_no_dict))  

  return(dummies_ds)
}

parse_dummies <- function(data){
  
  dummies_df <- data %>% dplyr::select(key, lead_time_hours, developer,
                                       tester, customer, 
                                       priority, issuetype,
                                       projectkey, category) %>% 
    dummy_columns(select_columns =  c("priority", "issuetype", "projectkey",
                                      "category", "developer", "tester", 
                                      "customer"),
                  remove_selected_columns = T,
                  remove_most_frequent_dummy = T,
                  ignore_na = F)
  
  return(dummies_df)
}

process_dummies <- function(data){
  
  dummies_df <- parse_dummies(data)
  
  terms_dummies <- colnames(dummies_df)
  
  write.csv(terms_dummies, "models/dictionary/terms_dummies.csv", row.names = FALSE)
  
  return (dummies_df)
}

clear_text <- function(data){
  
  ## Preparação das stopwords e stopwords técnicas não relevantes para análise
  stop_words <- stopwords("pt")
  
  tech_stopwords <- read.delim("data/techstopwords.txt") 
  tech_stopwords <- tech_stopwords$word
  
  ## Criação do Corpus
  vcorpus = VCorpus(VectorSource(data$user_story)) 
  vcorpus = tm_map(vcorpus, content_transformer(tolower))
  vcorpus = tm_map(vcorpus, removeWords, tech_stopwords) 
  vcorpus = tm_map(vcorpus, removeNumbers) 
  vcorpus = tm_map(vcorpus, removePunctuation) 
  vcorpus = tm_map(vcorpus, removeWords, stop_words) 
  vcorpus = tm_map(vcorpus, stripWhitespace) 
  vcorpus = tm_map(vcorpus, stemDocument, language="portugues")
  

  return (vcorpus)
}

## Função para a tokenização em Bigrama
NgramTokenizer <- function(x) {
  
  x_ascii <- iconv(x, to = "ASCII//TRANSLIT")
  
  # Tokeniza o texto usando ngrams
  tokens <- unlist(lapply(NLP::ngrams(words(x_ascii), 2), paste, collapse = " "), use.names = FALSE)
  
  # Retorna os tokens
  return(tokens)
}

process_corpus_dictionary <- function(data){
  vcorpus <- clear_text(data)
  
  terms <- read.csv("models/dictionary/terms_corpus.csv", header = FALSE)$V1
  
  ## Criação do DocumentMatriz
  gdtm <- DocumentTermMatrix(vcorpus, control=list(tokenize = NgramTokenizer,  
                                                   stripWhitespace = TRUE,
                                                   language = "pt", 
                                                   encoding = "UTF-8",
                                                   dictionary = terms))
  return (as.data.frame(as.matrix(gdtm)))
}

process_corpus <- function(data){
  
  vcorpus <- clear_text(data)
  
  ## Criação do DocumentMatriz
  gdtm <- DocumentTermMatrix(vcorpus, control=list(tokenize = NgramTokenizer,  
                                                   stripWhitespace = TRUE,
                                                   language = "pt", 
                                                   encoding = "UTF-8"))
  
 # gdtm <- removeSparseTerms(gdtm, 0.999)
  
  write.csv(gdtm$dimnames$Terms, "models/dictionary/terms_corpus.csv", row.names = FALSE)
    
  return (as.data.frame(as.matrix(gdtm)))
    
}

## Cross Validation ajustado em Gamma
process_cv_glmnet <- function(X, Y, kfold){
  
  ##Cross Validation para melhor alpha. (Elastic Net.)
  cv_gamma <- list(best_alpha <- NA, best_lambda <- NA, best_dev.ratio <- Inf)
  
  ##Cross Validation do modelo ajustado em Gamma
  gamma_best_params <- cv.glmnet(x = X, y = Y,  nfolds = kfold,  
                                   family = Gamma())
  
  sq_alphas <- seq(0, 1,  by = 0.1)

  for (alpha in sq_alphas){
    
    gamma_best_model <- glmnet(x = X, 
                       y = Y, alpha = alpha, 
                       lambda = gamma_best_params$best_lambda)
    cat("best model", gamma_best_model$dev.ratio)
    cat("best control",  cv_gamma$best_dev.ratio)
    
    if (gamma_best_model$dev.ratio < cv_gamma$best_dev.ratio){
      cv_gamma$best_dev.ratio <- gamma_best_model$dev.ratio
      cv_gamma$best_alpha <- alpha
      cv_gamma$best_lambda <- gamma_best_model$lambda
    }
    
  }
  
  return (cv_gamma)
  
}

create_team_combo <- function(data, developers, testers){
  team_combo_ds <- data[FALSE, ]
  for (i in 1:nrow(data)){
    ##Combina todas as possibilidade do par developer/tester
    for (d in 1:nrow(developers)){
      for (t in 1:nrow(testers)){
        row_data <- data[i,]
        
        row_data$developer <- developers[d,1]
        
        row_data$tester <- testers[t,1]
        
        team_combo_ds <- rbind(team_combo_ds, row_data)
      }
    }
  }
  return(team_combo_ds)
}













