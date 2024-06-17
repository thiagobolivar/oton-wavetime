# Cria o mapa para armazenar os sinônimos encontrados
synonyms_map <- new.env()

##Função otimizada para retorno de sinonimos
## se o sinonimo ja existir não faz o scraping da pagina, retorna do map
synonyms <- memoise(function(word) {
  
  # Verifica se o sinônimo já foi buscado anteriormente
  if (word %in% names(synonyms_map)) {
    return(sample(synonyms_map[[word]], 1))
  }
  
  # palavra a ser pesquisada em sinonimos com br
  url <- paste0("https://www.sinonimos.com.br/", word)
  
  page <- try(read_html(url), silent = TRUE)
  
  # Verifica se ocorreu um erro HTTP 404
  if (inherits(page, "try-error")) {
    return(word)
  }
  
  # faz o scraping da página e extrai os sinônimos
  synonyms <- page %>%
    html_nodes(".container") %>%
    html_nodes(".sinonimo") %>%
    html_text()
  
  if (length(synonyms) > 0){
    # Armazena os sinônimos encontrados no mapa
    synonyms_map[[word]] <- synonyms
    
    # retorna um dos sinonimos de forma aleatória
    return(sample(synonyms, 1))
  }else{
    return(word)
  }
})

## Faz o ngram substituindo pelo sinonimo de cada palavra
ngrams <- function(text) {
  words <- unlist(strsplit(text, " "))
  grams <- sapply(1:(length(words) + 1), function(i) {
    synonyms(words[i:(i)])
  })
  return(grams)
}

# Função para embaralhar n-gramas em um texto
shuffle_ngrams <- function(text, n = 1, rate = 0.8) {
  grams <- ngrams(text)
  shuffle_ngrams <- sample(grams, floor(length(grams) * rate))
  
  for (i in seq_along(shuffle_ngrams)) {
    if (nchar(shuffle_ngrams[i]) > 0) {
      text <- gsub(shuffle_ngrams[i], grams[i], text, fixed = TRUE)
    }
  }
  
  return(text)
}

###Teste de embaralhamento por n-gram com sinonimos
shuffle_ngrams("Para ambos os cenários das mensagens, o TES não está realizando o estorno do valor na conta corrente do cliente.\n\n2. O TES não está inserindo as mensagens de retorno TES0017 e TES0037 com erro, na tela Consulta de Retornos (R1)")


#Embaralhar dataframe e substituir por n-gramas aleatoriaos
#shuffle_type = tipo de função de embaralhamento pode ser synonyms ou ngram
#n - é a quantidade de vezes que um datafram será replicato
shuffle_dataframe <- function(df, n = 2) {
  #Embaralhar e gerar novas chaves
  shuffle_and_generate_keys <- function(df) {
    shuffled_df <- df[sample(nrow(df)), ]
      
      ##Unindo summary e description
      shuffled_df$user_story <- paste(shuffled_df$summary, shuffled_df$description)
    
      shuffled_df <- shuffled_df %>% 
        mutate(user_story = iconv(sapply(user_story, shuffle_ngrams), to = "ASCII//TRANSLIT", sub = "byte"),
               key = paste(projectkey, "-", sample(90000:95000, nrow(shuffled_df), replace = TRUE), sep = ""))
    
    return(shuffled_df)
  }
  
  # Clonar e embaralhar dataframe n vezes
  cloned_dfs <- lapply(1:n, function(x) shuffle_and_generate_keys(df))
  
  
  final_df <- do.call("rbind", cloned_dfs)
  
  return(final_df)
}



