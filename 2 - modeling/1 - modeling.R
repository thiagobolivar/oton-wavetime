library("tm")
library("readxl")
library("SnowballC")
library("ggplot2")
library("PerformanceAnalytics")
library("fastDummies")
library("car")
library("olsrr")
library("nortest")
library("kableExtra")
library("jtools")
library("lmtest")
library("overdisp")
library("MASS")
library("plotly")
library("correlation")
library("caret")
library("glmnet")
library("forecast")
library("fitdistrplus")
library("moments")
library("car")
library("xgboost")
library("reshape2")
library("questionr")
library("ggside")
library("tidyquant")
library("tidyverse")
library("viridis")
library("scales")
library("purrr")
library("statmod")
library("tweedie")

issues_df <- read.csv("data/treated/cc-2022-augmentation.csv")

################################################################################
#                          PROCEDIMENTO N-1 DUMMIES                            #
################################################################################
#Dummizando as variáveis developer, tester, customer, priority, issuetype,
# projectkey e category. O código abaixo, automaticamente, fará: a) o
# estabelecimento de dummies que representarão cada uma das regiões da base de 
# dados; b)removerá a variável dummizada original; c) estabelecerá como categoria 
# de referência a dummy mais frequente.
set.seed(123)


dummies_df <-process_dummies(issues_df)

dtm_df <- process_corpus(issues_df)


## voltar para a união cbind(issues_dammies_df, dtm_df)
adjusted_df <- cbind(dummies_df, dtm_df)

# Remover as variavies que tem correlação quase perfeita
# Calcular a matriz de correlação para evitar MULTICOLINEARIDADE
set.seed(123)

#cor_issues_ds <- cor(adjusted_df)
#high_cor_issues_ds <- findCorrelation(cor_issues_ds, cutoff = 0.999, verbose = TRUE)
#final_df <- final_df[, -high_cor_issues_ds]

final_df <- adjusted_df[2:ncol(adjusted_df)]

## Separação das bases para análise e validação dos modelos
set.seed(123)

# índices de amostragem
train_ratio <- 0.6
validation_ratio <- 0.2
test_ratio <- 0.2


n_rows <- nrow(final_df)
train_size <- round(train_ratio * n_rows)
validation_size <- round(validation_ratio * n_rows)


train_idx <- sample(1:n_rows, size = train_size, replace = FALSE)
remaining_idx <- setdiff(1:n_rows, train_idx)
validation_idx <- sample(remaining_idx, size = validation_size, replace = FALSE)
test_idx <- setdiff(remaining_idx, validation_idx)


train_ds <- final_df[train_idx, ]
test_ds <- final_df[test_idx, ]

validation_ds <- adjusted_df[validation_idx, ] ## Mantendo a chave para filtrar e comparar na validação

#write_csv(issues_df[validation_idx, ], "data/treated/cc-2022-validation.csv")
#write_csv(test_ds, "data/treated/cc-2022-test.csv")
#write_csv(train_ds, "data/treated/cc-2022-train.csv")

###############################################################################
#         Entendendo a distribuição dos dados gráfico de Cullen e Frey 
###############################################################################
skewness_valores <- apply(final_df, 2, skewness)
kurtosis_valores <- apply(final_df, 2, kurtosis)

media_skewness <- mean(skewness_valores)
media_kurtosis <- mean(kurtosis_valores)

sd_skewness <- sd(skewness_valores)
sd_kurtosis <- sd(kurtosis_valores)

descdist(c(media_skewness, sd_skewness, media_kurtosis, sd_kurtosis), boot = 50, discrete = FALSE)

gamma_fit <- fitdist(final_df$lead_time_hours, "gamma",  method = "mle") ##Loglikelihood:  -7943.583   AIC:  15891.17   BIC:  15901.63 

summary(gamma_fit)
lognorm_fit <- fitdist(final_df$lead_time_hours, "lnorm", method = "mle")  ##Loglikelihood: -8430.359   AIC:  16864.72   BIC:  16875.19 
summary(lognorm_fit)


plot.legend <- c("gama", "lognormal")
denscomp(list(gamma_fit, lognorm_fit), legendtext = plot.legend)



#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'lead_time_hours'
final_df %>%
  summarise(Média = mean(lead_time_hours),
            Variância = var(lead_time_hours)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 30)


################################################################################
#                    MODELO DISTRIBUIÇÃO GAMMA COM XGBoost                     #
################################################################################
set.seed(123)

xgb_train_X <- as.matrix(train_ds[, -1])

xgb_train_Y <- train_ds$lead_time_hours

kfold <- 10

xgb_train_dall <- xgb.DMatrix(data = xgb_train_X, label = xgb_train_Y)

#tweedie_variance_power = 1.5,
#objective = "reg:tweedie"

# Parâmetros do modelo
xgb_params <- list(objective = "reg:tweedie", 
                   booster = "gbtree",
                   tweedie_variance_power = 1.5,
                   nthread = 5,
                   max_depth = 7,
                   eval_metric = "rmse")

## Cross Validation
xgb_best_params <- xgb.cv(
    params = xgb_params,
    data = xgb_train_dall,
    nrounds = 300,
    nfold = kfold,
    early_stopping_rounds = 10,
    verbose = 1
  )

##19
xgb_train_best_params <- xgb_best_params$best_iteration
xgb_train_best_params

best_xgb_model <- xgb.train(
    params = xgb_params,
    data = xgb_train_dall,
    nrounds = xgb_train_best_params,
)

xgb_train_Y_Pred <- predict(best_xgb_model, newdata = xgb_train_X)


xgb_train_rsq <- 1 - sum((xgb_train_Y - xgb_train_Y_Pred)^2) / sum((xgb_train_Y - mean(xgb_train_Y))^2)
cat("R quadrado:", xgb_train_rsq, "\n")

summary(best_xgb_model)

###############################################################################
##                          TESTE XGBoost                                     #
###############################################################################
set.seed(123)

xgb_test_X <- as.matrix(test_ds[, -1])

xgb_test_Y <- test_ds$lead_time_hours

# Fazer previsões no conjunto de teste
xgb_test_Y_Pred <- predict(best_xgb_model, newdata = xgb_test_X)

# Avaliar o desempenho do modelo
xgb_test_mse <- mean((xgb_test_Y_Pred - test_ds$lead_time_hours)^2)


# 3. Coeficiente de Determinação (R²)
actual_mean <- mean(test_ds$lead_time_hours)
total_sum_of_squares <- sum((test_ds$lead_time_hours - actual_mean)^2)
residual_sum_of_squares <- sum((xgb_test_Y_Pred - test_ds$lead_time_hours)^2)
r_squared <- 1 - (residual_sum_of_squares / total_sum_of_squares)
print(paste("R^2:", r_squared))


xgb_teste_y_fit <- fitdist(xgb_test_Y, dgamma, discrete = FALSE)
xgb_teste_y_fit_shape <- xgb_teste_y_fit$estimate[1]
xgb_teste_y_fit_rate <- xgb_teste_y_fit$estimate[2]
xgb_teste_estimate <- fitdistr(xgb_test_Y_Pred, densfun = "gamma",  start = list(shape = xgb_teste_y_fit_shape, rate = xgb_teste_y_fit_rate))
xgb_teste_estimate$loglik ##LogLik Gama -1495.031 ##Loglik Tweedie -1583.123



# Calcula o AIC e o BIC
xgb_teste_nparam <- ncol(xgb_test_X)
xgb_teste_nobs <- length(xgb_test_X)
xgb_teste_aic <- 2 * xgb_teste_nparam - 2 * xgb_teste_estimate$loglik
xgb_teste_bic <- xgb_teste_nparam * log(xgb_teste_nobs) - 2 *xgb_teste_estimate$loglik
xgb_teste_aic
xgb_teste_bic
xgb_teste_estimate$loglik


###############################################################################
#                 SALVANDO O MODELO XGBoost                                   #
###############################################################################
xgb.save(best_xgb_model, "models/ip_xgboost_model_v1.model")


################################################################################
#                             GLMNET  MODELO  GAMMA                            #
################################################################################
set.seed(123)


gamma_ds <- train_ds

gamma_X <- as.matrix(gamma_ds[, -1])

gamma_Y <- gamma_ds$lead_time_hours

kfold <- 10

##Cross Validation do modelo ajustado em Gamma
gamma_best_gradient <- cv.glmnet(x = gamma_X, y = gamma_Y, nfolds = kfold,  
                                 family = Gamma(), type.measure = "mse")
##Cross Validation para melhor alpha.
best_alpha <- 0
best_dev.ratio <- Inf

sq_alphas <- seq(0, 1,  by = 0.1)

for (alpha in sq_alphas){
  gamma_cv <- glmnet(x = gamma_X, y = gamma_Y, lambda = gamma_best_gradient$lambda.min, alpha = alpha)
  if (gamma_cv$dev.ratio > best_dev.ratio){
    best_dev.ratio <- gamma_cv$dev.ratio
    best_alpha <- alpha
  }
}

gamma_fit <- glmnet(x = gamma_X, 
                    y = gamma_Y, alpha = best_alpha, 
                    lambda = gamma_best_gradient$lambda.min)

gamma_train_y_pred <- predict(gamma_fit, newx = gamma_X) 

gamma_train_rsq <- 1 - sum((gamma_Y - gamma_train_y_pred)^2) / sum((gamma_Y - mean(gamma_Y))^2)
gamma_train_rsq

################################################################################
#                               TESTE GLMNET COM GAMMA                         #
################################################################################
set.seed(123)

gamma_test_ds <-test_ds

gamma_test_X <- as.matrix(gamma_test_ds[, -1])

gamma_test_Y <- gamma_test_ds$lead_time_hours

gamma_test_Y_pred <- predict(gamma_fit, newx = gamma_test_X)

gamma_test_rsq <- 1 - sum((gamma_test_Y - gamma_test_Y_pred)^2) / sum((gamma_test_Y - mean(gamma_test_Y))^2)
cat("R quadrado:", gamma_test_rsq, "\n")

gamma_residuals <- gamma_test_Y - gamma_test_Y_pred


gamma_test_y_fit <- fitdist(gamma_test_Y, dgamma, discrete = FALSE)
gamma_test_y_fit_shape <- gamma_test_y_fit$estimate[1]
gamma_test_y_fit_rate <- gamma_test_y_fit$estimate[2]
gamma_test_estimate <- fitdistr(gamma_test_Y_pred, densfun = "gamma",  start = list(shape = gamma_test_y_fit_shape, rate = gamma_test_y_fit_rate))
gamma_test_estimate$loglik ##LogLik Gama -1495.031 ##Loglik Tweedie -1583.123


# Calcula o AIC e o BIC
gamma_test_nparam <- ncol(gamma_test_X)
gamma_test_nobs <- length(gamma_test_X)
gamma_test_aic <- 2 * gamma_test_nparam - 2 * gamma_test_estimate$loglik
gamma_test_bic <- gamma_test_nparam * log(gamma_test_nobs) - 2 *gamma_test_estimate$loglik
gamma_test_aic
gamma_test_bic
gamma_test_estimate$loglik




################################################################################
#                          SALVANDO O MODELO GLMNET
################################################################################

saveRDS(gamma_fit, "models/ip_glmnet_model_v1.rds")

logLik()
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)

# Ajustar o modelo de regressão linear
modelo_linear <- lm(y ~ x)

# Calcular e imprimir a log-verossimilhança do modelo
log_likelihood <- logLik(modelo_linear)
print(log_likelihood)

