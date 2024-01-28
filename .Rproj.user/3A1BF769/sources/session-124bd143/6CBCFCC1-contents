# Session-3:  Apprentissage supervisé avec R
set.seed(10)

# Générer des données simulées
sim_data1 = runif(1000) * 100
sim_data2 = sim_data1 + runif(1000) * 100
depvar = 40 + sim_data2 + sim_data1 + rnorm(1000, 0, 20)
model_data = data.frame(cbind(depvar, sim_data1, sim_data2))

# Régression linéaire
summary(lm(data = model_data, depvar ~ sim_data1 + sim_data2))

X = as.matrix(model_data[c("sim_data1", "sim_data2")])
X = cbind(rep(1, 1000), X)
colnames(X)[1] = "intercept"
Y = as.matrix(model_data["depvar"])

# Calcul des coefficients de régression
beta = solve(t(X) %*% X) %*% (t(X) %*% Y)
beta
predictions = X %*% beta
head(predictions)

residuals = predictions - Y

# Calcul de l'écart-type
sd_c = var(residuals)[1]
cov_matrix = solve(t(X) %*% X) * sd_c
diag(cov_matrix) = sqrt(diag(cov_matrix))
print(paste("Erreur standard:", diag(cov_matrix)))
Y = as.matrix(model_data["depvar"])

# Lab 3.2
install.packages("sjPlot")
library(sjPlot)
library(lme4)

clients <- read.csv("./sales_clients_combos.csv")

# Modèle linéaire
model1 <- lm(Sales ~ Strategy + (Client) + (Salesman), data = clients)
tab_model(model1)

# Modèle mixte linéaire
model2 <- lmer(Sales ~ Strategy + Client + (1|Salesman), data = clients)
tab_model(model2)

tab_df(clients, title = "Jeu de données des clients", alternate.rows = TRUE)

# Lab 3.3
data = read.csv("./shoes_sales.csv")

library(caret)
library(car)

X = data[-1]

# Trouver les combinaisons linéaires
findLinearCombos(X)

X = as.matrix(X)
det(t(X) %*% X)

# Modèle linéaire
model = lm(data = data, Sales ~ women_apparel_price + male_apparel_price + shoes_female_price + shoes_male_price + shoes_kids_prices + shoes_male_price_b + prices_shoes)
summary(model)

det(t(X[, c(-6, -7)]) %*% X[, c(-6, -7)])

fixedmodel = lm(data = data, Sales ~ women_apparel_price + male_apparel_price + shoes_female_price + shoes_male_price + shoes_kids_prices)
summary(fixedmodel)
vif(fixedmodel)

aggregated_apparel = data$women_apparel_price + data$male_apparel_price
aggregated_femalekids = data$shoes_female_price + data$shoes_kids_prices
finalmodel = lm(data = data, Sales ~  aggregated_apparel + shoes_male_price + aggregated_femalekids)
summary(finalmodel)
vif(finalmodel)

# Lab 3.4
install.packages("multcomp")
library(multcomp)

data = read.csv("./house_prices.csv")

# Modèle linéaire
model = lm(Property_price ~ size + number.bathrooms + number.bedrooms + number.entrances +  size_balcony  + size_entrance, data = data)
summary(model)

summary(glht(model, linfct = c("number.bathrooms + number.entrances + number.bedrooms + size_balcony + size_entrance - size  = 0")))

summary(glht(model, linfct = c("number.entrances + number.bathrooms - size_balcony - size_entrance = 0")))

# Lab 3.5
data = read.csv("./people_shopping.csv")

# Modèle linéaire
model = lm(sales ~ people_in + discount, data = data)
plot(model)

library("lmtest")
bptest(model)

# Lab 3.6
data = read.csv("./people_shopping.csv")

# Modèle linéaire
model = lm(sales ~ people_in + discount, data = data)

library("lmtest")
bptest(model)

summary(model)

library(sandwich)
coeftest(model, vcov = vcovHC(model))

# Lab 3.7
install.packages("olsrr")
library("olsrr")
library(dplyr)

model = lm(data = Boston, medv ~ .)

head(ols_step_all_possible(model) %>% arrange(desc(adjr)))

ols_step_forward_p(model)

ols_step_backward_p(model)

ols_step_both_p(model)

ols_step_forward_aic(model)

ols_step_backward_aic(model)

ols_step_both_aic(model)


# Lab 3.8
data = read.csv("./employee_performance.csv")

# Modèle linéaire
model = lm(data = data, Performance ~ Experience + Education + Training)
summary(model)

library("car")
vif(model)

# Lab 3.9
data = read.csv("./employee_turnover.csv")

# Modèle logistique
model = glm(data = data, Turnover ~ Satisfaction + Evaluation + ProjectCount + AverageMonthlyHours + YearsAtCompany + WorkAccident + PromotionLast5Years, family = "binomial")
summary(model)

library("car")
vif(model)

# Lab 3.10
data = read.csv("./loan_approval.csv")

# Modèle logistique
model = glm(data = data, Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, family = "binomial")
summary(model)

library("car")
vif(model)


# Lab 3.8
library(MASS)
library(tidyr)
library(ggplot2)
library(glmnet)
library(dplyr)
set.seed(100)

# Définition d'une fonction pour obtenir les résultats de la régression ridge
get_results<- function(lambda){
  coeffs_total=data.frame(V1=numeric(),V2=numeric(),V3=numeric(),V4=numeric(),V5=numeric())
  for (q in 1:100){
    V1 = runif(1000)*100
    V2 = runif(1000)*10 + V1
    
    V3 = runif(1000)*100
    V4 = runif(1000)*10 + V3
    
    V5 = runif(1000)*100
    Residuals = runif(1000)*100
    Y = V1 + V2 + V3 + V4 + Residuals
    
    # Régression linéaire
    coefs_lm <- lm(Y ~ V1 + V2 + V3 + V4 + V5)$coefficients
    
    # Régression ridge
    coefs_rd <- glmnet(cbind(V1, V2, V3, V4, V5), Y, lambda = lambda, alpha = 0)$beta
    
    frame1 <- data.frame(V1 = coefs_lm[2], V2 = coefs_lm[3], V3 = coefs_lm[4], V4 = coefs_lm[5], V5 = coefs_lm[6], method = "lm")
    frame2 <- data.frame(V1 = coefs_rd[1], V2 = coefs_rd[2], V3 = coefs_rd[3], V4 = coefs_rd[4], V5 = coefs_rd[5], method = "ridge")
    coeffs_total <- rbind(coeffs_total, frame1, frame2)
  }
  transposed_data = gather(coeffs_total, "variable", "value", 1:5)
  print(transposed_data %>% group_by(variable, method) %>% summarise(median = median(value)))
  ggplot(transposed_data, aes(x = variable, y = value, fill = method)) + geom_boxplot()
}

# Appel de la fonction avec lambda = 0.1
get_results(0.1)

V1 = runif(1000)*100
V2 = runif(1000)*10 + V1
V3 = runif(1000)*100
V4 = runif(1000)*10 + V3
V5 = runif(1000)*100
Residuals = runif(1000)*100
Y = V1 + V2 + V3 + V4 + Residuals

# Régression lasso avec validation croisée
cv.lasso = cv.glmnet(cbind(V1, V2, V3, V4, V5), Y, alpha = 1)
cv.lasso
plot(cv.lasso)

# Lab 3.9
library(MASS)
library(tidyr)
library(ggplot2)
library(glmnet)
library(dplyr)
set.seed(100)

# Définition d'une fonction pour obtenir les résultats de la régression lasso
get_results <- function(lambda){
  coeffs_total = data.frame(V1 = numeric(), V2 = numeric(), V3 = numeric(), V4 = numeric(), V5 = numeric())
  for (q in 1:100){
    V1 = runif(1000) * 100
    V2 = runif(1000) * 10 + V1
    
    V3 = runif(1000) * 100
    V4 = runif(1000) * 10 + V3
    
    V5 = runif(1000) * 100
    Residuals = runif(1000) * 100
    Y = V1 + V2 + V3 + V4 + Residuals
    
    # Régression linéaire
    coefs_lm <- lm(Y ~ V1 + V2 + V3 + V4 + V5)$coefficients
    
    # Régression lasso
    coefs_lasso <- glmnet(cbind(V1, V2, V3, V4, V5), Y, lambda = lambda, alpha = 1)$beta
    
    frame1 <- data.frame(V1 = coefs_lm[2], V2 = coefs_lm[3], V3 = coefs_lm[4], V4 = coefs_lm[5], V5 = coefs_lm[6], method = "lm")
    frame2 <- data.frame(V1 = coefs_lasso[1], V2 = coefs_lasso[2], V3 = coefs_lasso[3], V4 = coefs_lasso[4], V5 = coefs_lasso[5], method = "lasso")
    coeffs_total <- rbind(coeffs_total, frame1, frame2)
  }
  transposed_data = gather(coeffs_total, "variable", "value", 1:5)
  print(transposed_data %>% group_by(variable, method) %>% summarise(median = median(value)))
  ggplot(transposed_data, aes(x = variable, y = value, fill = method)) + geom_boxplot()
}

# Appel de la fonction avec lambda = 8
get_results(8)
# Appel de la fonction avec lambda = 0.1
get_results(0.1)

V1 = runif(1000) * 100
V2 = runif(1000) * 10 + V1
V3 = runif(1000) * 100
V4 = runif(1000) * 10 + V3
V5 = runif(1000) * 100
Residuals = runif(1000) * 100
Y = V1 + V2 + V3 + V4 + Residuals

# Régression lasso avec validation croisée
cv.lasso = cv.glmnet(cbind(V1, V2, V3, V4, V5), Y, alpha = 1)
cv.lasso
plot(cv.lasso)

# Lab 3.10
library(car)

data = read.csv("./house_prices_aug.csv")

# Modèle linéaire
model = lm(Property_price ~ size + number.bathrooms + number.bedrooms + number.entrances + size_balcony + size_entrance, data = data)
plot(model)

# Graphiques de levier
leveragePlots(model)

# Test de valeurs aberrantes
outlierTest(model)

# Graphique des valeurs de levier
plot(hatvalues(model), type = "h")

# Calcul des distances de Cook
cooksd <- sort(cooks.distance(model))
cutoff <- 4/((nrow(data) - length(model$coefficients) - 1))

# Plot des valeurs de Cook
plot(model, which = 4, cook.levels = cutoff)

# Régression linéaire sans l'observation 408
model2 = lm(Property_price ~ size + number.bathrooms + number.bedrooms + number.entrances + size_balcony + size_entrance, data = data[-c(408),])

model
model2
