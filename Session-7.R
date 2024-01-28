# Session-1:    

set.seed(95)

# Fonction pour générer une série temporelle autoregressive
build_series = function(coef){
  start_value    = 90
  values         = c(start_value)
  previous_value = values
  
  for (x in 1:200){
    current_value  = coef*previous_value + rnorm(1,0,10)
    values         = c(values,current_value)
    previous_value = current_value
  }
  return (values)
}

# Génération de séries temporelles avec différents coefficients
ss1 = build_series(1)
ss2 = build_series(1)
ss3 = build_series(1)

# Tracé des séries temporelles
plot(ss1, type = "l", ylim = c(-200, 300))
lines(ss2, col = "red")
lines(ss3, col = "green")

s1 = build_series(0.2)
s2 = build_series(0.2)
s3 = build_series(0.2)

plot(s1, type = "l", ylim = c(-50, 50))
lines(s2, col = "red")
lines(s3, col = "green")

# Fonction d'autocorrélation
acf(s1)

# Modèle ARIMA pour s1
arima(s1, order = c(1, 0, 0), include.mean = FALSE)

# Autocorrélation de la série ss1
acf(ss1)

# Modèle ARIMA pour ss1
arima(ss1, order = c(1, 0, 0), include.mean = FALSE)

# Différenciation de la série s1
s1_d = diff(s1)
plot(s1_d, type = "l")
acf(s1_d)

# Lab 7.2
library(forecast)  

# Chargement des données sur la production automobile
car_production = read.csv("./car_production.csv")  

# Conversion de la colonne d'indice de temps en date
car_production$indice_tiempo = as.Date(car_production$indice_tiempo, "%Y-%m-%d")  

# Création d'une série temporelle
car_production = ts(car_production$produccion_automotriz_unidades, start = c(1981, 1), frequency = 12)  

# Tracé de la série temporelle
plot.ts(car_production) 

# Autocorrélation de la série temporelle différenciée
acf(diff(car_production)) 

# Modèle ARIMA pour la série temporelle
results <- arima(car_production, order = c(1, 1, 0), seasonal = c(2, 0, 0, 12)) 
results 

# Prédictions
predictions <- forecast(results, 24) 
plot(predictions) 
plot.ts(results$residuals) 
qqnorm(results$residuals) 

# Lab 7.3
library(forecast) 

# Chargement des données de température moyenne
average_temp = read.csv("./temperature.csv") 
average_temp$indice_tiempo = as.Date(average_temp$indice_tiempo, "%Y-%m-%d") 
average_temp = ts(average_temp$temperatura_promedio, start = c(2001, 1), frequency = 12)

# Tracé de la série temporelle
plot.ts(average_temp) 

# Identification automatique du meilleur modèle ARIMA
best_mode = auto.arima(average_temp, max.p = 5, max.q = 5, max.Q = 2, max.P = 2, allowmean = TRUE, allowdrift = FALSE)

# Tracé de la série temporelle et des valeurs ajustées
plot.ts(average_temp) 
lines(best_mode$fitted, col = "red") 

# Prédictions
predictions <- forecast(best_mode$fitted, 48) 
plot(predictions) 

# Lab 7.4
install.packages("vars")
library(vars) 

# Chargement des données sur la production de pétrole et de gaz
oilgas = read.csv("./fuel_series.csv") 
colnames(oilgas) = c("time_index", "oil_processed", "gasoil_prod", "fueloil_prod", "butane")
joined_data = ts(oilgas[-1], start = c(1996, 1), frequency = 12) 

# Modèle VAR
m = VAR(joined_data, p = 12) 
summary(m)
restrict(m, method = "ser") 
any(roots(m) > 0.9999)

normalitytest <- normality.test(m)
plot(normalitytest)
plot(m)

# Fonction de densité conditionnelle
var.2c.fevd <- fevd(m, n.ahead = 5)
plot(var.2c.fevd)

# Fonction de réponse à l'impulsion
var.2c.irf <- irf(m, impulse = "oil_processed", response = c("butane", "gasoil_prod", "fueloil_prod"), boot = TRUE)
plot(var.2c.irf)

# Prédictions
var.2c.prd <- predict(m, n.ahead = 24, ci = 0.95)
plot(var.2c.prd)

# Lab 7.5
install.packages("imputeTS")
install.packages("timesboot")
library(imputeTS) 
library(timesboot) 

# Chargement des données sur l'acier
steel = read.csv("./steel.csv") 
steel = ts(steel$valor, start = c(1993, 1), frequency = 12) 
plot.ts(steel)  

# Estimation du spectre
spectrum(steel, spans = c(5, 7), lty = 1) 

# Décomposition en tendance et résidus
steel_trend = decompose(steel) 
corrected = steel - steel_trend$trend 

# Imputation des données manquantes
corrected = imputeTS::na.kalman(corrected) 
plot.ts(corrected)  

# Détection des points aberrants
td = timesboot::boot_spec(corrected, de_trend = FALSE) 
