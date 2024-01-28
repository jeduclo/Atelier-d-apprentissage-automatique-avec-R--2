# Session-8:    

data = read.csv("./sample_random_regression.csv") 

# Conversion de la colonne 'clientid' en facteur
data$clientid = as.factor(data$clientid) 

# Chargement de la bibliothèque lme4
library("lme4") 

# Modèle mixte linéaire (LMM) sans intercept et avec effets aléatoires pour 'salespeople_involved' et 'time_spent_deal'
lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (-1 + salespeople_involved|clientid) + (-1 + time_spent_deal|clientid) ) 

# LMM avec intercept et effet aléatoire pour 'salespeople_involved'
lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (1 + salespeople_involved|clientid) ) 

# LMM avec intercept et effets aléatoires pour 'salespeople_involved' et 'time_spent_deal'
lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (1 + time_spent_deal + salespeople_involved|clientid) ) 

# Modèle LMM avec intercept et effets aléatoires pour 'salespeople_involved' et 'time_spent_deal'
model = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (-1 + salespeople_involved|clientid) + (-1 + time_spent_deal|clientid) ) 

# Affichage des effets aléatoires
ranef(model) 

# Lab 8.2
install.packages("sjmisc")
library(dplyr) 
library("lme4") 
library(ggplot2) 
library(sjPlot)
library(sjmisc)

# Chargement des données
data = read.csv("./sample_random_regression.csv") 

# Conversion de la colonne 'clientid' en facteur
data$clientid = as.factor(data$clientid) 

# Tracé d'un graphique de dispersion avec une régression linéaire
ggplot(data=data, aes(x=salespeople_involved, y=deal_size, col=clientid))+  
  geom_point(size=.7, alpha=.8, position = "jitter")+ 
  geom_smooth(method=lm,se=FALSE, size=0.6, alpha=.5)+theme_minimal()+labs(title="Régression linéaire",  subtitle="Les ventes", col= "Client") 

# Modèle LMM avec intercept et effets aléatoires pour 'time_spent_deal' et 'salespeople_involved'
model = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal +  (1+time_spent_deal+salespeople_involved|clientid) )

# Tracé de plusieurs graphiques en facettes
ggplot(data) +  
  aes(x = time_spent_deal, y = deal_size) +  
  geom_point() + 
  facet_wrap("clientid") 

# Création d'un tableau de coefficients
F = coef(model)$clie
F$clientid = row.names(F)

# Tracé d'un graphique en barres
ggplot(F,aes(x=reorder(clientid, -salespeople_involved), y=salespeople_involved)) +  
  geom_bar(stat="identity", color="blue", fill="white") + labs(x = "Client",y = "Effet Fixe + Effet Aléatoire",title =  
                                                                 "Pente Salespeople_involved / Deal size") 

# Tracé des effets aléatoires
plot_model(model,type="re")

# Tracé d'un graphique de résidus en fonction de 'clientid'
plot(model, clientid ~ resid(., scaled=TRUE)) 

# Tracé d'un graphique de résidus en fonction de 'deal_size' et 'clientid'
plot (model, deal_size ~ fitted(.) | clientid, abline = c (0,1)) 

# Tracé d'un graphique de résidus en fonction de 'salespeople_involved' et 'clientid'
plot(model, resid(., scaled=TRUE) ~ salespeople_involved | clientid, abline = 0) 

# Tracé d'un graphique de résidus en fonction de 'time_spent_deal' et 'clientid'
plot(model, resid(., scaled=TRUE) ~ time_spent_deal | clientid, abline = 0) 


# Lab 8.3
set.seed(10) 

# Génération de données aléatoires X
X = 7*runif(1000) 

# Création d'un vecteur G
G = c() 

# Remplissage du vecteur G
for (x in 1:100){ 
  G = c(G,rep(x,10)) 
} 

# Création d'un cadre de données 'pre____frame' avec X, G et du bruit
pre____frame = cbind(X=X, G=G, NOISE = rnorm(1000,0,0.03)) 

# Création d'un cadre de données 'shocks_frame' avec G et des chocs aléatoires
shocks_frame = cbind(G = 1:100, shocks = rnorm(100,0,1)) 

# Fusion des deux cadres de données 'pre____frame' et 'shocks_frame' par la colonne 'G'
merged_frame = merge(pre____frame, shocks_frame, by="G") 

# Calcul de la colonne 'Y' en fonction de 'X', 'shocks', et 'NOISE'
merged_frame$Y = 1/(1+exp(-merged_frame$X + merged_frame$shocks)) + merged_frame$NOISE 

# Sélection des colonnes 'X', 'G' et 'Y' dans le cadre de données 'merged_frame'
XYG  = merged_frame[,c(1,2,5)] 

# Définition d'une fonction 'get__loglikelihood' pour calculer la log-vraisemblance
get__loglikelihood = function(params){ 
  BETA1  = params[1] # Paramètre BETA1
  SIGMAG = params[2] # Paramètre SIGMAG
  SIGMA  = params[3] # Paramètre SIGMA
  GROUP_LIK = 0 # Initialisation de la log-vraisemblance du groupe
  IS_ERROR = FALSE # Initialisation d'un indicateur d'erreur
  
  for (q in 1:100){ # Pour chaque groupe
    group_data = XYG[XYG$G==q,] # Sélection des données du groupe actuel
    average = 0; 
    
    for (sim in 1:500){ # Simulation Monte Carlo (500 échantillons)
      group_data$shock = rnorm(10,0,SIGMAG) # Génération de chocs aléatoires
      group_data$pred = 1/(1+exp(-BETA1*group_data$X + group_data$shock)) # Prédiction du modèle
      
      mult = 1 
      
      for (x in 1:10){ # Calcul de la densité de probabilité multivariée
        mult = mult * dnorm(group_data$pred[x]-group_data$Y[x],0,SIGMA) 
      } 
      
      average = average + mult 
      
    } 
    
    average = average/500 # Calcul de la moyenne sur les échantillons Monte Carlo
    average = log(average) # Calcul du logarithme naturel de la moyenne
    
    GROUP_LIK = GROUP_LIK + average # Ajout de la log-vraisemblance du groupe actuel à la log-vraisemblance totale
  } 
  
  if (is.na(GROUP_LIK) | GROUP_LIK == -Inf){ # Gestion d'erreurs possibles
    GROUP_LIK = -1000000000 # Valeur minimale de log-vraisemblance en cas d'erreur
  } 
  
  return(-GROUP_LIK) # Retourne la négation de la log-vraisemblance totale
} 

# Affichage de l'heure actuelle
Sys.time() 

# Optimisation des paramètres de la fonction 'get__loglikelihood' avec la méthode BFGS
optim(c(1,1,0.03), get__loglikelihood, method="BFGS", control=list(trace=1, REPORT=1)) 

# Affichage de l'heure actuelle
Sys.time() 


# Lab 8.4
data = read.csv("./company_areas.csv") 

# Tableau croisé dynamique pour les variables Group et Person
xtabs(~ Group + Person, data) 

# Modèle linéaire mixte sans intercept pour évaluer l'effet aléatoire de Group et Person
lmer(Rating ~ -1 + (1 | Group/Person), data = data) 

# Modèle linéaire mixte sans intercept pour évaluer l'effet aléatoire de Group et Person
lmer(Rating ~ -1 + (1 | Group) + (1 | Person), data = data) 

data2 = read.csv("./company_areas2.csv") 

# Tableau croisé dynamique pour les variables Group et Person dans data2
xtabs(~ Group + Person, data2)

# Modèle linéaire mixte sans intercept pour évaluer l'effet aléatoire de Group et Person dans data2
lmer(Rating ~ -1 + (1 | Group/Person), data = data2) 

# Modèle linéaire mixte sans intercept pour évaluer l'effet aléatoire de Group et Person dans data2
lmer(Rating ~ -1 + (1 | Group) + (1 | Person), data = data2) 

# Lab 8.5
install.packages("robustlmm")
library(lme4)
library(robustlmm)
set.seed(10) 

X = 7*runif(1000) 
G = c() 

for (x in 1:100){ 
  G = c(G,rep(x,10)) 
} 

pre____frame = cbind(X=X, G=G, NOISE = rnorm(1000,0,0.03)) 
shocks_frame = cbind(G = 1:100, shocks = rnorm(100,0,1)) 
merged_frame = merge(pre____frame, shocks_frame, by="G") 
merged_frame$Y = 10 + merged_frame$shocks + merged_frame$NOISE 
XYG = merged_frame[,c(1,2,5)] 

# Modèle linéaire mixte avec un effet aléatoire pour G
lmer(data=XYG, Y ~ 1 + (1|G))
# Modèle linéaire mixte robuste avec un effet aléatoire pour G
rlmer(data=XYG, Y ~ 1 + (1|G))

# Sélection aléatoire de 50 positions dans XY pour introduire des erreurs
positions = sample(1:1000, 50, replace=T)
XYG[positions,"Y"] = rnorm(1,50,10)

# Modèle linéaire mixte avec un effet aléatoire pour G après l'introduction d'erreurs
lmer(data=XYG, Y ~ 1 + (1|G))
# Modèle linéaire mixte robuste avec un effet aléatoire pour G après l'introduction d'erreurs
rlmer(data=XYG, Y ~ 1 + (1|G))

# Affichage du graphique du modèle robuste
model = rlmer(data=XYG, Y ~ 1 + (1|G))
plot(model)

# Lab 8.6
install.packages("cAIC4")
library("lme4") 
library(cAIC4) 
set.seed(25) 

data = read.csv("./sample_random_regression.csv") 
data$clientid = as.factor(data$clientid) 

# Ajout de variables ERR_1, ERR_2, ERR_3 et ERR_4 au jeu de données
data$ERR_1 = rnorm(100,0,10) 
data$ERR_2 = rnorm(100,0,10) 
data$ERR_3 = rnorm(100,0,10) 
data$ERR_4 = rnorm(100,0,10) 

# Création de plusieurs modèles linéaires mixtes
m1 = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (-1 + salespeople_involved|clientid) + (-1 + time_spent_deal|clientid) ) 
m2 = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (1  + salespeople_involved|clientid) ) 
m3 = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (1  + time_spent_deal + salespeople_involved|clientid) )  
m4 = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + ERR_1 + ERR_2 + ERR_3 + ERR_4 + (1  + time_spent_deal + salespeople_involved|clientid) )  
m5 = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + ERR_1 + ERR_2 + ERR_3  + (1  + time_spent_deal + salespeople_involved|clientid) )  
m6 = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + ERR_1 + ERR_2  + (1  + time_spent_deal + salespeople_involved|clientid) )  
m7 = lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + ERR_1  + (1  + time_spent_deal + salespeople_involved|clientid) ) 

# Calcul de l'information de Akaike corrigée (cAIC) pour chaque modèle
cAIC(m1)$caic 
cAIC(m2)$caic 
cAIC(m3)$caic 
cAIC(m4)$caic 
cAIC(m5)$caic 
cAIC(m6)$caic 
cAIC(m7)$caic 

# Lab 8.7
install.packages("emmeans")
library(lme4) 
library(emmeans) 
library(MASS) 
set.seed(10) 

# Modèle de régression linéaire généralisée pour l'effet fixe
fixed_std_model  = glm(decrease ~ treatment, family=poisson(), data=OrchardSprays) 

# Résumé du modèle de régression linéaire généralisée
summary(fixed_std_model) 

# Comparaison des moyennes ajustées par traitement avec l'ajustement de Tukey
emmeans(fixed_std_model, list(pairwise ~ treatment), adjust = "tukey", type="response")

# Prédiction de la réponse pour le traitement "D"
predict(fixed_std_model, data.frame(treatment="D"), type="response")

# Graphique du modèle de régression linéaire généralisée
plot(fixed_std_model) 

# Modèle linéaire mixte généralisé avec effets aléatoires pour colpos et rowpos
model_1 = lme4::glmer(decrease ~ treatment + (1|colpos) + (1|rowpos), family = poisson(), data = OrchardSprays) 

# Extraction des estimations des paramètres theta et fixef
ss <- getME(model_1, c("theta", "fixef")) 

# Mise à jour du modèle avec les estimations initiales
model_2 <- update(model_1, start = ss) 

# Résumé du modèle linéaire mixte généralisé mis à jour
summary(model_2) 

# Effets aléatoires estimés
ranef(model_2) 

# Effets fixes estimés
fixef(model_2) 

# Variance des effets aléatoires
VarCorr(model_2) 

# Graphiques des résidus en fonction des ajustements pour colpos et rowpos
plot(model_2, resid(., scaled=TRUE) ~ fitted(.) | colpos, abline = 0)  
plot(model_2, resid(., scaled=TRUE) ~ fitted(.) | rowpos, abline = 0) 

# Intervalle de confiance pour le modèle linéaire mixte généralisé
confint.merMod(model_2) 

# Comparaison des moyennes ajustées par traitement avec l'ajustement de Tukey pour le modèle linéaire mixte généralisé
emmeans(model_2, list(pairwise ~ treatment), adjust = "tukey", type="response") 

# Modèle de régression linéaire négative binomiale pour l'effet fixe
fixed_std_model  = glm.nb(decrease ~ treatment, data = OrchardSprays) 

# Résumé du modèle de régression linéaire négative binomiale
summary(fixed_std_model)  

# Modèle linéaire mixte généralisé avec effet fixe et effets aléatoires pour colpos et rowpos
model_2 = lme4::glmer.nb(decrease ~ treatment + (1|colpos) + (1|rowpos), data = OrchardSprays) 

# Extraction des estimations des paramètres theta et fixef
ss2 <- getME(model_2, c("theta", "fixef")) 

# Mise à jour du modèle linéaire mixte généralisé mis à jour
model_3 <- update(model_1, start = ss2) 

# Résumé du modèle linéaire mixte généralisé mis à jour
summary(model_3) 

# Lab 8.8
data = read.csv("C:\\R_book\\sample_random_regression.csv") 

# Conversion de la variable clientid en facteur
data$clientid = as.factor(data$clientid) 

# Modèle linéaire mixte sans intercept pour évaluer l'effet aléatoire de clientid
lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (-1 + salespeople_involved|clientid) + (-1 + time_spent_deal|clientid) ) 

# Modèle linéaire mixte sans intercept pour évaluer l'effet aléatoire de clientid
lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (1  + salespeople_involved|clientid) ) 

# Modèle linéaire mixte sans intercept pour évaluer l'effet aléatoire de clientid
lmer(data=data, deal_size ~ salespeople_involved + time_spent_deal + (1  + time_spent_deal + salespeople_involved|clientid) ) 

# Extraction des effets aléatoires du modèle
ranef(model) 

