# Session-2:  Visualisation de données avec ggplot2 et Test

# Chargement des bibliothèques nécessaires
library(dplyr)
library(car)

# Lecture des données depuis un fichier CSV
data = read.csv("./heights.csv")

# Transformation de la variable "Sample" en facteur
data$Sample = as.factor(data$Sample)

# Test de Levene pour vérifier l'égalité des variances
leveneTest(Height ~ Sample, data)

# Sélection des échantillons 1 et 2
sample1 = data %>% filter(Sample==1) %>% select(Height)
sample2 = data %>% filter(Sample==2) %>% select(Height)

# Test t pour comparer les échantillons avec variance égale
t.test(sample1, sample2, var.equal=TRUE, conf.level = .95, alternative="two.sided")

# Lab 2.2
# Fonction pour calculer les alphas effectifs
calc_effective_alphas <- function(n, sd1, sd2, equalvar) {  
  rejected <- 0 
  for (x in 1:100000) { 
    data1 <- rnorm(n, 10, sd1) 
    data2 <- rnorm(n, 10, sd2) 
    result <- t.test(data1, data2, var.equal = equalvar)$p.value 
    if (result < 0.05) { 
      rejected <- rejected + 1 
    } 
  } 
  return (rejected/100000) 
} 

# Affichage des alphas effectifs pour différentes conditions
print(paste("n=10 / sd1=2 / sd2=5 / alpha effectif =", calc_effective_alphas(10, 2, 5, TRUE))) 
print(paste("n=10 / sd1=2 / sd2=20/ alpha effectif =", calc_effective_alphas(10, 2, 20, TRUE))) 
print(paste("n=10 / sd1=2 / sd2=5 / alpha effectif =", calc_effective_alphas(10, 2, 5, FALSE))) 
print(paste("n=10 / sd1=2 / sd2=20/ alpha effectif =", calc_effective_alphas(10, 2, 20, FALSE))) 

# Fonction pour calculer la puissance
calc_power <- function(n, sd1, sd2, equalvar) {  
  rejected <- 0 
  for (x in 1:100000) { 
    data1 <- rnorm(n, 12, sd1) 
    data2 <- rnorm(n, 10, sd2) 
    result <- t.test(data1, data2, var.equal = equalvar)$p.value 
    if (result < 0.05) { 
      rejected <- rejected + 1 
    } 
  } 
  return (rejected/100000) 
} 

# Affichage de la puissance pour différentes conditions
print(paste("n=10 / sd1=2 / sd2=20/ puissance effectif =", calc_power(10, 2, 2, TRUE))) 
print(paste("n=10 / sd1=2 / sd2=20/ puissance effectif =", calc_power(10, 2, 2, FALSE)))

# Lab 2.3
# Lecture des données depuis un fichier CSV
data = read.csv("./pre_post_employee.csv") 

# Test t apparié pour les données pré/post
t.test(data$post_bonus, data$pre_bonus, conf.level = .95, alternative = "greater", paired = TRUE) 
t.test(data$post_bonus, data$pre_bonus, conf.level = .95, alternative = "less", paired = TRUE) 

# Lab 2.4
# Lecture des données depuis un fichier CSV
data = read.csv("./anova__lot_type.csv") 

# Régression linéaire pour l'analyse de variance (ANOVA)
result = lm(Result ~ Lot, data = data) 

# Calcul des sommes de carrés pour "Lot"
SS_LOT = sum((predict(result) - mean(data$Result))**2) 

# Régression linéaire pour l'ANOVA avec "Food.Type"
result = lm(Result ~ Lot + Food.Type, data = data) 

# Calcul des sommes de carrés pour "Food.Type"
SS_FOODTYPE = sum((predict(result) - mean(data$Result))**2) - SS_LOT 

# Calcul des sommes de carrés de l'erreur
SS_ERROR = sum((predict(result) - data$Result)**2) 

# Calcul des statistiques F et p-values
FF_LOT = (SS_LOT/1)/(SS_ERROR/56) 
FF_FOODTYPE = (SS_FOODTYPE/2)/(SS_ERROR/56) 
pval_LOT = 1 - pf(FF_LOT, 1, 56) 
pval_FOODTYPE = 1 - pf(FF_FOODTYPE, 2, 56) 

# Affichage des résultats
print(paste("SS(ERREUR) =", SS_ERROR)) 
print(paste("SS(LOT) =", SS_LOT, "/ F(LOT) =", FF_LOT, "/ p-valeur =", pval_LOT)) 
print(paste("SS(FOODTYPE) =", SS_FOODTYPE, "/ F(FOODTYPE) =", FF_FOODTYPE, "/ p-valeur =", pval_FOODTYPE)) 

# Analyse de variance (ANOVA)
anova(result) 

# Lab 2.5
# Lecture des données depuis un fichier CSV
data = read.csv("./2wayanova.csv") 

# ANOVA à deux facteurs avec interaction
d = aov(Sales ~ Colour + Font + Font*Colour, data = data) 

# Graphique des résidus pour l'interaction
plot(d, 2) 

# Test de normalité des résidus
shapiro.test(residuals(d)) 

# Suppression des lignes 22 et 44 des données
data = data[-c(22,44),] 

# Réexécution de l'ANOVA après suppression des lignes
d = aov(Sales ~ Colour + Font + Font*Colour, data = data) 

# Graphiques des résidus après suppression des lignes
plot(d, 2) 
shapiro.test(residuals(d)) 

# Graphique principal
plot(d, 1) 

# ANOVA à deux facteurs avec interaction (après suppression de lignes)
anova(d) 

# Tests de comparaisons multiples de Tukey
rbind(TukeyHSD(d)$Colour, TukeyHSD(d)$Font) 

# Lab 2.6
# Chargement des bibliothèques nécessaires
library(dplyr)
library(car)

# Lecture des données depuis un fichier CSV
r = read.csv("./2wayanova.csv") 

# Suppression des lignes 22 et 44 des données
r = r[-c(22,44),] 

# ANOVA à deux facteurs avec interaction
type1 = aov(Sales ~ Colour + Font + Font*Colour, data = r) 

# Résumé de l'ANOVA
summary(type1)

# ANOVA de type II
type2 = Anova(aov(Sales ~ Colour + Font, data = r), type = c("II")) 
type2

# Configuration des contrastes
options(contrasts = c("contr.sum", "contr.poly")) 

# ANOVA de type III
type3 = Anova(aov(Sales ~ Colour + Font + Font*Colour, data = r), type = c("III")) 
type3 

# Lab 2.7
# Chargement des bibliothèques nécessaires
install.packages("lme4")
install.packages("lmerTest")
install.packages("lsmeans")
library(lme4) 
library(lmerTest) 
library(lsmeans) 

# Lecture des données depuis un fichier CSV
clients <- read.csv("./clients.csv") 

# Modèle linéaire mixte (LMM)
E = lmer(Sales ~ -1 + Strategy + (1|Client) + (1|Salesman), data = clients)
summary(E) 

# Comparaisons des moyennes avec ajustement de Bonferroni
result_lsmeans = lsmeans(E, pairwise ~ Strategy) 
print(result_lsmeans) 

# Analyse de variance (ANOVA) pour le modèle linéaire mixte
anova(E)


# Lab 2.8

# Charger les bibliothèques nécessaires
library(ggplot2)
library(nlme)
library(lmerTest)
library(lsmeans)

# Lire les données à partir d'un fichier CSV
data_company = read.csv("./employee_time_bonus.csv", stringsAsFactors = TRUE)

# Créer un graphique de lignes et de points avec facettes
ggplot(data_company, aes(x = Time, y = Performance)) + geom_line() + 
  geom_point(data = data_company, aes(x = Time, y = Performance)) + 
  facet_wrap(~Employee, nrow = 3)

# Modèle mixte linéaire (lme) avec corrélation AR(1)
fit <- lme(Performance ~ x_Bonus + x_Sector + Time + Time:x_Bonus, 
           random = list(~1 | Employee),
           correlation = corAR1(form = ~Time | Employee),
           data = data_company)
summary(fit)

# Analyse de la variance
anova(fit)

# Régression linéaire mixte sans corrélation AR(1)
fit <- lme(Performance ~ x_Bonus + x_Sector + Time + Time:x_Bonus, 
           random = list(~1 | Employee),
           data = data_company)
summary(fit)

# Régression linéaire mixte avec corrélation AR(1) sans interaction
fit <- lme(Performance ~ x_Bonus + x_Sector + Time, 
           random = list(~1 | Employee),
           correlation = corAR1(form = ~Time | Employee),
           data = data_company)
print(lsmeans(fit, pairwise ~ x_Bonus))

# Régression linéaire mixte avec interaction et corrélation AR(1)
fit <- lme(Performance ~ x_Bonus + x_Sector + Time + Time:x_Bonus, 
           random = list(~1 + Time | Employee),
           correlation = corAR1(form = ~Time | Employee),
           data = data_company)
summary(fit)

# Lab 2.9

# Charger les bibliothèques nécessaires
library(MASS)
install.packages("Hotelling")
library(Hotelling)

# Lire les données de classe
class1 = read.csv("./class1.csv")
class2 = read.csv("./class2.csv")

# Calculer les moyennes des deux classes
sapply(class1, mean)
sapply(class2, mean)

# Effectuer un test Hotelling T^2
test_hotelling = hotelling.test(class1, class2)
print(test_hotelling)

# Charger la bibliothèque heplots
install.packages("heplots")
library(heplots)

# Créer un facteur de groupe et fusionner les données
class1$group = "1"
class2$group = "2"
combined = rbind(class1, class2)
combined$group = as.factor(combined$group)

# Effectuer un test Box's M
boxM(cbind(combined$Math, combined$History, combined$Sociology) ~ group, data = combined)

# Lab 2.10

# Charger la bibliothèque MASS
library(MASS)

# Créer une matrice de covariance
f = matrix(c(2, 1, 1, 1, 2, 1, 1, 1, 2), 3, 3)

# Générer des échantillons multivariés
x1 = mvrnorm(50, c(10, 10, 10), f)
x1 = cbind(x1, 1)
x2 = mvrnorm(50, c(10, 10, 10), f)
x2 = cbind(x2, 2)
x3 = mvrnorm(50, c(30, 10, 10), f)
x3 = cbind(x3, 3)

# Créer un dataframe total
total_data = data.frame(rbind(x1, x2, x3))
colnames(total_data) = c("Histoire", "Math", "Biologie", "classe")

# Effectuer une MANOVA
result = manova(cbind(Histoire, Math, Biologie) ~ classe, data = total_data)
summary(result)

# Effectuer une ANOVA
summary.aov(result)

# Lab 2.11

# Charger la bibliothèque dplyr
library(dplyr)

# Lire les données à partir d'un fichier CSV
data = read.csv("./heights.csv")

# Convertir la variable Sample en facteur
data$Sample = as.factor(data$Sample)

# Effectuer un test de Levene
leveneTest(Height ~ Sample, data)

# Filtrer les échantillons par groupe
sample1 = data %>% filter(Sample == 1) %>% select(Height)
sample2 = data %>% filter(Sample == 2) %>% select(Height)

# Effectuer un test t de Student
t.test(sample1, sample2, var.equal = TRUE, conf.level = 0.95, alternative = "two.sided")
