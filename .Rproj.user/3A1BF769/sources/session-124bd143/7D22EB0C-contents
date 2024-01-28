# Session-5:    
# Chargement des données
data = read.csv("./heights.csv") 
data$Sample = as.factor(data$Sample) 

# Test de Wilcoxon pour comparer les hauteurs entre les échantillons
wilcox.test(Height ~ Sample, data = data) 

# Chargement des données
data = read.csv("./pre_post_employee.csv") 

# Test de Wilcoxon pour comparer les bonus avant et après
wilcox.test(data$post_bonus, data$pre_bonus, paired = TRUE) 

# Lab 5.2
install.packages("FSA")
library(FSA) 
library(dplyr) 

# Chargement des données
t = read.csv("./anova__lot_type.csv") 

# Test de Kruskal-Wallis pour comparer les résultats en fonction du type de nourriture
kruskal.test(Result ~ Food.Type, data = t)

# Test de Dunn pour les comparaisons post-hoc
dunnTest(Result ~ Food.Type, data = t) 

# Lab 5.3
x = seq(1,100) 
y = 20/(1+exp(x-50)) 

# Tracé d'un graphique
plot(x, y) 

# Test de corrélation de Spearman
cor.test( ~ x + y, method = "spearman", conf.level = 0.95) 

# Test de corrélation de Pearson
cor.test( ~ x + y, method = "pearson", conf.level = 0.95) 

x = seq(1,100) 
y = sapply(x, function(x) {(runif(1)-0.5)*10 + 20/(1+exp(x-50))})  
plot(x, y) 

# Test de corrélation de Spearman
cor.test( ~ x + y, method = "spearman", conf.level = 0.95) 

# Test de corrélation de Pearson
cor.test( ~ x + y, method = "pearson", conf.level = 0.95) 

salary = c(10,50,45,87,69,100) 
educ_level = c(1,2,3,4,5,6) 

# Test de corrélation de Spearman entre le salaire et le niveau d'éducation
cor.test( ~ salary + educ_level, method = "spearman", conf.level = 0.95) 

# Lab 5.4
data = read.csv("./price__sales.csv") 

# Ajustement d'un modèle de lissage loess avec différents paramètres de span
model_loess1 = loess(data$sales ~ data$price,  span = 2/3, degree = 2, family = c("gaussian")) 
model_loess2 = loess(data$sales ~ data$price,  span = 0.1, degree = 2, family = c("gaussian")) 

# Fonctions de prédiction pour les modèles loess
loess1_wrapper <- function(x) { 
  return (predict(model_loess1, x)) 
}  

loess2_wrapper <- function(x) { 
  return (predict(model_loess2, x)) 
} 

# Comparaison des prédictions à différentes valeurs de price
loess1_wrapper(5) - loess1_wrapper(10)
loess1_wrapper(10) - loess1_wrapper(15)

# Tracé du nuage de points et des courbes de lissage
plot(data$price, data$sales) 
curve(loess1_wrapper, add = TRUE, col = "red", lwd = 3) 
curve(loess2_wrapper, add = TRUE, col = "blue", lwd = 3) 
legend(17.7, 0.5, legend = c("span=0.75", "span=0.1"), col = c("red", "blue"), lty = 1:1, cex = 0.8) 

# Tracé avec ggplot
ggplot(data, aes(x = price, y = sales)) + geom_point(size = 2, shape = 1) + geom_smooth(se = TRUE, method = "loess") 

# Lab 5.5
install.packages("acepack")
library(acepack)

# Chargement des données
data = read.csv("./house_prices.csv") 
x = data[,2:7] 
y = data[,1] 

# Modèle de régression linéaire multiple
lm_model = lm(data = data, Property_price ~ size + number.bathrooms + number.bedrooms + number.entrances + size_balcony + size_entrance) 
summary(lm_model) 

# Modèle ACE (Average Causal Effect)
ace_model = ace(x, y) 
ace_model$rsq 

par(mfrow=c(1,2))
plot(ace_model$x[1,], ace_model$tx[,1], xlab = "untransformed size_entrance", ylab = "transformed size_entrance") 
plot(ace_model$x[5,], ace_model$tx[,5], xlab = "untransformed size_balcony", ylab = "transformed size_balcony") 

# Lab 5.6
install.packages("npmv")
library(npmv)

# Test non paramétrique de MANOVA
nonpartest(Sepal.Length | Sepal.Width | Petal.Length | Petal.Width ~ Species, data = iris, permreps = 2000) 

# Test non paramétrique de MANOVA avec des contraintes spécifiques
ssnonpartest(Sepal.Length | Sepal.Width | Petal.Length | Petal.Width ~ Species, data = iris, test = c(1, 0, 0, 0), alpha = 0.05, factors.and.variables = TRUE) 

# Lab 5.7
install.packages("SemiPar")
library(SemiPar) 

set.seed(10)

x1 = rnorm(100, 20, 6) 
x2 = runif(100, 1, 8) 
y = 10 + x1 + x2 + rnorm(100, 0, 5)  
data_sim = data.frame(x1 = x1, x2 = x2, y = y) 

par(mfrow=c(1,2))
attach(data_sim)
fit <- spm(y ~ x1 + x2) 
summary(fit)
plot(fit) 

x1 = rnorm(100, 20, 6) 
x2 = runif(100, 1, 8) 
y = 10 + x1 + 150*exp(-x2) + rnorm(100, 0, 5)  
data_sim = data.frame(x1 = x1, x2 = x2, y = y) 

fit <- spm(data_sim$y ~ f(data_sim$x1) + f(data_sim$x2)) 
summary(fit) 
plot(fit) 

fit <- spm(data_sim$y ~ data_sim$x1 + f(data_sim$x2)) 
summary(fit) 

fit <- spm(data_sim$y ~ data_sim$x1 + f(data_sim$x2, spar = 20)) 
summary(fit) 
plot(fit)

fit <- spm(data_sim$y ~ data_sim$x1 + f(data_sim$x2, df = 6)) 
summary(fit) 
plot(fit)
