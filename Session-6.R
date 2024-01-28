# Session-6:    

set.seed(10)
x1 = rnorm(100, 0, 2)
x2 = rnorm(100, 0, 2)
y = x1 + x2 + rnorm(100, 0, 1)
y[100] = 100

# Tracé d'un graphique de x1 par rapport à y
plot(x1, y)

# Régression linéaire ordinaire (OLS)
e = lm(y ~ -1 + x1 + x2) 
summary(e) 

# Modèle de régression linéaire robuste avec la méthode de Huber
rlm_model = rlm(y ~ -1 + x1 + x2, psi = psi.huber) 
summary(rlm_model) 

# Modèle de régression linéaire robuste avec la méthode de Hampel
rlm_model = rlm(y ~ -1 + x1 + x2, psi = psi.hampel) 
summary(rlm_model) 

# Modèle de régression linéaire robuste avec la méthode de Bisquare
rlm_model = rlm(y ~ -1 + x1 + x2, psi = psi.bisquare) 
summary(rlm_model) 

# Lab 6.2
install.packages("robust")
library(MASS) 
library(robust) 

# Création d'une matrice de covariance
Sigma <- matrix(c(2, 1, 1, 2), 2, 2) 
d <- mvrnorm(n = 1000, mu = c(5, 5), Sigma) 

# Matrice de covariance classique
covClassic(d, cor = TRUE) 

# Matrice de covariance robuste
cov.rob(d, cor = TRUE) 

# Modification des données
d[1:50, 1:2] = matrix(rnorm(100, 20, 10), c(50, 2)) 

# Matrice de covariance classique avec des données modifiées
covClassic(d, cor = TRUE) 

# Matrice de covariance robuste avec des données modifiées
cov.rob(d, cor = TRUE) 

# Modification des données
d[1:200, 1:2] = matrix(rnorm(400, 20, 10), c(50, 2)) 

# Matrice de covariance classique avec des données modifiées
covClassic(d, cor = TRUE) 

# Matrice de covariance robuste avec des données modifiées
cov.rob(d, cor = TRUE) 

# Lab 6.3
library(robust) 
set.seed(1000)
x1 = rnorm(1000)   
x2 = rnorm(1000) 
link_val = 2 + 2*x1 + 5*x2   
pr = 1/(1+exp(-link_val))      
y = rbinom(1000, 1, pr)   
df = data.frame(y = y, x1 = x1, x2 = x2) 

# Modèle de régression logistique classique
glm(y ~ x1 + x2, data = df, family = "binomial") 

# Modèle de régression logistique robuste
robust::glmRob(y ~ x1 + x2, data = df, family = "binomial") 

# Modification des données
x1[1:50] = 10*rnorm(50) 
df = data.frame(y = y, x1 = x1, x2 = x2) 

# Modèle de régression logistique classique avec des données modifiées
glm(y ~ x1 + x2, data = df, family = "binomial") 

# Modèle de régression logistique robuste avec la méthode "cubif" et des données modifiées
robust::glmRob(y ~ x1 + x2, method = "cubif", data = df, family = "binomial") 

# Modification des données
x1[1:100] = 10*rnorm(100) 
df = data.frame(y = y, x1 = x1, x2 = x2) 

# Modèle de régression logistique classique avec des données modifiées
glm(y ~ x1 + x2, data = df, family = "binomial") 

# Modèle de régression logistique robuste avec la méthode "cubif" et des données modifiées
robust::glmRob(y ~ x1 + x2, method = "cubif", data = df, family = "binomial")

# Lab 6.4
library(robust) 
r = PlantGrowth
d = aov(weight ~ group, data = r) 
summary(d)
plot(d, 2) 
robanova = robust::lmRob(weight ~ group, data = r) 
robust::anova.lmRob(robanova)
r[1,1] = 50
r[2,1] = 50
d = aov(weight ~ group, data = r) 
plot(d, 2) 
summary(d)
robanova = robust::lmRob(weight ~ group, data = r) 
robust::anova.lmRob(robanova) 

# Lab 6.5
install.packages("rospca")
library(MASS) 
library(rospca) 
set.seed(100) 

# Matrice de covariance
matrix = diag(10) 
matrix[2, 1] = 0.8 
matrix[1, 2] = 0.8 
matrix[4, 3] = 0.8 
matrix[3, 4] = 0.8 
matrix[5, 6] = 0.8 
matrix[6, 5] = 0.8 

d = mvrnorm(n = 1000, mu = rep(0, 10), matrix) 

# Analyse en composantes principales (PCA)
prcomp(d, scale = TRUE, center = TRUE) 

# Analyse en composantes principales robuste (robpca)
x1 = rospca::robpca(d) 
x1$eigenvalues 

# Modification des données
d[1:1, 1:10] = -400 
prcomp(d, scale = TRUE, center = TRUE) 

x1 = rospca::robpca(d) 
x1$eigenvalues 

# Lab 6.6
install.packages("qclust")
library(qclust) 
data("geyser2")
result_robust_mixture <- qclust::Qclust(geyser2, K = 3, q = 0.9999) 

# Tracé des résultats
plot(result_robust_mixture) 

# Tracé de la densité
plot(result_robust_mixture, what = "density", type = "persp")

# Tracé des frontières
plot(result_robust_mixture, what = "boundaries", ngrid = 200) 

# Lab 6.7
install.packages("tclust")
library(tclust) 
library(ggplot2)
data("geyser2") 

# K-means classique
clus_kmeans <- kmeans(geyser2, centers = 3) 

# K-means tronqué
clus_tkmeans <- tkmeans(geyser2, k = 3, alpha = 0.05) 

# Attribution des clusters
geyser2$cluster <- as.factor(clus_kmeans$cluster) 

# Tracé des points avec des couleurs selon les clusters
ggplot(geyser2, aes(x = geyser2$`Eruption length`, y = geyser2$`Previous eruption length`, color = geyser2$cluster)) +
  labs(x = "Eruption length", y = "Previous eruption length") + theme(legend.position = "none") + geom_point(size = 3, alpha = 0.2) 

# Tracé des résultats de K-means tronqué
plot(clus_tkmeans, main = "Robust k-means", xlab = "X-axis label", ylab = "y-axix label")


