# Session-1:  Introduction à R et à l'analyse de données

# Charger la bibliothèque bbmle
install.packages("bbmle")
library(bbmle)

# Définir la taille de l'échantillon
N <- 1000

# Générer un échantillon gamma
xx <- rgamma(N, shape = 20, rate = 2)

# Définir une fonction de vraisemblance
LL <- function(shape, rate) {
  R = suppressWarnings(dgamma(xx, shape = shape, rate = rate))
  return(-sum(log(R)))
}

# Estimer les paramètres du modèle pour N = 1000
P_1000 = mle2(LL, start = list(shape = 1, rate = 1))
summary(P_1000)

# Réduire la taille de l'échantillon à N = 10
N <- 10
x <- rgamma(N, shape = 20, rate = 2)

# Réutiliser la fonction de vraisemblance pour N = 10
P_10 = mle2(LL, start = list(shape = 1, rate = 1))
summary(P_10)

# Calculer les intervalles de confiance pour N = 1000 et N = 10
confint(P_1000)
confint(P_10)

# Lab 1.2

# Générer un échantillon de valeurs normales
vals = rnorm(10000, 0, 1)

# Créer un graphique de densité
plot(main = "densité", density(vals))
abline(v = 2, col = "red")

# Calculer et afficher des statistiques de distribution
print(paste("Zone à gauche de x=2", pnorm(2, 0, 1)))
print(paste("Zone à droite de x=2", 1 - pnorm(2, 0, 1)))
print(paste("90ème quantile : valeur x ayant 97,72 % à gauche", qnorm(0.9772, 0, 1)))
print(paste("Zone à gauche de x=3", pchisq(3, 33)))

# Lab 1.3

# Charger les bibliothèques ggplot2 et reshape
library(ggplot2)
library(reshape)

# Lire les données à partir d'un fichier CSV
datag = read.csv("./ctgs.csv")

# Transformer les données pour les rendre adaptées à la visualisation
transformed_data = melt(datag, id.vars = "Company")

# Créer des graphiques à barres
ggplot(transformed_data, aes(x = Company, y = value, fill = variable)) + geom_bar(stat = "identity")
ggplot(transformed_data, aes(x = Company, y = value, fill = variable)) + geom_bar(stat = "identity", position = position_dodge())

# Lab 1.4

# Charger les bibliothèques ggplot2 et tidyr
library(ggplot2)
library(tidyr)

# Générer des échantillons de données aléatoires
rnorm_result = data.frame(rnorm = rnorm(10000, 0, 1))
inverse_way = data.frame(inverse = qnorm(runif(10000), 0, 1))

# Combinez les résultats en un seul tableau
total_table = cbind(rnorm_result, inverse_way)
transp_table = gather(total_table)
colnames(transp_table) = c("méthode", "valeur")

# Créer un graphique de densité
ggplot(transp_table, aes(x = valeur, fill = méthode)) + geom_density(alpha = 0.25)

# Générer des échantillons de données de Poisson
rpois_result = data.frame(rpois = rpois(10000, 5))
inverse_way = data.frame(inverse = qpois(runif(10000), 5))

# Combinez les résultats en un seul tableau
total_table = cbind(rpois_result, inverse_way)
transp_table = gather(total_table)
colnames(transp_table) = c("méthode", "valeur")

# Créer un histogramme
ggplot(transp_table, aes(x = valeur, fill = méthode)) + geom_histogram(alpha = 0.8, binwidth = 1)

# Lab 1.5

# Charger les bibliothèques dplyr et ggplot2
library(dplyr)
library(ggplot2)

# Afficher les données mtcars
mtcars

# Résumer les données en regroupant par am et gear
mtcars %>% group_by(am, gear) %>% summarise(mean_hp = mean(hp), sum_mpg = sum(mpg)) %>% ungroup %>% arrange(mean_hp)

# Lab 1.6

# Charger la bibliothèque plot3D
library(plot3D)

# Générer des données aléatoires
x = rnorm(100)
y = rnorm(100)
z = x + y + rnorm(100, 0, 0.3)
idrow = 1:100

# Créer un graphique 3D
scatter3D(x, y, z, bty = "g", colkey = TRUE, main = "Graphique x-y-z", phi = 10, theta = 50)
text3D(x, y, z, labels = idrow, add = TRUE, colkey = FALSE, cex = 0.5)

# Lab 1.7

# Charger les bibliothèques expss et formattable
library(expss)
library(formattable)

# Lire les données à partir d'un fichier CSV
data = read.csv("/Users/admin/Documents/R_book/chapter01/07/person_salary.csv", stringsAsFactors = FALSE)

# Définir des couleurs
Green = "#71CA97"
Green2 = "#DeF7E9"

# Créer une table de fréquence
table__out = data.frame(table(data$Career, data$Age))
colnames(table__out) = c("Carrière", "Âge", "Freq")

# Formater la table pour la visualisation
formattable(table__out, align = c("c", "c", "c"), list("Freq" = color_tile(Green, Green2)))

# Formater les données pour la visualisation
formattable(data, align = c("c", "c", "c", "c"), list(
  "Personne" = formatter("span", style = ~style(color = "grey", font.weight = "bold")),
  "Salaire" = color_tile(Green, Green2),
  "Contacté" = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Oui", "Non")))))

# Lab 1.8

# Charger les bibliothèques dplyr et ggplot2
library(dplyr)
library(ggplot2)

# Lire les données à partir d'un fichier CSV
voters_data = read.csv("./voters_.csv")

# Initialiser des vecteurs pour stocker les proportions
proportions_10sample = c()
proportions_50sample = c()
proportions_100sample = c()
proportions_500sample = c()

# Boucle pour calculer les proportions pour différentes tailles d'échantillon
for (q in 2:1000) {
  sample_data = mean(sample(voters_data$Vote, 10, replace = FALSE))
  proportions_10sample = c(proportions_10sample, sample_data)
}

for (q in 2:1000) {
  sample_data = mean(sample(voters_data$Vote, 50, replace = FALSE))
  proportions_50sample = c(proportions_50sample, sample_data)
}

for (q in 2:1000) {
  sample_data = mean(sample(voters_data$Vote, 100, replace = FALSE))
  proportions_100sample = c(proportions_100sample, sample_data)
}

for (q in 2:1000) {
  sample_data = mean(sample(voters_data$Vote, 500, replace = FALSE))
  proportions_500sample = c(proportions_500sample, sample_data)
}

# Créer des dataframes pour stocker les résultats
joined_data50 = data.frame("taille_échantillon" = 50, "moyenne" = mean(proportions_50sample), "q2.5" = quantile(proportions_50sample, 0.025), "q97.5" = quantile(proportions_50sample, 0.975))
joined_data10 = data.frame("taille_échantillon" = 10, "moyenne" = mean(proportions_10sample), "q2.5" = quantile(proportions_10sample, 0.025), "q97.5" = quantile(proportions_10sample, 0.975))
joined_data100 = data.frame("taille_échantillon" = 100, "moyenne" = mean(proportions_100sample), "q2.5" = quantile(proportions_100sample, 0.025), "q97.5" = quantile(proportions_100sample, 0.975))
joined_data500 = data.frame("taille_échantillon" = 500, "moyenne" = mean(proportions_500sample), "q2.5" = quantile(proportions_500sample, 0.025), "q97.5" = quantile(proportions_500sample, 0.975))

# Regrouper les données
data_sim = rbind(joined_data10, joined_data50, joined_data100, joined_data500)

# Calculer les intervalles de confiance
data_sim = data_sim %>% mutate(Nq2.5 = moyenne - 1.96 * sqrt(moyenne * (1 - moyenne) / taille_échantillon), N97.5 = moyenne + 1.96 * sqrt(moyenne * (1 - moyenne) / taille_échantillon))
data_sim$taille_échantillon = as.factor(data_sim$taille_échantillon)

# Créer un graphique
ggplot(data_sim, aes(x = taille_échantillon, y = moyenne, group = 1)) +
  geom_point(aes(size = 2), alpha = 0.52) +
  theme(legend.position = "none") +
  geom_errorbar(width = .1, aes(ymin = q2.5, ymax = q97.5), colour = "darkred") +
  labs(x = "Taille de l'échantillon", y = "Ratio du candidat A", title = "Ratio du candidat A par taille d'échantillon", subtitle = "Proportion de personnes votant pour le candidat A, en supposant une chance de 50-50", caption = "Le cercle représente la moyenne / Les bandes représentent les intervalles de confiance à 95 %")

# Lab 1.9

# Installer la bibliothèque DiagrammeR si elle n'est pas déjà installée
install.packages('DiagrammeR')

# Charger la bibliothèque DiagrammeR
library('DiagrammeR')

# Créer un diagramme de graphe
grViz("
digraph dot {
      
      graph [layout = dot]
      
      node [shape = circle,
      style = filled,
      color = grey,
      label = '']
      
      node [fillcolor = white,fixedsize = true, width = 2]
      a[label = 'Compagnie A']
      
      node [fillcolor = white]
      b[label = 'Conseil en IT+RD'] c[label = 'Conseil général'] d[label = 'Autres activités']
      
      node [fillcolor = white]
      
      edge [color = grey]
      a -> {b c d}
      b -> {e[label = '254';color=blue] f[label = '83%';color=green]}
      c -> {k[label = '132';color=blue] l[label = '61%';color=green]}
      d -> {q[label = '192';color=blue] r[label = '47%';color=green]}
      }")
