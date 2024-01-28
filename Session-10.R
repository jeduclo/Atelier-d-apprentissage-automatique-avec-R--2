# Session-10

# Chargement de la bibliothèque "bnlearn" pour l'apprentissage bayésien
install.packages("bnlearn")
library(bnlearn) 

# Chargement des données depuis un fichier CSV, en excluant la première colonne
data = read.csv("./employee_data.csv")[-1] 

# Création d'un réseau bayésien (DAG - Directed Acyclic Graph) en spécifiant les relations entre les variables
dag = model2network("[Area][travel_time|Area][performance|travel_time:diet_quality]  
                    [Recently_had_child][Sleep_quality|Recently_had_child:Area][diet_quality|Sleep_quality]") 

# Affichage du DAG
plot(dag) 

# Ajustement du modèle bayésien aux données
fitted = bn.fit(dag, data) 

# Calcul de probabilités conditionnelles pour différentes requêtes
cpquery(fitted, (performance=="HIGH"), (Area=="URBAN")) 
cpquery(fitted, (performance=="HIGH"), (Area=="SUBURBAN")) 
cpquery(fitted, (performance=="HIGH"), (travel_time=="HIGH" & Sleep_quality=="HIGH")) 
cpquery(fitted, (performance=="HIGH"), (travel_time=="HIGH" & Sleep_quality=="LOW")) 
cpquery(fitted, (Sleep_quality=="HIGH"), (performance=="HIGH")) 
cpquery(fitted, (Sleep_quality=="LOW"), (performance=="HIGH")) 

# Affichage d'un graphique de distribution pour la variable "diet_quality" du modèle ajusté
bn.fit.dotplot(fitted$diet_quality) 

# Réalisation d'une recherche de structure de graphe avec le critère "hill climbing" (hc)
dag2 = hc(data, maxp=2) 
plot(dag2) 

# Définition d'une liste blanche pour restreindre la recherche de structure
whitelist = data.frame(from=c("travel_time","diet_quality"),to=c("performance", "performance")) 
dag2 = hc(data, maxp=2, whitelist=whitelist) 
plot(dag2) 

# Ajustement du modèle bayésien aux données après restriction
fitted2 <- bn.fit(dag2, data) 

# Calcul du score AIC pour les deux modèles
score(dag, data, type = "aic") 
score(dag2, data, type = "aic") 


# Lab 10.2
# Chargement de la bibliothèque "bnlearn" pour l'apprentissage bayésien
library(bnlearn)
library(ggplot2)

# Chargement des données depuis un fichier CSV, en excluant la première colonne
data    = read.csv("./employee_data.csv")[-1]

# Création d'un réseau bayésien (DAG - Directed Acyclic Graph) en spécifiant les relations entre les variables
dag     = model2network("[Area][travel_time|Area][performance|travel_time:diet_quality][Recently_had_child][Sleep_quality|Recently_had_child:Area][diet_quality|Sleep_quality]")

# Test d'indépendance conditionnelle entre les variables "performance" et "Sleep_quality"
ci.test("performance", "Sleep_quality", c("diet_quality", "travel_time"), test = "mi", data = data)

# Test d'indépendance conditionnelle entre les variables "diet_quality" et "Area"
ci.test("diet_quality", "Area", c("Sleep_quality"), test = "mi", data = data)

# Test d'indépendance conditionnelle entre les variables "travel_time" et "Area"
ci.test("travel_time", "Area", test = "mi", data = data)

# Calcul des forces des arcs dans le DAG en utilisant le critère d'information mutuelle
arc_strengths = arc.strength(dag, data = data, criterion = "mi")

# Création d'un graphique à barres pour visualiser les forces des arcs
ggplot(data=arc_strengths, aes(x=paste0(arc_strengths$from,"->",arc_strengths$to), y=strength)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x=element_blank())

# Test de d-separation dans le DAG
dsep(dag,"travel_time","Recently_had_child")
dsep(dag,"travel_time","Sleep_quality")
dsep(dag,"travel_time","Sleep_quality","Area")


# Lab 10.3
# Chargement de la bibliothèque "bnlearn" pour l'apprentissage bayésien
library(bnlearn)

# Chargement des données à partir d'une URL et renommage des colonnes
data           = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data")
colnames(data) = c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight","rings") 

# Transformation logarithmique de la variable "rings"
data$rings     = log(data$rings)

# Création d'un réseau bayésien (DAG - Directed Acyclic Graph) en spécifiant les relations entre les variables
dag     = model2network("[sex][length|sex][diameter|sex][height|sex][whole_weight|length:diameter:height][shucked_weight|length:diameter:height][viscera_weight|length:diameter:height][shell_weight|length:diameter:height][rings|whole_weight:shucked_weight:viscera_weight:shell_weight]")

# Paramètres pour le graphviz.plot
parm    = list(nodes = nodes(dag), arcs = arcs(dag), col = "black", textCol = "black")

# Affichage du DAG avec des mises en évidence
graphviz.plot(dag, highlight = parm)

# Ajustement du modèle bayésien aux données
fitted = bn.fit(dag, data)

# Calcul de probabilités conditionnelles
cpquery(fitted, event = (sex == "M"), evidence = list(diameter=0.65,whole_weight=.8), method = "lw")
cpquery(fitted, event = (sex == "M"), evidence = list(diameter=0.15,whole_weight=.8), method = "lw")

# Calcul de la distribution conditionnelle de la variable "height"
cpdist(fitted, nodes = c("height"), evidence = (viscera_weight > 0.4))

# Prédictions pour les variables "rings" et "whole_weight"
head(predict(fitted, "rings", data))
head(predict(fitted, "whole_weight", data))

# Lab 10.4
# Chargement des bibliothèques "bnviewer" et "bnlearn"
install.packages("bnviewer")
install.packages("bnlearn")
library("bnviewer")
library("bnlearn")

# Chargement des données à partir d'une URL et renommage des colonnes
data           = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data")
colnames(data) = c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight","rings") 

# Transformation logarithmique de la variable "rings"
data$rings     = log(data$rings)

# Création d'un réseau bayésien (DAG - Directed Acyclic Graph) en spécifiant les relations entre les variables
dag     = model2network("[sex][length|sex][diameter|sex][height|sex][whole_weight|length:diameter:height][shucked_weight|length:diameter:height][viscera_weight|length:diameter:height][shell_weight|length:diameter:height][rings|whole_weight:shucked_weight:viscera_weight:shell_weight]")

# Paramètres pour la visualisation du DAG avec bnviewer en mode "layout_with_sugiyama"
viewer(dag,
       bayesianNetwork.width    = "100%",
       bayesianNetwork.height   = "80vh",
       bayesianNetwork.layout   = "layout_with_sugiyama",
       bayesianNetwork.title    = "Réseau Abalone",
       bayesianNetwork.subtitle = "Réseau défini par l'utilisateur",
       bayesianNetwork.footer   = "Fig. 1 - diagramme en direct"
)

# Paramètres pour la visualisation du DAG avec bnviewer en mode "layout_as_tree"
viewer(dag,
       bayesianNetwork.width    = "100%",
       bayesianNetwork.height   = "80vh",
       bayesianNetwork.layout   = "layout_as_tree",
       bayesianNetwork.title    = "Réseau Abalone",
       bayesianNetwork.subtitle = "Réseau défini par l'utilisateur",
       bayesianNetwork.footer   = "Fig. 1 - diagramme en direct"
)



# Lab 10.5
# Chargement des données à partir d'un fichier CSV
data = read.csv("./subway_data.csv")

# Création d'une nouvelle variable "weather_sunny" en fonction de la condition "sunny" dans la variable "weather"
data$weather_sunny = 0
data$weather_sunny[data$weather=="sunny"] = 1

# Création d'un modèle de mélange caché (HMM) avec deux états en utilisant la famille de distribution Poisson
hmm    <- depmix(data$people ~ 1  + data$weather_sunny, family = poisson(), nstates = 2, data=data,respstart=c(10,10,10,10))
hmmfit <- fit(hmm, verbose = TRUE)

# Calcul des probabilités postérieures
post_probs <- posterior(hmmfit)

# Affichage de plusieurs graphiques
layout(1:2)
plot(data$people,type="l")
data$state_pred = post_probs$state
matplot(post_probs[,-1], type='l', main='Probabilités Postérieures de Régime', ylab='Probabilité')
legend(x='topright', c('Fermé','Ouvert'), fill=1:3, bty='n')

# Tableau croisé des états réels et prédits
table(data$state,data$state_pred)

# Chargement des données étendues à partir d'un autre fichier CSV
data = read.csv("./subway_data_extended.csv")

# Création de la variable "weather_sunny" pour les données étendues
data$weather_sunny = 0
data$weather_sunny[data$weather=="sunny"] = 1

# Création d'un modèle HMM avec des variables supplémentaires (transition=~1 + machinery_road)
hmm    <- depmix(data = data, people ~ 1  + weather_sunny,transition=~1 + machinery_road,  family = poisson(), nstates = 2, respstart=c(10,10,10,10) )
hmmfit <- fit(hmm, verbose = TRUE)

# Calcul des probabilités postérieures pour les données étendues
post_probs <- posterior(hmmfit)

# Ajout de la variable "state_pred" aux données
data$state_pred = post_probs$state

# Tableau croisé des états réels et prédits pour les données étendues
table(data$state,data$state_pred)

# Lab 10.6
# Chargement de la bibliothèque 'depmixS4'
install.packages("depmixS4")
library('depmixS4') 

# Chargement des données à partir d'un fichier CSV et calcul des rendements
datas           = read.csv("./hist_PAM20190304.csv")[5] 
returns        = diff(datas$cierre)/datas$cierre[-length(datas$cierre)] 

# Affichage des rendements et de l'histogramme des rendements
plot(returns,type="l") 
returns        = data.frame(returns =returns) 
hist(returns$returns,main="Histogramme des Rendements") 

# Création d'un modèle HMM avec deux états en utilisant la famille de distribution gaussienne
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns)) 
hmmfit <- fit(hmm, verbose = FALSE) 

# Calcul des probabilités postérieures
post_probs <- posterior(hmmfit) 

# Affichage de plusieurs graphiques
layout(1:4) 
plot(datas$cierre,type="l") 
plot(returns$returns,type="l") 
plot(post_probs$state, type='s', main='Régimes Réels', xlab='', ylab='Régime') 
matplot(post_probs[,-1], type='l', main='Probabilités Postérieures de Régime', ylab='Probabilité') 
legend(x='topright', c('Plat-Ours','Bull'), fill=1:3, bty='n') 

# Création d'un modèle HMM avec trois états en utilisant la famille de distribution gaussienne
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns)) 

