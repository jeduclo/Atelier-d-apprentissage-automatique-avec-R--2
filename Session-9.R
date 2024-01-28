# Session-9:


library(caret)

# Lecture des données sur les champignons depuis une URL
mushroom_data = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", head=FALSE)
colnames(mushroom_data) = c("comestible", "forme_chapeau", "surface_chapeau", "couleur_chapeau", "contusions", "odeur",
                            "attachement_lames", "espacement_lames", "taille_lames", "couleur_lames", "forme_pied",
                            "racine_pied", "surface_tige_sup", "surface_tige_inf", "couleur_tige_sup",
                            "couleur_tige_inf", "type_voile", "couleur_voile", "nombre_anneaux", "type_anneau",
                            "couleur_spore", "population", "habitat")

# Création de partitions de données d'entraînement et de test
trainIndex <- createDataPartition(mushroom_data$comestible, p = .75, list = FALSE, times = 1)
traindata <- mushroom_data[trainIndex,]
testdata <- mushroom_data[-trainIndex,]

# Calcul des proportions d'éléments comestibles dans les données totales, d'entraînement et de test
proportion_totale <- nrow(mushroom_data[mushroom_data$comestible == "e",]) / nrow(mushroom_data)
proportion_entrainement <- nrow(traindata[traindata$comestible == "e",]) / nrow(traindata)
proportion_test <- nrow(testdata[testdata$comestible == "e",]) / nrow(testdata)
print(paste("Proportion d'éléments comestibles dans les données =", round(proportion_totale, 3),
            "/ Proportion d'éléments comestibles dans les données d'entraînement =", round(proportion_entrainement, 3),
            "/ Proportion d'éléments comestibles dans les données de test =", round(proportion_test, 3)))

# Création d'échantillons de Bootstrap
bootstrap_sample <- createResample(mushroom_data$comestible, times = 10, list = FALSE)

# Création de plis (folds) pour la validation croisée
kfolds_results = createFolds(mushroom_data$comestible, k = 4, list = FALSE)
r1 = nrow(mushroom_data[kfolds_results == 1 & mushroom_data$comestible == "e",]) / nrow(mushroom_data[kfolds_results == 1,])
r2 = nrow(mushroom_data[kfolds_results == 2 & mushroom_data$comestible == "e",]) / nrow(mushroom_data[kfolds_results == 2,])
r3 = nrow(mushroom_data[kfolds_results == 3 & mushroom_data$comestible == "e",]) / nrow(mushroom_data[kfolds_results == 3,])
r4 = nrow(mushroom_data[kfolds_results == 4 & mushroom_data$comestible == "e",]) / nrow(mushroom_data[kfolds_results == 4,])
print(paste("Proportion d'éléments comestibles dans le pli 1 =", r1,
            "/ Proportion d'éléments comestibles dans le pli 2 =", r2,
            "/ Proportion d'éléments comestibles dans le pli 3 =", r3,
            "/ Proportion d'éléments comestibles dans le pli 4 =", r4))

# Création de séquences temporelles
r = rnorm(10, 0, 1)
createTimeSlices(r, 4, horizon = 2)

# Lab 9.2
library(MASS)
library(caret)
library(RANN)
set.seed(100)

# Lecture des données sur les métaux depuis un fichier
data <- read.csv("./metals.csv")
data = data[-1]

# Création de partitions de données d'entraînement et de test
trainIndex <- createDataPartition(data$metal_strength, p = .75, list = FALSE, times = 1)
traindata <- data[trainIndex,]
testdata <- data[-trainIndex,]

# Prétraitement des données
preprocess_object <- preProcess(traindata[-1], method = c("medianImpute", "scale", "center"))
x_transformed <- predict(preprocess_object, traindata[-1])
combined_train_data <- cbind(x_transformed, traindata[1])

# Définition du contrôle de l'entraînement
control <- trainControl(method = "none")

# Réglage des hyperparamètres pour SVM linéaire
tunegrid <- expand.grid(C = c(0.01))
m3 <- train(metal_strength ~ ., data = combined_train_data, method = "svmLinear", metric = "RMSE", tuneGrid = tunegrid, trControl = control)

# Préparation des données de test
test_xdata <- predict(preprocess_object, testdata[-1])
y_test_pred <- predict(m3, test_xdata)

# Évaluation de la performance du modèle
postResample(pred = y_test_pred, obs = testdata$metal_strength)

# Réglage des hyperparamètres pour SVM linéaire avec une autre valeur de C
control <- trainControl(method = "none")
tunegrid <- expand.grid(C = c(0.9))
m3 <- train(metal_strength ~ ., data = combined_train_data, method = "svmLinear", metric = "RMSE", tuneGrid = tunegrid, trControl = control)

# Préparation des données de test
test_xdata <- predict(preprocess_object, testdata[-1])
y_test_pred <- predict(m3, test_xdata)

# Évaluation de la performance du modèle
postResample(pred = y_test_pred, obs = testdata$metal_strength)

# Réglage des hyperparamètres pour SVM linéaire avec validation croisée répétée
control <- trainControl(method = "repeatedcv", number = 4, repeats = 1)
tunegrid <- expand.grid(C = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5))
m3 <- train(metal_strength ~ ., data = combined_train_data, method = "svmLinear",
            preProcess = c("medianImpute", "scale", "center"), metric = "RMSE", tuneGrid = tunegrid, trControl = control)
m3
m3$bestTune



# Lab 9.3
library(MASS) 
library(caret) 
library(randomForest)

# Définition du contrôle de l'entraînement avec validation croisée répétée
control <- trainControl(method = "repeatedcv", number = 4, repeats = 1) 

# Grille de valeurs à tester pour le paramètre mtry (nombre de variables à choisir à chaque division de l'arbre)
tunegrid <- expand.grid(.mtry = c(2, 3, 4, 5, 6, 7, 8)) 

# Chargement des données Boston
data <- Boston 

# Entraînement d'un modèle Random Forest pour prédire la médiane des valeurs médianes de l'habitation (medv)
result <- train(medv ~ ., data = data, method = "rf", metric = "RMSE", tuneGrid = tunegrid,   
                trControl = control, importance = TRUE)$finalModel 

# Affichage du modèle final
result 

# Calcul de l'importance des variables avec le modèle Random Forest
gbmImp <- varImp(result) 

# Affichage de l'importance des variables
importance(result) 

# Tracé d'un graphique montrant l'importance des variables
varImpPlot(result) 

# Réglage du paramètre mtry à 1 pour un modèle avec seulement les variables "rm" et "lstat"
tunegrid <- expand.grid(.mtry = c(1)) 

# Entraînement d'un autre modèle Random Forest avec seulement les variables "rm" et "lstat"
result <- train(medv ~ ., data = data[, c("medv", "rm", "lstat")], method = "rf", metric = "RMSE",   
                tuneGrid = tunegrid, trControl = control)$finalModel 

# Affichage du modèle final
result 

# Configuration du contrôle pour la Recursive Feature Elimination (RFE)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10) 

# Application de la RFE sur les données pour sélectionner les meilleures variables
results <- rfe(as.matrix(data[-14]), as.matrix(data[14]), sizes = c(1:5), rfeControl = control) 

# Affichage des résultats de la RFE
print(results) 

# Affichage des variables sélectionnées par la RFE
predictors(results) 

# Tracé d'un graphique montrant les performances en fonction du nombre de variables
plot(results, type = c("g", "o")) 

# Lab 9.4
library(caret) 

# Définition de la graine aléatoire
set.seed(11) 

# Lecture des données sur les champignons depuis une URL
mushroom_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", head = FALSE) 

# Renommage des colonnes
colnames(mushroom_data) <- c("comestible", "forme_chapeau", "surface_chapeau", "couleur_chapeau", "contusions", "odeur", 
                             "attachement_lames", "espacement_lames", "taille_lames", "couleur_lames", "forme_pied", 
                             "racine_pied", "surface_tige_sup", "surface_tige_inf", "couleur_tige_sup", 
                             "couleur_tige_inf", "type_voile", "couleur_voile", "nombre_anneaux", "type_anneau", 
                             "couleur_spore", "population", "habitat") 

# Extraction de la colonne "comestible" pour la cible
edible <- mushroom_data[, 1] 

# Suppression de la colonne "veil_type" des données
mushroom_data <- mushroom_data[, -which(colnames(mushroom_data) == "veil_type")] 

# Encodage des variables catégorielles avec des variables factices (dummy variables)
mushroom_dummy_model <- dummyVars(data = mushroom_data, ~., sep = "__") 
mushroom_data_model <- cbind(data.frame(predict(mushroom_dummy_model, mushroom_data)), edible) 

# Configuration du contrôle de l'entraînement avec validation croisée répétée
control <- trainControl(method = "repeatedcv", number = 4, repeats = 1) 

# Métrique à utiliser pour évaluer la performance des modèles
metric <- "Accuracy" 

# Grille de valeurs à tester pour le paramètre mtry
tunegrid <- expand.grid(.mtry = c(2, 5, 7, 10)) 

# Entraînement d'un modèle Random Forest pour prédire l'attribut "comestible" des champignons
rf_default <- train(edible ~ ., data = mushroom_data_model, method = "rf", metric = metric,   
                    tuneGrid = tunegrid,  trControl = control) 

# Affichage des résultats du modèle
print(rf_default) 

# Affichage du modèle final
rf_default$finalModel 



# Lab 9.5
library(MASS) 
library(PRROC) 
library(precrec) 
library(pROC)

# Définition de la graine aléatoire
set.seed(10) 

# Chargement des données à partir d'un fichier CSV
data <- read.csv("./approved.csv") 

# Suppression de certaines colonnes non nécessaires
data <- data[, -c(1, 7)] 

# Création de la variable cible "Approved_" en tant que facteur
data$Approved_ <- "not_approved" 
data$Approved_[data$Approved == 1] <- "approved" 
data$Approved_ <- as.factor(data$Approved_) 
data <- data[, -1] 

# Division des données en ensembles d'entraînement et de test
trainIndex <- createDataPartition(data$Approved_, p = .75, list = FALSE, times = 1)  
traindata <- data[trainIndex,]  
testdata <- data[-trainIndex,]  

# Configuration du contrôle de l'entraînement avec validation croisée
rctrl1 <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)  

# Entraînement d'un modèle Gradient Boosting Machine (GBM) pour la ROC
model1 <- train(Approved_ ~ ., traindata,  
                method = "gbm", verbose = FALSE, 
                trControl = rctrl1, metric = "ROC", 
                tuneLength = 10) 

# Prédictions sur l'ensemble d'entraînement
predictions_train <- predict(model1, traindata) 

# Calcul de la matrice de confusion pour l'ensemble d'entraînement
confusionMatrix(traindata$Approved_, predictions_train) 

# Prédictions sur l'ensemble de test
predictions_test <- predict(model1, testdata) 

# Calcul de la matrice de confusion pour l'ensemble de test
confusionMatrix(testdata$Approved_, predictions_test) 

# Tracé de la courbe ROC pour l'ensemble d'entraînement et de test
plot.roc(traindata$Approved_, predict(model1, traindata, type = "prob")[, 1], main = "Courbes ROC, black = train, red = test") 
plot.roc(testdata$Approved_, predict(model1, testdata, type = "prob")[, 1], col = "red", add = TRUE) 

# Configuration des sous-graphiques
par(mfrow = c(2, 1)) 

# Tracé des courbes Sensibilité-Spécificité (SS) pour l'ensemble d'entraînement et de test
sscurves <- evalmod(scores = predict(model1, traindata, type = "prob")[, 2], labels = traindata$Approved_) 
plot(sscurves) 
sscurves <- evalmod(scores = predict(model1, testdata, type = "prob")[, 2], labels = testdata$Approved_) 
plot(sscurves) 

# Lab 9.6
install.packages("PRROC")
install.packages("precrec")
library(MASS) 
library(PRROC) 
library(precrec) 

# Définition de la graine aléatoire
set.seed(10) 

# Chargement des données à partir d'un fichier CSV
data <- read.csv("/Users/fjuretig/Documents/approved.csv") 

# Suppression de certaines colonnes non nécessaires
data <- data[, -c(1, 7)] 

# Création de la variable cible "Approved_" en tant que facteur
data$Approved_ <- "not_approved" 
data$Approved_[data$Approved == 1] <- "approved" 
data$Approved_ <- as.factor(data$Approved_) 
data <- data[, -1] 

# Division des données en ensembles d'entraînement et de test
trainIndex <- createDataPartition(data$Approved_, p = .75, list = FALSE, times = 1)  
traindata <- data[trainIndex,]  
testdata <- data[-trainIndex,]  

# Configuration du contrôle de l'entraînement avec validation croisée
rctrl1 <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)  

# Entraînement d'un modèle GBM sur les données de base
baseline <- train(Approved_ ~ ., traindata,  
                  method = "gbm", verbose = FALSE, 
                  trControl = rctrl1, 
                  metric = "ROC", 
                  tuneLength = 10) 

# Configuration du contrôle de l'entraînement avec sur-échantillonnage (up-sampling)
rctrl1 <- trainControl(method = "cv", number = 5, sampling = "up", classProbs = TRUE, summaryFunction = twoClassSummary)  

# Entraînement d'un modèle GBM avec sur-échantillonnage
up <- train(Approved_ ~ ., traindata,  
            method = "gbm", verbose = FALSE, 
            trControl = rctrl1, 
            metric = "ROC", 
            tuneLength = 10) 

# Configuration du contrôle de l'entraînement avec la méthode SMOTE (Synthetic Minority Over-sampling Technique)
rctrl1 <- trainControl(method = "cv", number = 5, sampling = "smote", classProbs = TRUE, summaryFunction = twoClassSummary)  

# Entraînement d'un modèle GBM avec la méthode SMOTE
smote <- train(Approved_ ~ ., traindata,  
               method = "gbm", verbose = FALSE, 
               trControl = rctrl1, 
               metric = "ROC", 
               tuneLength = 10) 

# Prédictions pour les modèles
predictions_baseline <- predict(baseline, testdata, type = "prob") 
predictions_up <- predict(up, testdata, type = "prob") 
predictions_smote <- predict(up, testdata, type = "prob") 

# Calcul des matrices de confusion pour les modèles
confusionMatrix(testdata$Approved_, predict(baseline, testdata)) 
confusionMatrix(testdata$Approved_, predict(up, testdata)) 
confusionMatrix(testdata$Approved_, predict(smote, testdata)) 
confusionMatrix(testdata$Approved_, predict(baseline, testdata), mode = "prec_recall") 
confusionMatrix(testdata$Approved_, predict(up, testdata), mode = "prec_recall") 
confusionMatrix(testdata$Approved_, predict(smote, testdata), mode = "prec_recall")



# Lab 9.7
# Configuration de la graine aléatoire
set.seed(100) 

# Chargement de la bibliothèque "caret" pour l'apprentissage automatique
library(caret) 

# Résumé de la régression linéaire multiple sur les données "longley"
summary(lm(data = longley, Employed ~ .)) 

# Configuration du contrôle de l'entraînement avec validation croisée
rctrl1 <- trainControl(method = "cv", number = 5) 

# Entraînement d'un modèle de régression linéaire multiple (OLS)
ols_ <- train(Employed ~ ., longley, 
              method = "lm",  
              trControl = rctrl1, 
              tuneLength = 4, metric = "RMSE", 
              preProc = c("center", "scale")) 

# Entraînement d'un modèle de régression Lasso
lasso_ <- train(Employed ~ ., longley,  
                method = "lasso",  
                trControl = rctrl1, 
                tuneLength = 10, metric = "RMSE", 
                preProc = c("center", "scale")) 

# Entraînement d'un modèle de régression Ridge
ridge_ <- train(Employed ~ ., longley,  
                method = "ridge",  
                trControl = rctrl1, 
                tuneLength = 10, metric = "RMSE", 
                preProc = c("center", "scale")) 

# Entraînement d'un modèle de régression Elastic Net
elasticnet_ <- train(Employed ~ ., longley,  
                     method = "glmnet",  
                     trControl = rctrl1, 
                     tuneLength = 10, metric = "RMSE", 
                     preProc = c("center", "scale")) 

# Affichage de l'importance des variables pour le modèle Elastic Net
varImp(elasticnet_) 


# Lab 9.8
# Configuration de la graine aléatoire
set.seed(10) 

# Chargement de la bibliothèque "caret" pour l'apprentissage automatique
library(caret)

# Chargement des données de basketball de 2019 à partir d'un fichier CSV
baseketball_data_2019 <- read.csv("./lakers_.csv") 

# Suppression de certaines colonnes non nécessaires
baseketball_data_2019 <- baseketball_data_2019[, -c(1, 2)] 

# Configuration du contrôle de l'entraînement avec validation croisée
rctrl1 <- trainControl(method = "cv", number = 5)  

# Conversion de la variable cible "win" en facteur
baseketball_data_2019$win <- as.factor(baseketball_data_2019$win) 

# Entraînement d'un modèle de régression logistique pour la prédiction de victoire
model1 <- train(win ~ ., baseketball_data_2019,  
                method = "logreg",   
                metric = "Accuracy", 
                trControl = rctrl1,  
                tuneLength = 4) 


