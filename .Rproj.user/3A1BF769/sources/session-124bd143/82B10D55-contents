# Session-4:
install.packages("rstan")
library(rstan) 

# Génération de données simulées
valeurs = list(y = rnorm(1000,5,3)) 

# Modèle Stan
modele =" 

data { 
  real y[1000]; 
} 

parameters { 
  real mu; 
  real sigma; 
} 

model { 
  mu    ~ normal(0,10);   
  sigma ~ normal(0,10);  
  y     ~ normal(mu,sigma); 
} 
" 

# Exécution du modèle Stan
resultat <- stan(model_code = modele, data = valeurs, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1) 
posterior = extract(resultat) 

# Tracé des graphiques
par(mfrow=c(2,1)) 
plot(posterior$mu, type = "l") 
plot(posterior$sigma, type = "l") 
traceplot(resultat) 
stan_dens(resultat) 
summary(resultat) 

# Lab 4.2
library(rstan) 

# Chargement des données
donnees = read.csv("./data/house_prices.csv") 

# Modèle Stan
modele =" 

data { 
  real y[481]; 
  real x[481,6]; 
} 

parameters { 
  real beta[6]; 
  real sigma; 
  real alpha; 
} 

model { 
  beta  ~ gamma(3,1); 
  for (n in 1:481) 
    y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] +   
    beta[6]*x[n,6], sigma); 
} 
" 

# Exécution du modèle Stan
xy     = list(y=donnees[,1],x=donnees[,2:7]) 
resultat    = stan(model_code = modele, data = xy, warmup = 500, iter = 5000, chains = 3, cores = 2, thin =   1,verbose=FALSE) 

# Tracé des graphiques
traceplot(resultat) 
summary(resultat) 
stan_dens(resultat) 

# Modification du modèle avec des bornes inférieures pour les paramètres
modele =" 

data { 
  real y[481]; 
  real x[481,6]; 
} 

parameters { 
  real<lower=0> beta[6]; 
  real sigma; 
  real alpha; 
} 

model { 
  beta  ~ normal(5,3); 
  for (n in 1:481) 
    y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] + beta[6]*x[n,6], sigma); 
} 
" 

# Exécution du modèle Stan avec les bornes inférieures
resultat    = stan(model_code = modele, data = xy, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1,verbose=FALSE) 
summary(resultat) 
stan_dens(resultat) 

# Lab 4.3
donnees   = read.csv("./data/house_prices.csv") 

modele =" 

data { 

real y[481]; 

real x[481,6]; 

} 

parameters { 

real beta[6]; 

real sigma; 

real alpha; 

} 

model { 

beta[1]  ~ uniform(0,1000); 

for (n in 1:481) 

y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] +   

beta[6]*x[n,6], sigma); 

} 
" 

# Exécution du modèle Stan
xy     = list(y=donnees[,1],x=donnees[,2:7]) 
resultat    = stan(model_code = modele, data = xy, warmup = 500, iter = 1000, chains = 1, cores = 1, thin =   1,verbose=FALSE) 
stan_dens(resultat) 

modele =" 

data { 

real y[481]; 

real x[481,6]; 

} 

parameters { 

real beta[6]; 

real sigma; 

real alpha; 

} 

model { 

beta[1]  ~ uniform(0,1000); 
beta[2]  ~ normal(1,0.1); 
beta[3]  ~ normal(1,0.1); 
beta[4]  ~ normal(1,0.1); 
beta[5]  ~ normal(1,0.1); 
beta[6]  ~ normal(1,0.1); 

for (n in 1:481) 

y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] +   

beta[6]*x[n,6], sigma); 

} 
" 

# Exécution du modèle Stan avec des priors différents
xy     = list(y=donnees[,1],x=donnees[,2:7]) 
resultat    = stan(model_code = modele, data = xy, warmup = 500, iter = 1000, chains = 1, cores = 1, thin =   1,verbose=FALSE) 
stan_dens(resultat) 

# Lab 4.4
library(ggplot2) 

# Génération de données simulées
v1_1 = rnorm(1000,10,1) 
v1_2 = rnorm(1000,10,1) 
v1_3 = rnorm(1000,10,1) 
v2_1 = rnorm(1000,10,1) 
v2_2 = rnorm(1000,10,1) 
v2_3 = rnorm(1000,10,1) 
U       = rnorm(1000,0,3) 
Y       = v1_1 + v1_2 + v1_3 + v2_1 + v2_2 + v2_3 + U 

# Fonction pour obtenir la vraisemblance
get_likelihood <- function(param){ 
  pred  = param[1]*v1_1 + param[2]*v1_2 + param[3]*v1_3 + param[4]*v2_1 + param[5]*v2_2 + param[6]*v2_3 
  likelihood_par_observation = dnorm(Y, mean = pred, sd = param[7], log = T) 
  sumll                      = sum(likelihood_par_observation) 
  return(sumll)    
} 

# Fonction pour obtenir le prior
get_prior <- function(param){ 
  c11_prior = dnorm(param[1], 0.5,5, log = T) 
  c12_prior = dnorm(param[2], 0.5,5, log = T) 
  c13_prior = dnorm(param[3], 0.5,5, log = T) 
  c21_prior = dnorm(param[4], 0.5,5, log = T) 
  c22_prior = dnorm(param[5], 0.5,5, log = T) 
  c23_prior = dnorm(param[6], 0.5,5, log = T) 
  sdprior   = dnorm(param[7], 0.5,5, log = T) 
  return(c11_prior+c12_prior+c13_prior+c21_prior+c22_prior+c23_prior+sdprior) 
} 

# Fonction pour obtenir le posterior
get_posterior <- function(param){ 
  return_value = get_likelihood(param) + get_prior(param) 
  return (return_value) 
} 

# Fonction pour générer une proposition
get_proposalfunction <- function(param){ 
  return(rnorm(7,mean = param, sd= c(.028,.028,.028,.028,.028,.028,.028))) 
} 

# Algorithme Metropolis-Hastings MCMC
MetropolisHastings_MCMC <- function(start_, iter_){ 
  chain_values     = array(dim = c(iter_+1,7)) 
  chain_values[1,] = start_ 
  for (i in 1:iter_){ 
    proposal = get_proposalfunction(chain_values[i,]) 
    probs    = exp(get_posterior(proposal) - get_posterior(chain_values[i,])) 
    if (is.nan(probs)){ 
      probs = 0 
    } 
    
    random_value = runif(1) 
    if (random_value < probs){ 
      chain_values[i+1,] = proposal 
    }else{ 
      chain_values[i+1,] = chain_values[i,] 
    } 
  } 
  return(chain_values) 
} 

# Paramètres initiaux
startvalue        = c(0.1,0.1,0.1,0.1,0.1,0.1,10) 

# Exécution de l'algorithme Metropolis-Hastings MCMC
chain_values      = MetropolisHastings_MCMC(startvalue, 50000) 

# Création d'un dataframe avec les résultats
donnees           = data.frame(chain_values) 
colnames(donnees) = c("v1_1","v1_2","v1_3","v2_1","v2_2","v2_3","sd") 
donnees$iter      = seq.int(nrow(donnees)) 

# Tracé des graphiques
ggplot(data=donnees,aes(x=iter, y=v1_1)) + geom_line(color="blue") 

# Sélection des données pour le tracé final
donnees           = donnees[10000:50000,] 
donnees           = donnees[seq(1, dim(donnees)[1], by = 5),]

# Tracé des graphiques finaux
ggplot(data=donnees,aes(x=iter, y=v1_1)) + geom_line(color="blue") 

# Calcul de la densité
seqs           = seq(1,nrow(donnees),2) 
plot(density(donnees[seqs,"v1_1"]),main="V1_1 densité postérieure")

# Lab 4.5
install.packages("coda")
install.packages("DrBats")
library(rstan) 
library(coda) 
library(DrBats) 

# Chargement des données
donnees = read.csv("./data/house_prices.csv") 

# Modèle Stan
modele =" 
data { 
  real y[481]; 
  real x[481,6]; 
} 

parameters { 
  real <lower=0> beta[6]; 
  real sigma; 
  real alpha; 
} 

model { 
  beta ~ normal(5,20); 
  for (n in 1:125) 
    y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] + beta[6]*x[n,6], sigma); 
} 
" 

# Exécution du modèle Stan
xy     = list(y=donnees[,1],x=donnees[,2:7]) 
resultat    = stan(model_code = modele, data = xy, warmup = 500, iter = 5000, chains = 4, cores = 2, thin = 1,verbose=FALSE) 

# Analyse des résultats
coda__obj = coda.obj(resultat) 
heidel.diag(coda__obj)
HPDinterval(coda__obj)
raftery.diag(coda__obj)
effectiveSize(coda__obj)
cumuplot(coda__obj)
gelman.plot(coda__obj)
geweke.plot(coda__obj)
crosscorr.plot(coda__obj)
autocorr.plot(coda__obj)

# Lab 4.6
v1_1 = rnorm(1000,10,1) 
v1_2 = rnorm(1000,10,1) 
v1_3 = rnorm(1000,10,1) 
v2_1 = rnorm(1000,10,1) 
v2_2 = rnorm(1000,10,1) 
v2_3 = rnorm(1000,10,1) 
U    = rnorm(1000,0,3) 

Y    = v1_1 + v1_2 + v1_3 + U 

lista = list(x=cbind(v1_1,v1_2,v1_3,v2_1,v2_2,v2_3),y=Y,n=length(Y)) 

install.packages("rjags")
library('rjags') 

mod <- " model{ 
    for (i in 1:n){ 
        mu[i] =  id[1]*b[1]*x[i,1] + id[2]*b[2]*x[i,2] + id[3]*b[3]*x[i,3] + id[4]*b[4]*x[i,4] + id[5]*b[5]*x[i,5] +   id[6]*b[6]*x[i,6] 
        y[i] ~ dnorm(mu[i], prec) 
    }             

    for (i in 1:6){ 
        b[i] ~ dnorm(0.0, 1/2) 
        id[i] ~ dbern(0.5) 
    } 
    prec ~ dgamma(1, 2) 
}" 

jags <- jags.model(textConnection(mod), data = lista, n.chains = 1, n.adapt = 100) 
update(jags, 5000) 
samps <- coda.samples( jags, c("b","id"), n.iter=1000 ) 
summary(samps) 

mod <- " model{ 
for (i in 1:n){ 
mu[i] =  id[1]*b[1]*x[i,1] + id[2]*b[2]*x[i,2] + id[3]*b[3]*x[i,3] + id[4]*b[4]*x[i,4] + id[5]*b[5]*x[i,5] +   id[6]*b[6]*x[i,6] 
y[i] ~ dnorm(mu[i], prec) 
}            

ka  ~ dbeta(5,5) 
for (i in 1:6){ 
b[i] ~ dnorm(0.0, 1/2) 
id[i] ~ dbern(ka) 
} 

prec ~ dgamma(1, 2) 
}" 

jags <- jags.model(textConnection(mod), data = lista, n.chains = 1, n.adapt = 100) 
update(jags, 2000) 
samps <- coda.samples( jags, c("b","id"), n.iter=1000 ) 
summary(samps) 

# Lab 4.7
library(rstan) 

# Chargement des données
donnees   = read.csv("./house_prices.csv") 

model =" 
data { 
  real y[481]; 
  real x[481,6]; 
  real ns[2,6]; 
} 

parameters { 
  real beta[6]; 
  real sigma; 
  real alpha; 
} 

model { 
  beta  ~ normal(5,10); 
  for (n in 1:481) 
    y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] + beta[6]*x[n,6], sigma); 
} 

generated quantities { 
  vector[2] y_preds; 
  for (n in 1:2) { 
    y_preds[n] = normal_rng(alpha + beta[1]*ns[n,1] + beta[2]*ns[n,2] + beta[3]*ns[n,3] + beta[4]*ns[n,4] + beta[5]*ns[n,5] + beta[6]*ns[n,6], sigma); 
  } 
}" 

topredict = rbind(c(10,3,3,3,3,20),c(7,3,3,3,3,10))
xy     = list(y=donnees[,1],x=donnees[,2:7],ns=topredict) 
fit    = stan(model_code = model, data = xy, warmup = 500, iter = 5000, chains = 4, cores = 2, thin = 1,verbose=FALSE) 

# Lab 4.8
library(rstan)

# Chargement des données
donnees   = read.csv("./clients.csv")

model =" 

data { 
  int<lower = 0, upper = 1> y[40]; 
  real x[40,3]; 
  real ns[2,3]; 
} 

parameters { 
  real beta[3]; 
  real alpha;
} 

model { 
  beta   ~ gamma(5,10); 
  alpha   ~ normal(0,20);
  for (n in 1:40) 
    y[n] ~ bernoulli_logit(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3]); 
} 

generated quantities { 

  vector[2] y_preds; 
  for (n in 1:2) { 
    y_preds[n] = bernoulli_logit_rng(alpha + beta[1]*ns[n,1] + beta[2]*ns[n,2] + beta[3]*ns[n,3]); 
  } 
}
" 

topredict = rbind(c(20,0,1),c(60,5,8))
xy     = list(y=donnees[,1],x=donnees[,2:4],ns=topredict) 
fit    = stan(model_code = model, data = xy, warmup = 500, iter = 5000, chains = 4, cores = 2, thin = 1,verbose=FALSE) 

rstan::traceplot(fit,pars=c("beta[1]","beta[2]","beta[3]","alpha"))
summary(fit) 
stan_dens(fit) 

# Lab 4.9
library(rstan) 

# Génération de données simulées
valeurs = list(y = rnorm(1000,5,3)) 

modele =" 

data { 
  real y[1000]; 
} 

parameters { 
  real mu; 
  real sigma; 
} 

model { 
  mu    ~ normal(0,10);   
  sigma ~ normal(0,10);  
  y     ~ normal(mu,sigma); 
} 
" 

# Exécution du modèle Stan
fit <- stan(model_code = modele, data = valeurs, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1) 
posterior = extract(fit) 

par(mfrow=c(2,1)) 
plot(posterior$mu, type = "l") 
plot(posterior$sigma, type = "l") 
traceplot(fit) 
stan_dens(fit) 
summary(fit) 
