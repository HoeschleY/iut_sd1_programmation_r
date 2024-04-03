rm(list = ls())

# Créer une toile de fond vide pour le graphique
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")

# Tracer la densité de probabilité pour chaque simulation
moyennes <- c(0, 0, 0, -2)
sigmas <- c(0.45, 1, 2.25, 0.7)
colors <- c("red", "blue", "green", "orange")
legend_labels <- c()
for (i in 1:length(moyennes)) {
  serie = rnorm(n = 1000, 
                mean = moyennes[i], 
                sd = sigmas[i])
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moyennes[i], ",", "s =", sigmas[i]))
}

# Ajouter une légende
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)

#Avec 10000 points
serie = rnorm(n = 1e4, mean = 0, sd = 1)
lines(density(serie),col = "pink")

#Histogramme

hist(x=serie,probability = TRUE , main = "loi normal centrée-réduite")
lines(density(serie))

median(serie)

quantile(serie,seq(0,1,0.25))


quantile(serie,seq(0,1,0.01))

quantile(serie, 
         probs = 0.95)
# environ 1,64


#Les commandes pnorm() et qnorm().

qnorm(p = 0.95, mean = 0, sd = 1)
pnorm(q = 1.644854, mean = 0, sd = 1)

qnorm(p = 0.975,mean = 0,sd = 1)

pnorm(q=1.96, mean = 0, sd = 1)

#Exercice 2 - Construire la table de loi normale



indices_lignes = seq(from = 0, to = 3.9, by = 0.1)
indices_cols = seq(0.00,0.09,0.01)

#on crée un vecteur vide pour ajouter les probas au fur et à mesure

resultat = NULL

#On parcourt les indices lignes
for (j in indices_cols){
  all_probas = c()
  
  for (i in indices_lignes){
    proba = pnorm(q = i+j, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  resultat = cbind(resultat,all_probas)
}
print(all_probas)

class(resultat)
table = data.frame(resultat)
colnames(table) = indices_cols
rownames(table) = indices_lignes
View(table)

print(resultat)


#Exercice 3 - Simulation d'une population

moyenne_pop<-171
sd_pop<-9
population<-rnorm(n = 1e7, 
                  mean=moyenne_pop, 
                  sd=sd_pop)

mean(population)
sd(population)

hist(population)


#observé
pop190 = population[population < 190]
length(pop190)
length(pop190) / length(population)

#en théorie
pnorm(q = 190, mean=moyenne_pop, sd=sd_pop)*1e7


#Observe
pop200 = population[population >= 200]
length(pop200)
length(pop200) / length(population)

#en théorie
#proba de P( X < 200cm)
proba_inf_200 = pnorm(q = 200, mean=moyenne_pop, sd=sd_pop)
#proba de P( X >= 200cm)
1 - proba_inf_200


#Exercice 4 - Simulation d'échantillon

taille_ech<-100
echantillon<-sample(x = population, 
                    size = taille_ech, 
                    replace = TRUE)
mean(echantillon)
sd(echantillon)

largeur<-qnorm(p = 0.975,mean=0,sd=1)*sd_pop/sqrt(taille_ech)
borne_inf<-moyenne_pop-largeur
borne_sup <-moyenne_pop+largeur

#Les commandes sample() et apply()

taille_ech<-100
nb_replicat<-1000
echantillons<-replicate(n = nb_replicat,
                        expr =  sample(population,
                                       taille_ech, 
                                       replace = TRUE))

moyennes<-apply(X = echantillons,
                MARGIN = 2,
                FUN = function(x) mean(x))
ecart_types<-apply(echantillons,
                   MARGIN = 2,
                   FUN = function(x) sd(x))

hist(moyennes)
hist(ecart_types)
mean(moyennes)
sd(moyennes)


#observé
moy172 = moyennes[moyennes > 172]
length(moy172)
length(moy172) / length(moyennes)

#en théorie
#proba de P( X < 172cm)
proba_inf_172 = pnorm(q = 172, 
                      mean=moyenne_pop, 
                      sd=sd_pop/sqrt(taille_ech))
#proba de P( X >= 172cm)
1 - proba_inf_172



largeur<-apply(X = echantillons,
               MARGIN = 2,
               FUN = function(x) pnorm(0.975)*sd(x)/taille_ech)

borne_inf_IC<-moyennes-largeur
borne_sup_IC<-moyennes+largeur




resultat = data.frame(largeur,borne_inf_IC,borne_sup_IC) 
View(resultat)

