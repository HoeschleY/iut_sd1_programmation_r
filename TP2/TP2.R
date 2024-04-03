rm(list = ls())
getwd()
setwd("C:/Users/yanni/BUT/R/TP2")

dffao = read.csv("fao.csv",header = TRUE, sep = ";", dec = "," )
View(dffao)

#2 Combien de pays sont présents dans ce dataset ?

nrow(dffao)
 
#3 Affichez un résumé des données avec la fonction adaptée. Il semble que ce jeu de données présente quelques valeurs manquantes.

summary(dffao)

#Exercice 2 - Statistiques descriptives

moyennealimparpop = mean(dffao[,"Dispo_alim"]/mean(dffao[,"Population"],na.rm = TRUE))

nbhabitanttot = sum(dffao[,"Population"],na.rm = TRUE)

medianprodviande = median(dffao[,"Prod_viande"],na.rm = TRUE)

quartileKcal = quantile(dffao[,"Dispo_alim"],seq(0.25,0.75,0.25))
print(quartileKcal)

centileKcal = quantile(dffao[,"Import_viande"],seq(0,1,0.01))
print(centileKcal)

#Exercice 3 - Tris et filtres

#Construire une requête pour extraire les lignes du dataset avec les 5 pays les moins peuplés.

f = order(dffao$Population)

resultat = dffao[f,]
View(resultat[1:5,])

#Construire une requête pour extraire les lignes du dataset avec les 5 pays les plus peuplés.

f = order(dffao$Population,decreasing =  TRUE)

resultat = dffao[f,]
View(resultat[1:5,])

#Construire une requête pour extraire les lignes du dataset avec les 5 pays qui importent le plus de viande.

f = order(dffao$Prod_viande,decreasing =  TRUE)

resultat = dffao[f,]
View(resultat[1:5,])

#En moyenne, le besoin énergétique moyen d’une adulte est de 2300 kcal par jour. Construire une requête pour extraire les lignes du dataset avec les pays qui ont une disponibilité alimentaire supérieure ou égale à 2300 kcal. Combien de pays sont concernés ?

extract = subset(dffao,dffao$Dispo_alim > 2300)
nrow(extract)

#Construire une requête pour extraire les lignes du dataset avec les pays qui ont une disponibilité alimentaire strictement supérieure à 3500 kcal et qui importe un volume de viande supérieure ou égale à 1 000 000 tonnes par an. Combien de pays sont concernés ?

extract = subset(dffao,dffao$Dispo_alim > 3500 & dffao$Import_viande > 1000)
nrow(extract)

#Construire une requête pour extraire les lignes du dataset avec la France et la Belgique.

extract = subset(dffao,dffao[,"Nom"] %in% c("France","Belgique"))
nrow(extract)
View(extract)

#Exercice 4 - Modifier le dataframe

dffao$Part_export = round(dffao[,"Export_viande"] / dffao[,"Prod_viande"],2)

dffao$Dispo_alim_pays = round(dffao[,"Dispo_alim"] * dffao[,"Population"],2)

View(dffao)

write.table(x = dffao,file = "C:/Users/yanni/BUT/R/TP2/ExportTp2.csv",sep =";", row.names = FALSE)

dispalimmonde = sum(dffao$Dispo_alim_pays,na.rm = TRUE)

adultequeonpeutnourrir = dispalimmonde/2300

#Exercice 5 - Corrélation
install.packages("corrplot") 
library(corrplot)

plot(dffao[,"Export_viande"],dffao[,"Prod_viande"],plot = "scatter",main="Cle titre")

cor(x = dffao$Export_viande ,y = dffao$Prod_viande,use = "complete")

cov(x = dffao$Export_viande ,y = dffao$Prod_viande,use = "complete")

matrice = round(cor(dffao[,-1],use = "complete"),2)
View(matrice)
corrplot(matrice,"circle")
