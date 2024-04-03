rm(list = ls())
getwd()
setwd("C:/Users/yanni/BUT/R/TD3")
install.packages("readxl")

library(readxl)

#Les fonctions read_excel() et as.factor().

dfpoke = read_excel("pokemon.xlsx","pokemon")
View(dfpoke)

dim(dfpoke)

summary(dfpoke)

#AS FACTOR POUR LES STATS QUALITATIVE
dfpoke$is_legendary <-as.factor(dfpoke$is_legendary)
dfpoke$generation <-as.factor(dfpoke$generation)
dfpoke$type <-as.factor(dfpoke$type)

summary(dfpoke)

#Exercice 2 - Création de colonne
#ifelse() cut() et is.na()

#Créer une colonne attack_group avec la valeur attack+ si la valeur d'attack est supérieure ou égale à la médiane, sinon attack-. Convertir cette variable en factor puis effectuer un résumé de cette colonne avec la fonction summary().

med = median(dfpoke$attack)
dfpoke$attack_group = ifelse(dfpoke$attack >= med, "attack+","attack-")
dfpoke$attack_group = as.factor(dfpoke$attack_group)
summary(dfpoke$attack_group)



dfpoke$waterfire = ifelse(dfpoke$type == "fire", "fire", ifelse(dfpoke$type == "water","water","no"))
dfpoke$waterfire = as.factor(dfpoke$waterfire)
summary(dfpoke$waterfire)

centilebestattack = quantile(dfpoke$attack,seq(0.75,1,0.01))
centilebestspeed = quantile(dfpoke$speed,seq(0.75,1,0.01))
centilebestdefense = quantile(dfpoke$defense,seq(0.75,1,0.01))
dfpoke$best = ifelse(dfpoke$attack %in% centilebestattack & dfpoke$speed %in% centilebestspeed & dfpoke$defense %in% centilebestdefense,"yes","no")

dfpoke$best = as.factor(dfpoke$best)
summary(dfpoke$best)

#Filtrer les données dans un objet nommé requete avec les pokemons ayant des valeurs manquantes sur la colonne weight_kg.

requete = subset(dfpoke, !is.na(weight_kg))
View(requete)

medweigth = median(dfpoke$weight_kg,na.rm = TRUE)
medheight = median(dfpoke$height_m,na.rm = TRUE)

dfpoke$weight_kgNa = ifelse(is.na(dfpoke$weight_kg),medweigth,dfpoke$weight_kg)
dfpoke$height_mNA = ifelse(is.na(dfpoke$height_m),medheight,dfpoke$height_m)


#La fonctions cut().

dfpoke$weight_group = cut(dfpoke$weight_kgNa,3,c("leger","moyen","lourd"))
summary(dfpoke$weight_group)

#SEP PRECISE

dfpoke$height_group = cut(dfpoke$height_mNA,breaks = c(0,1,2,3,max(dfpoke$height_mNA,na.rm = TRUE)))
summary(dfpoke$height_group)

#SEP QUANTILE

dfpoke$defense_group = cut(dfpoke$defense,breaks = quantile(dfpoke$defense,na.rm = TRUE),include.lowest = TRUE)
summary(dfpoke$defense_group)

#La fonction aggregate().
#Calculer la moyenne d'attack par type.

aggregate(x = attack ~ type, data = dfpoke,FUN = function(x) mean(x))

#Calculer la mediane d'attack par generation et type.

aggregate(x = attack ~ type + generation, data = dfpoke,FUN = function(x) median(x))

#Calculer l'effectif par type.

aggregate(x = pokedex_number ~ type, data = dfpoke,FUN = function(x) length(x))

#Calculer la moyenne et la mediane de la statistique speed pour chaque generation et type. Afficher également les effectifs de chaque paire.

aggregate(x = speed ~ generation,dfpoke, function(x) c(effectif = length(x),moy = mean(x),med = median(x)))

