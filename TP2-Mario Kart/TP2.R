#Exercice 1 - Importer des données
#Les fonctions getwd(),setwd() et read.csv().

setwd("C:/Users/yanni/BUT/R/TP2/dataset")
getwd()

dftires= read.csv("tires.csv",header=TRUE,dec = ",",sep ="\t")
dfgliders= read.csv("gliders.csv",header=TRUE,dec = ".",sep ="|")
dfbodies= read.csv("bodies_karts.csv",header=TRUE,dec = ",",sep =";")
dfdrivers= read.csv("drivers.csv",header=TRUE,dec = ",",sep =";")

View(dftires)
View(dfgliders)
View(dfbodies)
View(dfdrivers)

dim(dftires)
dim(dfgliders)
dim(dfbodies)
dim(dfdrivers)

rm(list = ls())

#Exercice 2 - Statistique
#cor(),cov(),plot(),install.packages(),library(),summary()

summary(dftires)
summary(dfgliders)
summary(dfbodies)
summary(dfdrivers)

plot(x = dfdrivers$Weight,
     y = dfdrivers$Acceleration, 
     main = "Weight / Acceleration")

#Il semble que les deux variables soient corrélées négativement
#Il y a autant de points mais ils sont superposés car certains drivers ont les mêmes statistiques

cor(x = dfdrivers$Weight,
    y = dfdrivers$Acceleration)

#CALCUL A LA MAIN Exo 4
covXY = cov(x = dfdrivers$Weight,
            y = dfdrivers$Acceleration)
sdX = sd(dfdrivers$Weight)
sdY = sd(dfdrivers$Acceleration)
print(covXY / (sdX*sdY))

#Exo 5

coefCorr = cor(x = dfdrivers$Weight,
               y = dfdrivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)


#Exo 6

matricecordriver = cor(dfdrivers[,-1])
View(round(matricecordriver,2))
#Toutes les variables semblent fortement corrélées entre elles.

#Exo 7

install.packages("corrplot") 
library(corrplot)
corrplot(matricecordriver,"circle")

matricecortires = round(cor(dftires[,-1]),1)

matricecorbodies = round(cor(dfbodies[,-1]),1)


matricecorgliders = round(cor(dfgliders[ ,- 1]),1)

corrplot(matricecorgliders, "circle")

#On remarque une erreur liée à la variable Ground.Handling car tous les gliders ont la même valeur. Il n'est donc pas possible de calculer le coefficient de corrélation car l'écart-type de cette variable est nul.


#Exercice 3 - Manipulation de data frame

#1

resultat = dfdrivers[, c("Driver","Weight")]
View(resultat)

#2SELECT LIGNE COL

resultat = dfdrivers[1:10, c("Driver","Weight","Acceleration")]
View(resultat)

#3 SANS SA:

resultat = dfdrivers[ c(-5 , -7, -9) , c("Driver","Weight","Acceleration")]
View(resultat)
#-c(...) marche aussi
#4 Sans col

resultat = dfdrivers[  , c(-1,-3)]
View(resultat)

#5 Reorder


resultat = dfdrivers[, c("Driver","Acceleration","Weight")]
View(resultat)
#Les colonnes sont dans l'ordre défini par le vecteur.

#6 
resultat = dfdrivers[ c(3,12,32) , ]
View(resultat)

#7
resultat = dfdrivers[ c(32,3,12) , ]
View(resultat)
#Les lignes sont dans l'ordre défini par le vecteur.

#8 Croissant

rang = order(dfdrivers$Weight)
resultat = dfdrivers[ rang  , c("Driver", "Weight") ]
View(resultat)

#9 Decroissant

rang = order(dfdrivers$Acceleration, decreasing = TRUE)
resultat = dfdrivers[ rang  , c("Driver", "Acceleration") ]
View(resultat)

#10 Double tri

rang = order(dfdrivers$Acceleration, dfdrivers$Weight, decreasing = c(TRUE,FALSE))
resultat = dfdrivers[ rang  , c("Driver", "Acceleration","Weight") ]
View(resultat)


#Exercice 4 - GOAT

help(subset)
topDriver = subset(x = dfdrivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))

#⚠️ Dans la fonction subset() il n'est pas nécessaire de mentionner drivers$... car le dataframe utilisé est déjà mentionné dans l'argument x.

#Créer un object topGlider, topTires et topBody avec la même logique de conserver uniquement les meilleurs statistiques d'Acceleration.

topGlider = subset(x = dfgliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))

topTires = subset(x = dftires,
                  subset = Acceleration == max(Acceleration), 
                  select = c("Tire","Acceleration"))

topBody = subset(x = dfbodies,
                 subset = Acceleration == max(Acceleration), 
                 select = c("Body","Acceleration"))
View(topDriver)
View(topGlider)
View(topTires)
View(topBody)
