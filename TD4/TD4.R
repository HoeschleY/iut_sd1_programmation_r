rm(list = ls())
getwd()
setwd("C:/Users/yanni/BUT/R/TD4/")



dfvelo = read.csv(file = "velov.csv",header = TRUE,sep = ";", dec = "," )
View(dfvelo)

summary(dfvelo)

class(dfvelo$status)
class(dfvelo$CodePostal)

dfvelo$status = as.factor(dfvelo$status)
dfvelo$CodePostal = as.factor(dfvelo$CodePostal)

class(dfvelo$status)
class(dfvelo$CodePostal)

dfvelo$bornes = ifelse(dfvelo$capacity != (dfvelo$bikes + dfvelo$stands), "KO" , "OK")
dfvelo$bornes = as.factor(dfvelo$bornes)
table(dfvelo$bornes)
#en réalité, c'est aussi peut-être car la station est fermée OU que des usagers ont déposé leur vélo pile au moment de l'extraction.

hist(dfvelo$capacity,main = "Capacity")

hist(dfvelo$capacity,main = "Capacity",breaks= 6)


hist(dfvelo$capacity,main = "Capacity",breaks= 6,col = "Red")

hist(dfvelo$capacity,main = "Titre Capacity",breaks= 6,col = "Red",xlab = "Capacity")
abline(a= 100, b= 0,col = "Blue")

#Construire le même graphique mais avec la densité plutôt que les effectifs. Supprimer l'argument break pour rétablir les classes par défaut.

hist(dfvelo$capacity,main = "Titre Capacity",probability = TRUE,col = "Red",xlab = "Capacity")
#Ajouter la courbe densité de cette distribution à l'aide des fonctions lines() et density(). On peut mettre cette courbe en bleu en changeant la taille de la courbe avec l'argument lwd.

lines(density(dfvelo$capacity),col = "Blue",lwd = "3")

#Pour voir la courbe density en entier, modifier les bornes de l'axe des ordonnées de l'histogramme avec l'argument ylim. Relancer l'ensemble des commandes pour tracer à nouveau le graphique.

hist(x = dfvelo$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "Red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))


lines(density(dfvelo$capacity),
      lty = 2,
      col = "Blue",
      lwd = 2)

boxplot(x = dfvelo$capacity, main = "Boxplot de \n la capacité des stations",outline= FALSE)

moy = mean(dfvelo$capacity)

points(moy, col = "Red", pch = 4, cex = 4)

par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes
#7ème
dfvelo7 = subset(dfvelo, CodePostal == "69007")
boxplot(x = dfvelo7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
dfvelo8 = subset(dfvelo, CodePostal == "69008")
boxplot(x = dfvelo8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))
#C'est plus simple d'analyser les deux graphiques si la borne des ordonnées est la même.
# On remarque que la disponibilité des stations est plus homogènes sur le 8ème.


# Calculer les moyennes de chaque groupe
means <- tapply(X = dfvelo$bikes, 
                INDEX = dfvelo$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "Red", pch = 19)


#Créer un diagramme en barre de la réparition du nombre de station bonus à l'aide de la fonction barplot(). N'oublier pas de mettre un titre.

effectif = table(dfvelo$bonus)
barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus")


#Construire le même graphique mais pivoter horizontalement.

barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus",
        horiz = TRUE)

#Construire le même graphique mais en pourcentage.

frequence = prop.table(effectif)
barplot(height = frequence,
        main = "Répartition en % du nombre \n de station bonus",
        horiz = TRUE)

#Construire un diagramme bivariés avec la répartition du nombre de station bonus en fonction du nombre de station avec un terminal de paiement. Les deux variables ayant les mêmes modalités TRUE / FALSE, il est important de définir le nom de l'axe des abscisses. Que remarque t-on ?
#⚠️ l'ordre des variables dans la fonction table() influence le niveau de lecture du graphique.

effectif = table(dfvelo$banking, dfvelo$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.

#Afficher une legend pour pouvoir distinguer les couleurs associées aux modalités avec vert pour TRUE et rouge pour FALSE. On peut vérifier si le graphique est cohérent en vérifiant avec l'objet frequence.
#Calcul des pourcentages
frequence = prop.table(x = effectif)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)


#Même question mais en pourcentage colonne.
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

#Même question mais avec un diagramme bivarié non empilé à l'aide de l'argument beside.

#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)


#Créer un diagramme circulaire de la réparition du nombre de station bonus à l'aide de la fonction pie() en différenciant les deux catégories avec la couleur jaune et vert. N'oublier pas de mettre un titre.

pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"))

#Construire le même graphique à l'aide de l'argument labels et la fonction paste() afin d'ajouter les etiquettes de données avec les effectifs.

etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"),
    labels = etiquette)

#Construire dans un diagramme en barre le top 10 des codes postaux avec le plus de station velo'v. On peut pivoter les étiquettes à l'aide de l'argument las pour une meilleur lecture du graphique. Utiliser la fonction palette() comme couleur pour les barres. Que remarque t-on ?

effectif = table(dfvelo$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = palette(),
        las = 2)  # Rotation des étiquettes à 90 degrés
#On remarque que les deux premières couleurs se répetent.
print(palette()) # la fonction `palette()` ne dispose que de 8 couleurs

#⚠️ En effet, la fonction palette() ne dispose que de 8 couleurs par défaut.

barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = colors(),
        las = 2)  # Rotation des étiquettes à 90 degrés

print(colors())

#⚠️ C'est pour cela qu'on peut utiliser les couleurs avec le nom. R les associe ensuite aux vecteurs palette() ou colors().

dev.print(device = png, file = "export.png", width = 600)

#NUAGE DE POINTS
plot(x = dfvelo$stands, y = dfvelo$capacity,
     main = "Place disponible vs Capacité")

plot(x = dfvelo$stands, y = dfvelo$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     pch=19)



























#FIN
install.packages("leaflet")
install.packages("dplyr")
install.packages("ggplot2")

# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(dfvelo) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte
