rm(list = ls())

setwd("C:/Users/yanni/BUT/R/TD6")


df <- read.csv(file = "nba2014_2015.csv", sep = ",",
               header = TRUE, dec = ".")#header , sep , dec, filepath


View(df)
nrow(df)
ncol(df)#faire gaffe aux noms
colnames(df)


df$PERIOD <- as.factor(df$PERIOD)
df$PTS_TYPE <- as.factor(df$PTS_TYPE)#pas oublier maj
df$SHOOTER = as.factor(df$SHOOTER)#deux O


#EXO 2
length(levels(df$PERIOD)) #ne sais pas ecrire length
length(df$PTS_TYPE)#pas oublier maj et _
length(df$SHOOTER)
summary(df)
sd(df$SHOT_DIST)

     sd(df$SHOT_CLOCK, na.rm = TRUE) #Oublie na rm
     
     #combien de tirs manqués/réussis
     table(df$SHOT_RESULT)#Use $
     #les quartiles
     quantile(df$SHOT_CLOCK, probs = seq(0,1,0.25),na.rm = TRUE) #probs avec seq et na.rm
     #les déciles
     quantile(df$CLOSE_DEF_DIST, probs = seq(0,1,0.1),na.rm = TRUE) #probs avec seq et na.rm et enlever le "s" erreur de nom et ) en trop
     #nombre de matches différents
     liste_game <- unique(df$GAME_ID) # ) en trop
length(liste_game)#manque _
#nombre de joueurs différents
df$SHOOTER <- as.factor(df$SHOOTER) # . a la place de _
levels(df$SHOOTER) #nlevel = levels et rajouté )
       #conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
       df$SHOT_DIST_METRE = df$SHOT_DIST * 0.30 #no == et df
       #nombre de points qu'a rapporté la tentative (0,2 ou 3)  
       df$PTS_MARQUES <- ifelse(df$SHOT_RESULT == "made", yes = df$PTS_TYPE, 0)#faut ==
       #On supprime la variable GAME_RESULT car elle n'est pas utile
       df$GAME_RESULT <- NULL # NULL
       
       #création d'un objet sans la première colonne GAME_ID
       df2 <- df[,-1]#mettre le - 1 du bon coter

       
#EXO 3
       
       #Les 100 tirs réussis ou manqués les plus loin
       rang <- order(df$SHOT_DIST, decreasing = TRUE)#FALSE -> TRUE
       df3 <- df[rang, ]#rang du bon coter
       df3 <- df3[c(1:100),]#c() et df3
       
       #Les 100 tirs réussis les plus loin
       df3 <- df[rang, ]#rang du bon coter
       df4 = subset(df3, SHOT_RESULT == "made")# == et ""
       df4 <- df4[c(1:100), ]
       
       #Combien de tirs à 3 points a réussi Kobe Bryant ?
       df_kobe = subset(df,SHOT_RESULT == "made" &
                          PTS_TYPE == 3 & 
                          SHOOTER == "kobe bryant") #" maj et ==
       
       dim(df_kobe)
       
       #Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
       df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
       rang =order(df_total$PTS_MARQUES,decreasing = TRUE)
       df_total_tri <- df_total[rang,]
       df_top5 <-  df_total_tri[c(1:5),  ]
       
       
#EXO 4
       
#Des graphiques adaptés selon le type de variable
       
#construction de la fonction
build_graph = function(une_colonne, nom_colonne) 
{
if(is.numeric(une_colonne)) {
  print(boxplot(une_colonne, main = nom_colonne))}

else if(is.factor(une_colonne)) {
  tri <- table(une_colonne)
  print(barplot(tri, main = nom_colonne))}
}

#on déroule la fonction sur chaque colonne du data frame.

for (colonne in (colnames(df))) 
     {
  build_graph(une_colonne = df[,colonne], nom_colonne = colonne)
}
       
View(df)
