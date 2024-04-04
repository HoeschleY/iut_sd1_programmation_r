rm(list = ls())
getwd()
setwd("C:/Users/yanni/BUT/R/TP5/nba")

fichiers <- list.files(path = "C:/Users/yanni/BUT/R/TP5/nba", pattern = ".csv$", full.names = TRUE)

print(fichiers)

library(tools) #pour file_path_sans_ext()

basename(fichiers[1])

file_path_sans_ext(basename(fichiers[1]))

# Lire le fichier CSV et l'affecter à une variable avec le nom du fichier

assign(file_path_sans_ext(fichiers[1]),read.csv(fichiers[1],sep = ",",dec = "_"))
#un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.

#INCORRECT :
for(i in fichiers){
  nom_fichiers = file_path_sans_ext(fichiers[i])
  assign(nom_fichiers,read.csv(fichiers[i],sep = ",",dec = "_"))
}

#CORRECTION :

# Boucle pour lire chaque fichier CSV
for (fichier in fichiers) {
  # Extraire le nom du fichier sans extension
  nom_objet <- file_path_sans_ext(basename(fichier))
  
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  start_time <- Sys.time()
  assign(nom_objet, read.csv(fichier, sep = ",",dec = "."))
  end_time <- Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}

#EXERCICE 2 LES JOINTURES

df_LA = subset(team, city == "Los Angeles", select = c("id", "city"))
df_game = subset(game, select = c("game_id", "team_id_home"))
dfJoin = merge(x = df_LA, y = df_game, 
               by.x = "id", 
               by.y = "team_id_home", 
               all.x = TRUE)
nrow(dfJoin)
View(dfJoin)

#ATTENDANCE

df_affluence = subset(game_info, select = c("game_id","attendance"))
df_join2 = merge(x = dfJoin,y = df_affluence,by ="game_id",all.x = TRUE)

mean(df_join2$attendance,na.rm = TRUE)

#ARBITRE

df_x = subset(game_summary, season == 2020,
              select = c("game_id", "season"))
dfJoin = merge(x = df_x, y = officials, 
               by = "game_id",
               all.x = TRUE)
length(unique(dfJoin$official_id))
View(dfJoin)

df_x = subset(officials,first_name == "Dick" & last_name == "Bavetta")

df_y = subset(game_summary,select = c("game_id","season"))


dfJoinOff = merge(df_x,df_y,by = "game_id",all.x =TRUE ,all.y = FALSE)

#NB GAME OFF PAR SEASON
View(dfJoinOff)
table(dfJoinOff$season)




