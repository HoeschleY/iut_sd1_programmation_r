rm(list = ls())

#EXO 1
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 100)
{
  if(is.numeric(salaire_brut) != TRUE){
    return("Valeur non valide salaire")
  }
  
  if(temps_travail<0 | temps_travail>100 | is.numeric(temps_travail) != TRUE){
    return("Valeur non valide temps")
  }
  salaire_net_avant_impot = (salaire_brut * 0.75)*temps_travail/100
  return (salaire_net_avant_impot)
  }

salaire_net_cadre(salaire_brut = 1000,temps_travail = 140)


#DEUXIEME FONCTION


salaire_net = function(salaire_brut = 2500,temps_travail = 100,statut = "Cadre"){
  if(is.numeric(salaire_brut) != TRUE){
    return("Valeur non valide salaire")
  }
  
  if(temps_travail<0 | temps_travail>100 | is.numeric(temps_travail) != TRUE){
    return("Valeur non valide temps")
  }
  if(!statut %in% c("Cadre","Non cadre")){
    return("Valeur statut non valide")
  }
  if(statut == "Cadre"){
    salaire_net_avant_impot = (salaire_brut * 0.75)*temps_travail/100 
  }
  if(statut == "Non cadre"){
    salaire_net_avant_impot = (salaire_brut * 0.78)*temps_travail/100 
  }
  return (salaire_net_avant_impot)
}

salaire_net(statut = "Non cadre")



#TROISIEME FONCTION


salaire_after_prelevement = function(salaire_brut = 2500,temps_travail = 100 ,statut = "Cadre"){
  if(is.numeric(salaire_brut) != TRUE){
    return("Valeur non valide salaire")
  }
  
  if(temps_travail<0 | temps_travail>100 | is.numeric(temps_travail) != TRUE){
    return("Valeur non valide temps")
  }
  if(!statut %in% c("Cadre","Non cadre")){
    return("Valeur statut non valide")
  }
  if(statut == "Cadre"){
    salaire_net_avant_impot = (salaire_brut * 0.75)*temps_travail/100 
  }
  if(statut == "Non cadre"){
    salaire_net_avant_impot = (salaire_brut * 0.78)*temps_travail/100 
  }
  
  if(salaire_brut < 1591){
    salaire_net_avant_impot = salaire_net_avant_impot * 1
  }else if(salaire_brut < 2006) {
    salaire_net_avant_impot = salaire_net_avant_impot * (1-0.029)
  }else if(salaire_brut < 3476) {
    salaire_net_avant_impot = salaire_net_avant_impot * (1-0.099)
  }else if(salaire_brut < 8557) {
    salaire_net_avant_impot = salaire_net_avant_impot * (1-0.2)
  }else{
    salaire_net_avant_impot = salaire_net_avant_impot * (1-0.43)
  }
  return (salaire_net_avant_impot)
  
}
salaire_after_prelevement(salaire_brut = 10000)

#FONCTION 3

shifumi = function(choix){
  choixpc = sample(c("pierre","papier","ciseaux"),size = 1)
  if(choixpc == choix){
    return("Tie")
  }else if((choixpc == "pierre" & choix == "papier") | (choixpc == "ciseaux" & choix == "pierre") | (choixpc == "papier" & choix == "ciseaux")){
    return("You Win")
  }else{
    return("You lose")
  }
}

shifumi("papier")

#FONCTION 4

resultat = 0
for (element in c(1,2,3,4,5)) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
}

element = 1
resultat = 0
while (resultat <= 50) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
  print(paste("le programme s'est arrêté à la valeur : ", element))
  element = element + 1
}


for (colonnes in colnames(iris)){
  print(class(iris[,colonnes]))
}

# Initialisation de l'indice de colonne
indice_colonne <- 1

# Tant qu'il reste des colonnes à parcourir dans iris
while (indice_colonne <= ncol(iris)) {
  # Récupération du nom de la colonne
  nom_colonne <- colnames(iris)[indice_colonne]
  
  # Récupération du type de données de la colonne
  type_colonne <- class(iris[, nom_colonne])
  
  # Affichage du résultat
  print(paste("la colonne ", nom_colonne, " est de type : ", type_colonne))
  
  # Passage à la colonne suivante
  indice_colonne <- indice_colonne + 1
}
  
for (i in 1:5){
  nb = as.numeric(readline(prompt = "entrer nb "))
  nb = nb**2
  print(nb)
}

doc = "C:/Users/yanni/BUT/Stat Inférentiel/SAE"

fichiers = list.files(doc,full.names = TRUE)

for (fichier in fichiers){
  info = file.info(fichier)
  print(info$size)
}




# Parcourir toutes les colonnes du dataframe iris
for (colonne in colnames(iris)) {
  # Vérifier si la colonne est de type numeric
  if (is.numeric(iris[,colonne])) {
    # Si c'est le cas, construire un boxplot avec un titre
    boxplot(iris[,colonne], main = paste("Boxplot de", colonne))
  } else {
    # Sinon, construire un barplot avec un titre
    barplot(table(iris[,colonne]), main = paste("Barplot de", colonne))
  }
}


# Boucle while pour jouer au shifumi jusqu'à ce que l'utilisateur décide d'arrêter
continuer <- TRUE
while (continuer) {
  # Appeler la fonction shifumi et afficher le résultat
  choix <- readline(prompt = "Choix ")
  resultat <- shifumi(choix = choix)
  cat("Résultat du jeu :", resultat, "\n")
  
  # Demander à l'utilisateur s'il souhaite continuer
  reponse <- readline(prompt = "Voulez-vous continuer à jouer ? (oui/non) : ")
  
  # Vérifier la réponse de l'utilisateur
  if (tolower(reponse) == "non") {
    print("Arrêt du jeu.")
    continuer <- FALSE  # Mettre fin à la boucle
  }
}

# Fonction pour le jeu du juste prix
juste_prix <- function() {
  # Génération d'un nombre aléatoire entre 1 et 100
  nombre_a_deviner <- sample(1:100, 1)
  
  # Initialisation de la réponse de l'utilisateur
  reponse <- -1
  
  # Boucle tant que l'utilisateur n'a pas trouvé le bon nombre
  while (reponse != nombre_a_deviner) {
    # Demande à l'utilisateur de saisir un nombre
    reponse <- as.integer(readline(prompt = "Devinez le nombre : "))
    
    # Vérifie si le nombre est trop grand, trop petit ou correct
    if (reponse < nombre_a_deviner) {
      cat("C'est plus !\n")
    } else if (reponse > nombre_a_deviner) {
      cat("C'est moins !\n")
    } else {
      cat("Bravo, vous avez trouvé le juste prix !\n")
    }
  }
}

# Appel de la fonction juste_prix pour commencer le jeu
juste_prix()