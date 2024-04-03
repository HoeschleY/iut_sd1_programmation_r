#Exercice 1 - Utilisation d'un dataframe existant un dataframe

iris
class(iris)

View(iris)

nrow(iris)

ncol(iris)

colnames(iris)

summary(iris)

iris$Sepal.Length
iris$Species
#iris[ , c("Sepal.Length","Species")]


iris[c(100,103,105),]

iris[seq(50,100,1),]

mean(iris[,"Sepal.Length"])


mean(iris[,"Sepal.Width"])

sd(iris[,"Petal.Length"])

sd(iris[,"Petal.Width"])

#Exercice 2 - Import/Exporter un dataframe

dfAnime = read.csv("C:/Users/yanni/BUT/R/TP1/anime.csv",header=TRUE,dec = ".",sep =",")

dfManga = read.csv("C:/Users/yanni/BUT/R/TP1/manga.csv",header=TRUE,dec = ".",sep =",")

class(dfAnime)
class(dfManga)

View(dfAnime)
View(dfManga)

dim(dfAnime)
dim(dfManga)

colnames(dfAnime)
colnames(dfManga)

mean(dfAnime[,"Score"])

mean(dfManga[,"Score"])
#Anime a la plus haute moyenne

sum(dfAnime[,"Score"])

sum(dfManga[,"Score"])
#Anime a le plus haut nb de votes

sd(dfAnime[,"Score"])

sd(dfManga[,"Score"])
#Anime a le plus haut écart type

quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))
quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))
#CORRECTION
#Anime a le plus haut Q1

#Filtre sur les Mangas

extract1 = subset(dfManga,dfManga[,"Score"] > 9)
nrow(extract1)

extract2 = subset(dfManga,Vote > 20000)
nrow(extract2)


extract3 = subset(dfManga,Vote > 20000 & Score > 8)
nrow(extract3)


extract4 = subset(dfManga,Score >= 7 & Score =< 8)
nrow(extract4)

#Filtre sur les Animes

effectif = table(dfAnime$Rating)
print(effectif)
length(effectif)
prop.table(effectif)

extractA1 = subset(dfAnime,Rating == "R - 17+ (violence & profanity)")
nrow(extractA1)

extractA2 = subset(dfAnime,Rating == "R - 17+ (violence & profanity)" & Score >=8)
nrow(extractA2)

extractA3 = subset(dfAnime,Rating != "R - 17+ (violence & profanity)")
nrow(extractA3)

#extractA4 = subset(dfAnime,Rating == "PG - Children" & Rating == "G - All Ages")
#nrow(extractA4)

#CORRECTION
extraction5 <- subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))
nrow(extraction5)

extractA5 = subset(dfAnime,Rating != "PG - Children" & Rating != "G - All Ages")
nrow(extractA5)
#extraction6 <- subset(dfAnime, !Rating %in% c("PG - Children","G - All Ages"))
#nrow(extraction6)

extractA6 = subset(dfAnime,Score >= 9 & Vote >= 400000)
nrow(extractA6)

dfAnime = dfAnime[, c("Title","Score","Title","Ranked")]
dfManga = dfManga[, c("Title","Score","Title","Ranked")]

dfAnime$Type = "Anime"
dfManga$Type = "Manga"

View(dfAnime)
View(dfManga)

dfConcat = rbind(dfAnime,dfManga)
View(dfConcat)

write.table(x = dfConcat,file = "C:/Users/yanni/BUT/R/TP1/ExportTp1.csv",sep =";", row.names = FALSE)

