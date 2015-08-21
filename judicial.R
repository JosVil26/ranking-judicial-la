library(data.table)
library(xlsx)

judicial <- read.xlsx('BD-RankingPoderJudicial-AL.xlsx', 2, stringsAsFactors=FALSE)
preguntas <- read.xlsx('BD-RankingPoderJudicial-AL.xlsx', 3, stringsAsFactors=FALSE)
paises <- data.table(cbind(unique(judicial$pais), 0))
names(paises) <- c("pais", "puntos")

for (paisActual in paises$pais) {
    auxPais <- subset(judicial, judicial$pais==paisActual & judicial$cumple >= 0)
    suma <- sum(auxPais$cumple)
    indice <- which(paises$pais == paisActual)
    paises[indice]$puntos <- suma
}
#paises$puntos <- as.integer(paises$puntos)
rm (suma, auxPais, indice)
ranking <- paises[order(paises$puntos, decreasing = TRUE)]

minPuntos <- min(as.integer(ranking$puntos))
maxPuntos <- max(as.integer(ranking$puntos))
totalPaises <- length(ranking$puntos)

plot.new()
plot(factor(ranking$pais, levels=ranking$pais), ranking$puntos, labels = FALSE, ylim=c(minPuntos, maxPuntos))
axis(1, at=1:totalPaises, labels=ranking$pais, las=2)
axis(2, at=minPuntos:maxPuntos)

ranking$puntos <- as.integer(ranking$puntos)
warnings()

categorias <- unique(substring(preguntas$id, 1, 2))
nombres <- names(paises)

for (categoria in categorias) {
    rankingActual <- c()
    nombres <- c(nombres, paste(categoria, "x", sep=""))
    for (paisActual in paises$pais) {
        preguntasCategoria <- subset(preguntas$id, substring(preguntas$id,1,2) == categoria)
        auxPais <- subset(judicial, judicial$pais == paisActual & judicial$id %in% preguntasCategoria & judicial$cumple > 0)
        suma <- sum(auxPais$cumple)
        rankingActual <- c(rankingActual, suma)
    }
    paises <- cbind(paises, nombreRankingActual = rankingActual)
    names(paises) <- nombres
}
rm(auxPais, nombres, rankingActual, preguntasCategoria, categoria, categorias)

message ("Los resultados totales se encuentran almacenados en la variable: paises\n
Para consultar un ranking determinado se puede utilizar algo asi como lo siguiente:\n
  paises[order(paises$`1.x`, decreasing = TRUE)]\n
donde `1.x` representa la columna de paises por la cual se quieren ordenar.\n
Para listar los paises que se encuentran por encima de la media de una columna determinada:\n
  subset(paises, `3.x` > mean(paises$`3.x`))\n")
