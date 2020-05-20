install.packages("readxl")

library(circlize)
library(RColorBrewer)
library(readxl)

# vengono installati i pacchetti e le librerie necessarie per 
# raffigurare il `diagramma a corda'

dataset.flussi <- read_xlsx("/Users/Cava/Desktop/Tesi/Dati/Matrice flussi.xlsx", range = "B2:AJ36", col_names = TRUE)
View(dataset.flussi)

# carichiamo il file excel scaricato dal sito della Commissione Europea 

Names <- array (dataset.flussi[,1])

host.country <- t(Names)

matrice.flussi <- as.matrix(dataset.flussi[2:34])

row.names(matrice.flussi) <- host.country

matrice.flussi <- matrice.flussi[-34,]

# trasformiamo i dati in una matrice così da poterla usare come
# oggetto da passare alla funzione chordDiagram()

chordDiagram(matrice.flussi, grid.col = c("#377eb8", "#d95f02", "midnightblue", "#e7298a", "#66a61e"), grid.border = "black", transparency = 0.1, 
             column.col = c(rep("red", 9), "#377eb8", "red", "#d95f02", rep("red", 4), "midnightblue", rep("red", 12), "#e7298a", rep("red", 2),"#66a61e"), directional = 1, reduce = 0.06)

# il parametro reduce guarda la percentuale di cerchio occupata  
# dal singolo arco ed elimina tutte quelle che sono al di sotto 
# del valore assegnato a tale parametro (in questo caso 6%)
# il valore del parametro directional pari a 1 indica che i flussi
# sono direzionali e vi è quindi un'origine ed una destinazione
