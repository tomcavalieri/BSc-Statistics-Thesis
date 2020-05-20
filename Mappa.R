install.packages("maps")
install.packages("geosphere")
install.packages("readxl")

library(maps)
library(geosphere)
library(readxl)

# installo i pacchetti necessari per disegnare una mappa ed i flussi 
# al suo interno

xlim <- c(-24.534166666666668,40.31388888888888)
ylim <- c(34.800555555555555, 65.18555555555555)

# limito la mappa all'area geografica dell'UE

flussi <-  read_xlsx("/Users/Cava/Desktop/Tesi/Dati/Mappa.xlsx", range = "A1:C1090", col_names = TRUE)

# carico un file excel appositamente creato a partire dalla tabella 
# precedente contenente i dati sui flussi

coordinate <- read_xlsx("/Users/Cava/Desktop/Tesi/Dati/Capital cities.xlsx", range = "C1:E34", col_names = TRUE)

# carcio un file excel contenente le coordinte (latitudine e longitudine)
# delle capitali degli stati membri dell'UE   

origin <- merge(flussi, coordinate, by.x = "ORIGIN", by.y = "CountryCode")

origin.destination <- merge(origin, coordinate, by.x = "DESTINATION", by.y = "CountryCode")
names(origin.destination) <- c("Destination", "Origin", "Flow", "OriginLatitude", "OriginLongitude", "DestinationLatitude", "DestinationLongitude")

# unisco il file dei flussi a quello delle coordinate per ottenere
# un unico dataset che utilizzeremo per disegnare la mappa

map("world", col="#353535", fill = TRUE, lwd = 1.2, xlim = xlim, ylim = ylim, bg = "black", lforce = "e")

for (j in 1 : length(flussi$ORIGIN)) {
  
  from.long <- origin.destination[j,]$OriginLongitude
  from.lat <- origin.destination[j,]$OriginLatitude
  to.long <- origin.destination[j,]$DestinationLongitude
  to.lat <- origin.destination[j,]$DestinationLatitude
  dim.flow <- origin.destination[j,]$Flow
    
  inter <- gcIntermediate( c(from.long, from.lat), c(to.long, to.lat), n = 500, addStartEnd = TRUE)
  
  lines(inter, col = "lightblue", lwd = log(dim.flow)/100)
  
  # traccio delle linee tra le varie capitali unendo 500 puntini (n) con
  # una linea larga un 100esimo (lwd) del flusso tra i due paesi
}

