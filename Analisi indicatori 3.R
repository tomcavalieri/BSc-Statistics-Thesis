library(readxl)

dataset.indicatori3 <- read_xlsx("/Users/Cava/Desktop/Tesi/Dati/Dataset indicatori copia 2.xlsx", range = "A1:R337", col_names = TRUE)
View(dataset.indicatori3)


mat <- as.matrix(IC02, IC10, IC11, IC06, IC18, IC25, voto, ritardo)

dati3 <- dataset.indicatori3
dati3$Internazionalizzazione[dati3$IC10 < 0.05] <- "No"
dati3$Internazionalizzazione[dati3$IC10 >= 0.05] <- "Si"

tapply(dati3$IC06, INDEX = dati3$Internazionalizzazione, FUN = median)

cor(mat, mat, method = "pearson")

