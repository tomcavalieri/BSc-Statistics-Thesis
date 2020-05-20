install.packages("readxl")

library(readxl)

dataset.indicatori2 <- read_xlsx("/Users/Cava/Desktop/Tesi/Dati/Dataset indicatori copia.xlsx", range = "A1:R345", col_names = TRUE)
View(dataset.indicatori2)

dati2 <- dataset.indicatori2
dati2$Internazionalizzazione[dati2$IC10 < 0.05] <- "No"
dati2$Internazionalizzazione[dati2$IC10 >= 0.05] <- "Si"

tapply(dati2$`Voto medio di laurea`, INDEX = dati2$Internazionalizzazione, FUN = mean)
tapply(dati2$IC02, INDEX = dati2$Internazionalizzazione, FUN = mean)
tapply(dati2$`Indice di ritardo`, INDEX = dati2$Internazionalizzazione, FUN = mean)
tapply(dati2$IC18, INDEX = dati2$Internazionalizzazione, FUN = mean)
tapply(dati2$IC25, INDEX = dati2$Internazionalizzazione, FUN = mean)


