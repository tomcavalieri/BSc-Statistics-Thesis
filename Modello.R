install.packages("readxl")
install.packages("lme4")
install.packages("Matrix")

library(readxl)
library(Matrix)
library(lme4)

# installiamo le librerie ed i pacchetti necessari
# (lme4 è il pacchetto che ci servirà per il modello gerarchico)

dati.modello <- read_xlsx("/Users/Cava/Desktop/Tesi/Dati/Dataset indicatori copia 2.xlsx", range = "A1:P337", col_names = TRUE)
View(dati.modello)

# carichiamo il file appositamente preparato contenente i dati da analizzare

IC10 <- t(dati.modello[,8])
IC11 <- t(dati.modello[,9])
IC06 <- t(dati.modello[,10])

# salviamo le variabili che utilizeremo

plot(IC06, IC10)

prov.names <- c("Ancona", "Genova","Modena & Reggio Emilia", "Napoli", "Siena", "Teramo", "Trieste", "Venezia", "Verona")


dati.modello$prov.factor <- as.factor(dati.modello$Provincia)
levels(dati.modello$prov.factor) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

# fattorizziamo la variabile Provincia

mod_1 <- lmer(IC06 ~ IC10 + (1 + IC10 | prov.factor), data = dati.modello)
summary(mod_1)

ranef(mod_1)


lm.pooled <- lm(IC06 ~ IC10, data = dati.modello) # regressione con dati aggregati
lm.unpooled <-  lm(formula = IC06 ~ IC10 + factor(Provincia) - 1, data = dati.modello) #regressione con dati separati per gruppo


par (mfrow=c(3,3))


a.hat.M1 <- coef(mod_1)$prov.factor[,1]  # vettore con le intercette
b.hat.M1 <- coef(mod_1)$prov.factor[,2] # vettore con le pendenze
x.jitter <- IC10 + runif(n = 336, min = -.05,max = .05) # utilizziamo il jittering come metodo per evitare problemi di overplotting

for (j in 1 : 9){
  
  plot (x.jitter[dati.modello$prov.factor==j], IC06[dati.modello$prov.factor==j], xlim = c(0,0.18),
        ylim = c(0,1), xlab = "IC10", ylab = "IC06", main = prov.names[j]) 
  
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2] * x, lty = 2, col = "gray10", add = TRUE)
  curve (coef(lm.unpooled)[j + 1] + coef(lm.unpooled)[1] * x, col = "gray10", add = TRUE)
  curve (a.hat.M1[j] + b.hat.M1[j] * x, lwd = 1, col = "red", add = TRUE) 
}

mod_2 <- lmer(formula = IC06 ~ IC10 + Tasso.disocc + (1 + IC10 | prov.factor), data = dati.modello)
summary(mod_2)

ranef(mod_2)


