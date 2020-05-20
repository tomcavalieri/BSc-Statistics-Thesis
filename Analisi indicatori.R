install.packages("readxl")

library(readxl)

dataset.indicatori <- read_xlsx("/Users/Cava/Desktop/Tesi/Dati/Dataset indicatori.xlsx", range = "A1:R370", col_names = TRUE)
View(dataset.indicatori)

ciclo <- t(dataset.indicatori[,3])
classe <- t(dataset.indicatori[,4])
IC02 <- t(dataset.indicatori[,11])
IC10 <- t(dataset.indicatori[,12])
IC11 <- t(dataset.indicatori[,13])
IC06 <- t(dataset.indicatori[,14])
IC18 <- t(dataset.indicatori[,15])
IC25 <- t(dataset.indicatori[,16])
voto <- t(dataset.indicatori[,17])
ritardo <- t(dataset.indicatori[,18])

#####ANALISI DESCRITTIVA#####

#medie
mean(IC02)
mean(IC10)
max(IC10)
mean(IC11)
max(IC11)
mean(IC06, na.rm = TRUE)
mean(IC18, na.rm = TRUE)
mean(IC25, na.rm = TRUE)
mean(voto, na.rm = TRUE)
mean(ritardo, na.rm = TRUE)

#grafici
plot(IC02, IC11, main = "L'impatto dell'internazionalizzazione (IC11)")
plot(IC02, IC10, main ="L'impatto dell'internazionalizzazione (IC10)")

boxplot(IC10[ciclo=="Primo"], IC10[ciclo=="Secondo"], IC10[ciclo=="Unico"], names=c("Primo","Secondo", "Unico"), border = c("Blue", "Red", "Green"), horizontal = TRUE, main = "Un confronto tra triennali e magistrali")

#correlazioni
cor(dataset.indicatori$IC02, dataset.indicatori$IC10)
cor(dataset.indicatori$IC06, dataset.indicatori$IC10, use = "complete.obs")
cor(dataset.indicatori$IC18, dataset.indicatori$IC10, use = "complete.obs")
cor(dataset.indicatori$IC25, dataset.indicatori$IC10, use = "complete.obs")
cor(dataset.indicatori$`Voto medio di laurea`, dataset.indicatori$IC10, , use = "complete.obs")
cor(dataset.indicatori$`Indice di ritardo`, dataset.indicatori$IC10, , use = "complete.obs")

cor(dataset.indicatori$IC02, dataset.indicatori$IC11)
cor(dataset.indicatori$IC06, dataset.indicatori$IC11, use = "complete.obs")
cor(dataset.indicatori$IC18, dataset.indicatori$IC11, use = "complete.obs")
cor(dataset.indicatori$IC25, dataset.indicatori$IC11, use = "complete.obs")
cor(dataset.indicatori$`Voto medio di laurea`, dataset.indicatori$IC11, , use = "complete.obs")
cor(dataset.indicatori$`Indice di ritardo`, dataset.indicatori$IC11, , use = "complete.obs")

cor(dataset.indicatori$IC10, dataset.indicatori$IC11, use = "complete.obs")



dati <- dataset.indicatori

dati$Area[dati$Classe=="L/SNT1"] <- "Sanitaria"
dati$Area[dati$Classe=="L/SNT2"] <- "Sanitaria"
dati$Area[dati$Classe=="L/SNT3"] <- "Sanitaria"
dati$Area[dati$Classe=="L/SNT4"] <- "Sanitaria"
dati$Area[dati$Classe=="L-13"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-17"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-2"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-21"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-22"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-23"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-25"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-26"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-27"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-28"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-29"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-30"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-31"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-32"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-34"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-35"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-38"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-4"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-41"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-43"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-7"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-8"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-9"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="L-1"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-10"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-11"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-12"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-14"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-15"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-16"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-18"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-19"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-20"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-24"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-3"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-33"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-36"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-37"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-39"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-40"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-42"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-5"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="L-6"] <- "Umanistico-Sociale"

dati$Area[dati$Classe=="LM/SNT1"] <- "Sanitaria"
dati$Area[dati$Classe=="LM/SNT2"] <- "Sanitaria"
dati$Area[dati$Classe=="LM/SNT3"] <- "Sanitaria"
dati$Area[dati$Classe=="LM/SNT4"] <- "Sanitaria"
dati$Area[dati$Classe=="LM-41"] <- "Sanitaria"
dati$Area[dati$Classe=="LM-42"] <- "Sanitaria"
dati$Area[dati$Classe=="LM-46"] <- "Sanitaria"
dati$Area[dati$Classe=="LM-3"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-4"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-6"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-7"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-8"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-9"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-10"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-11"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-12"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-17"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-18"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-20"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-21"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-22"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-23"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-24"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-25"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-26"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-27"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-28"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-29"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-30"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-31"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-32"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-33"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-34"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-35"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-40"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-44"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-47"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-48"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-53"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-54"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-58"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-60"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-61"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-66"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-67"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-68"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-69"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-70"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-71"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-72"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-73"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-74"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-75"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-79"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-82"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-83"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-86"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-91"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-13"] <- "Scientifico-Tecnologica"
dati$Area[dati$Classe=="LM-1"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-2"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-5"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-14"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-15"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-16"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-19"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-36"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-37"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-38"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-39"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-43"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-45"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-49"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-50"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-51"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-52"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-55"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-56"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-57"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-59"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-62"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-63"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-64"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-65"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-76"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-77"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-78"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-80"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-81"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-84"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-85"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-87"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-88"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-89"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-90"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-92"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-93"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-94"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LM-85bis"] <- "Umanistico-Sociale"
dati$Area[dati$Classe=="LMG/01"] <- "Umanistico-Sociale"

boxplot(IC10[dati$Area=="Sanitaria"], IC10[dati$Area=="Scientifico-Tecnologica"], IC10[dati$Area=="Umanistico-Sociale"], names=c("Sanitaria","Scientifico-Tecnologica", "Umanistico-Sociale"), border = c("Blue", "Red", "Green"), horizontal = TRUE, main = "Un confronto tra le aree disciplinari")

tapply(dati$IC11, INDEX = dati$Area, FUN = mean)
tapply(dati$IC10, INDEX = dati$Area, FUN = mean)

tapply(dati$IC10, INDEX = dati$Ciclo, FUN = mean)




#regressioni
reg.IC02 <- lm(IC02~IC10, dataset.indicatori) #significativo
reg.IC06 <- lm(IC06~IC10, dataset.indicatori) #significativo
reg.IC18 <- lm(IC18~IC10, dataset.indicatori) #no significativo
reg.IC25 <- lm(IC25~IC10, dataset.indicatori) #significativo ma inverso
reg.voto <- lm(voto~IC10, dataset.indicatori, na.omit(dataset.indicatori))
reg.ritardo <- lm(ritardo~IC10, dataset.indicatori)
?lm

reg2.IC02 <- lm(IC02~IC11, dataset.indicatori) #significativo
reg2.IC06 <- lm(IC06~IC11, dataset.indicatori) #no significativo
reg2.IC18 <- lm(IC18~IC11, dataset.indicatori) #no significativo
reg2.IC25 <- lm(IC25~IC11, dataset.indicatori) #significativo ma inverso
reg2.voto <- lm(voto~IC11, dataset.indicatori)
reg2.ritardo <- lm(ritardo~IC10, dataset.indicatori)


summary(reg.IC02)
summary(reg.IC06)
summary(reg.IC18)
summary(reg.IC25)

summary(reg.voto)
summary(reg.ritardo)

summary(reg2.IC02)
summary(reg2.IC06)
summary(reg2.IC18)
summary(reg2.IC25)

summary(reg2.voto)
summary(reg2.ritardo)
