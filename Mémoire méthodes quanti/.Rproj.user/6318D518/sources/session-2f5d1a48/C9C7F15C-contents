####     Charger les extensions et la base de données ----

###      Charger les extensions

library(tidyverse) 
#Extension essentielle ! Le tidyverse rend les fonctions plus intuitive 
library(readxl) 
#Afin de lire les bases de données sous forme de tableaux Excel
library(questionr) 
#Facilite grandement le recodage grâce à une interface plus intuitive 
library(esquisse) 
#Facilite grandement la création de graphiques sur ggplot par une interface plus intuitive
library(naniar) 
#Facilite la reconnaissance, l'élimination et la substitution des valeurs manquantes
library(FactoMineR) 
#Facilite l'analyse factorielle
library(explor) 
#Facilite l'exploration graphique des analyses factorielles
library(dplyr)

###      Charger les bases de données

QBon <- read_excel("Data/QBon.xlsx")
#Charge la base de données du questionnaire en ligne
QBoff <- read_excel("Data/QBoff.xlsx")
#Charge la base de données du questionnaire hors ligne
QB <- bind_rows(QBon, QBoff)
#Assemble les deux bases de données
View(QB)

####     Préparer la base de données ----

###      Uniformiser toutes les valeurs manquantes

QB <- replace_with_na_all(QB, condition = ~.x %in% common_na_strings)

###      Transformer les variables qualitatives en facteurs

QB <- modify_if(QB, is.character, as.factor)
#Les variables "catégorielles" (type "factor") sont plus faciles à traiter dans R

###     Regrouper les modalités des variables de la question à choix multiple (question
###     sur les moyens de transport)

##       Recodage de QB$`B07Q01[SQ001]` en op1
QB$op1 <- QB$`B07Q01[SQ001]` %>%
        fct_recode(
                NULL = "Non",
                "marche" = "Oui"
        )

##      Recodage de QB$`B07Q01[SQ002]` en op2
QB$op2 <- QB$`B07Q01[SQ002]` %>%
        fct_recode(
                NULL = "Non",
                "vélo" = "Oui"
        )

##      Recodage de QB$`B07Q01[SQ003]` en op3
QB$op3 <- QB$`B07Q01[SQ003]` %>%
        fct_recode(
                NULL = "Non",
                "trotinette" = "Oui"
        )

##      Recodage de QB$`B07Q01[SQ004]` en op4
QB$op4 <- QB$`B07Q01[SQ004]` %>%
        fct_recode(
                NULL = "Non",
                "scooter ou moto" = "Oui"
        )

##      Recodage de QB$`B07Q01[SQ005]` en op5
QB$op5 <- QB$`B07Q01[SQ005]` %>%
        fct_recode(
                NULL = "Non",
                "voiture" = "Oui"
        )

##      Recodage de QB$`B07Q01[SQ006]` en op6
QB$op6 <- QB$`B07Q01[SQ006]` %>%
        fct_recode(
                NULL = "Non",
                "métro, RER" = "Oui"
        )

##      Recodage de QB$`B07Q01[SQ001]` en op7
QB$op7 <- QB$`B07Q01[SQ007]` %>%
        fct_recode(
                NULL = "Non",
                "tram, bus" = "Oui"
        )

##      Regroupement des modalités dans une nouvelle variable
QB <- unite(QB, col = "B07Q01", starts_with("op"), remove = T, na.rm = T, 
            sep = ", ")

###     Supprimer les colonnes en trop
QB <- select(QB, -ends_with("Time"), -starts_with("groupTime"))