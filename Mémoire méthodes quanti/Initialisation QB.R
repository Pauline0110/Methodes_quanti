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

####    Corriger les erreurs de codage ----

###      Éliminer les valeurs aberrantes des variables numériques

##      Variable "anciennete"
QB$anciennete <- QB$anciennete %>%
        as.character() %>%
        fct_recode(
                "0.5" = "-1",
                "11" = "2013"
        ) %>%
        as.character() %>%
        as.numeric()

##      Variable "attachement"
QB$attachement <- QB$attachement %>%
        as.character() %>%
        fct_recode(
                "6.5" = "65"
        ) %>%
        as.character() %>%
        as.numeric()

###     Simplifier la variable "diplome"
QB$diplome <- QB$diplome %>%
        fct_recode(
                NULL = "Autre",
                "Bac" = "Bac général, technologique",
                "Bac" = "Bac pro, CAP",
                "Inférieur au bac" = "Brevet",
                "Inférieur au bac" = "Certificat d'études",
                "Bac+2/+3" = "DEUG/BTS",
                "Bac+5 et supérieur" = "Doctorat",
                "Bac+2/+3" = "Licence",
                "Bac+5 et supérieur" = "Master/Maîtrise/DEA"
        )

#### Ajouter des variables à la base de données ----

### Créer une variable "application" aux modalités online/offline
QB$application <- QB$binome %>%
        fct_recode(
                "Offline" = "B01",
                "Offline" = "B02",
                "Offline" = "B03",
                "Offline" = "B04",
                "Offline" = "B05",
                "Offline" = "B06",
                "Offline" = "B07",
                "Offline" = "B08",
        ) %>%
        fct_explicit_na("Online")

### Créer une variable revenu avec indication du centre
QB$revenu_centres <- QB$revenu %>%
        fct_recode(
                "1625" = "Entre 1250 et 2000 euros",
                "2500" = "Entre 2000 et 3000 euros",
                "3750" = "Entre 3000 et 4500 euros",
                "5250" = "Entre 4500 et 6000 euros",
                "7000" = "Entre 6000 et 8000 euros",
                "625" = "Moins de 1250 euros",
                "10000" = "Plus de 8000 euros"
        ) %>%
        as.character() %>%
        as.numeric()

## Créer une variable "prénoms"
QB$prenoms <- paste(QB$B03Q01, QB$B03Q02)
QB$prenoms <- QB$prenoms %>%
        fct_recode(
                "Oui, mais de quelques-uns seulement" = "NA Oui, mais de quelques-uns seulement",
                "Non, d'aucun" = "Non, d'aucun NA",
                "Non, d'aucun" = "Non, d'aucun Non, d'aucun",
                "Oui, de plusieurs" = "Non, d'aucun Oui, de plusieurs",
                "Oui, mais de quelques-uns seulement" = "Non, d'aucun Oui, mais de quelques-uns seulement",
                "Oui, de plusieurs" = "Oui, de plusieurs Non, d'aucun",
                "Oui, de plusieurs" = "Oui, de plusieurs Oui, de plusieurs",
                "Oui, de plusieurs" = "Oui, de plusieurs Oui, mais de quelques-uns seulement",
                "Oui, la plupart" = "Oui, la plupart Non, d'aucun",
                "Oui, la plupart" = "Oui, la plupart Oui, de plusieurs",
                "Oui, la plupart" = "Oui, la plupart Oui, mais de quelques-uns seulement",
                "Oui, mais de quelques-uns seulement" = "Oui, mais de quelques-uns seulement Non, d'aucun",
                "Oui, de plusieurs" = "Oui, mais de quelques-uns seulement Oui, de plusieurs",
                "Oui, mais de quelques-uns seulement" = "Oui, mais de quelques-uns seulement Oui, mais de quelques-uns seulement"
        )
QB$prenoms <- QB$prenoms %>%
        fct_recode(
                NULL = "NA NA"
        )

#### Renommer les variables

QB <- rename(QB, "soc_agglo" = B04Q01, "soc_habitat" = B04Q02, 
                 "emena_etud" = B06Q01, "emena_retraite" = B06Q02, 
                 "emena_travail" = B06Q03, "emena_famille" = B06Q04, 
                 "famille_quartier" = B05Q01, "lieu_travail" = B05Q03, 
                 "mobi_ext" = B05Q06, "scolarite_quartier" = B01Q03, "senti_securite" = B03Q05, 
                 "enfants" = B01Q05, "parent_eleve" = B01Q07, "odeurs" = B07Q03, 
                 "bruits" = B07Q04, "origines_asie" = B08Q02, "nationalite" = B08Q03, 
                 "amis_quartier" = B05Q02, 
                 "freq_cafes" = B05Q04,  
                 "freq_ballade" = B04Q03, 
                 "invitation_voisins" = B03Q04, 
                 "activite_periscol" = B01Q10, "gps" = B07Q02, 
                 "conseils_quartier" = B02Q02, "trottinettes" = B02Q05, 
                 "nom_maire" = B02Q03, "nouvel_an" = B08Q06)