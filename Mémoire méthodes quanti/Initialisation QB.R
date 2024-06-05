####     Charger les extensions et la base de données ----

###      Charger les extensions ----

library(tidyverse) 
library(readxl) 
library(questionr) 
library(esquisse) 
library(naniar) 
# Pour l'ACM : 
library(FactoMineR)
library(explor) 

###      Charger les bases de données ----

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



## Renomer ----
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
