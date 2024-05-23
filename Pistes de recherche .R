# RECODAGE HABITUEL ---- 

# Installation des extensions 
library(questionr)
library(readxl)
library(tidyverse)
library(esquisse)
library(FactoMineR)
library(explor)
library(dplyr)

# On change le répertoire de travail : On va chercher le chemin dans le "plus" de fichier 
setwd("~/Desktop/CPES 2 /Méthodes quantitatives /SEMESTRE 2 /R - Méthode quanti 13ème")

# Importe la base de données 
QBon <- read_excel("QBon.xlsx")
QBoff <- read_excel("QBoff.xlsx")

setwd("~/Desktop/CPES 2 /R ")

#Réunir les deux bases de données 
QB <- bind_rows(QBon,QBoff)

# On uniformise les NA 
QB <- replace_with_na_all(QB, condition = ~.x%in% common_na_strings)

# Pour toutes les mettre en variable catégorielle 
QB <- modify_if(QB, is.character, as.factor)

# On veut supprimer les colonnes de time 
QB <- select(QB, -ends_with("Time"))

#LANCER LE SCRIPT INITIALISATION COMME CA ON A LE MËME NOM DE VARIABLES

# Créer une base de données avec que les questions qui nous intéresse ----

## Base de variables intéressantes pour notre sujet ---- 
Base_T <- subset(QB, select = c(freq_ballade, mobi_ext, senti_securite, gps, odeurs, bruits)) # T pour transport 
# B07Q02 : GPS 
# B07Q03 : odeurs 
# B07Q04 : bruits  

# Problème : notre question B07Q01 (Mode de transport est réunit en 7 variables (oui/ non pour chaque modalité ) => gros travail sur cette variable à faire 
# et c'est notre variable la plus importante. 

## Base de variables intéressantes pour tout le monde sur l'attachement ou variables de positions ----
Base_A <- subset(QB, select = c(sexe,quartier, age, rester, anciennete, profession, activite, entreprise, revenu, diplome, attachement)) # A pour attachement 


# GÉRER B07Q01 ---- 
# On créer une base de données avec les 7 variables de modalités de réponses 
# Problème si je mets : Mode_transport <-  subset(QB, select = c(B07Q01[SQ001],B07Q01[SQ002]))
# Problème n°2 : dans la base de données excel son nom est B07Q01[SQ001] mais dans R il ne reconnaît pas cette colonne => solution : mettre des ``
Mode_transport <-  subset(QB, select = c(`B07Q01[SQ001]`,`B07Q01[SQ002]`, `B07Q01[SQ003]`,`B07Q01[SQ004]`, `B07Q01[SQ005]`, `B07Q01[SQ006]`, `B07Q01[SQ007]` ))
Mode_transport

# On regarde et Mode_transport ce sont des colonnes de oui et non. On veut rassembler 2 à 2 les gens qui ont les mêmes vecteurs de réponses oui/non

# On crée une variable qui représente l'enchaînement des réponses : 

vecteur <- function(...) { 
  reponses <- c(...)
  
  return(reponses)
}

Mode_transport <- Mode_transport %>%
  mutate(vecteur_reponses = pmap(select(., starts_with("B07Q01")), vecteur))

#Vérification : 
Mode_transport$vecteur_reponses
# Trop bien ! 

# Je veux que les vecteurs soit des factors pour pouvoir les recoder 
Mode_transport$reponses_rec <- sapply(Mode_transport$vecteur_reponses, paste, collapse = "")



#RECODAGE 
Mode_transport$reponses_rec2 <- Mode_transport$reponses_rec %>%
  fct_recode(
    "NA" = "NANANANANANANA",
    "tram, bus" = "NonNonNonNonNonNonOui",
    "métro, RER" = "NonNonNonNonNonOuiNon",
    "métro, RER / tram, bus" = "NonNonNonNonNonOuiOui",
    "voiture" = "NonNonNonNonOuiNonNon",
    "voiture/ métro, RER" = "NonNonNonNonOuiOuiNon",
    "scooter, moto" = "NonNonNonOuiNonNonNon",
    "scooter, moto / métro, RER" = "NonNonNonOuiNonOuiNon",
    "scooter, moto / voiture" = "NonNonNonOuiOuiNonNon",
    "trottinette" = "NonNonOuiNonNonNonNon",
    "vélo" = "NonOuiNonNonNonNonNon",
    "vélo / tram, bus" = "NonOuiNonNonNonNonOui",
    "vélo / métro, RER" = "NonOuiNonNonNonOuiNon",
    "vélo / voiture" = "NonOuiNonNonOuiNonNon",
    "marche" = "OuiNonNonNonNonNonNon",
    "marche / tram, bus" = "OuiNonNonNonNonNonOui",
    "marche / métro, RER" = "OuiNonNonNonNonOuiNon",
    "marche / voiture" = "OuiNonNonNonOuiNonNon",
    "marche / scooter, moto" = "OuiNonNonOuiNonNonNon",
    "marche / vélo" = "OuiOuiNonNonNonNonNon"
  )


freq(Mode_transport$reponses_rec2) 
# On a certaines modalités qui sont très très peu représentées en effectifs (inférieur à 5) , on va les enlever car on ne pourra pas en tirer de conclusion statistiques 
# En particulier les scooters ont de tout petits effectifs donc on les rassemble 
# Et les vélos / tram bus et vélo / voiture aussi sont en faible effectif donc on les rassemble également 
# Et trottinette on a qu'un effectif on le met en NA 


Mode_transport$reponses_rec3 <- Mode_transport$reponses_rec %>%
  fct_recode(
    "NA" = "NANANANANANANA",
    "tram, bus" = "NonNonNonNonNonNonOui",
    "métro, RER" = "NonNonNonNonNonOuiNon",
    "métro, RER / tram, bus" = "NonNonNonNonNonOuiOui",
    "voiture" = "NonNonNonNonOuiNonNon",
    "voiture/ métro, RER" = "NonNonNonNonOuiOuiNon",
    "scooter, moto (+ autre)" = "NonNonNonOuiNonNonNon",
    "scooter, moto (+ autre)" = "NonNonNonOuiNonOuiNon",
    "scooter, moto (+ autre)" = "NonNonNonOuiOuiNonNon",
    "NA" = "NonNonOuiNonNonNonNon",
    "vélo" = "NonOuiNonNonNonNonNon",
    "vélo / tram, bus ou voiture" = "NonOuiNonNonNonNonOui",
    "vélo / métro, RER" = "NonOuiNonNonNonOuiNon",
    "vélo / voiture" = "NonOuiNonNonOuiNonNon",
    "marche" = "OuiNonNonNonNonNonNon",
    "marche / tram, bus ou voiture" = "OuiNonNonNonNonNonOui",
    "marche / métro, RER" = "OuiNonNonNonNonOuiNon",
    "marche / voiture" = "OuiNonNonNonOuiNonNon",
    "marche / scooter, moto" = "OuiNonNonOuiNonNonNon",
    "marche / vélo" = "OuiOuiNonNonNonNonNon"
  )

# essayer d'avoir une base où il y aurait qu'une réponse genre juste la marche


# les autres variables :

