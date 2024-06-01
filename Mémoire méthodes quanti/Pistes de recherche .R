# RECODAGE HABITUEL ---- 

# Installation des extensions 
library(questionr)
library(readxl)
library(tidyverse)
library(esquisse)
library(FactoMineR)
library(explor)
library(dplyr)

############################################################################################
# Je vais créer un lien Github pour que t'aies le code en direct quand je le modifie       #
# qu'on n'ait pas à s'envoyer 15 mails                                                     #
# Je vais voir si je peux t'ajouter en tant que collaboratrice du Github comme ça tu       #
# pourras me demander d'appliquer les changements du code quand tu le modifieras et comme  #
# ça on aura toujours le même code pour pas se perdre                                      #
############################################################################################

###############################################################################
# LANCER LES SCRIPTS INITIALISATION COMME CA NOS VARIABLES AURONT LE MÊME NOM #
###############################################################################

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

# Idée : essayer d'avoir une base où on aurait les effectifs pour une réponse -> je tenterai ça demain

#La Base_T ----

table(Base_T$odeurs, useNA="ifany") #très peu de beaucoup -> regrouper un peu et beaucoup ???
table(Base_T$bruits, useNA="ifany") #variable m'a l'air ok, à réordonner
table(Base_T$gps, useNA="ifany") # 1 seul toujours -> regrouper avec souvent ?
table(Base_T$freq_ballade, useNA="ifany") #la variable est bonne, à réordonner
table(Base_T$mobi_ext, useNA="ifany") #ok, à réordonner
table(Base_T$senti_securite, useNA="ifany") # très peu de pas du tout et de pas vraiment par rapport au reste -> les regrouper ?

##recodage des variables qui vont pas (sujet à changement) ----
## Recodage de Base_T$odeurs
Base_T$odeurs <- Base_T$odeurs %>%
  fct_recode(
    "Oui" = "Oui, beaucoup",
    "Oui" = "Oui, un peu"
  )

## Recodage de Base_T$gps
Base_T$gps <- Base_T$gps %>%
  fct_recode(
    "Souvent" = "Toujours"
  )

## Recodage de Base_T$senti_securite
Base_T$senti_securite <- Base_T$senti_securite %>%
  fct_recode(
    "Pas vraiment" = "Pas du tout"
  )

#mtn :
freq(Base_T$odeurs)
freq(Base_T$gps)
freq(Base_T$senti_securite)
#c'est déjà mieux je pense pas qu'on puisse faire plus

##réordonner les variables ----

## Réordonnancement de Base_T$senti_securite
Base_T$senti_securite <- Base_T$senti_securite %>%
  fct_relevel(
    "Pas vraiment", "La plupart du temps", "Tout le temps"
  )

## Réordonnancement de Base_T$bruits
Base_T$bruits <- Base_T$bruits %>%
  fct_relevel(
    "Non, pas spécialement", "Oui, un peu", "Oui, beaucoup"
  )

## Réordonnancement de Base_T$freq_ballade
Base_T$freq_ballade <- Base_T$freq_ballade %>%
  fct_relevel(
    "Jamais ou rarement", "Au moins une fois par mois", "Au moins une fois toutes les semaines",
    "Tous les jours"
  )

## Réordonnancement de Base_T$mobi_ext
Base_T$mobi_ext <- Base_T$mobi_ext %>%
  fct_relevel(
    "Non, rarement ou jamais", "Oui, quelques fois par mois", "Oui, une ou plusieurs fois par semaine",
    "Oui, une ou plusieurs fois par jour"
  )

#ok pour la Base_T

#la Base_A ----

freq(Base_A$attachement) #il reste un 13 ???? -> en NA ou en 10 ?
freq(Base_A$anciennete) #m'a l'air ok, bon après il y a vraiment des gens qui restent 70 ans au même endroit ??, recoder ?
freq(Base_A$revenu) #14% de NA mais sinon ok, juste REORDONNER
freq(Base_A$diplome) #ok, REORDONNER
freq(Base_A$rester) #ok
freq(Base_A$age) #recoder en quintiles je pense
freq(Base_A$sexe) #ok, plus de femmes mais de 4,1% donc ça va
freq(Base_A$activite) #peu de chômeurs mais sinon la variable est bien utilisable
#pour profession on attendra le script des PCS en entier

## Recodage des variables ----

#pour attachement je sais pas trop quoi faire
## Recodage de Base_A$attachement


#Recodage de l'âge en quintiles
Base_A$age_quint <- quant.cut(Base_A$age, 5)
freq(Base_A$age_quint) #le découpage est pas si mal et les catégories m'ont pas l'air si bizarres
## Recodage de Base_A$age_quint
Base_A$age_quint <- Base_A$age_quint %>%
  fct_recode(
    "14-20 ans" = "[14,20.4)",
    "21-32 ans" = "[20.4,32)",
    "33-46 ans" = "[32,46)",
    "47-62 ans" = "[46,62.6)",
    "63-87 ans" = "[62.6,87]"
  )
freq(Base_A$age_quint)

#recodage de l'ancienneté ? (essai)
Base_A$anciennete_rec <- quant.cut(Base_A$anciennete, 5)
freq(Base_A$anciennete_rec) #les catégories m'ont l'air bien
## Recodage de Base_A$anciennete_rec
Base_A$anciennete_rec <- Base_A$anciennete_rec %>%
  fct_recode(
    "0 à 2,5 ans" = "[0,2.5)",
    "2,5 à 6 ans" = "[2.5,6)",
    "6 à 12 ans" = "[6,12)",
    "13 à 23 ans" = "[12,23.6)",
    "24 à 70 ans" = "[23.6,70]"
  )
freq(Base_A$anciennete_rec)

##Réordonnancement des variables ----

## Réordonnancement de Base_A$revenu
Base_A$revenu <- Base_A$revenu %>%
  fct_relevel(
    "Moins de 1250 euros", "Entre 1250 et 2000 euros", "Entre 2000 et 3000 euros",
    "Entre 3000 et 4500 euros", "Entre 4500 et 6000 euros", "Entre 6000 et 8000 euros",
    "Plus de 8000 euros"
  )

## Réordonnancement de Base_A$diplome
Base_A$diplome <- Base_A$diplome %>%
  fct_relevel(
    "Sans diplôme", "Inférieur au bac", "Bac", "Bac+2/+3", "Bac+5 et supérieur"
  )

freq(Base_A$anciennete_rec)
freq(Base_A$revenu)
freq(Base_A$diplome)
freq(Base_A$age_quint)

#Base_A ok

#Idée -> séparer les scripts de recodage et d'analyse comme ça on se perd pas après comme tu le sens

# Créer des modalités pour ceux qui ont répondu au moins une fois un mode de transport ----

Marche <- QB[QB$`B07Q01[SQ001]`=="Oui", ]
Metro_RER <- QB[QB$`B07Q01[SQ006]`=="Oui", ]
Voiture <- QB[QB$`B07Q01[SQ005]`=="Oui", ]
Vélo <- QB[QB$`B07Q01[SQ002]`=="Oui", ]
Tram_bus <- QB[QB$`B07Q01[SQ007]`=="Oui", ]
Trottinette <- QB[QB$`B07Q01[SQ003]`=="Oui", ]
Scooter_moto <- QB[QB$`B07Q01[SQ004]`=="Oui", ]

rm(Trottinette)
rm(Scooter_moto)
#pour notre pbq on pourrait se concentrer que sur Marche, Metro/RER, Voiture, Vélo, Tram/bus

#Mettre ensemble dans une seule base toutes les variables (pour tester des trucs avec esquisse) ----
Base_All <- subset(QB, select= c(freq_ballade, mobi_ext, senti_securite, gps, odeurs, bruits, 
                                 sexe,quartier, age, rester, anciennete, profession, activite,
                                 entreprise, revenu, diplome, attachement))

## Recodage de Base_All$odeurs
Base_All$odeurs <- Base_All$odeurs %>%
  fct_recode(
    "Oui" = "Oui, beaucoup",
    "Oui" = "Oui, un peu"
  )

## Recodage de Base_All$gps
Base_All$gps <- Base_All$gps %>%
  fct_recode(
    "Souvent" = "Toujours"
  )

## Recodage de Base_All$senti_securite
Base_All$senti_securite <- Base_All$senti_securite %>%
  fct_recode(
    "Pas vraiment" = "Pas du tout"
  )

#mtn :
freq(Base_All$odeurs)
freq(Base_All$gps)
freq(Base_All$senti_securite)
#c'est déjà mieux je pense pas qu'on puisse faire plus

##réordonner les variables ----

## Réordonnancement de Base_All$senti_securite
Base_All$senti_securite <- Base_All$senti_securite %>%
  fct_relevel(
    "Pas vraiment", "La plupart du temps", "Tout le temps"
  )

## Réordonnancement de Base_All$bruits
Base_All$bruits <- Base_All$bruits %>%
  fct_relevel(
    "Non, pas spécialement", "Oui, un peu", "Oui, beaucoup"
  )

## Réordonnancement de Base_All$freq_ballade
Base_All$freq_ballade <- Base_All$freq_ballade %>%
  fct_relevel(
    "Jamais ou rarement", "Au moins une fois par mois", "Au moins une fois toutes les semaines",
    "Tous les jours"
  )

## Réordonnancement de Base_All$mobi_ext
Base_All$mobi_ext <- Base_All$mobi_ext %>%
  fct_relevel(
    "Non, rarement ou jamais", "Oui, quelques fois par mois", "Oui, une ou plusieurs fois par semaine",
    "Oui, une ou plusieurs fois par jour"
  )

#Recodage de l'âge en quintiles
Base_All$age_quint <- quant.cut(Base_All$age, 5)
freq(Base_All$age_quint) #le découpage est pas si mal et les catégories m'ont pas l'air si bizarres
## Recodage de Base_All$age_quint
Base_All$age_quint <- Base_All$age_quint %>%
  fct_recode(
    "14-20 ans" = "[14,20.4)",
    "21-32 ans" = "[20.4,32)",
    "33-46 ans" = "[32,46)",
    "47-62 ans" = "[46,62.6)",
    "63-87 ans" = "[62.6,87]"
  )
freq(Base_All$age_quint)

#recodage de l'ancienneté ? (essai)
Base_All$anciennete_rec <- quant.cut(Base_All$anciennete, 5)
freq(Base_All$anciennete_rec) #les catégories m'ont l'air bien
## Recodage de Base_All$anciennete_rec
Base_All$anciennete_rec <- Base_All$anciennete_rec %>%
  fct_recode(
    "0 à 2,5 ans" = "[0,2.5)",
    "2,5 à 6 ans" = "[2.5,6)",
    "6 à 12 ans" = "[6,12)",
    "13 à 23 ans" = "[12,23.6)",
    "24 à 70 ans" = "[23.6,70]"
  )
freq(Base_All$anciennete_rec)

##Réordonnancement des variables ----

## Réordonnancement de Base_All$revenu
Base_All$revenu <- Base_All$revenu %>%
  fct_relevel(
    "Moins de 1250 euros", "Entre 1250 et 2000 euros", "Entre 2000 et 3000 euros",
    "Entre 3000 et 4500 euros", "Entre 4500 et 6000 euros", "Entre 6000 et 8000 euros",
    "Plus de 8000 euros"
  )

## Réordonnancement de Base_All$diplome
Base_All$diplome <- Base_All$diplome %>%
  fct_relevel(
    "Sans diplôme", "Inférieur au bac", "Bac", "Bac+2/+3", "Bac+5 et supérieur"
  )

#on rajoute notre grosse variable à la base avec tout
Base_All$transport <- Mode_transport$reponses_rec3
