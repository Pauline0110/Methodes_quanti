#################################################################
# Ce script nécessite le lancement des scripts suivants :       #
# QB - importer, organiser et nettoyer la base de données et    #
# QB - Ajouts de variables et recodages généraux                #
#################################################################

# Base de variables intéressantes pour notre sujet ---- 

# Base de variables sur le transport (sans les B07Q01)
Base_T <- subset(QB, select = c(id, freq_ballade, mobi_ext, senti_securite, gps, odeurs, bruits)) # T pour transport 
# Base de variables sur position et l'attachement 
Base_A <- subset(QB, select = c(id, sexe,quartier, age, rester, anciennete, profession, activite, entreprise, revenu, diplome, attachement)) # A pour attachement 


# RECODAGE 1 DE B07Q01 : 2 modalités par personnes  ---- 

# On créer une base de données avec les 7 variables de modalités de réponses 
#ATTENTION : Ne pas oublier les `` 
Mode_transport <-  subset(QB, select = c(id, `B07Q01[SQ001]`,`B07Q01[SQ002]`, `B07Q01[SQ003]`,`B07Q01[SQ004]`, `B07Q01[SQ005]`, `B07Q01[SQ006]`, `B07Q01[SQ007]` ))
# Vérification 
View(Mode_transport) 

#On veut rassembler 2 à 2 les gens qui ont les mêmes vecteurs de réponses oui/non

# On crée une variable qui représente l'enchaînement des réponses : 

vecteur <- function(...) { 
  reponses <- c(...)
  
  return(reponses)
}

Mode_transport <- Mode_transport %>%
  mutate(vecteur_reponses = pmap(select(., starts_with("B07Q01")), vecteur))

#Vérification : 
Mode_transport$vecteur_reponses

# Mets en factors pour pouvoir recoder 
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

## Recodage des petits effectifs 
freq(Mode_transport$reponses_rec2) 
# On enlève les catégories avec un effectif inférieur à 5 
# On rassemble les scooters 
# On rassemble les vélos tram bus et les vélos voiture 
# On supprime la trottinette 


Mode_transport$reponses_rec1 <- Mode_transport$reponses_rec %>%
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


# Recodage de Base_T ----
## Recodage modalités ---- 
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

#Vérification :
freq(Base_T$odeurs)
freq(Base_T$gps)
freq(Base_T$senti_securite)
#c'est déjà mieux je pense pas qu'on puisse faire plus


## Recodage réordonner les variables ---- 

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





# Recodage des variables Base_A ----
## Recodage des modalités ----
# Recodage de Base_A$attachement (je met le 13 en 10)
Base_A$attachement_rec <- Base_A$attachement %>%
  as.character() %>%
  fct_recode(
    "10" = "13"
  )

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



# RECODAGE 2 DE B07Q01 : une modalité de transport par personne ----
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

#On crée une grosse base de données avec toutes nos variables recodées---- 
Base_All <- inner_join(Base_A, Base_T, by = "id")
View(Base_All)

# Problème : il y a des individus en double 
Base_All <- Base_All %>%
  distinct(id, .keep_all = TRUE)
View(Base_All)
# Problème réglé 
nrow(Base_All)

# Même problème d'ailleurs pour Mode de transport 
Mode_transport <- Mode_transport %>%
  distinct(id, .keep_all = TRUE)
nrow(Mode_transport)


##################################### RECODAGE POSTÉRIEUR #########################
# Recodage attachement en classe ----

Base_All$attachement_classe <- Base_All$attachement_rec %>%
  fct_recode(
    "[0-4]" = "0",
    "[0-4]" = "1",
    "[8-10]" = "10",
    "[0-4]" = "2",
    "[0-4]" = "3",
    "[0-4]" = "3.5",
    "[0-4]" = "4",
    "[5-7]" = "5",
    "[5-7]" = "5.5",
    "[5-7]" = "6",
    "[5-7]" = "6.5",
    "[5-7]" = "7",
    "[8-10]" = "7.5",
    "[8-10]" = "8",
    "[8-10]" = "8.5",
    "[8-10]" = "9",
    "[8-10]" = "9.5"
  )

# On réordonne les classes d'attachement 
Base_All$attachement_classe <- Base_All$attachement_classe %>%
  fct_relevel(
    "[8-10]", "[5-7]", "[0-4]"
  )

# Mettre la variable mode de transport dans Base_All pour pouvoir faire des graphiques ----
Mode_transport$reponses_rec1
Base_All$reponses_rec1 <- Mode_transport$reponses_rec1

Base_All <- Base_All %>%
  left_join(Mode_transport %>% select(id, reponses_rec1), by = "id")

# Vérification 
table(Mode_transport$reponses_rec1)
table(Base_All$reponses_rec1.x)
#Les NA sont affichés en "NA"
## Recodage de Base_All$reponses_rec1.x
Base_All$reponses_rec1.x <- Base_All$reponses_rec1.x %>%
  fct_recode(
    NULL = "NA"
  )

