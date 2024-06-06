####################################################################
# Ce script nécessite le lancement des scripts suivants :          #
# 01_QB - importer, organiser et nettoyer la base de données,      #
# 02_QB - Ajouts de variables et recodages généraux et             #
# 03_Recodage de nos variables                                     #
####################################################################

# GPS - Rester  ----
# On enlève les NA 
Base_All_NA <- Base_All %>%
  drop_na(gps, rester)

# Graphique 
ggplot(Base_All_NA) +
  aes(x = gps, fill = rester) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()


# GPS - Attachement ----
# On enlève les NA 
Base_All_NA <- Base_All %>%
  drop_na(attachement_classe, gps)

# Graphique 
ggplot(Base_All_NA) +
  aes(x = gps, fill = Base_All_NA$attachement_classe) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()


# Odeur - Sécurite ----

# On enlève les NA 
Base_All_NA <- Base_All %>%
  drop_na(senti_securite, odeurs)

# Graphique 
ggplot(Base_All_NA) +
  aes(x = senti_securite, fill = odeurs) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

chisq.test(table(Base_All$odeurs, Base_All$senti_securite)) #bien

# Le prof l'aime bien je crois, après y'a vraiment 0 intérêt pour notre sujet 


# Odeur - attachement ----
Base_All_NA <- Base_All %>%
  drop_na(attachement_classe, odeurs)

# Graphique 
ggplot(Base_All_NA) +
  aes(x = attachement_classe, fill = odeurs) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

chisq.test(table(Base_All$odeurs, Base_All$attachement_classe)) # oui 

#tests de qqes trucs
#plein air et age
Base_All %>%
  filter(!is.na(age_quint)) %>%
  filter(!is.na(Transport_pleinair2)) %>%
  ggplot() +
  aes(x = age_quint, fill = Transport_pleinair2) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
chisq.test(Base_All$age_quint, Base_All$Transport_pleinair2) #bonne p value (2.3e-6)

#plein air et odeurs
Base_All %>%
  filter(!is.na(odeurs)) %>%
  filter(!is.na(Transport_pleinair2)) %>%
  ggplot() +
  aes(x = odeurs, fill = Transport_pleinair2) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
chisq.test(Base_All$age_quint, Base_All$Transport_pleinair2) #bonne p value (2.3e-6)

#plein air et bruit (à mon avis c'est pas ouf)
Base_All %>%
  filter(!is.na(bruits)) %>%
  filter(!is.na(Transport_pleinair2)) %>%
  ggplot() +
  aes(x = bruits, fill = Transport_pleinair2) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
chisq.test(Base_All$bruits, Base_All$Transport_pleinair2) #nulle 72%

#in any case on peut faire des tableaux pour éviter de remplir le doc de graphiques
esquisser(viewer="browser")

#bruits et activité
Base_All %>%
  filter(!is.na(activite)) %>%
  filter(!is.na(bruits)) %>%
  ggplot() +
  aes(x = activite, fill = bruits) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
chisq.test(table(Base_All$bruits, Base_All$activite)) #0,15 p value donc pas terrible mais le prof a dit que c'était possible d'en parler
cramer.v(table(Base_All$bruits, Base_All$activite))

#odeurs et pcs
Base_All %>%
  filter(!is.na(activite)) %>%
  filter(!is.na(pcs1_int)) %>%
  filter(!is.na(bruits)) %>%
  ggplot() +
  aes(x = pcs1_int, fill = odeurs) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
chisq.test(table(Base_All$pcs1_int, Base_All$odeurs))
cramer.v(table(Base_All$pcs1_int, Base_All$odeurs))

#bruits et mobilité extérieure, je pense que ça peut se voir dans l'acm
Base_All %>%
  filter(!is.na(activite)) %>%
  filter(!is.na(pcs1_int)) %>%
  filter(!is.na(bruits)) %>%
  ggplot() +
  aes(x = mobi_ext, fill = bruits) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

chisq.test(table(Base_All$bruits, Base_All$mobi_ext))
cramer.v(table(Base_All$bruits, Base_All$mobi_ext))

#gps et fréquence ballades
Base_All %>%
  filter(!is.na(revenu)) %>%
  filter(!is.na(odeurs)) %>%
  ggplot() +
  aes(x = freq_ballade, fill = gps) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

chisq.test(table(Base_All$gps, Base_All$freq_ballade))
cramer.v(table(Base_All$gps, Base_All$freq_ballade))


