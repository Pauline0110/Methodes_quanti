#################################################################
# Ce script nécessite le lancement des scripts suivants :       #
# QB - importer, organiser et nettoyer la base de données,      #
# QB - Ajouts de variables et recodages généraux et             #
# Recodage de nos variables                                     #
#################################################################

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



