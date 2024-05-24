# Analyse de nos données
#Pour l'instant je fais que des graphiques sans les mettre au propre juste pour voir un peu ce qu'on a de pas mal à dire

# GPS ----
# utilisation du gps et attachement
ggplot(Base_All) +
  aes(x = "", y = attachement, fill = gps) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# gps et ancienneté
ggplot(Base_All) +
  aes(x = gps, fill = anciennete_rec) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# gps et rester
ggplot(Base_All) +
  aes(x = gps, fill = rester) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#gps et âge
Base_All %>%
  filter(!is.na(bruits)) %>%
  filter(!is.na(age)) %>%
  ggplot() +
  aes(x = age_quint, fill = gps) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# Bruits ----

#bruits et attachement
ggplot(Base_All) +
  aes(x = "", y = attachement, fill = bruits) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#bruits et âge
Base_All %>%
  filter(!is.na(bruits)) %>%
  filter(!is.na(age)) %>%
  ggplot() +
  aes(x = age_quint, fill = bruits) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#bruits et activité
Base_All %>%
  filter(!is.na(bruits)) %>%
  ggplot() +
  aes(x = activite, fill = bruits) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#Odeurs ----

#odeurs et sécurité
ggplot(Base_All) +
  aes(x = senti_securite, fill = odeurs) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#odeurs et ancienneté
ggplot(Base_All) +
  aes(x = anciennete_rec, fill = odeurs) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#Transports ----

#transport et attachement (attention gros bordel)
ggplot(Base_All) +
  aes(x = "", y = attachement, fill = transport) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#j'ai testé avec d'autres variables c'est un bordel absolu
#CONCLUSION notre variable transport a trop de modalités => c'est impossible de faire des graphiques bien lisibles
#SOIT : on fait des tableaux, ou alors on recode pour avoir que nos 7 modalités de base et pas toutes les combinaisons
#dans le 2e cas, il faut chercher voir comment on fait, j'ai réussi à faire des bases de données avec que les personnes qui ont mis tel ou tel transport
#mais après pour comparer il faut produire 7 fois le graphique par exemple ça me parait un peu abusé