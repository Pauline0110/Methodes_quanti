#################################################################
# Ce script nécessite le lancement des scripts suivants :       #
# QB - importer, organiser et nettoyer la base de données,      #
# QB - Ajouts de variables et recodages généraux et             #
# Recodage de nos variables                                     #
# Variable de transport recodage                                #
#################################################################

#Tri à plat de toutes nos variables ----

# GPS ----
# Recodages effectués : on enlève Toujours pour le mettre dans Souvent -> 1 seule réponse
freq(Base_T$gps)
write.csv2(freq(Base_T$gps),file="Sorties/freq_gps.csv",fileEncoding="UTF-8")

# Odeurs ----
#Recodages effectués : Oui, beacoup et Oui, un peu en Oui -> 9 oui, beaucoup
freq(Base_T$odeurs)
write.csv2(freq(Base_T$odeurs),file="Sorties/freq_odeurs.csv",fileEncoding="UTF-8")

#Bruits ----
#Recodages effectués : Rien à part réordonner
freq(Base_T$bruits)
write.csv2(freq(Base_T$bruits),file="Sorties/freq_bruits.csv",fileEncoding="UTF-8")

#Transports ----
##Transports avec toutes les possibilités ----
#Recodage = enlever les trottinettes et scooter moto seulement avec marche et sinon avec toutes les autres
freq(Base_All$reponses_rec1.x, sort="dec")
write.csv2(freq(Base_All$reponses_rec1.x, sort="dec"),file="Sorties/transports_all.csv",
           fileEncoding="UTF-8")

##Transports plein air ----
#Recodage = distinguer les transports en plein air et en souterrain
freq(Base_All$Transport_pleinair2, sort="dec")
write.csv2(freq(Base_All$Transport_pleinair2, sort="dec"),file="Sorties/transports_pleinair.csv",
           fileEncoding="UTF-8")

##Transports individuels et collectifs
#Recodage = bon c'est assez clair
freq(Base_All$Transport_individuel2, sort="dec") #là on traite avec la moitié de la base du coup
#je vais pas le faire vu que t'as dit que c'était pas le plsu intéressant

##Transports par 5 modalités les plus importantes
freq(Base_All$Transport_classe, sort="dec")
write.csv2(freq(Base_All$Transport_classe, sort="dec"),file="Sorties/transports_classe.csv",
           fileEncoding="UTF-8")

##Transports avec marche comme variable centrale ----
freq(Base_All$Transport_marche, sort="dec")
write.csv2(freq(Base_All$Transport_marche, sort="dec"), file="Sorties/transports_marche.csv",
           fileEncoding="UTF-8")

##Transports avec marche et vélo ----
freq(Base_All$Transport_MarcheVélo, sort="dec")
write.csv2(freq(Base_All$Transport_MarcheVélo, sort="dec"), file="Sorties/marche_vélo.csv",
           fileEncoding="UTF-8")
