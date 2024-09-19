getwd()

#ctrl shift H

dpe_v2_logement_existant = read.csv(file = "dpe-v2-logements-existants.csv", header = TRUE, sep = ",", dec = ".")
dpe_v2_logement_neuf = read.csv(file = "dpe-v2-logements-neufs.csv", header = TRUE, sep = ",", dec = ".")

dim(dpe_v2_logement_existant)
dim(dpe_v2_logement_neuf)



dpe_v2_logement_existant$Logement="ancien"
dpe_v2_logement_neuf$Logement="neuf"
dpe_v2_logement_neuf$Année_construction="2024"





liste_commune=intersect(names(dpe_v2_logement_neuf),names(dpe_v2_logement_existant))

dpe_v2_logement_existant=dpe_v2_logement_existant[,liste_commune]
dpe_v2_logement_neuf=dpe_v2_logement_neuf[,liste_commune]


dfconcat=rbind(dpe_v2_logement_existant,dpe_v2_logement_neuf)

#PRENDRE UNE ANNEE DANS UNE NEW COL :
dfconcat$Date_de_réception_du_DPE=format(dfconcat$Date_réception_DPE,"%Y")

str(dfconcat$Date_réception_DPE)
dfconcat$Date_réception_DPE=as.Date(dfconcat$Date_réception_DPE)
 

dfconcat$Verif= dfconcat$Coût_chauffage  + dfconcat$Coût_éclairage  + dfconcat$Coût_ECS + dfconcat$Coût_refroidissement + dfconcat$Coût_auxiliaires
dfconcat$Verif= dfconcat$Coût_total_5_usages==(dfconcat$Coût_chauffage  + dfconcat$Coût_éclairage  + dfconcat$Coût_ECS + dfconcat$Coût_refroidissement + dfconcat$Coût_auxiliaires) 


dfconcat$Année_construction=as.numeric(dfconcat$Année_construction)

cut(dfconcat$Année_construction, breaks = c(0,1960,1970,1980,1990,2000,2010,4000),
    labels = c("Avant 1960","1961 - 1970","1971 - 1980","1981 - 1990","1991 - 2000","2001 - 2010","Après 2010"))



repartition_etiquettedpe = table(dfconcat$Etiquette_DPE)
print(repartition_etiquettedpe)

repartion_datereceptiondpe=table(dfconcat$Date_de_réception_du_DPE)
print(dfconcat$Date_de_réception_du_DPE)


repartitiontype=table(dfconcat$Logement)
print(repartitiontype)

repartitionlogementtype=table(dfconcat$Type_bâtiment)
print(repartitionlogementtype)


