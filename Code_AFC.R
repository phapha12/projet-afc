##PARTIE 1: Chargement et manipulation des donnees

##Chargement de la library pour traiter les caract?res
library(stringr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(corrplot)

## repertoire de travail contenant les fichiers
setwd("/Volumes/apple/projetafc")

##Chargement des donn?es de cause de d?c?s par d?partement
donnees<-read.csv(file = "jeu.csv", header = TRUE,sep=" ")
##Nous renommons les colonnes de num?ro de d?partement
names(donnees)[1]<-"num"

##Chargement des r?gions
##https://www.insee.fr/fr/statistiques/2128766?sommaire=2127739

regions<-read.csv(file = "regions.csv", header = TRUE,sep=";")
##Nous renommons les colonnes de num?ro de d?partement
names(regions)[2]<-"num"

##Afin de faciliter la fusion, nous ajoutons un 0 au d?partement dont le code est de longueur 1
#regions$num<-ifelse(str_length(regions$num)==2,substr(regions$num,1,2),paste("0",substr(regions$num,1,1),sep=""))
#Fusion des donn?es 
Donnees<-merge(donnees,regions,by="num")
##Suppression de la colonne d?partement dupliqu?e
Donnees<-Donnees[,-11]
Donnees<-Donnees[,c(10,1:9)]
names(Donnees)[1]<-"REGION"

##Suppression des jeux de donn?es dont nous n'aurons plus besoin dans la suite pour liberer la memoire 
remove(donnees,regions)

##Regroupement par r?gions
Donnees_regions<-Donnees %>% group_by(REGION)%>%
  summarise(
    Nombre_Tumeurs= sum(Tumeurs),
    Nombre_cardio=sum(Maladies_cardio.vasculaires),
    Nombre_infectieuses=sum(Maladies_infectueuses),
    Nombre_autres=sum(Autres_maladies),
    Nombre_morts_violentes=sum(Morts_violentes),
    Nombre_mal_defini=sum(Causes_mal_définies_ou_non_déclarées)
    )
nom_ligne<-Donnees_regions$REGION
Donnees_regions<-Donnees_regions[,-1]
rownames(Donnees_regions)<-nom_ligne
Donnees_regions<-Donnees_regions
##   PARTIE 2: description du sujet 
row.names(Donnees_regions)[which.max(Donnees_regions$Nombre_Tumeurs)]
## retour la regions ou il ya plus de mortalites pour les tumeurs 
# et ici on constate que la region rhone alpes a plus de nombres de mort pour les tumeurs 
#moyenne par type de cause 
A=colMeans(Donnees_regions)
# apres executions on constate que la moyennes  de  nombres de mortalites est plus elevees pour tumeurs 
# les regions qui le nombres de morts causees par tumeurs superieurs a la moyennes
row.names(Donnees_regions)[which(Donnees_regions$Nombre_Tumeurs>A[1])]
# apres executions on constate que les regions comme Aquitaine,Île-de-France,Lorraine, Pays de la Loire,Rhône-Alpes,Centre,Languedoc-Roussillon,Midi-Pyrénées et Provence-Alpes-Côte d'Azur sont superieur a la moyenne 
## frequence en lignes ##
prop.table(Donnees_regions)



##PARTIE 2:  AFC
chisq.test(Donnees_regions)
##degr? de libert?
df<-(nrow(Donnees_regions)-1)*(ncol(Donnees_regions)-1)
##Dans notre cas, le p_value=2.2e-16<5% donc on rejette l'hypoth?se d'ind?pendance
##Il y un lien entre les colonnes et les lignes de notre jeu de donn?es

##R?alisation de l'AFC
Donnees_regions_afc<-CA(Donnees_regions)
summary(Donnees_regions_afc)
##pour avoir les valeurs propres ainsi que la proportion
## de variances expliqu?es
get_eigenvalue(Donnees_regions_afc)
##A partir du pourcentage cumul? expliqu?, on se rend compte que 
#78% de la variance totale est expliqu?e par les 2 premi?res dimensions

fviz_eig(Donnees_regions_afc)
##dans le cas o? nos donn?es serait al?atoires, la valeur attendue de la valeur propre
# de chaque axe serait:
val_prop_alea<-1/(nrow(Donnees_regions)-1)

##L'axe moyen 
axe_moyen<-1/(ncol(Donnees_regions)-1)

fviz_screeplot (Donnees_regions_afc) +
  geom_hline (yintercept = 20, linetype = 2, color = "red")

fviz_ca_biplot(Donnees_regions_afc)
#les lignes sont repr?sent?es par les points bleus et les colonnes par les triancgles rouges
##Dans notre cas par exemple, les r?gions bretagne et midi pyrene sont associ?es ? Nombre_cardio
##Alpes cote d azur est associ?e ? Nombre de maladie de tumeurs 
##coordonn?es des r?gions dans les dimensions
head(row$coord)
#Graphique des points lignes
row<-get_ca_row(Donnees_regions_afc)
fviz_ca_row(Donnees_regions_afc, repel = TRUE)
##Les r?gions Bretagne, Aquitaine et Midi-Pyr?n?es sont regroup?es
## Elles sont n?gativement corr?l?es avec Provzence-Alpes-Cote d'Azur
## Les r?gions Corse , Ile de France et nord pas de calais  sont bien repr?sent?es

##Les contributions aux dimensions
row$contrib
corrplot(row$contrib,is.corr = FALSE)
fviz_contrib(Donnees_regions_afc,choice = "row",axes = 1, top =22)
fviz_contrib(Donnees_regions_afc,choice = "row",axes = 2, top =22)
##Les r?gions Ile de France et nord pas de calais explique plus la variabilite

# qualitees de representation
row$cos2
corrplot(row$cos2, is.corr = FALSE)
fviz_cos2 (Donnees_regions_afc, choice = "row", axes = 1:2)
#dans notre afc on a choisir 2 dimensions et  ile de france ,nord-pas de calais ,Languedoc-Roussillon , aquitaine ,loraine, basse normandie , franche comite , corse et limousin sont bien represente  parce que la somme de leurs cos^2 sont proche de 1   

##pour le profil colone 
##coordonn?es des maladies dans les regions
head(colonne$coord)
colonne<-get_ca_col(Donnees_regions_afc)
#Graphique des points colonnes 
fviz_ca_col(Donnees_regions_afc, repel = TRUE)
#ici on constate que nombre morts violents et maladidie mal definies sont negativement corelees par raport a la premiers dimensions 
# nombres de maladies infectieuses et nombre_morts de mal definies sont  bien representes 

##Les contributions aux dimensions
colonne$contrib
corrplot(colonne$contrib,is.corr = FALSE)
fviz_contrib (Donnees_regions_afc, choice = "col", axes = 1:2)
# dans notre cas on constate que mal definie , nombres mort violent et  nombres infectieuse  expliques plus la variabilitee

# qualitees de representation
colonne$cos2
fviz_cos2 (Donnees_regions_afc, choice = "col", axes = 1:2)
# ici on constate que  nombre mal defini, nombre de morts violentes ,nombre cardio  et nombres de maladies infectieuses sont bien representees dans les 2 dimensions 




