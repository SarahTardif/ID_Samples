## script pour prédire la classe de nouvelles données
## pour toutes questions, contacter Gauthier Lapa, gauthier.lapa@gmail.com
## version du 2023-04-03


library(dplyr)
#install.packages("randomForest")
library(randomForest)
## charger les paquets nécessaires pour le  modèle choisi

## charger le modèle retenu
model<-readRDS("./modelRF_species_balanced_essentials_20250110.rds")

## charger les  données à classifier
setwd("./Ech_21-40")
PrimaryDirectory<-getwd() #verifier d'etre dans le bon repertoire avant
## Récupère les noms des fichiers à identifier dans le répertoire de travail
FileNames <- list.files(path=PrimaryDirectory, pattern = ".csv")     # fichier csv dans une liste
as.matrix(FileNames) # en matrice


## identifier les pollens dans tous les échantillons (fichiers csv) du répertoire en question et les stocker dans nouveau répertoire
#créer un nouveau répertoire
# pour créer un sous dossier pour les fichier csv, nom au format "Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"
x <- Sys.time()
x <- gsub(":", "-", x)
x <- gsub(" ", "_", x)

newdir <- "../Ech21_40" # peut être remplacé par newdir<-"FolderName"
dir.create(paste0(newdir), showWarnings = FALSE)
getwd<-newdir

for(File in FileNames){
  brutfile<-read.csv(File, head=T)
  brutfile <- brutfile[, !names(brutfile) %in% c("Time","SampleID")]#"FSC_H","FSC_A","FSC_Width","SSC_H","SSC_A")]
  ## mise en forme des données
  ## nettoyage, pour supprimer les lignes sans valeurs (inf, NA)
  completerecords <- na.omit(brutfile) 
  completerecords2 <-  completerecords %>% 
    filter_if(~is.numeric(.), all_vars(!is.infinite(.))) # checking only numeric columns:
  pred<-predict(model, completerecords2, type="prob")
  #species_max <- apply(pred, 1, function(row) names(pred)[which.max(row)])
  #value_max <- apply(pred, 1, function(row) max(row))
  #predict <- data.frame(species = species_max, prob = value_max)
  write.csv(pred, file.path("C:/Users/sarah/OneDrive - UQAM/PhD/GitHub/ID_Samples/Ech_21-40/Ech_21-40_essentials_ID", paste0("ID_",File)), row.names = FALSE)
}

##### EN DEVELOPPEMENT #####
## prendre uniquement les pollens avec prob classification >0.9
# Modifier le nom de l'espèce pour les lignes où prob est inférieur à 0.90
data<-read.csv("./test_ID/ID_CY 21.csv")
data$species[data$prob < 0.60] <- "NI"
# Compter l'occurrence de chaque espèce
data <- table(data$species)
##### EN DEVELOPPEMENT #####

## créer un fichier avec tous les échantillons identifiés (ID)
## pour combiner tous les nouveaux fichiers .csv en un seul :
library(dplyr)
library(here)
library(readr)
library(purrr)
library(fs)

rm(list=ls())

## Crée un vecteur des noms de fichiers, avec tout le chemin d'accès
##vérifier qu'on est dans le bon repertoire
dir_list <- list.files(here(getwd()),
                       pattern = "^ID.*\\.csv$", full.names = TRUE)

## Nomme le vecteur avec seulement le nom de fichier, sans l'extension
names(dir_list) <- path_ext_remove(list.files(here(getwd()),
                       pattern = "^ID.*\\.csv$", full.names = TRUE))
names(dir_list) <-path_ext_remove(basename(dir_list))

files_df <- map_dfr(dir_list, read_csv, .id = "Sample") ## combine tous les fichiers csv en un, ajoute une colonne Sample_name avec le nom de l'échantillon
files_df<-table(files_df)
files_df<-as.data.frame(files_df)
rownames(files_df) = gsub(" ", "_", rownames(files_df))
rownames(files_df) = gsub("ID_", "", rownames(files_df))

write.csv(files_df, "../../ID_Ech2140_balanced_essentials.csv", row.names = F)


