## script pour prédire la classe de nouvelles données
## pour toutes questions, contacter Gauthier Lapa, gauthier.lapa@gmail.com
## version du 2023-04-03


library(dplyr)
#install.packages("randomForest")
library(randomForest)
## charger les paquets nécessaires pour le  modèle choisi

## charger le modèle retenu
model<-readRDS("../modelRF_species_20240925.rds")

## charger les  données à classifier
PrimaryDirectory<-getwd() #verifier d'etre dans le bon repertoire avant
## Récupère les noms des fichiers à identifier dans le répertoire de travail
FileNames <- list.files(path=PrimaryDirectory, pattern = ".csv")     # fichier csv dans une liste
as.matrix(FileNames) # en matrice


## identifier les pollens dans tous les échantillons (fichiers csv) du répertoire en question

for(File in FileNames){
  brutfile<-read.csv(File, head=T)
  ## mise en forme des données
  ## nettoyage, pour supprimer les lignes sans valeurs (inf, NA)
  completerecords <- na.omit(brutfile) 
  completerecords2 <-  completerecords %>% 
    filter_if(~is.numeric(.), all_vars(!is.infinite(.))) # checking only numeric columns:
  pred<-predict(model, completerecords2)
  completerecords2$Class_pred<-pred
  write.csv(pred, file.path(PrimaryDirectory, paste0("ID_",File)), row.names = FALSE)
}

## créer un fichier avec tous les échantillons identifiés (ID)
## pour combiner tous les nouveaux fichiers .csv en un seul :
library(dplyr)
library(here)
library(readr)
library(purrr)
library(fs)

rm(list=ls())

## Crée un vecteur des noms de fichiers, avec tout le chemin d'accès
dir_list <- list.files(here(getwd()),
                       pattern = "^ID.*\\.csv$", full.names = TRUE)

## Nomme le vecteur avec seulement le nom de fichier, sans l'extension
names(dir_list) <- path_ext_remove(list.files(here(getwd()),
                       pattern = "^ID.*\\.csv$", full.names = TRUE))
names(dir_list) <-path_ext_remove(basename(dir_list))

files_df <- map_dfr(dir_list, read_csv, .id = "Sample_Name") ## combine tous les fichiers csv en un, ajoute une colonne Sample_name avec le nom de l'échantillon
files_df<-table(files_df)
rownames(files_df) = gsub(" ", "_", rownames(files_df))
rownames(files_df) = gsub("ID_", "", rownames(files_df))

write.csv(files_df, "./ID_2023_2140_CY.csv", row.names = T)


##reste à ajouter nom premiere colonne / changer nom des lignes pour enlever le ID et _ 
## essayer de remplacer le x dans les fichiers de chaque echantillon par Class ou genre ou whatever
