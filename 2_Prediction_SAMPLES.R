## script to predict class of new data

## load packages and libraries
library(dplyr)
library(tidyr)
library(readr)
library(here)
library(purrr)
library(fs)
library(tibble)
#install.packages("randomForest")
library(randomForest)

## load the chosen model
model<-readRDS("./modelRF_genus_balanced_20250110.rds")

## load data to classify
setwd("./Maya_data")
PrimaryDirectory<-getwd() # make sure to be in the right directory first
## Retrieves the names of the files to be identified in the working directory
FileNames <- list.files(path=PrimaryDirectory, pattern = ".csv")     # csv file in a list
as.matrix(FileNames) # in matrix


## identify pollen grains in all samples (csv files) in the chosen directory and store them in the new directory

for(File in FileNames){
  brutfile<-read.csv(File, head=T)
  brutfile <- brutfile[, !names(brutfile) %in% c("Time","SampleID")]#"FSC_H","FSC_A","FSC_Width","SSC_H","SSC_A")]
  ## data formatting
  ## cleaning to remove all lines with no data (inf, NA)
  completerecords <- na.omit(brutfile) 
  completerecords2 <-  completerecords %>% 
    filter_if(~is.numeric(.), all_vars(!is.infinite(.))) # checking only numeric columns:
  pred<-predict(model, completerecords2, type="prob")
  species_max <- apply(pred, 1, function(row) names(pred)[which.max(row)])
  value_max <- apply(pred, 1, function(row) max(row))
  predict <- data.frame(species = species_max, prob = value_max)
  write.csv(predict, file.path("C:/Users/sarah/OneDrive - UQAM/PhD/GitHub/ID_Samples/Maya_data/Maya_data_ID", paste0("ID_",File)), row.names = FALSE)
}

## create a file with all identified samples (ID)
setwd("./Maya_data_ID")
## Create a vector of filenames, with all paths
## check to be in the right directory
dir_list <- list.files(here(getwd()),
                       pattern = "^ID.*\\.csv$", full.names = TRUE)

## Names vector with file name only, no extension
names(dir_list) <- path_ext_remove(list.files(here(getwd()),
                       pattern = "^ID.*\\.csv$", full.names = TRUE))
names(dir_list) <-path_ext_remove(basename(dir_list))

ID_all <- map_dfr(dir_list, read_csv, .id = "Sample") ## combines all csv files into one, adds Sample_name column with sample name
ID_all <-as.data.frame(ID_all)
rownames(ID_all) = gsub(" ", "_", rownames(ID_all))
rownames(ID_all) = gsub("ID_", "", rownames(ID_all))

#write.csv(ID_all, "../../ID_test_all.csv", row.names = F)


## keep pollen grains only if the classification probability is superior or equal to the threshold value for the taxon

## data cleaning to have the same taxa for ID_all and Valseuil
ID_all$species <- ifelse(ID_all$species == "Malus"|ID_all$species =="Prunus"|ID_all$species =="Pyrus"|ID_all$species =="Sorbus"|ID_all$species =="Amelanchier", "Rosaceae", ID_all$species)
ID_all$species <- ifelse(ID_all$species == "Juniperus", "Cupressaceae", ID_all$species)
ID_all$species <- ifelse(ID_all$species == "Pinus"|ID_all$species =="Picea", "Pinaceae", ID_all$species)
ID_all$species <- ifelse(ID_all$species == "Corylus"|ID_all$species =="Ostrya", "Corylus.Ostrya", ID_all$species)
ID_all$species <- ifelse(ID_all$species == "Carpinus"|ID_all$species =="Celtis"|ID_all$species =="Robinia", "NI.Others", ID_all$species)

valseuil<-read.csv("../../valseuil.csv", sep=",", h=T)
valseuil <- pivot_longer(valseuil, cols=everything(),names_to = "taxon", values_to = "probaseuil")
ID_all_final<-data.frame()
for(taxon in unique(ID_all$species)){
  datataxon<-ID_all[ID_all$species==taxon,]
  proba <- valseuil[valseuil$taxon == taxon, "probaseuil"][[1]]
  if(proba>0){
    datataxon<-datataxon[datataxon$prob >= proba, ]
    ID_all_final<- rbind(ID_all_final,datataxon)
  }
  else {
    datataxon<-datataxon[datataxon$prob >= 0, ]
    ID_all_final<- rbind(ID_all_final,datataxon)
  }
}
ID_all_final <- ID_all_final[, -which(names(ID_all_final) %in% c("prob"))]
ID_all_final<-table(ID_all_final$Sample, ID_all_final$species)
ID_all_final<-as.data.frame.matrix(ID_all_final)
ID_all_final$TOTAL<-rowSums(ID_all_final)
ID_all_final<-rownames_to_column(ID_all_final, var = "Sample")

write.csv(ID_all_final, "../../ID_Maya_data_all.csv", row.names = F)