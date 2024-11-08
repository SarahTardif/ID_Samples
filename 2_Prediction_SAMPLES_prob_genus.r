library(dplyr)
library(tidyverse)

setwd("./Ech_21-40/Ech_21-40_ID_prob")
PrimaryDirectory<-getwd() #verifier d'etre dans le bon repertoire avant
## Récupère les noms des fichiers à identifier dans le répertoire de travail
FileNames <- list.files(path=PrimaryDirectory, pattern = ".csv")     # fichier csv dans une liste
as.matrix(FileNames) # en matrice

all_data_genus <- list()
for(File in FileNames){
  data<-read.csv(File, head=T)
  # Extraction des genres à partir des noms de colonnes
  genres <- sub("_.*", "", colnames(data))
# Créer une dataframe pour stocker les résultats de chaque groupe
  data_genus <- data.frame(matrix(ncol = length(unique(genres)), nrow = nrow(data)))
  colnames(data_genus) <- unique(genres)
# Pour chaque genre, calculer la somme des prob
  for (genre in unique(genres)) {
    cols_to_sum <- data[, genres == genre, drop = FALSE]
    data_genus[[genre]] <- rowSums(cols_to_sum)
  }
  genus_max <- apply(data_genus, 1, function(row) names(data_genus)[which.max(row)])
  prob_max <- apply(data_genus, 1, function(row) max(row))
  data_genus1 <- data.frame(genus = genus_max, prob = prob_max)
  data_genus1$genus[data_genus1$prob < 0.75] <- "NI"
# Compter l'occurrence de chaque espèce
  datafin <- as.data.frame(table(data_genus1$genus))
  df_pivoted <- datafin %>% pivot_wider(names_from = Var1, values_from = Freq)
  all_data_genus[[File]] <- df_pivoted
}
#
all_data_genus1 <- bind_rows(all_data_genus, .id = "sample")
all_data_genus1[is.na(all_data_genus1)] <- 0
all_data_genus1<-as.data.frame(all_data_genus1)
all_data_genus1$sample <- gsub("ID_|\\.csv", "", all_data_genus1$sample)


# Enregistrer le fichier final
write.csv(all_data_genus1, "C:/Users/sarah/OneDrive - UQAM/PhD/GitHub/ID_Samples/data_genus_21-40_prob75.csv", row.names = FALSE)
