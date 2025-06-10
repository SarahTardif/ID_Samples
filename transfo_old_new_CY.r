
# Liste des fonctions par colonne
equation_list <- list(
  FSC_H = function(x) 0.91*x-6965.64,
  FSC_A = function(x) 0.91*x-6965.64,
  SSC_H = function(x) 0.67*x+291400.7,
  SSC_A = function(x) 0.67*x+291400.7,
  FITC_H = function(x) 1.35*x+9821.47,
  FITC_A = function(x) 1.35*x+9821.47,
  PE_H = function(x) 1.22*x+195349.58,
  PE_A = function(x) 1.22*x+195349.58,
  PC5.5_H = function(x) 1.35*x+82716.52,
  PC5.5_A = function(x) 1.35*x+82716.52,
  PC7_H = function(x) 1.34*x+18320.34,
  PC7_A = function(x) 1.34*x+18320.34,
  APC_H = function(x) 0.47*x+826.04,
  APC_A = function(x) 0.47*x+826.04,
  APC_A700_H = function(x) 0.94*x+574.25,
  APC_A700_A = function(x) 0.94*x+574.25,
  APC_A750_H = function(x) 1.31*x+193.45,
  APC_A750_A = function(x) 1.31*x+193.45,
  PB450_H = function(x) 0.44*x+138610.6,
  PB450_A = function(x) 0.44*x+138610.6,
  KO525_H = function(x) 0.54*x+310260.51,
  KO525_A = function(x) 0.54*x+310260.51,
  Violet610_H = function(x) 0.64*x+77840.75,
  Violet610_A = function(x) 0.64*x+77840.75,
  FSC_Width = function(x) 1.09*x-578.14
)

## load data to classify
setwd("./2023_samples_newcy/2023_samples_newcy_CSV")
PrimaryDirectory<-getwd() # make sure to be in the right directory first
## Retrieves the names of the files to be identified in the working directory
FileNames <- list.files(path=PrimaryDirectory, pattern = ".csv")     # csv file in a list
as.matrix(FileNames) # in matrix

# Appliquer chaque transformation
for(File in FileNames){
  brutfile<-read.csv(File, head=T)
  brutfile <- brutfile[, !names(brutfile) %in% c("Time","SampleID")]
    for (col in names(equation_list)) {
    brutfile[[col]] <- equation_list[[col]](brutfile[[col]])
    }
  write.csv(brutfile, file.path("C:/Users/sarah/OneDrive - UQAM/PhD/GitHub/ID_Samples/2023_samples_newcy/2023_samples_newcy_CSV_correct", paste0("correct_",File)), row.names = FALSE)
}

