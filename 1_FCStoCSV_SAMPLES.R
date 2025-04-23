## Script to convert FCS file (cytometer format) into CSV
## adapted from Thomas Ashhurst's script
## https://github.com/sydneycytometry/CSV-to-FCS/blob/master/FCS-to-CSV%20v2.0.R

#####
## Some packages need BiocManager, see below for installation :
#install.packages("BiocManager")
#BiocManager::install("flowCore") 
#BiocManager::install("Biobase")

## load packages and libraries
library('flowCore')
library('Biobase')
library('data.table')

## set working directory
setwd("./2023_samples_W1-W4") # /!/ check that files (samples) have the right name, rename them now if necessary
getwd() # check
PrimaryDirectory <- getwd()
PrimaryDirectory # re check

## Retrieves .fcs file names from the working directory
FileNames <- list.files(path=PrimaryDirectory, pattern = ".fcs")     # fsc files in a list
as.matrix(FileNames) # in matrix

## read data from files in a dataframe
DataList=list() # create an empty list

for (File in FileNames) { # loop to read files in the list
  fcsfile <- read.FCS(File, transformation = FALSE,truncate_max_range = FALSE)
  tempdata <- exprs(fcsfile)
  colnames(tempdata)<-fcsfile@parameters@data$desc ## retrieves variables names
  tempdata <- tempdata[1:nrow(tempdata),1:ncol(tempdata)]
  File <- gsub(".fcs", "", File)
  DataList[[File]] <- tempdata
}
rm(tempdata)
rm(fcsfile)
AllSampleNames <- names(DataList)

## Check data
#head(DataList)



##### END USER INPUT #####
# to create a subfolder for csv files, name in the format “Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S”
x <- Sys.time()
x <- gsub(":", "-", x)
x <- gsub(" ", "_", x)

newdir <- "./2023_samples_W1-W4_CSV" 

setwd(PrimaryDirectory)
dir.create(paste0(newdir), showWarnings = FALSE) # creates a subfolder with the name newdir
setwd(newdir)

# take care not to overwrite pre-existing files
for(i in c(1:length(AllSampleNames))){
  data_subset <- DataList[i][[1]]
  data_subset <- as.data.frame(data_subset)
  colnames(data_subset) = gsub("-", "_", colnames(data_subset)) ## replaces - with _ in column names (because R doesn't like -)
  a <- names(DataList)[i]
  data_subset$SampleID <- seq.int(nrow(data_subset)) ## add sample name as ID
  write.csv(data_subset, paste0(a, ".csv"), row.names = FALSE)
}


