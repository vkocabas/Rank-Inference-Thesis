# Rank Inference Thesis
# Script Nr.1: Data Inspection and Wrangling
# Author: Venus Kocabas
# Last Updated: 15.10.2026

# 0. Importing Libraries --------------------------------------------------

libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl')

missingLibs <- libraries[!(libraries %in% installed.packages()[, 'Package'])]

if (length(missingLibs) > 0) {
  install.packages(missingLibs)
}

invisible(lapply(libraries, library, character.only = TRUE))

# 1.A Reading the data sets (DOW 30 + SPY) ---------------------------------

dataDir <- 'C:/Users/user/Desktop/THESIS/DoD data'

filePaths <- list.files(dataDir, pattern = "_HistoricalData_.*\\.csv$", full.names = TRUE)

DOWList <- character() 

for (i in seq_along(filePaths)) {
  path <- filePaths[i]
  fileName <- basename(path)
  
  # Extract the pattern before the first underscore (e.g., 'AAPL' or 'SPY')
  prefix <- sub('^(.*?)[_].*', '\\1', fileName)
  df <- read.csv(path)
  
  # Separate SPY from the Dow 30 components
  if (prefix == "SPY") {
    assign("SPY", df, envir = .GlobalEnv)
    message('Run #', i, ': Saved market baseline -> SPY')
  } else {
    assign(prefix, df, envir = .GlobalEnv)
    DOWList <- c(DOWList, prefix)
    message('Run #', i, ': Created individual DOW dataset -> ', prefix) # CHECKPOINT Nr.1
  }
}

# 1.B Reading the data sets (US Treasury) ---------------------------------

dataDirUS <- 'C:/Users/user/Desktop/THESIS/DoD data/us treasury'

filePathsUS <- list.files(dataDirUS, full.names = TRUE)

for (path in filePathsUS) {
  fileNameUS <- basename(path)
  
  prefix <- paste0('USTreasury_', fileNameUS)
  
  df <- read.csv(path)
  
  assign(prefix, df, envir = .GlobalEnv)
  
  message('Created individual dataset: ', prefix) # CHECKPOINT Nr.2
}

# 2. Data Inspection ------------------------------------------------------

# e.g. AAPL
# All data pulled from NASDAQ so they all have the same structure




























