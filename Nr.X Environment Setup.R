# Rank-Inference-Thesis
# Nr.0 Environment Setup
# Author: Venüs Kocabas
# Last Updates: 02.11.2026

# Definition of the needed libraries
libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl', 'lubridate', 'tidyquant')

# Installing the packages if needed
missingLibs <- libraries[!(libraries %in% installed.packages()[, 'Package'])]
if (length(missingLibs) > 0) install.packages(missingLibs)

# Calling the Libraries into the Environment
invisible(lapply(libraries, library, character.only = TRUE))

