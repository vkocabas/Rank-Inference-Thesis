# # Rank Inference Thesis
# Pulling SPY Data
# Author: Venus Kocabas
# Last Updated: 28.10.2026

library <- c('tidyquant')

missingLibs <- libraries[!(libraries %in% installed.packages()[, 'Package'])]

if (length(missingLibs) > 0) install.packages(missingLibs)

invisible(lapply(libraries, library, character.only = TRUE))

SPY <- tidyquant::tq_get('SPY', get = 'stock.prices', from = '1999-12-31', to = '2025-01-01')

save(SPY, file="SPYdata.Rda")
