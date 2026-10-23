# Rank Inference Thesis
# Script Nr.1: Data Inspection and Wrangling
# Author: Venus Kocabas
# Last Updated: 15.10.2026

# 0. Importing Libraries --------------------------------------------------

libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl', 'lubridate', 'tidyquant')

missingLibs <- libraries[!(libraries %in% installed.packages()[, 'Package'])]

if (length(missingLibs) > 0) {
  install.packages(missingLibs)
}

invisible(lapply(libraries, library, character.only = TRUE))

# 1.A Reading the data sets (US Treasury) ---------------------------------

dataDirUS <- 'C:/Users/user/Desktop/THESIS/DoD data/us treasury'

filePathsUS <- list.files(dataDirUS, full.names = TRUE)

for (path in filePathsUS) {
  fileNameUS <- basename(path)
  
  prefix <- paste0('USTreasury_', fileNameUS)
  
  df <- read.csv(path) %>% dplyr::rename_with(tolower)
  
  assign(prefix, df, envir = .GlobalEnv)
  
  message('Created individual dataset: ', prefix)
}

UsTreasuryData <- dplyr::bind_rows(`USTreasury_par-yield-curve-rates-1990-2023.csv`,
                            `USTreasury_2024 daily-treasury-rates.csv`,
                            `USTreasury_2025 daily-treasury-rates.csv`) |>
  dplyr::mutate(date = as.Date(date,'%m/%d/%Y')) |>
  dplyr::filter(date > as.Date('12/31/2009', '%m/%d/%Y')) |>
  dplyr::filter(date < as.Date('01/01/2025', '%m/%d/%Y')) |>
  dplyr::arrange(date)

# 1.B Pulling the Data for DOW30 ------------------------------------------

DoDInfo <- readxl::read_xlsx('C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data.xlsx','NeededData')

DOW30Companies <- DoDInfo$Tickers
DOW30Companies

failed <- character()

for (i in seq_along(DOW30Companies)) {
  name <- DOW30Companies[i]
  df <- tidyquant::tq_get(name, get = 'stock.prices',
                          from = '2010-01-01', to = '2025-01-01')
  if (!is.data.frame(df) || nrow(df) == 0) {
    failed <- c(failed, name)
    message('FAILED: ', name)
    next
  }
  assign(name, df, envir = .GlobalEnv)
}
failed

# 1.C Pulling Data for Market Baseline (SPY) ------------------------------

SPY <- tidyquant::tq_get('SPY', get = 'stock.prices', from = '2005-01-01', to = '2025-01-01')

# 2. Initial Data Cleaning ------------------------------------------------

monthlydata <- function(data) {
  newdf <- data |>
    dplyr::group_by(symbol, year = year(date), month = month(date)) |>
    dplyr::slice_max(date, n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-year, -month)
  return(newdf)
}

for (i in seq_along(DOW30Companies)) {
  name <- DOW30Companies[i]
  if (exists(name, envir = .GlobalEnv)) {
    df <- get(name, envir = .GlobalEnv)
    df_monthly <- monthlydata(df)
    assign(paste0(name, "_monthly"), df_monthly, envir = .GlobalEnv)
    message("Run #", i, ": Monthly dataset created -> ", name, "_monthly")
  } else {
    message("Run #", i, ": Skipping ", name, " (dataset does not exist)")
  }
}

# 3. Retrieving the main regression data ----------------------------------

lastTrade <- function(df) {
  df %>%
    mutate(year_month = floor_date(.data[['date']], "month")) %>%
    group_by(year_month) %>%
    slice_max(order_by = .data[['date']], n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-year_month) %>%
    arrange(.data[['date']])
}

lastTrade(V) |>  View()
V
