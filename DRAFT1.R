# Rank Inference Thesis
# Script Nr.1: Data Inspection and Wrangling
# Author: Venus Kocabas
# Last Updated: 25.10.2026

# 0.  Importing Libraries --------------------------------------------------

libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl', 'lubridate', 'tidyquant')

missingLibs <- libraries[!(libraries %in% installed.packages()[, 'Package'])]

if (length(missingLibs) > 0) install.packages(missingLibs)

invisible(lapply(libraries, library, character.only = TRUE))

baseDir <- 'C:/Users/user/Desktop/THESIS'

# 1.A Reading the data sets (US Treasury) ---------------------------------

dataDirUS <- file.path(baseDir, 'DoD data', 'us treasury')
filePathsUS <- list.files(dataDirUS, pattern = '\\.csv$', full.names = TRUE)

treasury_list <- list()
failed_treasury <- character()

for (i in seq_along(filePathsUS)) {
  path <- filePathsUS[i]
  fileNameUS <- basename(path)
  prefix <- paste0('USTreasury_', fileNameUS)
  
  df <- tryCatch({
    read.csv(path) |> dplyr::rename_with(tolower)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(df) || nrow(df) == 0) {
    failed_treasury <- c(failed_treasury, fileNameUS)
    message('FAILED: ', fileNameUS)
    next
  }
  
  assign(prefix, df, envir = .GlobalEnv)
  treasury_list[[prefix]] <- df
  message('Run #', i, ': Treasury dataset created -> ', prefix)
}

# Bind sep. loaded treasury data sets
UsTreasuryData <- dplyr::bind_rows(treasury_list) |>
  dplyr::mutate(date = as.Date(lubridate::parse_date_time(date, orders = c('mdy', 'ymd', 'dmy')))) |>
  dplyr::filter(date > as.Date('2009-11-30') & date < as.Date('2025-01-01')) |>
  dplyr::arrange(date)

# 1.B Pulling the Data for DOW30 ------------------------------------------

excelPath <- file.path(baseDir, 'Rank-Inference-Thesis', 'DoD data.xlsx')
DoDInfo <- readxl::read_xlsx(excelPath, 'NeededData')

DOW30Companies <- DoDInfo$Tickers
DOW30Companies

failed_download <- character()

for (i in seq_along(DOW30Companies)) {
  name <- DOW30Companies[i]
  df <- tryCatch({
    tidyquant::tq_get(name, get = 'stock.prices',,
                      from = '2009-12-01', to = '2025-01-01')
  }, error = function(e) {
    NULL
  })
  if (!is.data.frame(df) || nrow(df) == 0) {
    failed_download <- c(failed_download, name)
    message('FAILED: ', name)
    next
  }
  assign(name, df, envir = .GlobalEnv)
  message('Run #', i, ': Downloaded stock data -> ', name)
}

failed_download

# 1.C Pulling Data for Market Baseline (SPY) ------------------------------

SPY <- tidyquant::tq_get('SPY', get = 'stock.prices', from = '2009-12-01', to = '2025-01-01')

# 2.  Monthly data conversion ----------------------------------------------

lastTrade <- function(df) {
  if ('symbol' %in% names(df)) {
    # If it's a stock/index dataset (has a ticker column)
    df |>
      dplyr::group_by(symbol) |>
      dplyr::mutate(year_month = floor_date(date, 'month')) |>
      dplyr::group_by(symbol, year_month) |>
      dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(-year_month) |>
      dplyr::arrange(symbol, date)
  } else {
    # If it's the US Treasury dataset (no ticker column)
    df |>
      dplyr::mutate(year_month = floor_date(date, 'month')) |>
      dplyr::group_by(year_month) |>
      dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(-year_month) |>
      dplyr::arrange(date)
  }
}

# DOW 30 Monthly Conversion
failed_conversion <- character()

for (i in seq_along(DOW30Companies)) {
  name <- DOW30Companies[i]
  if (exists(name, envir = .GlobalEnv)) {
    df <- get(name, envir = .GlobalEnv)
    df_monthly <- lastTrade(df)
    assign(paste0(name, '_monthly'), df_monthly, envir = .GlobalEnv)
    message('Run #', i, ': Monthly dataset created -> ', name, '_monthly')
  } else {
    failed_conversion <- c(failed_conversion, name)
    message('FAILED: ', name)
    next
  }
}
failed_conversion

# SPY Monthly Conversion
SPY_monthly <- lastTrade(SPY)

# US Treasury Monthly Conversion
UsTreasuryData_monthly <- lastTrade(UsTreasuryData) |>
  dplyr::rename_with(~ paste0('UST', '_', .x), -date) |>
  dplyr::select(c('date', 'UST_x1.mo'))

# 3.  Adjusted prices return calculation -----------------------------------

monthlyReturn <- function(df) {
  df |>
    dplyr::arrange(symbol, date) |>
    dplyr::group_by(symbol) |>
    dplyr::mutate(
      monthly_return = (adjusted / dplyr::lag(adjusted)) - 1,
      monthly_log_return = log(adjusted / dplyr::lag(adjusted))) |>
    dplyr::ungroup()
}

failed_monthly <- character()

for (i in seq_along(DOW30Companies)) {
  name <- paste0(DOW30Companies[i], '_monthly')
  if (exists(name, envir = .GlobalEnv)) {
    df <- get(name, envir = .GlobalEnv)
    df_monthly <- monthlyReturn(df)
    assign(paste0(name, 'Return'), df_monthly, envir = .GlobalEnv)
    message('Run #', i, ': Monthly return dataset created -> ', name, 'Return')
  } else {
    failed_monthly <- c(failed_monthly, name)
    message('FAILED: ', name)
    next
  }
}
failed_monthly

# SPY Returns
SPY_monthlyReturn <- monthlyReturn(SPY_monthly) |>
  dplyr::rename_with(~ paste0('SPY', '_', .x), -date)

# 4.  Experimenting --------------------------------------------------------

# Extracting the DoD Years
DOW30Years <- readxl::excel_sheets(excelPath) |>
  (\(df) {
    grep('[0-9]{4}', df, value = TRUE)
  })()

# Extracting Index Companies
Dow30IndexNames <- function(DOWyear) {
  data <- readxl::read_xlsx(excelPath, sheet = DOWyear)
  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ]
  tickers <- unlist(lapply(data$Ticker, function(x) paste0(x, '_monthlyReturn')))
  return(tickers)
}

# Creating the Lists for DOW years
for(i in seq_along(DOW30Years)){
  name <- paste0('DOWcomps_', DOW30Years[i])
  df <- Dow30IndexNames(DOW30Years[i])
  assign(paste0(name, 'Return'), df, envir = .GlobalEnv)
  message('Run #', i, ': DoD dataset created -> ', name)
}

# CAPM Function
CAPM <- function(comp, DOWyear){
  comp <- 'AAPL'
  name <- paste0(comp, '_monthlyReturn')
  compData <- get(name) |>
    dplyr::select(symbol, date, monthly_return, monthly_log_return) |>
    dplyr::rename_with(~ paste0(comp, '_', .x), -date)
  CAPMdata <- dplyr::full_join(compData, UsTreasuryData_monthly, SPY_monthlyReturn, by = 'date')
}

CAPMdata |>  View()

UsTreasuryData_monthly

# Linear Regression Function (CAPM Stub)
NeededYears <- readxl::read_xlsx(excelPath, sheet = 'NeededData')

CAPM <- function(dataName, DOWyear) {

}

# Test load for sheet 2015 as a single pipe
data <- readxl::read_xlsx(excelPath, sheet = '2015') |>
  (\(df) {
    colnames(df) <- as.character(df[1, ])
    df[-1, ]
  })()

for(i in seq_along(data)){
  print(i)
}
  