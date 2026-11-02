# Rank-Inference Thesis
# Author: Venüs Kocabas
# Last Updated: 31.10.2026

# 0. Importing Libraries --------------------------------------------------

libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl', 'lubridate', 'tidyquant')
missingLibs <- libraries[!(libraries %in% installed.packages()[, 'Package'])]
if (length(missingLibs) > 0) install.packages(missingLibs)
invisible(lapply(libraries, library, character.only = TRUE))

# 1.A Helper Function: Monthly Data Points --------------------------------

# Extracting the Last Trade Day per Month
lastTrade <- function(df, datecol = date) {
  # Safely convert datecol argument to a string whether passed as string or symbol
  date_col_str <- substitute(datecol)
  if (is.symbol(date_col_str) || rlang::is_quosure(date_col_str)) {
    date_col_str <- rlang::as_name(rlang::ensym(datecol))
  } else {
    date_col_str <- as.character(datecol)
  }
  
  # Fallback check if the specified column isn't found
  if (!date_col_str %in% names(df)) {
    if ('DATES' %in% names(df)) date_col_str <- 'DATES'
    else if ('date' %in% names(df)) date_col_str <- 'date'
  }
  
  date_sym <- rlang::sym(date_col_str)
  has_symbol <- 'symbol' %in% names(df)
  
  group_cols <- if (has_symbol) c('symbol', 'year_month') else 'year_month'
  arrange_cols <- if (has_symbol) c('symbol', date_col_str) else date_col_str
  
  df |>
    dplyr::mutate(year_month = lubridate::floor_date(!!date_sym, 'month')) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::slice_max(order_by = !!date_sym, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-year_month) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols)))
}

# 1.B Helper Function: Extracting the DOW Years ---------------------------

# Extracting Index Companies
Dow30IndexNames <- function(DOWyear, excelPath) {
  data <- readxl::read_xlsx(excelPath, sheet = DOWyear)
  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ]
  tickers <- unlist(lapply(data$Ticker, function(x) paste0(x, '_monthly')))
  return(tickers)
}

# 1.C Helper Function: DOW30 Initial Cleaning -----------------------------

InitialCleaning <- function(df) {
  df <- df[-1, ]
  col_names <- colnames(df)
  div_date_col <- col_names[grepl('DIV PAY DATE$', col_names)]
  
  if (length(div_date_col) > 0) {
    div_col_sym <- rlang::sym(div_date_col)
    
    df <- df |>
      dplyr::mutate(
        DATES_char = as.character(DATES),
        !!div_col_sym := dplyr::if_else(
          !is.na(!!div_col_sym) & !!div_col_sym != 'NA' & !!div_col_sym != '', 
          DATES_char, 
          NA_character_)) |>
      dplyr::select(-DATES_char) # remove temporary helper column
  }
  
  df <- df |>
    dplyr::mutate(DATES = as.Date(lubridate::parse_date_time(DATES, orders = c('mdy', 'ymd', 'dmy', 'ymd HMS'))))
  
  if (length(div_date_col) > 0) {
    div_col_sym <- rlang::sym(div_date_col)
    df <- df |>
      dplyr::mutate(
        !!div_col_sym := as.Date(lubridate::parse_date_time(!!div_col_sym, orders = c('mdy', 'ymd', 'dmy', 'ymd HMS'))))
  }
  
  numeric_cols <- col_names[!col_names %in% c('DATES') & !grepl('DIV PAY DATE$', col_names)]
  
  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), ~ suppressWarnings(as.numeric(.x))))
  
  return(df)
}

# 1.D Helper Function: Adjusted Prices ------------------------------------

AddAdjustedPrice <- function(df, pattern = 'MARKET PRICE', new_col = 'AdjustedPrice') {
  price_cols <- grep(pattern, names(df), value = TRUE)
  if (length(price_cols) == 0) {
    df[[new_col]] <- NA_real_
    return(df)
  }
  df[[new_col]] <- do.call(dplyr::coalesce, unname(as.list(df[price_cols])))
  df
}

# 1.E Helper Function: Monthly Returns ------------------------------------

MonthlyReturns <- function(dataset, adjprcol) {
  dataset |>
    dplyr::mutate(
      monthly_return = ({{ adjprcol }} / dplyr::lag({{ adjprcol }})) - 1,
      monthly_log_return = log({{ adjprcol }} / dplyr::lag({{ adjprcol }}))) |>
    dplyr::filter(!is.na(monthly_return))
}

# 2.A Creating the US Treasury Data Set -----------------------------------

dataDirUS <- file.path('C:/Users/user/Desktop/THESIS/DoD data/us treasury')
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
  dplyr::filter(date > as.Date('2009-12-30') & date < as.Date('2025-01-01')) |>
  dplyr::arrange(date) |>
  dplyr::select(c(date, x1.mo)) |>
  lastTrade()

# 2.B Creating the Individual Data Sets for DOW30 -------------------------

# Company Ticker Mapping per Years
DoDdataPath <-  'C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data.xlsx' 

# Data from 'Datastream'
DOW30Data <- readxl::read_xlsx('C:/Users/user/Desktop/THESIS/DoD data/DOW30 Data/Data VK.xlsx', sheet = 'Tabelle1')

# Initial Column Cleaning
CLEANDOW30Data <- DOW30Data[-1,] |>
  dplyr::select(-c('...2',                                        # DATE
                   '#ERROR...567','#ERROR...568','#ERROR...569',  # UTX Missing Stuff
                   '#ERROR...570','#ERROR...571','#ERROR...572',
                   '#ERROR...573','#ERROR...574','#ERROR...575',
                   '#ERROR...576','#ERROR...577','#ERROR...578',
                   '#ERROR...723','#ERROR...724','#ERROR...725',  # S&P 500 was absent?
                   '#ERROR...726','#ERROR...727','#ERROR...728',
                   '#ERROR...729','#ERROR...730','#ERROR...731',
                   '#ERROR...732','#ERROR...733','#ERROR...734',
                   '#ERROR...735','#ERROR...736','#ERROR...737',
                   '#ERROR...738')) |>
  dplyr::mutate(dplyr::across(everything(), ~ ifelse(.x == 'NA', NA, .x)))

# Personal Mapping Sheet with the Columns from DOW30Data
mappingSheet <- readxl::read_xlsx('C:/Users/user/Desktop/THESIS/DoD data/DOW30 Data/Data VK.xlsx', sheet = 'Sheet1')

# Ticker x Company Name Matching
# Mapping Manually Done in the Excel
neededCompanies <- readxl::read_xlsx(DoDdataPath, sheet = 'NeededData(15-25)x') |> 
  (\(df) {
    df |>
    dplyr::filter(`Last Appeared` >= 2015)
    })()

# Years that would be investigated (2015 - 2025)
DOW30Years <- seq(2015, 2025)

# Creating the Lists for DOW years
for(i in seq_along(DOW30Years)){
  name <- paste0('DOWcomps_', DOW30Years[i])
  df <- Dow30IndexNames(as.character(DOW30Years[i]), DoDdataPath) 
  assign(name, df, envir = .GlobalEnv)
  message('Run #', i, ': DoD dataset created -> ', name)
}

for (i in seq_along(mappingSheet)) {
  compCols <- colnames(mappingSheet)[i]
  
  compColsName <- neededCompanies |>
    dplyr::filter(`Datastream Mappings` == compCols) |>
    dplyr::pull(Tickers)
  
  if (length(compColsName) != 1 || is.na(compColsName) || compColsName == '') {
    message(paste0("SKIPPED: No match found for column -> '", compCols, "'"))
    next
  }
  
  raw_cols <- unname(unlist(mappingSheet[, i]))
  valid_cols <- raw_cols[!is.na(raw_cols) & raw_cols != '']
  
  # Base Cleaned
  compData <- CLEANDOW30Data |>
    dplyr::select(dplyr::all_of(c('DATES', valid_cols))) |>
    InitialCleaning()
  
  assign(compColsName, compData, envir = .GlobalEnv)
  
  message(paste0('Run #', i, ': dataset created -> ', compColsName))
}

# 2.C Adding the SPY Data Set ---------------------------------------------

load('SPYdata.Rda')
SPY_monthly <- lastTrade(SPY) |>
  MonthlyReturns(adjprcol = adjusted)

# 2.D Additional Individual Adjustments for the DOW30 Data Sets -----------

# DD
DDratio <- 1.2820

DD <- DD1 |>
  dplyr::filter(DATES < as.Date('2017-09-01')) |>
  dplyr::mutate(across(c(2, 4, 6:17), \(x) as.numeric(x) / DDratio)) |>
  setNames(names(DD2)) |>
  dplyr::bind_rows(DD2 |> dplyr::filter(DATES >= as.Date('2017-09-01'))) |>
  dplyr::arrange(DATES)

rm(DD1,DD2)

# 2.E Creating Monthly Data Sets ------------------------------------------

DOWTotal <- c(setdiff(neededCompanies$Tickers, c('DD1', 'DD2')), 'DD')

for(i in seq_along(DOWTotal)){
  compData <- get(DOWTotal[i])
  compDatamonthly <- compData |>
    AddAdjustedPrice(pattern = 'MARKET PRICE') |>
    lastTrade(datecol = 'DATES') |>
    MonthlyReturns(adjprcol = AdjustedPrice)
  assign(paste0(DOWTotal[i],'_monthly'), compDatamonthly, envir = .GlobalEnv)
}

# 3.A Helper Function: 'Franken' Data Set ---------------------------------

FinalDataSet <- function(stockData, marketData = SPY_monthly, rfData = UsTreasuryData) {
  stock <- stockData |>
    dplyr::transmute(
      ym            = lubridate::floor_date(DATES, 'month'),
      date          = DATES,
      stock_ret     = monthly_return,
      stock_log_ret = monthly_log_return)
  
  mkt <- marketData |>
    dplyr::transmute(
      ym            = lubridate::floor_date(date, 'month'),
      mkt_ret       = monthly_return,
      mkt_log_ret   = monthly_log_return)
  
  # Risk-free: monthly decimal rate, lagged one month
  rf <- rfData |>
    dplyr::transmute(
      ym = lubridate::floor_date(date, 'month') %m+% months(1),
      rf = x1.mo / 100 / 12)
  
  stock |>
    dplyr::inner_join(mkt, by = 'ym') |>
    dplyr::inner_join(rf,  by = 'ym') |>
    dplyr::mutate(
      excess_ret = stock_ret - rf,
      excess_mkt = mkt_ret   - rf) |>
    dplyr::select(-ym)
}

# 3.B Helper Function: CAPM Function --------------------------------------

CAPMFunction <- function(dataset, DOWyear) {
  years <- (DOWyear - 5):(DOWyear - 1)
  filteredDataset <- dataset |> dplyr::filter(lubridate::year(date) %in% years)
  CAPM <- lm(excess_ret ~ excess_mkt, data = filteredDataset) |> summary()
  return(CAPM)
}

# 4.1 DOW 2015 ------------------------------------------------------------

DOW2015 <- list()

for (i in seq_along(DOWcomps_2015)) {
  currentComp     <- get(DOWcomps_2015[i])
  currentCompName <- sub("_monthly$", "", DOWcomps_2015[i])
  CAPMdata        <- FinalDataSet(currentComp)
  
  assign(paste0(currentCompName, "_CAPMData"), CAPMdata, envir = .GlobalEnv)
  
  capm <- CAPMFunction(CAPMdata, 2015)
  
  DOW2015[[currentCompName]] <- list(
    summary   = capm,
    intercept = capm$coefficients["(Intercept)", "Estimate"]
  )
}

sapply(DOW2015, function(x) x$intercept)




















