# Rank-Inference Thesis
# Nr.X Main Script
# Author: Venüs Kocabas
# Last Updated: 31.10.2026

# 0. Importing Libraries --------------------------------------------------

libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl', 'lubridate', 'tidyquant', 'csranks')
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

dataDirUS <- file.path('C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/US Treasury Data')
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
DoDdataPath <-  'C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/DoD_Mapping.xlsx' 

# Data from 'Datastream'
DOW30Data <- readxl::read_xlsx('C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/DOW30 Data/DatastreamDOW30.xlsx', sheet = 'Tabelle1')

# Initial Column Cleaning
CLEANDOW30Data <- DOW30Data[-1,] |>
  dplyr::select(-c('...2',                                        # DATE formatted as 'date' in excel
                   '#ERROR...567','#ERROR...568','#ERROR...569',  # UTX Missing Stuff -> RTX already has everything in it (Raytheon)
                   '#ERROR...570','#ERROR...571','#ERROR...572',
                   '#ERROR...573','#ERROR...574','#ERROR...575',
                   '#ERROR...576','#ERROR...577','#ERROR...578',
                   '#ERROR...723','#ERROR...724','#ERROR...725',  # S&P 500 was absent? --> will use tidyquant for the SPY data (Yahoo Finance)
                   '#ERROR...726','#ERROR...727','#ERROR...728',
                   '#ERROR...729','#ERROR...730','#ERROR...731',
                   '#ERROR...732','#ERROR...733','#ERROR...734',
                   '#ERROR...735','#ERROR...736','#ERROR...737',
                   '#ERROR...738')) |>
  dplyr::mutate(dplyr::across(everything(), ~ ifelse(.x == 'NA', NA, .x)))

# Personal Mapping Sheet with the Columns from DOW30Data
mappingSheet <- readxl::read_xlsx('C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/DOW30 Data/DatastreamDOW30.xlsx', sheet = 'Sheet1')

# Ticker x Company Name Matching
# Mapping Manually Done in the Excel
neededCompanies <- readxl::read_xlsx(DoDdataPath, sheet = 'NeededData')

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

load('C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/SPYdata.Rda')
SPY_monthly <- lastTrade(SPY) |>
  MonthlyReturns(adjprcol = adjusted)

# 2.D Additional Individual Adjustments for the DOW30 Data Sets -----------

# DD - merger in August 2017
DDratio <- 1.2820 # taken from the SEC filings for the merger!

DD <- DD1 |>
  dplyr::filter(DATES < as.Date('2017-09-01')) |>
  dplyr::mutate(across(c(2, 4, 6:17), \(x) as.numeric(x) / DDratio)) |>
  setNames(names(DD2)) |>
  dplyr::bind_rows(DD2 |> dplyr::filter(DATES >= as.Date('2017-09-01'))) |>
  dplyr::arrange(DATES)

# not needed anymore, would cause confusions later on
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

# 4. Running the CAPM Calculation for all of the DOW Years ----------------

minObs <- 12  # minimum monthly observations in the 5-year window

for (yr in DOW30Years) {
  list_name <- paste0('DOW', yr)
  year_list <- list()
  
  comps <- get(paste0('DOWcomps_', yr))   # <- constituents of THIS year
  for (i in seq_along(comps)) {
    currentComp     <- get(comps[i])
    currentCompName <- sub('_monthly$', '', comps[i])
    CAPMdata        <- FinalDataSet(currentComp)
    
    assign(paste0(currentCompName, '_CAPMData'), CAPMdata, envir = .GlobalEnv)
    
    # Skip regressions with too little data
    nObs <- sum(lubridate::year(CAPMdata$date) %in% (yr - 5):(yr - 1))
    
    capm <- if (nObs >= minObs) {
      tryCatch(CAPMFunction(CAPMdata, yr), error = function(e) NULL)
    } else {
      NULL
    }
    
    year_list[[currentCompName]] <- list(
      summary   = capm,
      intercept = if (is.null(capm)) NA_real_ else capm$coefficients['(Intercept)', 'Estimate'],
      SE        = if (is.null(capm)) NA_real_ else capm$coefficients['(Intercept)', 'Std. Error']
    )
  }
  assign(list_name, year_list, envir = .GlobalEnv)
}

for (yr in DOW30Years) {
  year_list <- get(paste0('DOW', yr))
  intercepts <- sapply(year_list, function(x) x$intercept)
  assign(paste0('DOW', yr), year_list[order(intercepts, decreasing = TRUE)], envir = .GlobalEnv)
}

# 5. Vanilla CAPM Ranking -------------------------------------------------

All_CAPM_Summary <- lapply(DOW30Years, function(yr) {
  year_list <- get(paste0('DOW', yr))
  
  data.frame(
    symbol    = names(year_list),
    alpha_hat = sapply(year_list, function(x) x$intercept),
    se        = sapply(year_list, function(x) x$SE),
    row.names = NULL)}) |>
  setNames(DOW30Years)

# exporting the results per year
writexl::write_xlsx(lapply(All_CAPM_Summary, head, 10), path = 'C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/Ranking Results/VanillaCAPM.xlsx')

# WORK IN PROGRESS --------------------------------------------------------

# WRONG CORR MATRIX?
# simulRank <- function(year){
#   df <- All_CAPM_Summary[[as.character(year)]]
#   covmat <- diag(df$se^2)
#   simRankInt <- csranks::csranks(
#     x = df$alpha_hat,
#     Sigma = covmat,
#     simul = TRUE
#   )
#   return(simRankInt)
# }

simulRank(2015)

CAPMcov <- function(yearList, num_months = 60){
  symbols     <- names(yearList)
  p           <- length(symbols)
  alpha_hat   <- sapply(yearList, function(x) x$intercept)
  se_vec      <- sapply(yearList, function(x) x$SE)
  resids_list <- lapply(yearList, function(x) residuals(x$summary))
  resids_mat  <- do.call(cbind, resids_list)
  colnames(resids_mat) <- symbols
  corr_mat <- cor(resids_mat)
  Sigma <- outer(se_vec, se_vec) * corr_mat
  list(
    df = data.frame(
      symbol    = symbols,
      alpha_hat = alpha_hat,
      se        = se_vec,
      stringsAsFactors = FALSE
    ),
    Sigma = Sigma
  )
}

capm_2025  <- CAPMcov(DOW2025)
df25       <- capm_2025$df
Sigma_2025 <- capm_2025$Sigma

simRankInt_2025 <- csranks::csranks(
  x     = df25$alpha_hat,
  Sigma = Sigma_2025,
  coverage = 0.99,
  simul = TRUE
)

tau10_idx_2025 <- csranks::cstaubest(
  x     = df25$alpha_hat,
  Sigma = Sigma_2025,
  tau   = 10,
  coverage = 0.99
)

tau10_stocks_2025 <- df25$symbol[tau10_idx_2025]


tau10_99 <- df25$symbol[csranks::cstaubest(x = df25$alpha_hat, Sigma = Sigma_2025, tau = 10, coverage = 0.99)]
tau10_95 <- df25$symbol[csranks::cstaubest(x = df25$alpha_hat, Sigma = Sigma_2025, tau = 10, coverage = 0.95)]
tau10_90 <- df25$symbol[csranks::cstaubest(x = df25$alpha_hat, Sigma = Sigma_2025, tau = 10, coverage = 0.90)]
tau10_80 <- df25$symbol[csranks::cstaubest(x = df25$alpha_hat, Sigma = Sigma_2025, tau = 10, coverage = 0.80)]
tau10_50 <- df25$symbol[csranks::cstaubest(x = df25$alpha_hat, Sigma = Sigma_2025, tau = 10, coverage = 0.50)]

cat("99% Coverage (alpha = 0.01):", length(tau10_99), "stocks\n")
cat("95% Coverage (alpha = 0.05):", length(tau10_95), "stocks\n")
cat("90% Coverage (alpha = 0.10):", length(tau10_90), "stocks\n")
cat("80% Coverage (alpha = 0.20):", length(tau10_80), "stocks\n")
cat("50% Coverage (alpha = 0.50):", length(tau10_50), "stocks\n")

