# Rank-Inference Thesis
# Nr.X Main Script
# Author: Venüs Kocabas
# Last Updated: 31.10.2026

# 0. Importing Libraries --------------------------------------------------

libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl', 'lubridate',
               'tidyquant', 'csranks', 'stringr')
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

# Use the adjusted daily price (2nd column) instead of the unadjusted month-close columns
AddAdjustedPrice <- function(df, new_col = 'AdjustedPrice') {
  df[[new_col]] <- df[[2]]          # the (P) column
  df
}

MonthlyReturns <- function(dataset, adjprcol) {
  dy_col <- grep('DIVIDEND YIELD$', names(dataset), value = TRUE)
  
  # approx. monthly dividend return from the previous month-end yield;
  # 0 if there is no yield column (SPY adjclose is already total return)
  dataset$dy_prev <- if (length(dy_col) == 1) {
    dplyr::coalesce(dplyr::lag(dataset[[dy_col]]), 0) / 100 / 12
  } else {
    0
  }
  
  dataset |>
    dplyr::mutate(
      price_return       = {{ adjprcol }} / dplyr::lag({{ adjprcol }}) - 1,
      monthly_return     = price_return + dy_prev,
      monthly_log_return = log1p(monthly_return)) |>
    dplyr::filter(!is.na(price_return))
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

SPY_raw <- tidyquant::tq_get('SPY', get = 'stock.prices',
                             from = '1999-12-01', to = '2025-01-31')

SPY_monthly <- lastTrade(SPY_raw, datecol = date) |>
  MonthlyReturns(adjprcol = adjusted)      # Yahoo's 'adjusted' includes dividends

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

# Helper Function for Daily Returns
DailyReturns <- function(dataset, adjprcol) {
  dataset |>
    dplyr::arrange(DATES) |>
    dplyr::mutate(
      price_return       = {{ adjprcol }} / dplyr::lag({{ adjprcol }}) - 1,
      daily_return       = price_return, # (You can add daily dividend yield if needed, usually negligible daily)
      daily_log_return   = log1p(daily_return)
    ) |>
    dplyr::filter(!is.na(price_return))
}

# Create Daily Stock Datasets (replacing or running alongside monthly)
DOWTotal <- c(setdiff(neededCompanies$Tickers, c('DD1', 'DD2')), 'DD')

for(i in seq_along(DOWTotal)){
  compData <- get(DOWTotal[i])
  compDatadaily <- compData |>
    AddAdjustedPrice() |> 
    DailyReturns(adjprcol = AdjustedPrice)
  
  assign(paste0(DOWTotal[i], '_daily'), compDatadaily, envir = .GlobalEnv)
}

# 2.F Creating the Fama-French 3-Factor Daily Dataset ---------------------
FF3_daily <- read.csv("C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/Fama French/F-F_Research_Data_Factors.csv", skip = 4) |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(date_raw = x) |>
  dplyr::filter(!is.na(mkt.rf) & date_raw != "") |>
  dplyr::mutate(
    # Daily Kenneth French dates are usually formatted as YYYYMMDD
    date   = as.Date(as.character(date_raw), format = "%Y%m%d"),
    mkt_rf = as.numeric(mkt.rf) / 100,
    smb    = as.numeric(smb) / 100,
    hml    = as.numeric(hml) / 100,
    rf     = as.numeric(rf) / 100
  ) |>
  dplyr::select(date, mkt_rf, smb, hml, rf) |>
  dplyr::filter(date >= as.Date('2000-01-01') & date <= as.Date('2025-01-31'))

# 3.A Daily FF3 Dataset Merger
FF3Dataset_daily <- function(stockData, ffData = FF3_daily) {
  stock <- stockData |>
    dplyr::transmute(
      date       = DATES,
      stock_ret  = daily_return
    )
  
  stock |>
    dplyr::inner_join(ffData, by = 'date') |>
    dplyr::mutate(
      excess_ret = stock_ret - rf
    )
}

# 3.B Daily FF3 Regression Function (e.g., 3-year rolling window: yr - 3 to yr - 1)
FF3Function_daily <- function(dataset, DOWyear) {
  start_year <- DOWyear - 3
  end_year   <- DOWyear - 1
  
  filteredDataset <- dataset |> 
    dplyr::filter(lubridate::year(date) >= start_year & lubridate::year(date) <= end_year)
  
  FF3_model <- lm(excess_ret ~ mkt_rf + smb + hml, data = filteredDataset) |> summary()
  return(FF3_model)
}

# 4. Running Daily Fama-French 3-Factor Calculations (Safe Version) ---------
minObsDaily <- 200  # minimum trading days in the estimation window

for (yr in DOW30Years) {
  list_name <- paste0('DOW_FF3_daily_', yr)
  year_list <- list()
  
  comps <- get(paste0('DOWcomps_', yr)) 
  for (i in seq_along(comps)) {
    comp_obj_name <- sub('_monthly$', '_daily', comps[i])
    if (!exists(comp_obj_name)) next
    
    currentComp     <- get(comp_obj_name)
    currentCompName <- sub('_daily$', '', comp_obj_name)
    
    ff3Data <- FF3Dataset_daily(currentComp)
    
    # Safely ensure date is a Date object and remove NAs
    ff3Data$date <- as.Date(ff3Data$date)
    ff3Data <- ff3Data[!is.na(ff3Data$date), ]
    
    # Count observations in the 3-year rolling window safely
    start_dt <- as.Date(paste0(yr - 3, "-01-01"))
    end_dt   <- as.Date(paste0(yr - 1, "-12-31"))
    
    nObs <- sum(ff3Data$date >= start_dt & ff3Data$date <= end_dt, na.rm = TRUE)
    
    ff3_res <- if (nObs >= minObsDaily) {
      tryCatch(FF3Function_daily(ff3Data, yr), error = function(e) NULL)
    } else {
      NULL
    }
    
    year_list[[currentCompName]] <- list(
      summary   = ff3_res,
      intercept = if (is.null(ff3_res)) NA_real_ else ff3_res$coefficients['(Intercept)', 'Estimate'],
      SE        = if (is.null(ff3_res)) NA_real_ else ff3_res$coefficients['(Intercept)', 'Std. Error']
    )
  }
  assign(list_name, year_list, envir = .GlobalEnv)
  message("Daily FF3 lists created for year: ", yr)
}
# 5. Exporting Fama-French Alpha Summary ----------------------------------

All_FF3_Summary <- lapply(DOW30Years, function(yr) {
  year_list <- get(paste0('DOW_FF3_daily_', yr))
  
  data.frame(
    symbol    = names(year_list),
    alpha_hat = sapply(year_list, function(x) x$intercept),
    se        = sapply(year_list, function(x) x$SE),
    row.names = NULL)}) |>
  setNames(DOW30Years)

writexl::write_xlsx(lapply(All_FF3_Summary, head, 10), path = 'C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/Ranking Results/FamaFrench3Factor.xlsx')

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

# simulRank(2015)

FF3cov <- function(yearList) {
  
  # Keep only stocks with a successful regression
  valid_list <- Filter(
    function(x) !is.null(x$summary),
    yearList
  )
  
  # Stop if nothing is valid
  if (length(valid_list) == 0) {
    stop("No valid regression models found in yearList.")
  }
  
  symbols   <- names(valid_list)
  
  alpha_hat <- sapply(
    valid_list,
    function(x) x$intercept
  )
  
  se_vec <- sapply(
    valid_list,
    function(x) x$SE
  )
  
  resids_list <- lapply(
    valid_list,
    function(x) residuals(x$summary)
  )
  
  resids_mat <- do.call(cbind, resids_list)
  
  # Make sure it is a matrix even if only one stock exists
  resids_mat <- as.matrix(resids_mat)
  
  colnames(resids_mat) <- symbols
  
  # Correlation matrix of regression residuals
  corr_mat <- cor(
    resids_mat,
    use = "pairwise.complete.obs"
  )
  
  # Covariance matrix of alpha estimates
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

# Point this to DOW_FF3_2025 instead of DOW2025
ff3_2025   <- FF3cov(DOW_FF3_daily_2025)
df25       <- ff3_2025$df
Sigma_2025 <- ff3_2025$Sigma

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

# 6. INSPECTION R SQUARE + ADJ R SQUARE (Fama-French Version) ----------

R2Inspection <- function(model_list) {
  res_matrix <- sapply(model_list, function(item) {
    # Guard against null summaries in case any stock failed completely
    if(is.null(item$summary)) {
      return(c(R_Squared = NA_real_, Adj_R_Squared = NA_real_))
    }
    c(R_Squared     = item$summary$r.squared,
      Adj_R_Squared = item$summary$adj.r.squared)
  })
  df <- as.data.frame(t(res_matrix))
  df$Ticker <- rownames(df)
  rownames(df) <- NULL
  df <- df[, c("Ticker", "R_Squared", "Adj_R_Squared")]
  return(df)
}

# Bind using the new DOW_FF3_ year lists
TEST <- rbind(
  cbind(year = '2015', R2Inspection(DOW_FF3_2015)),
  cbind(year = '2016', R2Inspection(DOW_FF3_2016)),
  cbind(year = '2017', R2Inspection(DOW_FF3_2017)),
  cbind(year = '2018', R2Inspection(DOW_FF3_2018)),
  cbind(year = '2019', R2Inspection(DOW_FF3_2019)),
  # cbind(year = '2020', R2Inspection(DOW_FF3_2020)), # Uncomment if 2020 is fixed
  cbind(year = '2021', R2Inspection(DOW_FF3_2021)),
  cbind(year = '2022', R2Inspection(DOW_FF3_2022)),
  cbind(year = '2023', R2Inspection(DOW_FF3_2023)),
  cbind(year = '2024', R2Inspection(DOW_FF3_2024)),
  cbind(year = '2025', R2Inspection(DOW_FF3_2025))
)

# Quick stats check on your new Fama-French R-squareds
print(TEST[which.max(TEST$Adj_R_Squared), ])
print(TEST[which.min(TEST$Adj_R_Squared), ])
print(paste("Mean Adjusted R-Squared:", mean(TEST$Adj_R_Squared, na.rm = TRUE)))

hist(TEST$Adj_R_Squared, 
     breaks = 10, 
     col = "darkgray", 
     main = "Histogram of FF3 Adjusted R-Squared", 
     xlab = "Adjusted R-Squared")




# =========================================================================
# 7. DAILY FAMA-FRENCH RANK BOUNDS & TAU-BEST SETS --------------------------
# =========================================================================

# Safe daily covariance helper that drops NULL models
FF3cov_safe_daily <- function(yearList) {
  valid_list  <- Filter(function(x) !is.null(x$summary), yearList)
  symbols     <- names(valid_list)
  alpha_hat   <- sapply(valid_list, function(x) x$intercept)
  se_vec      <- sapply(valid_list, function(x) x$SE)
  resids_list <- lapply(valid_list, function(x) residuals(x$summary))
  resids_mat  <- do.call(cbind, resids_list)
  colnames(resids_mat) <- symbols
  
  corr_mat <- cor(resids_mat, use = "pairwise.complete.obs")
  Sigma    <- outer(se_vec, se_vec) * corr_mat
  
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

# Print daily simultaneous tau-best sets per year (90% coverage)
for (yr in DOW30Years) {
  list_name <- paste0('DOW_FF3_daily_', yr)
  if (!exists(list_name)) {
    message("Skipping (not found): ", list_name)
    next
  }
  
  cov_data <- FF3cov_safe_daily(get(list_name))
  df       <- cov_data$df
  Sigma    <- cov_data$Sigma
  
  if (nrow(df) == 0 || all(is.na(Sigma))) next
  
  top3_idx  <- suppressWarnings(csranks::cstaubest(x = df$alpha_hat, Sigma = Sigma, tau = 3,  coverage = 0.90))
  top5_idx  <- suppressWarnings(csranks::cstaubest(x = df$alpha_hat, Sigma = Sigma, tau = 5,  coverage = 0.90))
  top10_idx <- suppressWarnings(csranks::cstaubest(x = df$alpha_hat, Sigma = Sigma, tau = 10, coverage = 0.90))
  
  cat("\n==================================================\n")
  cat(" DAILY FF3 YEAR:", yr, "(N =", nrow(df), "stocks)\n")
  cat("==================================================\n")
  cat(sprintf("Top-3  (90%%) [%2d stocks]: %s\n", length(top3_idx),  paste(df$symbol[top3_idx],  collapse = ", ")))
  cat(sprintf("Top-5  (90%%) [%2d stocks]: %s\n", length(top5_idx),  paste(df$symbol[top5_idx],  collapse = ", ")))
  cat(sprintf("Top-10 (90%%) [%2d stocks]: %s\n", length(top10_idx), paste(df$symbol[top10_idx], collapse = ", ")))
}