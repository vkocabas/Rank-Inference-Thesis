# Rank-Inference Thesis
# Nr.X Main Script
# Author: Venüs Kocabas
# Last Updated: 22.11.2026

# SETTING UNIVERSAL VARIABLES  --------------------------------------------

chosenCoverage <- 0.95
chosenhistAmount <- 2
chosenTau <- 10

# 0. Importing Libraries --------------------------------------------------

libraries <- c('dplyr', 'tidyr', 'readxl', 'writexl', 'lubridate', 'tidyquant', 'csranks', 'stringr')
missingLibs <- libraries[!(libraries %in% installed.packages()[, 'Package'])]
if (length(missingLibs) > 0) install.packages(missingLibs)
invisible(lapply(libraries, library, character.only = TRUE))

# ######################################################################### -----------------------------------------
# 1.A Helper Function: Extracting the DOW Years ---------------------------

# Extracting Index Companies
Dow30IndexNames <- function(DOWyear, excelPath, end = c("_daily", "_monthly")) {
  data <- readxl::read_xlsx(excelPath, sheet = DOWyear)
  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ]
  tickers <- unlist(lapply(data$Ticker, function(x) paste0(x, end)))
  return(tickers)
}

# 1.B Helper Function: DOW30 Initial Cleaning -----------------------------

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

# 1.C Helper Function: Monthly Data Points --------------------------------
  
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

# 1.D Helper Function: Daily Returns --------------------------------------

DailyReturns <- function(dataset, adjprcol) {
  dataset |>
    dplyr::arrange(dplyr::pick(dplyr::any_of(c("DATES", "date")))) |>
    dplyr::mutate(
      price_return       = {{ adjprcol }} / dplyr::lag({{ adjprcol }}) - 1,
      daily_return       = price_return, # (You can add daily dividend yield if needed, usually negligible daily)
      daily_log_return   = log1p(daily_return)
    ) |>
    dplyr::filter(!is.na(price_return))
}

# 1.E Helper Function: Adjusted Prices ------------------------------------

AddAdjustedPrice <- function(df, pattern = 'MARKET PRICE', new_col = 'AdjustedPrice') {
  price_cols <- grep(pattern, names(df), value = TRUE)
  if (length(price_cols) == 0) {
    df[[new_col]] <- NA_real_
    return(df)
  }
  df[[new_col]] <- do.call(dplyr::coalesce, unname(as.list(df[price_cols])))
  df
}

# 1.F Helper Function: Monthly Returns ------------------------------------

MonthlyReturns <- function(dataset, adjprcol) {
  dataset |>
    dplyr::mutate(
      monthly_return = ({{ adjprcol }} / dplyr::lag({{ adjprcol }})) - 1,
      monthly_log_return = log({{ adjprcol }} / dplyr::lag({{ adjprcol }}))) |>
    dplyr::filter(!is.na(monthly_return))
}

# 1.G Helper Function: Running Regressions --------------------------------

RunRegressions <- function(model = c("CAPM", "FF"),
                           freq = c("monthly", "daily"),
                           histAmount,
                           years = DOW30Years,
                           envir = .GlobalEnv) {
  
  model <- match.arg(model)
  freq  <- match.arg(freq)
  
  # _daily or _monthly
  suffix <- paste0("_", freq)
  
  # fixing DD DOW stuff
  dd_from <- paste0("DOW", suffix)
  dd_to   <- paste0("DDslot", suffix)
  
  prefix <- if (model == "CAPM") {
    paste0("CAPMResults", tools::toTitleCase(freq), "_")
  } else {
    paste0("FFResults", tools::toTitleCase(freq), "_")
  }
  
  for (yr in years) {
    comps <- get(paste0("DOWcomps_", yr, suffix), envir = envir)
    comps <- ifelse(comps == dd_from, dd_to, comps)
    yearList <- list()
    
    for (currentComp in comps) {
      currentCompName <- sub(paste0(suffix, "$"), "", currentComp)
      currentCompData <- get(currentComp, envir = envir)
      
      if (model == "CAPM") {
        reg_data <- if (freq == "monthly") {
          CAPMdatasetMONTH(currentCompData, DOWyear = yr, historicAmount = histAmount)
        } else {
          CAPMdatasetDAY(currentCompData, DOWyear = yr, historicAmount = histAmount)
        }
        check_cols <- c("excess_ret", "excess_mkt")
      } else {
        reg_data <- if (freq == "monthly") {
          FFdatasetMONTH(currentCompData, DOWyear = yr, historicAmount = histAmount)
        } else {
          FFdatasetDAY(currentCompData, DOWyear = yr, historicAmount = histAmount)
        }
        check_cols <- c("excess_ret", "excess_mkt", "smb", "hml")
      }
      
      complete_n <- nrow(stats::na.omit(reg_data[, check_cols]))
      message(
        "yr = ", yr,
        " | stock = ", currentCompName,
        " | complete rows = ", complete_n
      )
      
      reg_summary <- if (model == "CAPM") {
        CAPMFunction(reg_data, DOWyear = yr, historicAmount = histAmount)
      } else {
        FamaFrenchFunction(reg_data, DOWyear = yr, historicAmount = histAmount)
      }
      
      yearList[[currentCompName]] <- list(
        summary   = reg_summary,
        intercept = if (is.null(reg_summary)) NA_real_ else reg_summary$coefficients["(Intercept)", "Estimate"],
        SE        = if (is.null(reg_summary)) NA_real_ else reg_summary$coefficients["(Intercept)", "Std. Error"]
      )
    }
    
    assign(paste0(prefix, yr, "_", histAmount), yearList, envir = envir)
  }
}

# 1.H Helper Function: Vanilla Ranking ------------------------------------

RunVanillaRank <- function(model = c("CAPM", "FF"),
                           freq = c("monthly", "daily"),
                           histAmount,
                           years = DOW30Years,
                           envir = .GlobalEnv) {
  model <- match.arg(model)
  freq  <- match.arg(freq)
  
  freq_cap <- paste0(toupper(substr(freq, 1, 1)), substr(freq, 2, nchar(freq)))
  res_prefix  <- paste0(model, "Results", freq_cap, "_")
  rank_prefix <- paste0("VanillaRank", model, freq_cap, "_")
  
  for (yr in years) {
    res_list <- get(paste0(res_prefix, yr, "_", histAmount), envir = envir)
    comp_names <- names(res_list)
    
    # Map DDslot directly inside the loop based on yr
    comp_names <- ifelse(
      comp_names == "DDslot",
      if (yr >= 2019) "DOW" else "DD",
      comp_names
    )
    
    rank_df <- data.frame(
      year       = yr,
      comp       = comp_names,
      intercepts = unname(sapply(res_list, function(x) x$intercept)),
      stringsAsFactors = FALSE
    ) |>
      dplyr::arrange(dplyr::desc(intercepts)) |>
      dplyr::mutate(rank = dplyr::row_number())
    
    assign(paste0(rank_prefix, yr, "_", histAmount), rank_df, envir = envir)
  }
}

# 1.I Helper Function: 'csranks' Ranking ----------------------------------

RunCSRanks <- function(model = c("CAPM", "FF"),
                       freq = c("monthly", "daily"),
                       type = c('best', 'worst'),
                       histAmount = chosenhistAmount,
                       tau,
                       coverage = chosenCoverage,
                       stepdown = TRUE,
                       R = 1000,
                       years = DOW30Years,
                       seed = 123,
                       envir = .GlobalEnv) {
  
  model <- match.arg(model)
  freq  <- match.arg(freq)
  
  freq_cap    <- tools::toTitleCase(freq)
  res_prefix  <- paste0(model, "Results", freq_cap, "_")
  rank_prefix <- paste0("CSRank", model, freq_cap, "_tau", tau, "_")
  
  if (!is.null(seed)) set.seed(seed)
  
  for (yr in years) {
    res_list <- get(paste0(res_prefix, yr, "_", histAmount), envir = envir)
    
    alphas <- unname(sapply(res_list, function(x) x$intercept))
    ses    <- unname(sapply(res_list, function(x) x$SE))
    valid  <- !is.na(alphas) & !is.na(ses)
    
    # Extract residuals only for valid regressions
    res_list_valid <- res_list[valid]
    res_mat <- sapply(res_list_valid, function(x) resid(x$summary))
    
    if (!is.matrix(res_mat)) {
      stop("Residual lengths differ across stocks in year ", yr,
           ". Ensure all stocks share the exact same date window.")
    }
    
    Sigma <- outer(ses[valid], ses[valid]) * cor(res_mat)
    
    
    CSFUN <- if (type == "best") cstaubest else cstauworst
    
    in_top_tau_valid <- CSFUN(
      x        = alphas[valid],
      Sigma    = Sigma,
      tau      = tau,
      coverage = coverage,
      stepdown = stepdown,
      R        = R
    )
    
    in_top_tau <- rep(NA, length(alphas))
    in_top_tau[valid] <- as.logical(in_top_tau_valid)
    
    comp_names <- ifelse(
      names(res_list) == "DDslot",
      if (yr >= 2019) "DOW" else "DD",
      names(res_list)
    )
    
    rank_df <- data.frame(
      year        = yr,
      comp        = comp_names,
      intercepts  = alphas,
      SE          = ses,
      in_top_tau  = in_top_tau,
      stringsAsFactors = FALSE
    ) |>
      dplyr::arrange(dplyr::desc(intercepts)) |>
      dplyr::mutate(rank = dplyr::if_else(is.na(intercepts), NA_integer_, dplyr::row_number()))
    
    assign(paste0(rank_prefix, yr, "_", histAmount), rank_df, envir = envir)
  }
}


# ######################################################################### -----------------------------------------
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
  dplyr::select(c(date, x1.mo))

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

# Daily DOW
# Creating the Lists for DOW years
for(i in seq_along(DOW30Years)){
  name <- paste0('DOWcomps_', DOW30Years[i], '_daily')
  df <- Dow30IndexNames(as.character(DOW30Years[i]), DoDdataPath, end = '_daily') 
  df <- sub("^DD_daily$", "DOW_daily", df)
  assign(name, df, envir = .GlobalEnv)
  message('Run #', i, ': DoD dataset created -> ', name)
}

# Monthly DOW
# Creating the Lists for DOW years
for(i in seq_along(DOW30Years)){
  name <- paste0('DOWcomps_', DOW30Years[i], '_monthly')
  df <- Dow30IndexNames(as.character(DOW30Years[i]), DoDdataPath, end = '_monthly') 
  df <- sub("^DD_monthly$", "DOW_monthly", df)
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

SPY <- tidyquant::tq_get('SPY', get = 'stock.prices', from = '1999-12-01', to = '2025-01-31')

SPY_daily <- SPY |>
  dplyr::arrange(date) |>
  dplyr::mutate(AdjustedPrice = adjusted) |>
  DailyReturns(adjprcol = AdjustedPrice)

SPY_monthly <- SPY |>
  dplyr::arrange(date) |>
  dplyr::mutate(AdjustedPrice = adjusted) |>
  lastTrade(datecol = date) |>
  MonthlyReturns(adjprcol = AdjustedPrice)
# 2.D Adding the Fama-French Data set -------------------------------------

FFMONTH <- read.csv("C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/Fama French/F-F_Research_Data_Factors.csv", skip = 4) |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(date_raw = x) |>
  dplyr::mutate(date_raw = trimws(as.character(date_raw))) |>
  # monthly rows are YYYYMM; this drops the annual table and the footer
  dplyr::filter(nchar(date_raw) == 6) |>
  dplyr::mutate(
    ym     = as.Date(paste0(date_raw, '01'), format = '%Y%m%d'),
    mkt_rf = as.numeric(mkt.rf) / 100,
    smb    = as.numeric(smb) / 100,
    hml    = as.numeric(hml) / 100,
    rf     = as.numeric(rf) / 100) |>
  dplyr::select(ym, mkt_rf, smb, hml, rf) |>
  dplyr::filter(ym >= as.Date('2000-01-01') & ym <= as.Date('2025-01-31'))

FFDAY <- read.csv("C:/Users/user/Desktop/THESIS/Rank-Inference-Thesis/DoD data/Fama French/F-F_Research_Data_Factors_daily.csv", skip = 4) |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(date_raw = x) |>
  dplyr::mutate(date_raw = trimws(as.character(date_raw))) |>
  # daily rows are YYYYMMDD; this drops the footer
  dplyr::filter(nchar(date_raw) == 8) |>
  dplyr::mutate(
    date   = as.Date(date_raw, format = '%Y%m%d'),
    mkt_rf = as.numeric(mkt.rf) / 100,
    smb    = as.numeric(smb) / 100,
    hml    = as.numeric(hml) / 100,
    rf     = as.numeric(rf) / 100) |>
  dplyr::select(date, mkt_rf, smb, hml, rf) |>
  dplyr::filter(date >= as.Date('2000-01-01') & date <= as.Date('2025-01-31'))

# 2.D Additional Individual Adjustments for the DOW30 Data Sets -----------

# DOW ADJUSTMENT
dd1_last  <- DD1 |> dplyr::filter(DATES == as.Date('2017-08-31')) |> dplyr::pull(2)
dd2_first <- DD2 |> dplyr::filter(DATES == as.Date('2017-09-01')) |> dplyr::pull(2)
k1 <- dd2_first / dd1_last      # about 1.429

# Join 2: DD2 (DowDuPont) -> DOW (Dow Inc), switch on 2 Apr 2019.
# Both exist on 1 Apr 2019, so match their levels on that day.
dd2_apr1 <- DD2 |> dplyr::filter(DATES == as.Date('2019-04-01')) |> dplyr::pull(2)
dow_apr1 <- DOW |> dplyr::filter(DATES == as.Date('2019-04-01')) |> dplyr::pull(2)
k2 <- dd2_apr1 / dow_apr1       # about 1.816

DDslot <- DD1 |>
  dplyr::filter(DATES < as.Date('2017-09-01')) |>
  dplyr::mutate(dplyr::across(c(2, 4, 6:17), ~ .x * k1)) |>
  stats::setNames(names(DD2)) |>
  dplyr::bind_rows(
    DD2 |> dplyr::filter(DATES >= as.Date('2017-09-01') & DATES <= as.Date('2019-04-01'))
  ) |>
  dplyr::bind_rows(
    DOW |>
      dplyr::filter(DATES >= as.Date('2019-04-02')) |>
      dplyr::mutate(dplyr::across(c(2, 4, 6:17), ~ .x * k2)) |>
      stats::setNames(names(DD2))
  ) |>
  dplyr::arrange(DATES)

# 2.E Creating Monthly & Daily Data Sets  ----------------------------------

# added SPY for convenience 
DOWTotal <- c(setdiff(neededCompanies$Tickers, c('DD1','DD2','DOW')), 'DDslot')
# not needed anymore can be taken out
rm(DD1, DD2, DOW)

for(i in seq_along(DOWTotal)){
  compData <- get(DOWTotal[i])
  compDatamonthly <- compData |>
    AddAdjustedPrice(pattern = 'MARKET PRICE') |>
    lastTrade(datecol = 'DATES') |>
    MonthlyReturns(adjprcol = AdjustedPrice)
  assign(paste0(DOWTotal[i],'_monthly'), compDatamonthly, envir = .GlobalEnv)
}

for(i in seq_along(DOWTotal)){
  compData <- get(DOWTotal[i])
  compDatadaily <- compData |>
    AddAdjustedPrice() |> 
    DailyReturns(adjprcol = AdjustedPrice)
  assign(paste0(DOWTotal[i], '_daily'), compDatadaily, envir = .GlobalEnv)
}

# ######################################################################### -----------------------------------------
# 3.A CAPM data set -------------------------------------------------------

CAPMdatasetMONTH <- function(stockData, marketData = SPY_monthly, riskFreeData = UsTreasuryData, DOWyear = NULL, historicAmount = NULL) {
  stock <- stockData |>
    dplyr::transmute(
      ym            = lubridate::floor_date(DATES, "month"),
      date          = DATES,
      stock_ret     = monthly_return,
      stock_log_ret = monthly_log_return
    )
  
  mkt <- marketData |>
    dplyr::transmute(
      ym          = lubridate::floor_date(date, "month"),
      mkt_ret     = monthly_return,
      mkt_log_ret = monthly_log_return
    )
  
  # Risk-free: one value per month (last available day), lagged one month,
  # so the return in month t uses the rate known at the end of month t-1
  rf <- riskFreeData |>
    dplyr::filter(!is.na(x1.mo)) |>
    dplyr::mutate(ym = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(ym) |>
    dplyr::slice_max(date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      ym = ym %m+% months(1),
      rf = (1 + x1.mo / 100)^(1 / 12) - 1
    )
  
  out <- stock |>
    dplyr::inner_join(mkt, by = "ym") |>
    dplyr::inner_join(rf,  by = "ym") |>
    dplyr::mutate(
      excess_ret = stock_ret - rf,
      excess_mkt = mkt_ret   - rf
    ) |>
    dplyr::select(-ym)
  
  if (!is.null(DOWyear) && !is.null(historicAmount)) {
    years <- (DOWyear - historicAmount):(DOWyear - 1)
    out <- dplyr::filter(out, lubridate::year(date) %in% years)
  }
  
  out
}

CAPMdatasetDAY <- function(stockData, marketData = SPY_daily, riskFreeData = UsTreasuryData, DOWyear = NULL, historicAmount = NULL) {
  stock <- stockData |>
    dplyr::transmute(
      date          = as.Date(DATES),
      stock_ret     = daily_return,
      stock_log_ret = daily_log_return
    )
  
  mkt <- marketData |>
    dplyr::transmute(
      date        = as.Date(date),
      mkt_ret     = daily_return,
      mkt_log_ret = daily_log_return
    )
  
  rf <- riskFreeData |>
    dplyr::filter(!is.na(x1.mo)) |>
    dplyr::transmute(
      date = as.Date(date),
      rf   = (1 + x1.mo / 100)^(1 / 252) - 1
    )
  
  out <- stock |>
    dplyr::inner_join(mkt, by = "date") |>
    dplyr::left_join(rf, by = "date") |>
    dplyr::arrange(date) |>
    tidyr::fill(rf, .direction = "down") |>
    dplyr::mutate(
      excess_ret = stock_ret - rf,
      excess_mkt = mkt_ret   - rf
    )
  
  if (!is.null(DOWyear) && !is.null(historicAmount)) {
    years <- (DOWyear - historicAmount):(DOWyear - 1)
    out <- dplyr::filter(out, lubridate::year(date) %in% years)
  }
  
  out
}

# 3.B Fama-French data set ------------------------------------------------

FFdatasetMONTH <- function(stockData, ffData = FFMONTH, DOWyear = NULL, historicAmount = NULL) {
  stock <- stockData |>
    dplyr::transmute(
      ym            = lubridate::floor_date(DATES, 'month'),
      date          = DATES,
      stock_ret     = monthly_return,
      stock_log_ret = monthly_log_return
    )
  
  # No lag here: the FF rf is the return over month t itself
  out <- stock |>
    dplyr::inner_join(ffData, by = 'ym') |>
    dplyr::mutate(
      excess_ret = stock_ret - rf,
      excess_mkt = mkt_rf
    ) |>
    dplyr::select(-ym)
  
  if (!is.null(DOWyear) && !is.null(historicAmount)) {
    years <- (DOWyear - historicAmount):(DOWyear - 1)
    out <- dplyr::filter(out, lubridate::year(date) %in% years)
  }
  out
}

FFdatasetDAY <- function(stockData, ffData = FFDAY, DOWyear = NULL, historicAmount = NULL) {
  stock <- stockData |>
    dplyr::transmute(
      date          = as.Date(DATES),
      stock_ret     = daily_return,
      stock_log_ret = daily_log_return
    )
  
  out <- stock |>
    dplyr::inner_join(ffData, by = 'date') |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      excess_ret = stock_ret - rf,
      excess_mkt = mkt_rf
    )
  
  if (!is.null(DOWyear) && !is.null(historicAmount)) {
    years <- (DOWyear - historicAmount):(DOWyear - 1)
    out <- dplyr::filter(out, lubridate::year(date) %in% years)
  }
  out
}

# ######################################################################### -----------------------------------------
# 4.A CAPM Function -------------------------------------------------------

CAPMFunction <- function(dataset, DOWyear = NULL, historicAmount = NULL) {
  if (!is.null(DOWyear) && !is.null(historicAmount)) {
    years <- (DOWyear - historicAmount):(DOWyear - 1)
    dataset <- dplyr::filter(dataset, lubridate::year(date) %in% years)
  }
  lm(excess_ret ~ excess_mkt, data = dataset) |> summary()
}

# 4.B Fama-French Function ------------------------------------------------

FamaFrenchFunction <- function(dataset, DOWyear = NULL, historicAmount = NULL){
  if (!is.null(DOWyear) && !is.null(historicAmount)) {
    years <- (DOWyear - historicAmount):(DOWyear - 1)
    dataset <- dplyr::filter(dataset, lubridate::year(date) %in% years)
  }
  lm(excess_ret ~ excess_mkt + smb + hml, data = dataset) |> summary()
}

# ######################################################################### -----------------------------------------
# 5. Running Regressions for CAPM and Fama-French -------------------------

# CAPM Daily
RunRegressions(model = 'CAPM',
               freq = 'daily',
               chosenhistAmount,
               years = DOW30Years)

# CAPM Monthly
RunRegressions(model = 'CAPM',
               freq = 'monthly',
               chosenhistAmount,
               years = DOW30Years)

# Fama-French Daily
RunRegressions(model = 'FF',
               freq = 'daily',
               chosenhistAmount,
               years = DOW30Years)

# Fama-French Monthly
RunRegressions(model = 'FF',
               freq = 'monthly',
               chosenhistAmount,
               years = DOW30Years)

# ######################################################################### -----------------------------------------
# 6. Vanilla Ranking for CAPM and Fama-French -----------------------------

# CAPM Daily
RunVanillaRank(model = 'CAPM',
               freq = 'daily',
               chosenhistAmount,
               years = DOW30Years)

# CAPM Monthly
RunVanillaRank(model = 'CAPM',
               freq = 'monthly',
               chosenhistAmount,
               years = DOW30Years)

# Fama-French Daily
RunVanillaRank(model = 'FF',
               freq = 'daily',
               chosenhistAmount,
               years = DOW30Years)

# Fama-French Monthly
RunVanillaRank(model = 'FF',
               freq = 'monthly',
               chosenhistAmount,
               years = DOW30Years)

# ######################################################################### -----------------------------------------
# 7. csranks Ranking Base -------------------------------------------------

# CAPM Daily
RunCSRanks(model = 'CAPM',
           freq = 'daily',
           type = 'best',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)

# CAPM Monthly
RunCSRanks(model = 'CAPM',
           freq = 'monthly',
           type = 'best',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)

# Fama-French Daily
RunCSRanks(model = 'FF',
           freq = 'daily',
           type = 'best',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)

# Fama-French Monthly
RunCSRanks(model = 'FF',
           freq = 'monthly',
           type = 'best',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)

# ######################################################################### -----------------------------------------
# 8.A Alternative Approach: 'csranks' Worst Tau ---------------------------

# CAPM Daily
RunCSRanks(model = 'CAPM',
           freq = 'daily',
           type = 'worst',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)

# CAPM Monthly
RunCSRanks(model = 'CAPM',
           freq = 'monthly',
           type = 'worst',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)

# Fama-French Daily
RunCSRanks(model = 'FF',
           freq = 'daily',
           type = 'worst',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)

# Fama-French Monthly
RunCSRanks(model = 'FF',
           freq = 'monthly',
           type = 'worst',
           chosenhistAmount,
           chosenTau,
           chosenCoverage,
           years = DOW30Years)




