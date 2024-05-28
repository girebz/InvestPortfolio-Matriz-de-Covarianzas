library(shiny)
library(tidyquant)
library(rvest)
library(stringr)
library(DT)
library(tidyr)
library(dplyr)
library(httr)
library(jsonlite)
library(ggplot2)

# Función para cargar la base de datos desde un archivo CSV
load_portfolio <- function(file = "portfolio.csv") {
  if (file.exists(file)) {
    return(read.csv(file, stringsAsFactors = FALSE))
  } else {
    return(data.frame(Ticker = character(), Acciones = numeric(), Precio = numeric(), Divisa = character(), 
                      Tipo = character(), PE_Ratio = numeric(), Dividend_Yield = numeric(), 
                      Beta = numeric(), Volume = numeric(), EPS = numeric(), PB_Ratio = numeric(), 
                      stringsAsFactors = FALSE))
  }
}

# Función para guardar la base de datos en un archivo CSV
save_portfolio <- function(portfolio, file = "portfolio.csv") {
  write.csv(portfolio, file, row.names = FALSE)
}

# Listas de tickers conocidos para cada tipo de instrumento
known_etfs <- c("DGRO","URA", "ICLN", "SPY", "IVV", "VOO", "QQQ", "DIA", "IWM", "EFA", "EEM", "VWO", "VNQ", "VTI", "VEA", "IEFA", "AGG", "IJR", "IJH", "VUG", "VTV", "VO", "VB", "BND", "BNDX", "VCIT", "VNQI", "VYM", "IEMG", "VEU", "VT", "XLK", "XLF", "XLE", "XLV", "XLY", "XLP", "XLI", "XLU", "XLB", "XLRE", "IYR", "LQD", "HYG", "TIP", "JNK", "BIV", "BSCJ", "BWX", "PCY", "MBB", "SCHB", "SCHX", "SCHF", "SCHV", "SCHG", "SCHA", "SCHD", "SCHZ", "SDY", "DGRW", "USMV", "IVW", "IWF", "IWD", "IWN", "IWO", "IWB", "SHY", "IEF", "TLT", "GLD", "SLV", "IAU", "FXI", "EWZ", "EWW", "EWA", "EWC", "EWJ", "EWQ", "EWD", "EWL", "EWU", "EWS", "EWH", "EWT", "EWI", "EWG", "SCZ", "VXUS", "VSS", "VGK", "VPL", "VOE", "VBK", "VBR", "VTWO", "VTHR", "BSV", "BNDV", "BLV", "SHV", "VMBS", "VMAT", "ESGV", "SPLG", "ONEQ", "FDIS", "FSTA", "FXR", "FUTY", "FNCL", "FREL", "FMAT", "FUTY", "FHLC", "FENY", "SPYX", "XBI", "SMH", "FDN", "ARKK", "ARKW", "ARKQ", "ARKG", "ARKF", "PRNT", "IZRL", "PSYK", "SPGP", "SPTM", "SPYV", "SPYG", "SPLV", "SPMD", "SPSM", "SPBO", "SPTL", "SPTM", "SPTS", "SPIP", "SPMB", "SPLB", "SPAB", "SPHY", "SPXB", "SPMO", "SPMV", "SPFM", "SPMD", "SPYD", "SPHD", "SPHQ", "SPSB", "SPDW", "SPEU", "SPTS", "SPTL", "SPIB", "SPLG", "SPIP", "SPTS", "SPTL", "SPDW", "SPHB", "SPMO", "SPYD", "SPYG", "SPYV", "SPTM", "SPLV", "SPHD", "SPHQ", "SPMV", "SPFM")

known_cryptos <- c("BTC", "ETH", "ADA", "XRP", "LTC", "BCH", "BNB", "DOT", "DOGE", "SOL", "USDT", "LINK", "XLM", "USDC", "UNI", "AAVE", "ATOM", "AVAX", "BSV", "CRO", "DAI", "DASH", "EOS", "ETC", "FIL", "KSM", "MKR", "NEO", "TRX", "VET", "WBTC", "XTZ", "YFI", "ZEC")

# Map from ticker to CoinGecko id
crypto_id_map <- c("BTC" = "bitcoin", "ETH" = "ethereum", "ADA" = "cardano", "XRP" = "ripple", 
                   "LTC" = "litecoin", "BCH" = "bitcoin-cash", "BNB" = "binancecoin", 
                   "DOT" = "polkadot", "DOGE" = "dogecoin", "SOL" = "solana", 
                   "USDT" = "tether", "LINK" = "chainlink", "XLM" = "stellar", 
                   "USDC" = "usd-coin", "UNI" = "uniswap", "AAVE" = "aave", 
                   "ATOM" = "cosmos", "AVAX" = "avalanche-2", "BSV" = "bitcoin-cash-sv", 
                   "CRO" = "crypto-com-chain", "DAI" = "dai", "DASH" = "dash", 
                   "EOS" = "eos", "ETC" = "ethereum-classic", "FIL" = "filecoin", 
                   "KSM" = "kusama", "MKR" = "maker", "NEO" = "neo", "TRX" = "tron", 
                   "VET" = "vechain", "WBTC" = "wrapped-bitcoin", "XTZ" = "tezos", 
                   "YFI" = "yearn-finance", "ZEC" = "zcash")

known_stocks <- c("TM","GOLD", "ASML","AAPL", "GOOGL", "MSFT", "AMZN", "TSLA", "FB", "BRK.B", "JNJ", "WMT", "V", "JPM", "PG", "NVDA", "DIS", "MA", "PYPL", "NFLX", "KO", "PFE", "INTC", "CSCO", "PEP", "MRK", "T", "VZ", "BAC", "XOM", "CVX", "ABT", "ABBV", "CRM", "MCD", "COST", "NKE", "UNH", "ADBE", "ORCL", "IBM", "WBA", "MDT", "ACN", "HON", "AVGO", "LLY", "QCOM", "TXN", "UPS", "PM", "NEE", "AMGN", "BA", "LIN", "RTX", "BMY", "UNP", "GILD", "DHR", "TMO", "LOW", "INTU", "CAT", "LMT", "SPGI", "GE", "MMM", "SBUX", "MS", "NOW", "FIS", "ZTS", "MDLZ", "AXP", "BKNG", "ISRG", "SYK", "CL", "DE", "MO", "CCI", "ICE", "PLD", "NSC", "CB", "USB", "SCHW", "GS", "ANTM", "APD", "TGT", "TJX", "BDX", "CME", "SO", "PNC", "ITW", "DUK", "GM", "AON", "WM", "ETN", "ECL", "COP", "ADI", "EOG", "CI", "NOC", "ADM", "BSX", "REGN", "ROP", "CDNS", "FDX", "TRV", "VRTX", "DLR", "PSA", "ROP", "HUM", "AEP", "MAR", "CMG", "VRSK", "MSCI", "AIG", "MNST", "EA", "ILMN", "AFL", "MCO", "WLTW", "SPG", "CTSH", "PH", "FTNT", "HCA", "D", "BAX", "KHC", "EQR", "STZ", "IQV", "TWTR", "MPC", "PAYX", "ROK", "IDXX", "YUM", "HLT", "BIIB", "STT", "WMB", "EBAY", "SBAC", "BBY", "PCAR", "PSX", "FRC", "ROST", "AME", "CTAS", "MTD", "ES", "ARE", "TROW", "AEE", "MKC", "TER", "RMD", "PPG", "EXC", "EXPE", "AZO", "VFC", "PEG", "DLTR", "ALXN", "ANSS", "ATO", "ODFL", "RSG", "TSN", "WAT", "VLO", "IR", "FLT", "KEYS", "GWW", "TDG", "WEC", "TSCO", "PWR", "TT", "FAST", "ED", "LHX", "OMC", "MLM", "EIX", "MTCH", "ODFL", "CMS", "STX", "NRG", "AWK", "HSY", "IEX", "XYL", "K", "SYY", "XEL", "NTRS", "VMC", "DPZ", "FMC", "IT", "RJF", "CLX", "BLL", "CHRW", "HAS", "ZBH", "CHTR", "NUE", "PKI", "EPAM", "UAL", "HIG", "IP", "AVY", "SIVB", "BBY", "AMP", "WAB", "EXR", "CINF", "CDW", "CFG", "DOV", "NDSN", "BXP", "FDS", "NCLH", "CBOE", "DHI", "DXCM", "LUV", "FFIV", "JBHT", "CNP", "LDOS", "AAL", "ZBRA", "APA", "NI", "MAS", "DRI", "TXT", "CZR", "WHR", "AKAM", "LKQ", "HES", "REG", "MKTX", "ALK", "CTLT", "SNA", "BRO", "TAP", "MOS", "EMN", "BKR", "PFG", "FTV", "CAH", "HBAN", "ZION", "KEY", "WRB", "SBNY", "WDC", "WAB", "EXR", "AIV", "EQT", "AES", "ETSY", "WST", "CTXS", "WY", "IPG", "MPWR", "HOLX", "RCL", "ATO", "CAG", "UHS", "BMRN", "PNW", "SJM", "NVR", "STLD", "ESS", "NWSA", "TFC", "FANG", "VTR", "PEAK", "CPT", "HST", "NI", "FRT", "VNO", "HFC", "VRSN", "LYV", "LNT", "PRU", "SJM", "NRG", "TXT", "LEN", "SEE", "VTR", "PKG", "FLS", "RMD", "ETSY", "SJM", "NI", "ALB", "ALXN", "MOH", "DVA", "CAG", "ROL", "STLD", "DISH", "WEC", "XYL", "CZR", "NLOK", "TRMB", "PKI", "RCL", "FBHS", "SNPS", "CHD", "BXP", "DISH", "CNP", "EXR", "HBAN", "AES", "CLX", "AEE", "PKG", "PEAK", "HIG", "CAG", "ALK", "NWSA", "RE", "REG", "WAB", "AIZ", "AIV", "AOS", "AAL", "AAP", "ABMD", "ACN", "ADBE", "ADI", "ADM", "ADP", "ADS", "AEP", "AES", "AFL", "AIG", "AIV", "AIZ", "AJG", "AKAM", "ALB", "ALGN", "ALK", "ALL", "ALLE", "ALXN", "AMAT", "AMCR", "AMD", "AME", "AMG", "AMP", "AMT", "AMZN", "ANET", "ANSS", "ANTM", "AON", "AOS", "APA", "APD", "APH", "APTV", "ARE", "ATO", "ATVI", "AVB", "AVGO", "AVY", "AWK", "AXP", "AZO", "BA", "BAC", "BAX", "BBY", "BDX", "BEN", "BF.B", "BIIB", "BK", "BKNG", "BKR", "BLK", "BLL", "BMY", "BR", "BRK.B", "BSX", "BWA", "BXP", "C", "CAG", "CAH", "CARR", "CAT", "CB", "CBOE", "CBRE", "CCI", "CCL", "CDNS", "CDW", "CE", "CERN", "CF", "CFG", "CHD", "CHRW", "CHTR", "CI", "CINF", "CL", "CLX", "CMA", "CMCSA", "CME", "CMG", "CMI", "CMS", "CNC", "CNP", "COF", "COG", "COO", "COP", "COST", "CPB", "CPRT", "CRL", "CRM", "CSCO", "CSX", "CTAS", "CTLT", "CTSH", "CTXS", "CVS", "CVX", "D", "DAL", "DD", "DE", "DFS", "DG", "DGX", "DHI", "DHR", "DIS", "DISCA", "DISCK", "DISH", "DLR", "DLTR", "DOV", "DOW", "DPZ", "DRE", "DRI", "DTE", "DUK", "DVA", "DVN", "DXCM", "EA", "EBAY", "ECL", "ED", "EFX", "EIX", "EL", "EMN", "EMR", "ENPH", "EOG", "EPAM", "EQIX", "EQR", "ES", "ESS", "ETN", "ETR", "ETSY", "EVRG", "EXC", "EXPD", "EXPE", "EXR", "F", "FANG", "FAST", "FB", "FBHS", "FCX", "FDX", "FE", "FFIV", "FIS", "FISV", "FITB", "FLT", "FMC", "FOX", "FOXA", "FRC", "FRT", "FTNT", "FTV", "GD", "GE", "GILD", "GIS", "GL", "GLW", "GM", "GNRC", "GOOG", "GOOGL", "GPC", "GPN", "GPS", "GRMN", "GS", "GWW", "HAL", "HAS", "HBAN", "HBI", "HCA", "HD", "HES", "HFC", "HIG", "HII", "HLT", "HOLX", "HON", "HPE", "HPQ", "HRL", "HSIC", "HST", "HSY", "HUM", "IBM", "ICE", "IDXX", "IEX", "IFF", "ILMN", "INCY", "INFO", "INTC", "INTU", "IP", "IPG", "IPGP", "IQV", "IR", "IRM", "ISRG", "IT", "ITW", "IVZ", "J", "JBHT", "JCI", "JKHY", "JNJ", "JNPR", "JPM", "K", "KEYS", "KHC", "KIM", "KLAC", "KMB", "KMI", "KMX", "KO", "KR", "KSS", "KSU", "L", "LDOS", "LEG", "LEN", "LH", "LHX", "LIN", "LKQ", "LLY", "LMT", "LNC", "LNT", "LOW", "LRCX", "LUMN", "LUV", "LYB", "LYV", "MA", "MAA", "MAR", "MAS", "MCD", "MCHP", "MCK", "MCO", "MDLZ", "MDT", "MET", "MGM", "MHK", "MKC", "MKTX", "MLM", "MMC", "MMM", "MNST", "MO", "MOS", "MPC", "MRK", "MRO", "MS", "MSCI", "MSFT", "MSI", "MTB", "MTCH", "MTD", "MU", "NCLH", "NDAQ", "NDSN", "NEE", "NEM", "NFLX", "NI", "NKE", "NKTR", "NLOK", "NLSN", "NOC", "NOV", "NRG", "NSC", "NTAP", "NTRS", "NUE", "NVDA", "NVR", "NWL", "NWS", "NWSA", "O", "ODFL", "OKE", "OMC", "ORCL", "ORLY", "OTIS", "OXY", "PAYC", "PAYX", "PBCT", "PCAR", "PEAK", "PEG", "PENN", "PEP", "PFE", "PFG", "PG", "PGR", "PH", "PHM", "PKG", "PKI", "PLD", "PNC", "PNR", "PNW", "PPG", "PPL", "PRGO", "PRU", "PSA", "PSX", "PVH", "PWR", "PXD", "PYPL", "QCOM", "QRVO", "RCL", "RE", "REG", "REGN", "RF", "RHI", "RJF", "RL", "ROK", "ROL", "ROP", "ROST", "RSG", "RTX", "SBAC", "SBUX", "SCHW", "SEE", "SHW", "SIVB", "SJM", "SLB", "SNA", "SNPS", "SO", "SPG", "SPGI", "SRE", "STE", "STT", "STX", "STZ", "SWK", "SWKS", "SYF", "SYK", "SYY", "T", "TAP", "TDG", "TDY", "TEL", "TER", "TFC", "TFX", "TGT", "TJX", "TMO", "TMUS", "TPR", "TRMB", "TROW", "TRV", "TSCO", "TSLA", "TSN", "TT", "TTWO", "TWTR", "TXN", "TXT", "TYL", "UA", "UAA", "UAL", "UDR", "UHS", "ULTA", "UNH", "UNM", "UNP", "UPS", "URI", "USB", "V", "VAR", "VFC", "VIAC", "VLO", "VMC", "VNO", "VRSK", "VRSN", "VRTX", "VTR", "VZ", "WAB", "WAT", "WBA", "WDC", "WEC", "WELL", "WFC", "WHR", "WLTW", "WM", "WMB", "WMT", "WRB", "WRK", "WST", "WU", "WY", "WYNN", "XEL", "XLNX", "XOM", "XRAY", "XYL", "YUM", "ZBH", "ZBRA", "ZION", "ZTS")
                  
                  known_commodities <- c("GC", "CL", "SI", "HG", "NG", "PL", "PA", "ZC", "ZS", "ZW", "KC", "CT", "CC", "SB", "LE", "GF", "HE", "OJ", "LH", "LB", "NR")
                  
                  # Función para determinar el tipo de instrumento basado en el ticker
                  determine_instrument_type <- function(ticker) {
                    if (ticker %in% known_etfs) {
                      return("ETF")
                    } else if (ticker %in% known_cryptos) {
                      return("Crypto")
                    } else if (ticker %in% known_stocks) {
                      return("Stock")
                    } else if (ticker %in% known_commodities) {
                      return("Materia Prima")
                    } else {
                      return("Desconocido")
                    }
                  }
                  
                  # Función para agregar un ticker a la base de datos
                  add_ticker <- function(portfolio, ticker, acciones) {
                    tipo <- determine_instrument_type(ticker)
                    if (ticker %in% portfolio$Ticker) {
                      portfolio$Acciones[portfolio$Ticker == ticker] <- portfolio$Acciones[portfolio$Ticker == ticker] + acciones
                    } else {
                      new_entry <- data.frame(Ticker = ticker, Acciones = acciones, Precio = NA, Divisa = NA, Tipo = tipo, 
                                              PE_Ratio = NA, Dividend_Yield = NA, Beta = NA, Volume = NA, EPS = NA, 
                                              PB_Ratio = NA, stringsAsFactors = FALSE)
                      portfolio <- rbind(portfolio, new_entry)
                    }
                    return(portfolio)
                  }
                  
                  # Función para eliminar un ticker de la base de datos
                  remove_ticker <- function(portfolio, ticker) {
                    portfolio <- portfolio[portfolio$Ticker != ticker, ]
                    return(portfolio)
                  }
                  
                  # Función para modificar el número de acciones de un ticker existente
                  modify_ticker <- function(portfolio, ticker, acciones) {
                    if (ticker %in% portfolio$Ticker) {
                      portfolio$Acciones[portfolio$Ticker == ticker] <- acciones
                    } else {
                      showModal(modalDialog(
                        title = "Error",
                        "El ticker ingresado no existe en el portafolio.",
                        easyClose = TRUE,
                        footer = NULL
                      ))
                    }
                    return(portfolio)
                  }
                  
                  # Función para inferir la divisa basada en el sufijo del ticker
                  infer_currency <- function(ticker) {
                    if (grepl("\\.TO$", ticker)) {
                      return("CAD")
                    } else if (grepl("\\.L$", ticker)) {
                      return("GBP")
                    } else if (grepl("\\.AX$", ticker)) {
                      return("AUD")
                    } else {
                      return("USD")
                    }
                  }
                  
                  # Función para obtener datos adicionales usando rvest
                  get_additional_data <- function(ticker) {
                    yahoo_url <- paste0("https://finance.yahoo.com/quote/", ticker)
                    webpage <- tryCatch(read_html(yahoo_url), error = function(e) NULL)
                    
                    if (is.null(webpage)) {
                      return(list(pe_ratio = NA, dividend_yield = NA, beta = NA, volume = NA, eps = NA, pb_ratio = NA))
                    }
                    
                    pe_ratio <- webpage %>%
                      html_nodes(xpath = '//*[contains(text(), "PE Ratio (TTM)")]//following-sibling::*') %>%
                      html_text() %>%
                      as.numeric()
                    
                    dividend_yield <- webpage %>%
                      html_nodes(xpath = '//*[contains(text(), "Forward Dividend & Yield")]//following-sibling::*') %>%
                      html_text() %>%
                      str_extract("\\(([^\\)]+)\\)") %>%
                      str_remove_all("[\\(\\)%]") %>%
                      as.numeric()
                    
                    beta <- webpage %>%
                      html_nodes(xpath = '//*[contains(text(), "Beta (5Y Monthly)")]//following-sibling::*') %>%
                      html_text() %>%
                      as.numeric()
                    
                    volume <- webpage %>%
                      html_nodes(xpath = '//*[contains(text(), "Volume")]//following-sibling::*') %>%
                      html_text() %>%
                      str_remove_all(",") %>%
                      as.numeric()
                    
                    eps <- webpage %>%
                      html_nodes(xpath = '//*[contains(text(), "EPS (TTM)")]//following-sibling::*') %>%
                      html_text() %>%
                      as.numeric()
                    
                    pb_ratio <- webpage %>%
                      html_nodes(xpath = '//*[contains(text(), "Price/Book (mrq)")]//following-sibling::*') %>%
                      html_text() %>%
                      as.numeric()
                    
                    return(list(pe_ratio = ifelse(length(pe_ratio) == 0, NA, pe_ratio), 
                                dividend_yield = ifelse(length(dividend_yield) == 0, NA, dividend_yield), 
                                beta = ifelse(length(beta) == 0, NA, beta), 
                                volume = ifelse(length(volume) == 0, NA, volume), 
                                eps = ifelse(length(eps) == 0, NA, eps), 
                                pb_ratio = ifelse(length(pb_ratio) == 0, NA, pb_ratio)))
                  }
                  
                  # Función para obtener el precio de una criptomoneda usando CoinGecko
                  get_crypto_price <- function(ticker) {
                    coin_id <- crypto_id_map[ticker]
                    url <- paste0("https://api.coingecko.com/api/v3/simple/price?ids=", coin_id, "&vs_currencies=usd")
                    response <- fromJSON(content(GET(url), as = "text", encoding = "UTF-8"))
                    price <- response[[coin_id]]$usd
                    return(price)
                  }
                  
                  # Función para recolectar información de cada ticker
                  explore_info <- function(portfolio) {
                    for (i in 1:nrow(portfolio)) {
                      ticker <- portfolio$Ticker[i]
                      tipo <- portfolio$Tipo[i]
                      tryCatch({
                        if (tipo == "Crypto") {
                          portfolio$Precio[i] <- get_crypto_price(ticker)
                          portfolio$Divisa[i] <- "USD"
                        } else {
                          data <- getQuote(ticker, src = "yahoo")
                          portfolio$Precio[i] <- data$Last
                          portfolio$Divisa[i] <- infer_currency(ticker)
                          
                          additional_data <- get_additional_data(ticker)
                          portfolio$PE_Ratio[i] <- additional_data$pe_ratio
                          portfolio$Dividend_Yield[i] <- additional_data$dividend_yield
                          portfolio$Beta[i] <- additional_data$beta
                          portfolio$Volume[i] <- additional_data$volume
                          portfolio$EPS[i] <- additional_data$eps
                          portfolio$PB_Ratio[i] <- additional_data$pb_ratio
                        }
                      }, error = function(e) {
                        portfolio$Precio[i] <- NA
                        portfolio$Divisa[i] <- NA
                        portfolio$PE_Ratio[i] <- NA
                        portfolio$Dividend_Yield[i] <- NA
                        portfolio$Beta[i] <- NA
                        portfolio$Volume[i] <- NA
                        portfolio$EPS[i] <- NA
                        portfolio$PB_Ratio[i] <- NA
                      })
                    }
                    return(portfolio)
                  }
                  
                  # Función para calcular el valor total del portafolio
                  calculate_portfolio_value <- function(portfolio) {
                    total_value <- sum(portfolio$Acciones * portfolio$Precio, na.rm = TRUE)
                    return(total_value)
                  }
                  
                  # Función para obtener precios históricos y graficar
                  plot_prices <- function(tickers, start_date, end_date) {
                    prices <- tq_get(tickers, from = start_date, to = end_date, complete_cases = TRUE)
                    
                    # Verificar si prices es un data.frame
                    if (!inherits(prices, "data.frame")) {
                      showModal(modalDialog(
                        title = "Error",
                        "No se pudo obtener datos históricos para los tickers seleccionados.",
                        easyClose = TRUE,
                        footer = NULL
                      ))
                      return(NULL)
                    }
                    
                    # Verificar si hay datos
                    if (nrow(prices) == 0) {
                      showModal(modalDialog(
                        title = "Error",
                        "No se encontraron datos históricos para el rango de fechas seleccionado.",
                        easyClose = TRUE,
                        footer = NULL
                      ))
                      return(NULL)
                    }
                    
                    prices %>%
                      ggplot(aes(x = date, y = adjusted, color = symbol)) +
                      geom_line() +
                      labs(title = "Historical Prices", x = "Date", y = "Price") +
                      theme_minimal()
                  }
                  
                  # Función para calcular la matriz de covarianza
                  calculate_cov_matrix <- function(tickers, start_date, end_date) {
                    prices <- tq_get(tickers, from = start_date, to = end_date, complete_cases = TRUE)
                    
                    if (!inherits(prices, "data.frame") || nrow(prices) == 0) {
                      showModal(modalDialog(
                        title = "Error",
                        "No se pudieron obtener datos históricos para los tickers seleccionados.",
                        easyClose = TRUE,
                        footer = NULL
                      ))
                      return(NULL)
                    }
                    
                    prices_wide <- prices %>%
                      select(date, symbol, adjusted) %>%
                      spread(symbol, adjusted)
                    
                    cov_matrix <- cov(prices_wide[,-1], use = "complete.obs")
                    
                    # Convertir a data.frame y agregar nombres de columnas y filas
                    cov_matrix_df <- as.data.frame(cov_matrix)
                    colnames(cov_matrix_df) <- colnames(prices_wide)[-1]
                    rownames(cov_matrix_df) <- colnames(prices_wide)[-1]
                    
                    return(cov_matrix_df)
                  }
                  
                  # Función para calcular autovalores y autovectores
                  calculate_eigen <- function(cov_matrix) {
                    eigen_result <- eigen(cov_matrix)
                    eigenvalues <- eigen_result$values
                    eigenvectors <- eigen_result$vectors
                    
                    # Crear un data.frame con autovalores y autovectores
                    eigen_df <- as.data.frame(cbind(eigenvalues, eigenvectors))
                    colnames(eigen_df) <- c("Autovalores", colnames(cov_matrix))
                    
                    return(eigen_df)
                  }
                  
                  # Función para generar propuesta de inversión
                  generate_investment_proposal <- function(cov_matrix, amount_to_invest) {
                    eigen_result <- eigen(cov_matrix)
                    principal_component <- eigen_result$vectors[, 1]
                    weights <- principal_component / sum(principal_component)
                    investment_allocation <- weights * amount_to_invest
                    return(investment_allocation)
                  }
                  
                  # Definir la UI de la aplicación
                  ui <- fluidPage(
                    titlePanel("Gestión de Portafolio: Análisis de la Matriz de Covarianzas"),
                    
                    sidebarLayout(
                      sidebarPanel(
                        textInput("ticker", "Ticker:", ""),
                        numericInput("acciones", "Acciones:", value = 0),
                        actionButton("add_button", "Agregar Ticker"),
                        actionButton("remove_button", "Eliminar Ticker"),
                        actionButton("modify_button", "Modificar Ticker"),
                        br(),
                        br(),
                        actionButton("explore_button", "Explorar Información"),
                        br(),
                        br(),
                        actionButton("save_button", "Guardar Portafolio"),
                        br(),
                        br(),
                        dateRangeInput("date_range", 
                                       "Rango de fechas:", 
                                       start = seq(Sys.Date(), length = 2, by = "-3 months")[2], 
                                       end = Sys.Date()),
                        uiOutput("ticker_checklist"),
                        actionButton("select_all_button", "Seleccionar Todos"),
                        actionButton("deselect_all_button", "Seleccionar Ninguno"),
                        actionButton("plot_button", "Mostrar Gráfico"),
                        actionButton("select_stock_button", "Seleccionar Solo Stock"),
                        actionButton("select_etf_button", "Seleccionar Solo ETF"),
                        actionButton("select_crypto_button", "Seleccionar Solo Crypto"),
                        actionButton("calculate_cov_button", "Calcular Matriz de Covarianza"),
                        actionButton("calculate_eigen_button", "Calcular Autovalores y Autovectores"),
                        numericInput("investment_amount", "Cantidad a Invertir:", value = 1000),
                        actionButton("generate_proposal_button", "Generar Propuesta de Inversión")
                      ),
                      
                      mainPanel(
                        textOutput("portfolio_value"),
                        DTOutput("portfolio_table"),
                        plotOutput("price_plot"),
                        tableOutput("cov_matrix"),
                        tableOutput("eigen_table"),
                        tableOutput("investment_proposal")
                      )
                    )
                  )
                  
                  # Definir la lógica del servidor
                  server <- function(input, output, session) {
                    # Cargar el portafolio al inicio
                    portfolio <- reactiveVal(load_portfolio())
                    
                    # Agregar un ticker al portafolio
                    observeEvent(input$add_button, {
                      req(input$ticker)
                      req(input$acciones)
                      portfolio(add_ticker(portfolio(), input$ticker, input$acciones))
                    })
                    
                    # Eliminar un ticker del portafolio
                    observeEvent(input$remove_button, {
                      req(input$ticker)
                      portfolio(remove_ticker(portfolio(), input$ticker))
                    })
                    
                    # Modificar el número de acciones de un ticker existente
                    observeEvent(input$modify_button, {
                      req(input$ticker)
                      req(input$acciones)
                      portfolio(modify_ticker(portfolio(), input$ticker, input$acciones))
                    })
                    
                    # Explorar información de los tickers
                    observeEvent(input$explore_button, {
                      portfolio(explore_info(portfolio()))
                      showModal(modalDialog(
                        title = "Información Actualizada",
                        "La información de los tickers ha sido actualizada con éxito.",
                        easyClose = TRUE,
                        footer = NULL
                      ))
                    })
                    
                    # Calcular el valor total del portafolio
                    output$portfolio_value <- renderText({
                      value <- calculate_portfolio_value(portfolio())
                      paste("Valor Total del Portafolio: $", format(value, big.mark = ",", scientific = FALSE))
                    })
                    
                    # Guardar el portafolio en el archivo CSV
                    observeEvent(input$save_button, {
                      save_portfolio(portfolio())
                      showModal(modalDialog(
                        title = "Guardado",
                        "El portafolio ha sido guardado con éxito.",
                        easyClose = TRUE,
                        footer = NULL
                      ))
                    })
                    
                    # Mostrar el portafolio en una tabla
                    output$portfolio_table <- renderDT({
                      datatable(portfolio(), options = list(pageLength = 10, autoWidth = TRUE))
                    })
                    
                    # Mostrar la lista de tickers como checkboxes
                    output$ticker_checklist <- renderUI({
                      tickers <- portfolio()$Ticker
                      checkboxGroupInput("selected_tickers", "Selecciona Tickers:", choices = tickers)
                    })
                    
                    # Seleccionar todos los tickers
                    observeEvent(input$select_all_button, {
                      updateCheckboxGroupInput(session, "selected_tickers", selected = portfolio()$Ticker)
                    })
                    
                    # Deseleccionar todos los tickers
                    observeEvent(input$deselect_all_button, {
                      updateCheckboxGroupInput(session, "selected_tickers", selected = character(0))
                    })
                    
                    # Mostrar el gráfico de precios
                    observeEvent(input$plot_button, {
                      req(input$selected_tickers)
                      req(input$date_range)
                      start_date <- input$date_range[1]
                      end_date <- input$date_range[2]
                      output$price_plot <- renderPlot({
                        plot_prices(input$selected_tickers, start_date, end_date)
                      })
                    })
                    
                    # Seleccionar solo Stock
                    observeEvent(input$select_stock_button, {
                      stock_tickers <- portfolio()$Ticker[portfolio()$Tipo == "Stock"]
                      updateCheckboxGroupInput(session, "selected_tickers", selected = stock_tickers)
                    })
                    
                    # Seleccionar solo ETF
                    observeEvent(input$select_etf_button, {
                      etf_tickers <- portfolio()$Ticker[portfolio()$Tipo == "ETF"]
                      updateCheckboxGroupInput(session, "selected_tickers", selected = etf_tickers)
                    })
                    
                    # Seleccionar solo Crypto
                    observeEvent(input$select_crypto_button, {
                      crypto_tickers <- portfolio()$Ticker[portfolio()$Tipo == "Crypto"]
                      updateCheckboxGroupInput(session, "selected_tickers", selected = crypto_tickers)
                    })
                    
                    # Calcular y mostrar la matriz de covarianza
                    observeEvent(input$calculate_cov_button, {
                      req(input$selected_tickers)
                      req(input$date_range)
                      start_date <- input$date_range[1]
                      end_date <- input$date_range[2]
                      cov_matrix <- calculate_cov_matrix(input$selected_tickers, start_date, end_date)
                      output$cov_matrix <- renderTable({
                        if (!is.null(cov_matrix)) {
                          cov_matrix
                        }
                      }, rownames = TRUE)
                    })
                    
                    # Calcular y mostrar los autovalores y autovectores
                    observeEvent(input$calculate_eigen_button, {
                      req(input$selected_tickers)
                      req(input$date_range)
                      start_date <- input$date_range[1]
                      end_date <- input$date_range[2]
                      cov_matrix <- calculate_cov_matrix(input$selected_tickers, start_date, end_date)
                      if (!is.null(cov_matrix)) {
                        eigen_df <- calculate_eigen(cov_matrix)
                        output$eigen_table <- renderTable({
                          eigen_df
                        })
                      }
                    })
                    
                    # Generar y mostrar propuesta de inversión
                    observeEvent(input$generate_proposal_button, {
                      req(input$selected_tickers)
                      req(input$date_range)
                      req(input$investment_amount)
                      start_date <- input$date_range[1]
                      end_date <- input$date_range[2]
                      cov_matrix <- calculate_cov_matrix(input$selected_tickers, start_date, end_date)
                      if (!is.null(cov_matrix)) {
                        investment_allocation <- generate_investment_proposal(cov_matrix, input$investment_amount)
                        proposal_df <- data.frame(Ticker = input$selected_tickers, Allocation = investment_allocation)
                        output$investment_proposal <- renderTable({
                          proposal_df
                        })
                      }
                    })
                  }
                  
                  # Ejecutar la aplicación
                  shinyApp(ui = ui, server = server)
