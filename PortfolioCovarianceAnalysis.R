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
library(quadprog)

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

# Función para cargar listas de instrumentos desde archivos CSV
load_instrument_list <- function(file) {
  if (file.exists(file)) {
    return(read.csv(file, stringsAsFactors = FALSE)$Ticker)
  } else {
    return(character())
  }
}

# Función para guardar listas de instrumentos en archivos CSV
save_instrument_list <- function(list, file) {
  write.csv(data.frame(Ticker = list), file, row.names = FALSE)
}

# Cargar listas de instrumentos conocidos
known_etfs <- load_instrument_list("known_etfs.csv")
known_cryptos <- load_instrument_list("known_cryptos.csv")
known_stocks <- load_instrument_list("known_stocks.csv")
known_commodities <- load_instrument_list("known_commodities.csv")

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
add_ticker <- function(portfolio, ticker, acciones, tipo) {
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

# Función para eliminar un ticker de la base de datos y de los archivos CSV conocidos
remove_ticker <- function(portfolio, ticker) {
  portfolio <- portfolio[portfolio$Ticker != ticker, ]
  
  known_etfs <- known_etfs[known_etfs != ticker]
  known_cryptos <- known_cryptos[known_cryptos != ticker]
  known_stocks <- known_stocks[known_stocks != ticker]
  known_commodities <- known_commodities[known_commodities != ticker]
  
  save_instrument_list(known_etfs, "known_etfs.csv")
  save_instrument_list(known_cryptos, "known_cryptos.csv")
  save_instrument_list(known_stocks, "known_stocks.csv")
  save_instrument_list(known_commodities, "known_commodities.csv")
  
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

# Función para capturar datos históricos y guardarlos en un archivo CSV
capture_historical_data <- function(tickers, start_date, end_date, file = "historical_data.csv") {
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
  
  # Guardar los datos en un archivo CSV
  write.csv(prices, file, row.names = FALSE)
  
  return(prices)
}

# Función para calcular la matriz de covarianza y almacenarla en un valor reactivo
calculate_and_store_cov_matrix <- function(tickers, start_date, end_date) {
  prices <- read.csv("historical_data.csv", stringsAsFactors = FALSE)
  prices <- prices %>% filter(symbol %in% tickers & date >= start_date & date <= end_date)
  
  if (nrow(prices) == 0) {
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
  
  # Excluir columnas con valores NA
  prices_wide <- prices_wide[, colSums(is.na(prices_wide)) == 0]
  
  if (ncol(prices_wide) <= 1) {
    showModal(modalDialog(
      title = "Error",
      "No hay suficientes datos para calcular la matriz de covarianza después de excluir columnas con valores NA.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  }
  
  cov_matrix <- cov(prices_wide[,-1], use = "complete.obs")
  
  # Convertir a data.frame y agregar nombres de columnas y filas
  cov_matrix_df <- as.data.frame(cov_matrix)
  colnames(cov_matrix_df) <- colnames(prices_wide)[-1]
  rownames(cov_matrix_df) <- colnames(prices_wide)[-1]
  
  return(cov_matrix_df)
}

# Función para calcular autovalores y autovectores (descartar soluciones complejas)
calculate_eigen <- function(cov_matrix) {
  eigen_result <- eigen(cov_matrix)
  real_eigenvalues <- Re(eigen_result$values[Im(eigen_result$values) == 0])
  real_eigenvectors <- Re(eigen_result$vectors[, Im(eigen_result$values) == 0])
  
  # Crear un data.frame con autovalores y autovectores reales
  eigen_df <- as.data.frame(cbind(real_eigenvalues, real_eigenvectors))
  colnames(eigen_df) <- c("Autovalores", colnames(cov_matrix))
  
  return(eigen_df)
}

# Función para generar propuesta de inversión
generate_investment_proposal <- function(cov_matrix, amount_to_invest) {
  eigen_result <- eigen(cov_matrix)
  real_eigenvectors <- Re(eigen_result$vectors[, Im(eigen_result$values) == 0])
  principal_component <- real_eigenvectors[, 1]
  weights <- principal_component / sum(principal_component)
  investment_allocation <- weights * amount_to_invest
  return(investment_allocation)
}

# Función para calcular la varianza del portafolio
calculate_portfolio_variance <- function(portfolio, cov_matrix) {
  weights <- portfolio$Acciones * portfolio$Precio / sum(portfolio$Acciones * portfolio$Precio)
  portfolio_variance <- as.numeric(t(weights) %*% cov_matrix %*% weights)
  return(portfolio_variance)
}

# Función para calcular retornos esperados en porcentajes
calculate_expected_returns <- function(returns, weights) {
  periods <- c(1, 3, 6, 12)
  expected_returns <- sapply(periods, function(p) {
    annualized_return <- sum(mean(returns, na.rm = TRUE) * weights)
    (1 + annualized_return) ^ p - 1
  })
  names(expected_returns) <- paste(periods, "meses", sep = "_")
  return(expected_returns)
}

# Función para calcular beta promedio ponderado
calculate_weighted_beta <- function(portfolio) {
  weights <- portfolio$Acciones * portfolio$Precio / sum(portfolio$Acciones * portfolio$Precio)
  weighted_beta <- sum(weights * portfolio$Beta, na.rm = TRUE)
  return(weighted_beta)
}

# Definir la UI de la aplicación
ui <- fluidPage(

  titlePanel("Gestión de Portafolio: Análisis de la Matriz de Covarianzas"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Ticker:", ""),
      radioButtons("instrument_type", "Tipo de Instrumento:", choices = c("ETF", "Crypto", "Stock", "Materia Prima")),
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
      actionButton("select_stock_button", "Seleccionar Solo Stock"),
      actionButton("select_etf_button", "Seleccionar Solo ETF"),
      actionButton("select_crypto_button", "Seleccionar Solo Crypto"),
      actionButton("select_commodities_button", "Seleccionar Solo Materias Primas"),
      actionButton("capture_plot_button", "Capturar y Mostrar Datos Históricos"),
      numericInput("investment_amount", "Cantidad a Invertir:", value = 1000),
      actionButton("generate_proposal_button", "Generar Propuesta de Inversión")
    ),
    
    mainPanel(
      textOutput("portfolio_value"),
      DTOutput("portfolio_table"),
      plotOutput("price_plot"),
      tableOutput("cov_matrix"),
      tableOutput("eigen_table"),
      tableOutput("investment_proposal"),
      tableOutput("comparison_table"),
      textOutput("excluded_tickers")
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output, session) {
  # Cargar el portafolio al inicio
  portfolio <- reactiveVal(load_portfolio())
  
  # Valor reactivo para la matriz de covarianza
  cov_matrix <- reactiveVal(NULL)
  
  # Lista reactiva para tickers excluidos
  excluded_tickers <- reactiveVal(character())
  

  # Agregar un ticker al portafolio
  observeEvent(input$add_button, {
    req(input$ticker)
    req(input$acciones)
    req(input$instrument_type)
    
    tipo <- input$instrument_type
    ticker <- input$ticker
    
    if (tipo == "ETF") {
      known_etfs <<- c(known_etfs, ticker)
      save_instrument_list(known_etfs, "known_etfs.csv")
    } else if (tipo == "Crypto") {
      known_cryptos <<- c(known_cryptos, ticker)
      save_instrument_list(known_cryptos, "known_cryptos.csv")
    } else if (tipo == "Stock") {
      known_stocks <<- c(known_stocks, ticker)
      save_instrument_list(known_stocks, "known_stocks.csv")
    } else if (tipo == "Materia Prima") {
      known_commodities <<- c(known_commodities, ticker)
      save_instrument_list(known_commodities, "known_commodities.csv")
    }
    
    portfolio(add_ticker(portfolio(), ticker, input$acciones, tipo))
  })
  
  # Eliminar un ticker del portafolio y de los archivos conocidos
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
  
  # Capturar datos históricos y mostrar gráfico
  observeEvent(input$capture_plot_button, {
    req(input$selected_tickers)
    req(input$date_range)
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    prices <- capture_historical_data(input$selected_tickers, start_date, end_date)
    
    if (!is.null(prices)) {
      output$price_plot <- renderPlot({
        prices %>%
          ggplot(aes(x = date, y = adjusted, color = symbol)) +
          geom_line() +
          labs(title = "Historical Prices", x = "Date", y = "Price") +
          theme_minimal()
      })
    }
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
  
  # Seleccionar solo Materias Primas
  observeEvent(input$select_commodities_button, {
    commodities_tickers <- portfolio()$Ticker[portfolio()$Tipo == "Materia Prima"]
    updateCheckboxGroupInput(session, "selected_tickers", selected = commodities_tickers)
  })
  
  # Generar y mostrar propuesta de inversión
  observeEvent(input$generate_proposal_button, {
    req(input$selected_tickers)
    req(input$date_range)
    req(input$investment_amount)
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Calcular y almacenar la matriz de covarianza
    cov_matrix_data <- calculate_and_store_cov_matrix(input$selected_tickers, start_date, end_date)
    
    if (!is.null(cov_matrix_data)) {
      cov_matrix(cov_matrix_data)
      # Mostrar la matriz de covarianza
      output$cov_matrix <- renderTable({
        cov_matrix()
      }, rownames = TRUE)
      
      # Calcular y mostrar los autovalores y autovectores
      eigen_df <- calculate_eigen(cov_matrix())
      output$eigen_table <- renderTable({
        eigen_df
      })
      
      # Generar y mostrar propuesta de inversión
      tryCatch({
        investment_allocation <- generate_investment_proposal(cov_matrix(), input$investment_amount)
        current_investment <- portfolio() %>%
          filter(Ticker %in% input$selected_tickers) %>%
          mutate(Current_USD = Acciones * Precio)
        
        proposal_df <- data.frame(
          Ticker = input$selected_tickers, 
          Propuesta_de_Inversión_USD = investment_allocation,
          Inversión_Actual_USD = current_investment$Current_USD,
          Inversión_Final_USD = current_investment$Current_USD + investment_allocation
        )
        output$investment_proposal <- renderTable({
          proposal_df
        })
        
        # Calcular y mostrar la tabla comparativa
        initial_variance <- calculate_portfolio_variance(portfolio(), cov_matrix())
        final_portfolio <- portfolio()
        final_portfolio$Acciones[final_portfolio$Ticker %in% input$selected_tickers] <- 
          final_portfolio$Acciones[final_portfolio$Ticker %in% input$selected_tickers] + 
          investment_allocation / final_portfolio$Precio[final_portfolio$Ticker %in% input$selected_tickers]
        final_variance <- calculate_portfolio_variance(final_portfolio, cov_matrix())
        
        initial_returns <- calculate_expected_returns(diff(log(current_investment$Precio)), current_investment$Acciones * current_investment$Precio / sum(current_investment$Acciones * current_investment$Precio))
        final_returns <- calculate_expected_returns(diff(log(final_portfolio$Precio)), final_portfolio$Acciones * final_portfolio$Precio / sum(final_portfolio$Acciones * final_portfolio$Precio))
        
        initial_beta <- calculate_weighted_beta(portfolio())
        final_beta <- calculate_weighted_beta(final_portfolio)
        
        comparison_df <- data.frame(
          Metric = c("Varianza", "Beta Promedio", "Retorno Esperado (1 mes)", "Retorno Esperado (3 meses)", 
                     "Retorno Esperado (6 meses)", "Retorno Esperado (12 meses)"),
          Inicial = c(initial_variance, initial_beta, initial_returns["1_meses"], initial_returns["3_meses"], 
                      initial_returns["6_meses"], initial_returns["12_meses"]),
          Final = c(final_variance, final_beta, final_returns["1_meses"], final_returns["3_meses"], 
                    final_returns["6_meses"], final_returns["12_meses"])
        )
        
        output$comparison_table <- renderTable({
          comparison_df
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Ocurrió un error al generar la propuesta de inversión:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
