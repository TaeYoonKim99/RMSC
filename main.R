# Required libraries
required_packages <- c("httr", "jsonlite")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
sapply(missing_packages, install.packages)
sapply(required_packages, require, character.only = TRUE)

# API setup
api_key <- "CYYEH1N9"  # Replace with your API Key
base_url <- "http://localhost:9999/v1/"
api_headers <- add_headers(.headers = c("X-API-key" = api_key))

# Global variables
MAX_LONG_EXPOSURE <- 300000
MAX_SHORT_EXPOSURE <- -100000
ORDER_LIMIT <- 5000

# Function to get the current tick and case status
get_tick <- function() {
  response <- GET(paste0(base_url, "case"), config = api_headers)
  if (status_code(response) == 200) {
    case <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(list(tick = case$tick, status = case$status))
  } else {
    stop("Failed to fetch case status.")
  }
}

# Function to get bid and ask prices
get_bid_ask <- function(ticker) {
  response <- GET(paste0(base_url, "securities/book"), query = list(ticker = ticker), config = api_headers)
  
  if (status_code(response) != 200) {
    warning(paste("Failed to fetch bid/ask for ticker:", ticker))
    return(list(best_bid_price = NA, best_ask_price = NA))
  }
  
  book <- tryCatch(fromJSON(content(response, "text", encoding = "UTF-8")), error = function(e) NULL)
  
  # Validate book structure
  if (is.null(book) || !is.list(book)) {
    warning(paste("Invalid response structure for ticker:", ticker))
    return(list(best_bid_price = NA, best_ask_price = NA))
  }
  
  # Extract bid and ask prices
  best_bid_price <- tryCatch({
    if (!is.null(book$bids) && length(book$bids) > 0 && is.list(book$bids[[1]]) && "price" %in% names(book$bids[[1]])) {
      as.numeric(book$bids[[1]]$price)
    } else {
      NA
    }
  }, error = function(e) NA)
  
  best_ask_price <- tryCatch({
    if (!is.null(book$asks) && length(book$asks) > 0 && is.list(book$asks[[1]]) && "price" %in% names(book$asks[[1]])) {
      as.numeric(book$asks[[1]]$price)
    } else {
      NA
    }
  }, error = function(e) NA)
  
  return(list(best_bid_price = best_bid_price, best_ask_price = best_ask_price))
}

# Function to place orders
place_order <- function(ticker, price, action, type = "LIMIT") {
  response <- POST(paste0(base_url, "orders"), query = list(
    ticker = ticker, type = type, quantity = ORDER_LIMIT, price = price, action = action
  ), config = api_headers)
  if (status_code(response) != 200) {
    warning(paste("Failed to place", action, "order for ticker:", ticker))
  }
}

# Strategies ----------------------------------------------------------

# OWL: Spread-Based Market-Making
# Function to handle OWL trading (Spread-Based Market-Making)
trade_OWL <- function(ticker, bid_ask_prices) {
  # Ensure bid/ask prices are valid
  if (is.na(bid_ask_prices$best_bid_price) || is.na(bid_ask_prices$best_ask_price)) {
    warning(paste("Skipping", ticker, "due to invalid bid/ask prices."))
    return()
  }
  
  # Calculate buy and sell prices
  buy_price <- bid_ask_prices$best_bid_price + 0.01
  sell_price <- bid_ask_prices$best_ask_price - 0.01
  
  # Place buy order
  response_buy <- POST(paste0(base_url, "orders"), query = list(
    ticker = ticker, type = "LIMIT", quantity = ORDER_LIMIT, price = buy_price, action = "BUY"
  ), config = api_headers)
  
  if (status_code(response_buy) != 200) {
    warning(paste("Failed to place BUY order for ticker:", ticker))
  } else {
    print(paste("Placed BUY order for", ticker, "at", buy_price))
  }
  
  # Place sell order
  response_sell <- POST(paste0(base_url, "orders"), query = list(
    ticker = ticker, type = "LIMIT", quantity = ORDER_LIMIT, price = sell_price, action = "SELL"
  ), config = api_headers)
  
  if (status_code(response_sell) != 200) {
    warning(paste("Failed to place SELL order for ticker:", ticker))
  } else {
    print(paste("Placed SELL order for", ticker, "at", sell_price))
  }
}

# CROW: Mean Reversion
trade_CROW <- function(ticker) {
  prices <- get_time_sales(ticker)
  if (is.null(prices) || length(prices) < 15) {
    warning(paste("Not enough price data for", ticker))
    return()
  }
  short_ma <- mean(tail(prices, 5), na.rm = TRUE)
  long_ma <- mean(tail(prices, 15), na.rm = TRUE)
  last_price <- tail(prices, 1)
  
  if (last_price < long_ma) {
    place_order(ticker, last_price, "BUY")
  } else if (last_price > long_ma) {
    place_order(ticker, last_price, "SELL")
  }
}

# DOVE: Momentum-Based Strategy
trade_DOVE <- function(ticker) {
  prices <- get_time_sales(ticker)
  if (is.null(prices) || length(prices) < 10) {
    warning(paste("Not enough price data for", ticker))
    return()
  }
  last_price <- tail(prices, 1)
  momentum <- tail(prices, 3)
  
  if (momentum[3] > momentum[2] && momentum[2] > momentum[1]) {
    place_order(ticker, last_price * 1.01, "BUY")
  } else if (momentum[3] < momentum[2] && momentum[2] < momentum[1]) {
    place_order(ticker, last_price * 0.99, "SELL")
  }
}

# DUCK: Volatility-Adaptive Market-Making
trade_DUCK <- function(ticker, bid_ask_prices) {
  if (is.na(bid_ask_prices$best_bid_price) || is.na(bid_ask_prices$best_ask_price)) {
    warning(paste("Skipping", ticker, "due to invalid bid/ask prices."))
    return()
  }
  buy_price <- bid_ask_prices$best_bid_price * 0.98
  sell_price <- bid_ask_prices$best_ask_price * 1.02
  place_order(ticker, buy_price, "BUY")
  place_order(ticker, sell_price, "SELL")
}

# Main function -------------------------------------------------------
main <- function() {
  tick_status <- get_tick()
  tick <- tick_status$tick
  status <- tick_status$status
  
  ticker_list <- c("OWL", "CROW", "DOVE", "DUCK")
  
  while (status == "ACTIVE") {
    for (ticker_symbol in ticker_list) {
      print(paste("Processing ticker:", ticker_symbol))
      
      # Fetch bid/ask prices
      bid_ask_prices <- get_bid_ask(ticker_symbol)
      
      # Skip tickers with invalid bid/ask prices
      if (is.na(bid_ask_prices$best_bid_price) || is.na(bid_ask_prices$best_ask_price)) {
        warning(paste("Skipping ticker due to invalid bid/ask prices:", ticker_symbol))
        next
      }
      
      # Call the appropriate trading strategy
      if (ticker_symbol == "OWL") {
        trade_OWL(ticker_symbol, bid_ask_prices)
      } else if (ticker_symbol == "CROW") {
        trade_CROW(ticker_symbol)
      } else if (ticker_symbol == "DOVE") {
        trade_DOVE(ticker_symbol)
      } else if (ticker_symbol == "DUCK") {
        trade_DUCK(ticker_symbol, bid_ask_prices)
      }
    }
    
    Sys.sleep(0.75)  # Delay between iterations
    tick_status <- get_tick()
    tick <- tick_status$tick
    status <- tick_status$status
  }
  print("Trading session ended.")
}


# Run the main function
main()
