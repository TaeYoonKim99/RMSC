# Code for RMSC 2024 - Algorithmic Trading

# Required libraries
required_packages <- c("httr", "jsonlite")

# Download Missing packages
# missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
# sapply(missing_packages, install.packages)

# Load Packages
sapply(required_packages, require, character.only = TRUE)

# Global variables
MAX_LONG_EXPOSURE <- 300000
MAX_SHORT_EXPOSURE <- -100000
ORDER_LIMIT <- 5000
API_KEY <- "CYYEH1N9"
BASE_URL <- "http://localhost:9999/v1"

# Helper function to set headers
get_headers <- function() {
  add_headers(`X-API-key` = API_KEY)
}

# Function to get current tick and status
get_tick <- function() {
  resp <- GET(paste0(BASE_URL, "/case"), get_headers())
  if (status_code(resp) == 200) {
    case <- content(resp, as = "parsed")
    return(list(tick = case$tick, status = case$status))
  }
  return(NULL)
}

# Function to calculate VWAP
calculate_vwap <- function(ticker) {
  resp <- GET(paste0(BASE_URL, "/securities/tas"), query = list(ticker = ticker), get_headers())
  
  if (status_code(resp) == 200) {
    tas_data <- content(resp, as = "parsed")
    if (length(tas_data) > 0) {
      total_value <- sum(sapply(tas_data, function(trade) trade$price * trade$quantity))
      total_volume <- sum(sapply(tas_data, function(trade) trade$quantity))
      
      if (total_volume > 0) {
        return(total_value / total_volume)
      }
    }
  }
  
  return(NULL)  # Return NULL if VWAP cannot be calculated
}

# Function to determine trend using moving averages
calculate_trend <- function(ticker) {
  resp <- GET(paste0(BASE_URL, "/securities/tas"), query = list(ticker = ticker), get_headers())
  
  if (status_code(resp) == 200) {
    tas_data <- content(resp, as = "parsed")
    prices <- sapply(tas_data, function(trade) trade$price)
    
    if (length(prices) >= 10) {
      short_ma <- mean(tail(prices, 5))  # Short-term moving average
      long_ma <- mean(tail(prices, 10))  # Long-term moving average
      
      if (short_ma > long_ma) return("up")  # Upward trend
      if (short_ma < long_ma) return("down")  # Downward trend
    }
  }
  
  return("neutral")  # Default to neutral if trend cannot be determined
}

# Function to get bid and ask prices
get_bid_ask <- function(ticker) {
  resp <- GET(paste0(BASE_URL, "/securities/book"), query = list(ticker = ticker), get_headers())
  
  if (status_code(resp) == 200) {
    book <- content(resp, as = "parsed")
    best_bid_price <- if (!is.null(book$bids) && length(book$bids) > 0) book$bids[[1]]$price else NULL
    best_ask_price <- if (!is.null(book$asks) && length(book$asks) > 0) book$asks[[1]]$price else NULL
    
    if (!is.null(best_bid_price) || !is.null(best_ask_price)) {
      return(list(best_bid_price = best_bid_price, best_ask_price = best_ask_price))
    }
  }
  
  return(NULL)  # Return NULL if bid/ask prices are missing
}

# Dynamic VWAP spread threshold based on volatility
adjust_vwap_threshold <- function(volatility, ticker) {
  if (ticker == "DUCK") {
    return(ifelse(volatility == "High", 0.007, 0.005))  # Stricter for DUCK
  } else {
    return(ifelse(volatility == "High", 0.005, 0.003))  # More relaxed for other tickers
  }
}

# VWAP-based trading logic
trade_stock_vwap <- function(ticker, position, bid_ask, vwap, volatility) {
  if (is.null(bid_ask) || is.null(vwap)) return(NULL)
  
  best_bid_price <- bid_ask$best_bid_price
  best_ask_price <- bid_ask$best_ask_price
  order_size <- ORDER_LIMIT / 2
  
  # Determine VWAP threshold dynamically
  vwap_threshold <- adjust_vwap_threshold(volatility, ticker)
  
  # Aggressive trading for DUCK and OWL
  if (ticker == "DUCK" || ticker == "OWL") {
    if (!is.null(best_bid_price) && (vwap - best_bid_price) / vwap > vwap_threshold && position < MAX_LONG_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_bid_price + 0.01, action = "BUY"), 
           get_headers())
    }
    if (!is.null(best_ask_price) && (best_ask_price - vwap) / vwap > vwap_threshold && position > MAX_SHORT_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_ask_price - 0.01, action = "SELL"), 
           get_headers())
    }
  } else {
    # More conservative trading for CROW and DOVE
    if (!is.null(best_bid_price) && (vwap - best_bid_price) / vwap > vwap_threshold && position < MAX_LONG_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size / 2, price = best_bid_price + 0.01, action = "BUY"), 
           get_headers())
    }
    if (!is.null(best_ask_price) && (best_ask_price - vwap) / vwap > vwap_threshold && position > MAX_SHORT_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size / 2, price = best_ask_price - 0.01, action = "SELL"), 
           get_headers())
    }
  }
}

# Cancel unexecuted orders
cancel_orders <- function(ticker) {
  POST(paste0(BASE_URL, "/commands/cancel"), query = list(ticker = ticker), get_headers())
}

# Main function orchestrating the strategy
main <- function() {
  ticker_list <- c("OWL", "CROW", "DOVE", "DUCK")
  
  repeat {
    tick_status <- get_tick()
    if (is.null(tick_status)) break
    tick <- tick_status$tick
    status <- tick_status$status
    
    if (status != "ACTIVE") break
    
    for (ticker in ticker_list) {
      position <- get_position()
      bid_ask <- get_bid_ask(ticker)
      vwap <- calculate_vwap(ticker)
      trend <- calculate_trend(ticker)
      volatility <- ifelse(tick %% 10 == 0, "High", "Low")  # Simulated volatility condition
      
      trade_stock_vwap(ticker, position, bid_ask, vwap, volatility)
      
      Sys.sleep(0.75)  # Prevent API overload
      cancel_orders(ticker)  # Cancel unexecuted orders
    }
  }
}

# Run the main function
while (TRUE){
  main()
  Sys.sleep(1) #wait 1 second before running again
}

