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
      # Calculate VWAP
      total_value <- sum(sapply(tas_data, function(trade) trade$price * trade$quantity))
      total_volume <- sum(sapply(tas_data, function(trade) trade$quantity))
      
      if (total_volume > 0) {
        return(total_value / total_volume)
      }
    }
  }
  
  warning(sprintf("VWAP data unavailable for ticker: %s", ticker))
  return(NULL)
}

# Function to get bid and ask prices
get_bid_ask <- function(ticker) {
  resp <- GET(paste0(BASE_URL, "/securities/book"), query = list(ticker = ticker), get_headers())
  
  if (status_code(resp) == 200) {
    book <- content(resp, as = "parsed")
    if (!is.null(book$bids) && length(book$bids) > 0 &&
        !is.null(book$asks) && length(book$asks) > 0) {
      best_bid_price <- book$bids[[1]]$price
      best_ask_price <- book$asks[[1]]$price
      return(list(best_bid_price = best_bid_price, best_ask_price = best_ask_price))
    }
  }
  
  warning(sprintf("Invalid or empty order book for ticker: %s", ticker))
  return(NULL)
}

# Function to get current position
get_position <- function() {
  resp <- GET(paste0(BASE_URL, "/securities"), get_headers())
  
  if (status_code(resp) == 200) {
    securities <- content(resp, as = "parsed")
    total_position <- sum(sapply(securities, function(security) security$position))
    return(total_position)
  }
  
  warning("Unable to fetch positions.")
  return(0)
}

# VWAP-based trading logic
trade_stock_vwap <- function(ticker, position, bid_ask, vwap) {
  if (is.null(bid_ask) || is.null(vwap)) {
    warning(sprintf("Skipping VWAP trade for ticker: %s due to missing data.", ticker))
    return(NULL)
  }
  
  best_bid_price <- bid_ask$best_bid_price
  best_ask_price <- bid_ask$best_ask_price
  order_size <- ORDER_LIMIT / 2  # Smaller order size for better control
  
  # Buy if price is below VWAP and position is within limits
  if (best_bid_price < vwap && position < MAX_LONG_EXPOSURE) {
    POST(paste0(BASE_URL, "/orders"), 
         query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_bid_price + 0.01, action = "BUY"), 
         get_headers())
  }
  
  # Sell if price is above VWAP and position is within limits
  if (best_ask_price > vwap && position > MAX_SHORT_EXPOSURE) {
    POST(paste0(BASE_URL, "/orders"), 
         query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_ask_price - 0.01, action = "SELL"), 
         get_headers())
  }
}

# Cancel unexecuted orders
cancel_orders <- function(ticker) {
  POST(paste0(BASE_URL, "/commands/cancel"), query = list(ticker = ticker), get_headers())
}

# Main function orchestrating the VWAP strategy
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
      
      # Execute VWAP-based trading logic
      trade_stock_vwap(ticker, position, bid_ask, vwap)
      
      Sys.sleep(0.75)  # Avoid overwhelming the API
      cancel_orders(ticker)  # Cancel unexecuted orders
    }
  }
}


# Run the main function
while (TRUE){
  main()
  Sys.sleep(1) #wait 1 second before running again
}
