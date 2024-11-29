# Code for RMSC 2024 - Algorithmic Trading

# Required libraries
#required_packages <- c("httr", "jsonlite")

# Download Missing packages
#missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
#sapply(missing_packages, install.packages)

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

# Function to get bid and ask prices
get_bid_ask <- function(ticker) {
  resp <- GET(paste0(BASE_URL, "/securities/book"), query = list(ticker = ticker), get_headers())
  if (status_code(resp) == 200) {
    book <- content(resp, as = "parsed")
    if (length(book$bids) > 0 && length(book$asks) > 0) {
      best_bid_price <- book$bids[[1]]$price
      best_ask_price <- book$asks[[1]]$price
      if (!is.null(best_bid_price) && !is.null(best_ask_price) &&
          best_bid_price > 0 && best_ask_price > 0) {
        return(list(best_bid_price = best_bid_price, best_ask_price = best_ask_price))
      }
    }
  }
  return(NULL)  # Return NULL if prices are invalid
}

# Function to get position
get_position <- function() {
  resp <- GET(paste0(BASE_URL, "/securities"), get_headers())
  if (status_code(resp) == 200) {
    book <- content(resp, as = "parsed")
    positions <- sum(sapply(book, function(item) item$position))
    return(positions)
  }
  return(0)
}

# Function to cancel open orders for a ticker
cancel_orders <- function(ticker) {
  POST(paste0(BASE_URL, "/commands/cancel"), query = list(ticker = ticker), get_headers())
}

# Function to trade a stock based on its specific strategy
trade_stock <- function(ticker, position, volatility, rebate, fee) {
  bid_ask <- get_bid_ask(ticker)
  if (is.null(bid_ask)) {
    warning(sprintf("Skipping ticker due to invalid bid/ask prices: %s", ticker))
    return(NULL)
  }
  
  best_bid_price <- bid_ask$best_bid_price
  best_ask_price <- bid_ask$best_ask_price
  order_size <- ifelse(volatility == "High", ORDER_LIMIT / 2, ORDER_LIMIT)
  
  if (ticker == "OWL") {
    # OWL Strategy: Positive rebate, low volatility
    if (position < MAX_LONG_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_bid_price + 0.01, action = "BUY"), 
           get_headers())
    }
    if (position > MAX_SHORT_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_ask_price - 0.01, action = "SELL"), 
           get_headers())
    }
  } else if (ticker == "CROW") {
    # CROW Strategy: Negative rebate, medium volatility
    if (position < MAX_LONG_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_bid_price + 0.02, action = "BUY"), 
           get_headers())
    }
    if (position > MAX_SHORT_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_ask_price - 0.02, action = "SELL"), 
           get_headers())
    }
  } else if (ticker == "DOVE") {
    # DOVE Strategy: Negative rebate, high volatility
    if (position < MAX_LONG_EXPOSURE && best_bid_price < best_ask_price * 0.98) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "MARKET", quantity = order_size / 2, action = "BUY"), 
           get_headers())
    }
    if (position > MAX_SHORT_EXPOSURE && best_ask_price > best_bid_price * 1.02) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "MARKET", quantity = order_size / 2, action = "SELL"), 
           get_headers())
    }
  } else if (ticker == "DUCK") {
    # DUCK Strategy: Positive rebate, medium volatility
    if (position < MAX_LONG_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_bid_price + 0.02, action = "BUY"), 
           get_headers())
    }
    if (position > MAX_SHORT_EXPOSURE) {
      POST(paste0(BASE_URL, "/orders"), 
           query = list(ticker = ticker, type = "LIMIT", quantity = order_size, price = best_ask_price - 0.02, action = "SELL"), 
           get_headers())
    }
  }
}

# Main function to orchestrate trading
main <- function() {
  ticker_list <- list(
    OWL = list(volatility = "Low", rebate = 0.04, fee = 0.03),
    CROW = list(volatility = "Medium", rebate = -0.03, fee = -0.02),
    DOVE = list(volatility = "High", rebate = -0.04, fee = -0.03),
    DUCK = list(volatility = "Medium", rebate = 0.03, fee = 0.02)
  )
  
  repeat {
    tick_status <- get_tick()
    if (is.null(tick_status)) break
    tick <- tick_status$tick
    status <- tick_status$status
    
    if (status != "ACTIVE") break
    
    for (ticker in names(ticker_list)) {
      position <- get_position()
      trade_stock(
        ticker, 
        position, 
        ticker_list[[ticker]]$volatility, 
        ticker_list[[ticker]]$rebate, 
        ticker_list[[ticker]]$fee
      )
      
      Sys.sleep(0.75)  # Prevent overwhelming the API
      cancel_orders(ticker)  # Cancel unexecuted orders
    }
  }
}


# Run the main function
while (TRUE){
  main()
  Sys.sleep(1) #wait 1 second before running again
}
