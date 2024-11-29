# Global variables
MAX_LONG_EXPOSURE <- 300000
MAX_SHORT_EXPOSURE <- -100000
ORDER_LIMIT <- 5000

# Set API key in a global environment
api_key <- "NLJFH66O"
base_url <- "http://localhost:9999/v1"

# Helper function to set headers
get_headers <- function() {
  add_headers(`X-API-key` = api_key)
}

# Function to get current tick and status
get_tick <- function() {
  resp <- GET(paste0(base_url, "/case"), get_headers())
  if (status_code(resp) == 200) {
    case <- content(resp, as = "parsed")
    return(list(tick = case$tick, status = case$status))
  }
  return(NULL)
}

# Function to get bid and ask prices
get_bid_ask <- function(ticker) {
  resp <- GET(paste0(base_url, "/securities/book"), query = list(ticker = ticker), get_headers())
  if (status_code(resp) == 200) {
    book <- content(resp, as = "parsed")
    best_bid_price <- book$bids[[1]]$price
    best_ask_price <- book$asks[[1]]$price
    return(list(best_bid_price = best_bid_price, best_ask_price = best_ask_price))
  }
  return(NULL)
}

# Function to get time and sales data
get_time_sales <- function(ticker) {
  resp <- GET(paste0(base_url, "/securities/tas"), query = list(ticker = ticker), get_headers())
  if (status_code(resp) == 200) {
    book <- content(resp, as = "parsed")
    time_sales_book <- sapply(book, function(item) item$quantity)
    return(time_sales_book)
  }
  return(NULL)
}

# Function to get position
get_position <- function() {
  resp <- GET(paste0(base_url, "/securities"), get_headers())
  if (status_code(resp) == 200) {
    book <- content(resp, as = "parsed")
    positions <- sum(sapply(book, function(item) item$position))
    return(positions)
  }
  return(0)
}

# Function to get open orders
get_open_orders <- function(ticker) {
  resp <- GET(paste0(base_url, "/orders"), query = list(ticker = ticker), get_headers())
  if (status_code(resp) == 200) {
    orders <- content(resp, as = "parsed")
    buy_orders <- Filter(function(item) item$action == "BUY", orders)
    sell_orders <- Filter(function(item) item$action == "SELL", orders)
    return(list(buy_orders = buy_orders, sell_orders = sell_orders))
  }
  return(NULL)
}

# Function to get order status
get_order_status <- function(order_id) {
  resp <- GET(paste0(base_url, "/orders/", order_id), get_headers())
  if (status_code(resp) == 200) {
    order <- content(resp, as = "parsed")
    return(order$status)
  }
  return(NULL)
}

# Main function
main <- function() {
  tick_status <- get_tick()
  tick <- tick_status$tick
  status <- tick_status$status
  
  ticker_list <- c("OWL", "CROW", "DOVE", "DUCK")
  
  while (status == "ACTIVE") {
    for (ticker_symbol in ticker_list) {
      position <- get_position()
      bid_ask <- get_bid_ask(ticker_symbol)
      if (!is.null(bid_ask)) {
        best_bid_price <- bid_ask$best_bid_price
        best_ask_price <- bid_ask$best_ask_price
        
        if (position < MAX_LONG_EXPOSURE) {
          POST(paste0(base_url, "/orders"), 
               query = list(
                 ticker = ticker_symbol, 
                 type = "LIMIT", 
                 quantity = ORDER_LIMIT, 
                 price = best_bid_price, 
                 action = "BUY"
               ), 
               get_headers())
        }
        
        if (position > MAX_SHORT_EXPOSURE) {
          POST(paste0(base_url, "/orders"), 
               query = list(
                 ticker = ticker_symbol, 
                 type = "LIMIT", 
                 quantity = ORDER_LIMIT, 
                 price = best_ask_price, 
                 action = "SELL"
               ), 
               get_headers())
        }
        
        Sys.sleep(0.75)
        
        POST(paste0(base_url, "/commands/cancel"), 
             query = list(ticker = ticker_symbol), 
             get_headers())
      }
    }
    tick_status <- get_tick()
    tick <- tick_status$tick
    status <- tick_status$status
  }
}

# Run the main function
main()
