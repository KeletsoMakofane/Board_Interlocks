library(dplyr)
library(polite)
library(purrr)
library(rvest)

ticker.names <- read.csv("ticker_names.csv") %>%
  pull(Name) %>%
  str_split_fixed("-", 10) %>%
  {.[,1]}

company.names <- read.csv("ticker_names.csv") %>%
  pull(Name) 

company.info <- data.frame(ticker = ticker.names, company = company.names)
saveRDS(company.info, "company_info.R")

get_board_quick <- function(x) {
  Sys.sleep(1)
  
  data <- read_html( paste0("https://www.wsj.com/market-data/quotes/ZA/XJSE/", x, "/company-people")) 
  
  names <- data %>%
    html_nodes("table.cr_board_table span.data_data a")  %>%
    html_text() %>%
    str_trim() 
  
  titles <- data %>%
    html_nodes("table.cr_board_table span.data_data + span") %>%
    html_text() %>%
    str_trim() 
  
  data.frame(name = names, company = x, title = titles)
}


get_board_quick_safely <- safely(get_board_quick)

get_boards_quick <- function(ticker_names){
  map(ticker_names, get_board_quick_safely) 
}

list_data <- get_boards_quick(ticker.names)
saveRDS(list_data, "board_interlock/list_data.R")

data_clean <- list_data %>%
  map(function(x) x[[1]]) %>%
  bind_rows() %>%
  filter(!is.na(name))
  
saveRDS(data_clean, "board_interlock/data_clean.R")


