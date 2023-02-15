library(readxl)
library(tidyr)
library(dplyr)

data <- read_xlsx('~/Downloads/dados_bloomberg.xlsx', sheet = 1)

data <- readr::read_csv2('~/Downloads/dados_bloomberg.csv')

db_prices <- data %>% 
  mutate_all(~if_else(. == "#N/A", NA_character_, .)) %>% 
  mutate_all(~stringr::str_replace_all(., ',', '\\.')) %>% 
  mutate_at(vars(-Date), as.numeric) %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = c(-'date'), names_to = 'symbol', values_to = 'adjusted') %>% 
  arrange(symbol, date) %>% 
  mutate(date = as.Date(date, '%d/%m/%Y')) 

saveRDS(db_prices, 'data/db_prices2.RDS')

ticker_translate <- readr::read_csv2('~/Downloads/ticker_translate.csv')

db_symbol_country <- ticker_translate %>% 
  as_tibble() %>% 
  filter(!is.na(name)) %>% 
  rename(symbol = name_bloomberg)

saveRDS(db_symbol_country, file = 'data/db_symbol_country.RDS')
  
#---- new data ----

data <- readr::read_csv2('~/Downloads/dados_complementares.csv')

db_prices_complement <- data %>% 
  mutate_all(~if_else(. %in% c("#N/A", '-'), NA_character_, .)) %>% 
  mutate_all(~stringr::str_replace_all(., ',', '\\.')) %>% 
  mutate_at(vars(-Date), as.numeric) %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = c(-'date'), names_to = 'symbol', values_to = 'adjusted') %>% 
  arrange(symbol, date) %>% 
  mutate(date = as.Date(date, '%d/%m/%Y')) 

new_db <- bind_rows(readRDS('data/db_prices2.RDS'), db_prices_complement)

saveRDS(new_db, file = 'data/db_prices2.RDS')
