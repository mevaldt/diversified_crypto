# Libraries #
library(DT)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog) 
library(dplyr)
library(shiny)
library(tidyr)
library(readr)
library(purrr)
library(broom)
library(plotly)
library(ggplot2)
library(janitor)
library(shinyBS)
library(shinyjs)
library(stringr)
library(quantmod)
library(heatmaply)
library(tidyquant)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders) #devtools::install_github('andrewsali/shinycssloaders')
#library(rCharts) # devtools::install_github('ramnathv/rCharts')

# calling label.help before sourcing ui

label.help <- function(label,id){
  shiny::HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

# To avoid masking
lag <- dplyr::lag
filter <- dplyr::filter
select <- dplyr::select
spread <- tidyr::spread

### Sourcing additional functions ----

source('util_scripts/fPortfolio_functions.R')
source('util_scripts/risk_free_rate.R')
source('util_scripts/get_latest_data.R')

# world_map <- readRDS('data/world_map_simplified.rds') # Fetched from http://thematicmapping.org/downloads/world_borders.php and simplified with rmapshaper for increased performance

full_data <-
  readRDS('data/full_data.rds')

lst_names <-
  full_data %>% 
  select(
    name, symbol, type
  ) %>% 
  distinct() %>% 
  rename(
    origin = type
  ) %>% 
  mutate(
    origin = ifelse(origin == 'Crypto', 'Cryptocurrencies',
                    ifelse(origin == 'Ticker', 'Tickers', origin))
  ) %>% 
  arrange(origin, name) %>% 
  mutate(origin = factor(origin, levels = c('Cryptocurrencies', 'Tickers', 'Commodities', 'Corporate Bonds', 'Real Estate'))) %>% 
  group_by(origin) %>% 
  arrange(.by_group = TRUE) %>% 
  ungroup()

all_crypto_markers_name <- as.character(unique(full_data$name))

numeric_variables_names <- 
  c("Price Adjusted" = "price.adjusted",
    "Price Adjusted - USD" = 'price.adjusted.usd',
    "Price High" = "price.high",
    "Price Low" = "price.low",
    "Price Open" = "price.open",
    "Adjusted Prices Returns" = "ret.adjusted.prices",
    "Adjusted Prices Returns - USD" = "ret.adjusted.prices.usd",
    "Volume" = "volume"
  )
  
choices_hierarchical_list <- list(
  Cryptocurrencies = lst_names %>% 
    filter(origin == 'Cryptocurrencies') %>% 
    pull(name),
  `Stock Market Indexes` = lst_names %>% 
    filter(origin == 'Tickers') %>% 
    pull(name),
  Commodities = lst_names %>% 
    filter(origin == 'Commodities') %>% 
    pull(name),
  `Corporate Bonds` = lst_names %>% 
    filter(origin == 'Corporate Bonds') %>% 
    pull(name),
  `Real Estate` = lst_names %>% 
    filter(origin == 'Real Estate' & symbol != "") %>% 
    pull(name)
)

### Loading UI elements

source('ui/landing_page.R')
source('ui/time_series.R')
source('ui/portfolio_analysis.R')
source('ui/descriptive_statistics.R')
source('ui/about.R')
