# Format Data -------------------------------------------------------------
db_bloomberg <- readxl::read_excel("data/backtest_bloomberg.xlsx", skip = 3) %>%
  filter(row_number() > 1) %>%
  rename(date = Dates) %>%
  select(c("date", "NCI Index", "BCOM Index", "IFIX Index", "XFIX11 BZ Equity", "CBRAIMA BZ Equity",
           "IDADDI Index", "IFMMIFMM Index", "IHFAIHFA Index", "USDBRL Curncy", "EURBRL Curncy",
           "XAD Curncy", "XBNUSD Curncy", "XBI Curncy", "XBTUSD Curncy", "EOS Curncy",
           "XETUSD Curncy","XLCUSD Curncy", "XDTUSD Curncy", "XLM Curncy", "XRP Curncy"))

exchange_rate <- db_bloomberg$`USDBRL Curncy`

db_cci <- read.csv("data/backtest_cci30.csv") %>%
  transmute(date = as.Date(Date), `CCI30 Index` = Close)

db_backtest <- left_join(db_bloomberg, db_cci) %>%
  mutate(across(-date, as.numeric)) %>%
  pivot_longer(-date, values_to = "price.adjusted", names_to = "name") %>%
  left_join(readxl::read_excel("data/backtest_metadata.xlsx")) %>%
  mutate(price.adjusted = case_when(currency == "USD" ~ price.adjusted * exchange_rate, TRUE ~ price.adjusted),
         rf = case_when(country == "Brasil" ~ "CDI", TRUE ~ "TNX"),
         date = as.Date(date))

saveRDS(db_backtest, file = "data/db_backtest.RDS")


# Statistical Analisys ----------------------------------------------------
# Graphical analisys
base_graph <- ggplot(db_backtest) + facet_wrap(vars(name), scales = "free")

base_graph + geom_line(aes(date, price.close))
base_graph + geom_histogram(aes(price.close))
base_graph + geom_boxplot(aes(date, price.close))

# Proportional number of outliers with IQR criterion
db_backtest %>%
  group_split(name) %>%
  map_dbl(~ {length(boxplot.stats(.x$price.close)$out) /
              sum(!is.na(.x$price.close))} %>%
          round(3)) %>%
  set_names(unique(db_backtest$name))






