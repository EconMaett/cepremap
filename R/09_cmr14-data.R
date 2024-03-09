# 09 - Christiano, Motto and Rostagno (CMR) (2014) database for the United States -----
# URL: https://macro.cepremap.fr/article/2016-06/cmr14-data/
library(tidyverse)
library(zoo)
library(rdbnomics)
library(fredr)
library(kableExtra)
source("R/utils.R")
fig_path <- "figures/09_cmr14-data/"
# Twelve series are needed for the Christiano, Motto and Rostagno (CMR) model:
#    1. GDP
#    2. GDP deflator
#    3. Consumption
#    4. Investment
#    5. Investment deflator
#    6. Wages
#    7. Hours worked
#    8. Loans to non-financial corporations (NFCs)
#    9. Short-term interest rate
#   10. Entrepreneurial net worth
#   11. Credit spread
#   12. Term spread

# Two more series are added:
#    1. Loans to households (HH) and non-profit institutions serving households (NPISHs)
#    2. House prices

# Sources such as BEA, BIS, BLS, and OECD are accessed form DBnomics 
# with the `rdbnomics` R package.

# The Saint Louis Fed's FRED Database is used to retrieve:
#   1. The Wilshire 5000 Total Market Index
#   2. Moody's Seasoned Baa Corporate Bond Yield

## Raw data from BEA, BIS, BLS and OECD ----
df <- rdb(
  ids = c(
    "BEA/NIPA-T10106/A191RX-Q",
    "BEA/NIPA-T10109/A191RD-Q",
    "BEA/NIPA-T10106/A006RX-Q",
    "BEA/NIPA-T10109/A006RD-Q",
    "BIS/total_credit/Q.US.N.A.M.XDC.A",
    "BIS/total_credit/Q.US.H.A.M.XDC.A",
    "BIS/selected_pp/Q.US.N.628",
    "BLS/pr/PRS85006033",
    "BLS/pr/PRS85006103",
    "OECD/MEI/USA.IRLTLT01.ST.Q",
    "OECD/MEI/USA.LFWA64TT.STSA.Q"
    )
  ) |> 
  as_tibble() |> 
  mutate(
    series_name = case_when(
      str_detect(series_code, "RD-") ~ paste("Deflator,", series_name),
      str_detect(series_code, "RX-") ~ paste("Real,", series_name),
      str_detect(series_code, "Q.US.N.A.M.XDC.A") ~ paste("Loans to non-financial corporations,", series_name),
      str_detect(series_code, "Q.US.H.A.M.XDC.A") ~ paste("Loans to households and NPISHs", series_name),
      str_detect(series_code, "Q.US.N.628") ~ paste("Property prices", series_name),
      TRUE ~ series_name
    )
  ) |> 
  select(var_name = series_name, var_code = series_code, value, period)

# Rename the variable codes
df <- df |> 
  mutate(
    var_code = case_when(
      var_code == "A191RX-Q"            ~ "gdp",
      var_code == "A006RX-Q"            ~ "inves",
      var_code == "A191RD-Q"            ~ "defgdp",
      var_code == "A006RD-Q"            ~ "definves",
      var_code == "Q.US.H.A.M.XDC.A"    ~ "loans_hh",
      var_code == "Q.US.N.A.M.XDC.A"    ~ "loans_nfc",
      var_code == "Q.US.N.628"          ~ "houseprice",
      var_code == "PRS85006033"         ~ "hours",
      var_code == "PRS85006103"         ~ "wage",
      var_code == "USA.LFWA64TT.STSA.Q" ~ "pop",
      var_code == "USA.IRLTLT01.ST.Q"   ~ "longrate"
    )
  )

# Take quarterly means of the short-term interest rate 
shortrate <- rdb("FED", "H15", mask = "129.FF.O") |> 
  as_tibble() |> 
  mutate(period = paste(year(period), quarter(period), sep = "-")) |> 
  group_by(period) |> 
  summarise(value = mean(value)) |> 
  mutate(
    var_code = "shortrate",
    var_name = "Monthly - Federal funds - Overnight",
    period   = yq(period)
  )

### Special case of consumption ----
# - The Bureau of Economic Analysis (BEA) series of private consumption starts in 2002.
# - Use growth rates available before 2002 to deduce past consumption levels.
# - Aggregate consumption is the sum of non-durable goods and services.
# - Durable goods are associated with investment.

# Consumption levels, start in 2007-01-01?
conso_level <- rdb(
  ids = c(
    "BEA/NIPA-T20306/DDURRX-Q",
    "BEA/NIPA-T20306/DNDGRX-Q",
    "BEA/NIPA-T20306/DSERRX-Q"
    )
  ) |> 
  select(period, value, var_name = concept)

# Consumption growth rates - start in 1947/1948
conso_rate <- rdb(
  ids = c(
    "BEA/NIPA-T20301/DDURRL-Q",
    "BEA/NIPA-T20301/DNDGRL-Q",
    "BEA/NIPA-T20301/DSERRL-Q"
    )
  ) |> 
  select(period, value, var_name = concept)

# Original: 2002. Now: 2007
conso_level_07 <- conso_level |> 
  filter(period == "2007-01-01")

# Use past growth rates to retrieve past levels
conso <- conso_rate |> 
  filter(period <= "2007-01-01") |> 
  full_join(y = conso_level_07, by = join_by("var_name")) |> 
  group_by(var_name) |> 
  arrange(desc(period.x)) |> 
  mutate(value = value.y / lag(cumprod((1 + value.x/100)^(1/4)))) |> 
  ungroup() |> 
  mutate(
    period   = period.x,
    var_name = var_name,
    value    = value,
    .keep = "none"
  ) |> 
  na.omit() |> 
  bind_rows(conso_level) |> 
  filter(period >= "1980-01-01")

ggplot(conso, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~ var_name, nrow = 3, scales = "free_y") +
  my_theme() +
  ggtitle("Real Personal Consumption Expenditures")

ggsave("01_consumption.png", path = fig_path, height = 4, width = 6)
graphics.off()

# Create variables "var_code" and "var_name"
conso <- conso |> 
  mutate(
    var_code = case_when(
      var_name == "durable-goods"    ~ "conso_d",
      var_name == "nondurable-goods" ~ "conso_nd",
      var_name == "services"         ~ "conso_s"
    ),
    var_name = paste("Reap Personal Consumption Expenditures,", var_name)
  )

### Financial data from FRED ----
#   1. Moody's Seasoned Baa Corporate Bond Yield (BAA) 
#      URL: https://fred.stlouisfed.org/series/BAA
#   2. Wilshire 5000 Total Market Index (WILL5000IND) 
#      URL: https://fred.stlouisfed.org/series/WILL5000IND
params <- list(
  series_id          = c("BAA", "WILL5000IND"),
  frequency          = c("q", "q"),
  aggregation_method = c("avg", "avg")
)

fred_data <- pmap_dfr(.l = params, .f = ~ fredr(series_id = .x, frequency = .y)) |> 
  mutate(
    var_name = case_when(
      series_id == "BAA"         ~ "Moody's Seasoned Baa Corporate Bond Yield",
      series_id == "WILL5000IND" ~ "Wilshire 5000 Total Market Index"
    ),
    var_code = case_when(
      series_id == "BAA"         ~ "riskrate",
      series_id == "WILL5000IND" ~ "networth"
    )
  ) |> 
  mutate(
    period   = ymd(date),
    value    = value,
    var_code = var_code,
    var_name = var_name,
    .keep = "none"
  )

### Final database and normalization ----
rawdata <- bind_rows(conso, df, shortrate, fred_data) |> 
  filter(year(period) >= 1980)

var_names <- unique(rawdata$var_name)
var_names <- gsub("Expenditures,.*", "", var_names) |> 
  unique()

# Check the last available date for each variable
max_date <- rawdata |> 
  filter(var_code != "houseprice") |> 
  group_by(var_code) |> 
  summarise(max_date = max(period)) |> 
  arrange(max_date)

kable(max_date)

min_max_date <- min(max_date$max_date)
rawdata <- rawdata |> 
  filter(period <= min_max_date) |> 
  select(-var_name)

rawdata |> 
  pivot_wider(names_from = var_code, values_from = value) |> 
  write.csv("data/US_CMR_rawdata.csv", row.names = FALSE)

# Raw data here: http://shiny.cepremap.fr/data/US_CMR_rawdata.csv

# Normalize the data by population and price level.
US_CMR_data <- rawdata |> 
  pivot_wider(names_from = var_code, values_from = value) |> 
  mutate(
    period          = period,
    gdp_rpc         = 1e+9 * gdp / (1000 * pop),
    conso_rpc       = 1e+9 * (conso_nd + conso_s) / (1000 * pop),
    inves_rpc       = 1e+9 * (inves + conso_d) / (1000 * pop),
    defgdp          = defgdp / 100,
    wage_rph        = wage / defgdp,
    hours_pc        = 1e+9 * hours / (1000 * pop),
    pinves_defl     = definves / defgdp,
    loans_nfc_rpc   = 1e+9 * loans_nfc / (1000 * pop) / defgdp,
    loans_hh_rpc    = 1e+9 * loans_hh / (1000 * pop) / defgdp,
    houseprice_defl = houseprice / defgdp,
    networth_rpc    = 1e+9 * networth / (1000 * pop) / defgdp,
    re              = shortrate / 100,
    slope           = (longrate - shortrate) / 100,
    creditspread    = (riskrate - longrate) / 100,
    .keep = "none"
  )

US_CMR_data |> 
  mutate(period = gsub(" ", "", zoo::as.yearqtr(as.Date(period)))) |> 
  write.csv("data/US_CMR_data.csv", row.names = FALSE)

list_var <- list(
  "Real GDP per capita"            = "gdp_rpc",
  "Real consumption per capita"    = "conso_rpc",
  "Real investment per capita"     = "inves_rpc",
  "Real credit to NFC per capita"  = "loans_nfc_rpc",
  "Real credit to HH per capita"   = "loans_hh_rpc",
  "Real house price"               = "houseprice_defl",
  "Real net worth per capita"      = "networth_rpc",
  "Real price of investment"       = "pinves_defl",
  "Real wage per capita"           = "wage_rph",
  "GDP deflator"                   = "defgdp",
  "Hours worked per capita"        = "hours_pc",
  "Short-term interest rate (APR)" = "re",
  "Credit spread (APP)"            = "creditspread",
  "Term premium (APP)"             = "slope"
  )

plot_US_CMR_data <- US_CMR_data |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  mutate(var = as.factor(var))

levels(plot_US_CMR_data$var) <- list_var

ggplot(plot_US_CMR_data, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~ var, ncol = 3, scales = "free_y") +
  my_theme() +
  ggtitle("CMR data for the US")

ggsave("02_CMR_US.png", path = fig_path, height = 4, width = 6)
graphics.off()

# Normalized data: http://shiny.cepremap.fr/data/US_CMR_data.csv
# END