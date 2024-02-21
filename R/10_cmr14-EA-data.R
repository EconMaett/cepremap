# 10 - Automating update of a finance database for the Euro Area ----
# Update the database used in Christiano et al. (2014), but for the
# Euro area, on the basis of the Smets and Wouters (2003) database.
# URL: https://macro.cepremap.fr/article/2016-06/cmr14-EA-data/
library(tidyverse)
library(zoo)
library(rdbnomics)
library(kableExtra)
source(file = "R/utils.R")
# Four financial time series are used in Christiano et al. (2014):
#   - Loans to non-financial corporations
#   - Bank lending rates
#   - Entrepreneurial net worth
#   - Long-term interest rates

# We add two series:
#   - House prices
#   - Loans to households

# The sources are:
#   - The Area-Wide Model (AWM) proposed by Fagan et al. (2001)
#   - International Financial Statistics (IFS) from the International Monetary Fund (IMF)
#   - Bank of International Settlements (BIS)
#   - European Central Bank (ECB)

## Loans to non-financial corporations and to households -----
# We download the loan series from the Bank for International Settlements (BIS).
EAtot_code  <- c("DE", "FI", "FR", "IT", "PT", "AT", "GR", "IE", "NL", "BE", "ES", "XM")
url_country <- paste0(EAtot_code, collapse = "+")
url_filter  <- paste0("Q.", url_country, ".N+H.A.M.XDC.A")
#   - N or H: Borrowing sector : NFC or Households
#   - A:      Lending sector : All
#   - M:      Valuation method : Market value
#   - XDC:    Unit type: Domestic currency
#   - A:      Adjustment : Adjustment for breaks
df <- rdb(provider_code = "BIS", dataset_code = "total_credit", mask = url_filter)

loans <- df |> 
  as_tibble() |> 
  select(period, series_code, value, BORROWERS_CTY, series_name) |> 
  rename(var = series_code, country = BORROWERS_CTY) |> 
  na.omit() |> 
  filter(year(period) >= 1980) |> 
  arrange(var, period)

loans_nfc <- loans |> 
  filter(substr(x = var, start = 6, stop = 6) == "N") |> 
  mutate(var = "loans_nfc")

varname_nfc <- unique(as.character(filter(.data = loans_nfc, country == "XM")$series_name))

loans_nfc <- loans_nfc |> 
  select(-series_name)

loans_hh <- loans |> 
  filter(substr(x = var, start = 6, stop = 6) == "H") |> 
  mutate(var = as.factor("loans_hh"))

varname_hh <- unique(as.character(filter(.data = loans_hh, country == "XM")$series_name))

loans_hh <- loans_hh |> 
  select(-series_name)

# Check the first date available for loans to non-financial corporations (NFC) 
# and households (HH), where XM stands for the Euro area.
loans_nfc |> 
  group_by(country) |> 
  summarise(first_date = min(period)) |> 
  arrange(first_date) |> 
  ungroup() |> 
  kable()

loans_hh |> 
  group_by(country) |> 
  summarise(first_date = min(period)) |> 
  arrange(first_date) |> 
  ungroup() |> 
  kable()

# Retain countries that are available since 1990
available_countries <- filter(.data = loans_nfc, period == "1990-10-01")$country

loans_nfc_countries <- loans_nfc |> 
  filter(country %in% available_countries)

loans_nfc_EA <- loans_nfc |> 
  filter(country == "XM", period >= "1999-01-01") |> 
  mutate(country = "EA")

# Plot credit to NFC
ggplot(data = bind_rows(loans_nfc_countries, loans_nfc_EA), mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ country, ncol = 3, scales = "free_y") +
  dbnomics() +
  ggtitle("Loans to non-financial corporations (billions of euro)")

ggsave(filename = "01_BIS_credit_nfc.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

loans_hh_countries <- loans_hh |> 
  filter(country %in% available_countries)

loans_hh_EA <- loans_hh |> 
  filter(country == "XM") |> 
  mutate(country = "EA")

# Plot loans to HH
ggplot(data = bind_rows(loans_hh_countries, loans_hh_EA), mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ country, ncol = 3, scales = "free_y") +
  dbnomics() +
  ggtitle("Loans to households and NPISHs (billions of euro)")

ggsave(filename = "02_BIS_credit_hh.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

# Compare the raw sum over the available countries and the
# and the same chained to the EA series.
loans_nfc_countries <- loans_nfc_countries |> 
  select(-var) |> 
  mutate(var = country)

loans_nfc_sumAll <- loans_nfc_countries |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  mutate(var = "sum")

loans_nfc_sumNoNL <- loans_nfc_countries |> 
  filter(! var == "NL") |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  mutate(var = "sum")

loans_nfc_sumNoNLESBE <- loans_nfc_countries |> 
  filter(! var %in% c("NL", "ES", "BE")) |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  mutate(var = "sum")

loans_nfc_chainedNL <- chain(
  to_rebase  = loans_nfc_sumNoNL,
  basis      = loans_nfc_sumAll,
  date_chain = "1990-10-01"
)

loans_nfc_chained <- chain(
  to_rebase  = loans_nfc_sumNoNLESBE,
  basis      = loans_nfc_chainedNL,
  date_chain = "1980-10-01"
  ) |> 
  mutate(var = "chained")

loans_nfc_EA <- loans_nfc_EA |> 
  select(-country) |> 
  mutate(var = "EA")

ggplot(data = bind_rows(loans_nfc_sumAll, loans_nfc_EA, loans_nfc_chained), mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  dbnomics() +
  ggtitle("Loans to non-financial corporations (billions of euro) [1]")

ggsave(filename = "03_BIS_credit_nfc_chained.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

print(varname_nfc)

loans_hh_countries <- loans_hh_countries |> 
  select(-var) |> 
  mutate(var = country)

loans_hh_sumAll <- loans_hh_countries |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  mutate(var = "sum")

loans_hh_sumNoNL <- loans_hh_countries |> 
  filter(! var == "NL") |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  mutate(var = "sum")

loans_hh_sumNoNLESBE <- loans_hh_countries |> 
  filter(! var %in% c("NL", "ES", "BE")) |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  mutate(var = "sum")

loans_hh_chainedNL <- chain(
  to_rebase  = loans_hh_sumNoNL,
  basis      = loans_hh_sumAll,
  date_chain = "1990-10-01"
)

loans_hh_chained <- chain(
  to_rebase  = loans_hh_sumNoNLESBE,
  basis      = loans_hh_chainedNL,
  date_chain = "1980-10-01"
  ) |> 
  mutate(var = "chained")

loans_hh_EA <- loans_hh_EA |> 
  select(-country) |> 
  mutate(var = "EA")

ggplot(data = bind_rows(loans_hh_sumAll, loans_hh_EA, loans_hh_chained), mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  dbnomics() +
  ggtitle("Loans to households and NPISHs (billions of euro) [1]")

ggsave(filename = "04_BIS_credit_hh_chained.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

print(varname_hh)

# Use the EA series in levels after 1999, and the growth rates
# of the sum of loans for all available countries to complete
# the series for historical data.
loans_nfc <- chain(
  to_rebase  = mutate(.data = loans_nfc_chained, var = "loans_nfc"),
  basis      = mutate(loans_nfc_EA, var = "loans_nfc"),
  date_chain = "1999-01-01"
)

loans_hh <- chain(
  to_rebase  = mutate(.data = loans_hh_chained, var = "loans_hh"),
  basis      = mutate(.data = loans_hh_EA, var = "loans_hh"),
  date_chain = "1999-01-01"
)

# Bank lending rates ----
### Historical data from OECD ----
# Use the OECD's Main Economic Indicators (MEI) to build a long 
# series of lending rates.
# Historical series from the ECB are available since 2000 Q1.
# Thus we only consider *five* countries.
# As in the AWM methodology, we weight the sum of the lending rates
# by the gross domestic product based on purchasing power-parity (PPP)
# of each country in 1995, accordign to the IMF World Economic Outlook (WEO).
country_code <- c("BEL", "FRA", "DEU", "ITA", "ESP")
url_country  <- paste0(country_code, collapse = "+")

# Download the 5 countries' lending rates from the OECD
url_filter <- paste0(url_country, ".IR3TIB01.ST.Q")
df <- rdb("OECD", "MEI", mask = url_filter)

lendingrate_bycountry <- df |> 
  select(country = LOCATION, period, value) |> 
  filter(year(period) >= 1980)

# Download the 5 countries' PPP GDP from WEO
url_filter <- paste0(url_country, ".PPPGDP")
df <- rdb("IMF", "WEO:latest", mask = url_filter)

ppppgdp <- df |> 
  filter(period == "1995-01-01") |> 
  select(country = `weo-country`, value_pppgdp = value)

sum_pppgdp <- sum(ppppgdp$value_pppgdp)

# Merge the two data sets and build a weighted mean
lendingrate_old <- left_join(
  x  = lendingrate_bycountry, 
  y  = ppppgdp, 
  by = join_by("country")
  ) |> 
  mutate(
    period  = period,
    country = country,
    value   = value * value_pppgdp,
    .keep = "none"
  ) |> 
  group_by(period) |> 
  summarise(value = sum(value) / sum_pppgdp) |> 
  mutate(var = "lendingrate_old")

### Recent data from ECB ----
df <- rdb(ids = "ECB/MIR/M.U2.B.A2A.A.R.A.2240.EUR.N")

varname <- unique(as.character(df$series_name))

lendingrate_recent <- df |> 
  select(period, value) |> 
  mutate(period = paste(year(period), quarter(period))) |> 
  group_by(period) |> 
  summarise(value = mean(value)) |> 
  mutate(var = "lendingrate_recent", period = yq(period))

# The recent bank lending rates from ECB are described as:
print(varname)

### Chain historical and recent data ----
dataplot <- bind_rows(lendingrate_recent, lendingrate_old)

ggplot(data = dataplot, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  dbnomics() +
  ggtitle("Bank lending rate (%)")

ggsave(filename = "05_ECB_lendingrates.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

## Chain historical and recent data ----
lendingrecent_value2000Q1 <- lendingrate_recent |> 
  filter(period == "2000-01-01") |> 
  pull(value)

lendingold_value2000Q1 <- lendingrate_old |> 
  filter(period == "2000-01-01") |> 
  pull(value)

difference <- lendingrecent_value2000Q1 - lendingold_value2000Q1

lendingrate <- lendingrate_old |> 
  filter(period <= "1999-10-01") |> 
  mutate(value = value + difference) |> 
  bind_rows(lendingrate_recent) |> 
  mutate(var = "lendingrate")

## Long-term interest rate ----
# Take the historical long-term interest rate from the AWM database.
# Recent data comes from the ECB.
link_to_awm <- "https://eabcn.org/sites/default/files/awm19up18.csv"

if (! "awm19up18.csv" %in% list.files(path = "data/")) {
  download.file(url = link_to_awm, destfile = "data/awm19up18.csv", method = "auto")
}

awm <- read.csv(file = "data/awm19up18.csv", sep = ",")

longrate_old <- awm |> 
  as_tibble() |> 
  mutate(
    longrate = LTN, # Long-Term Interest Rate (Nominal)
    period   = as.Date(zoo::as.yearqtr(X)),
    .keep = "none"
  ) |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  filter(year(period) >= 1980)

df <- rdb(ids = "ECB/IRS/M.U2.L.L40.CI.0000.EUR.N.Z")
# Original: ECB/IRS/M.I8.L.L40.CI.0000.EUR.N.Z

varname <- unique(as.character(df$series_name))

longrate_recent <- df |> 
  select(period, value) |> 
  mutate(period = paste(year(period), quarter(period))) |> 
  group_by(period) |> 
  summarise(value = mean(value)) |> 
  mutate(var = "longrate", period = yq(period))

dataplot <- bind_rows(tibble(longrate_recent, ind = "recent"), tibble(longrate_old, ind = "old"))

ggplot(data = dataplot, mapping = aes(x = period, y = value, color = ind)) +
  geom_line(linewidth = 1.2) +
  dbnomics() +
  ggtitle("Long-term interest rate (%)")

ggsave(filename = "06_ECB_lendingrates_longterm.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

longrate <- chain(
  basis      = longrate_recent,
  to_rebase  = longrate_old,
  date_chain = "2001-01-01"
)

# The recent long-term interest rates from the ECB are described as
print(varname)

## Entrepreneurial net worth ----
# Similar to Christiano et al. (2014), entrepreneurial net worth is
# approximated through the Dow Jones index for the Euro area.
df <- rdb(ids = "ECB/FM/Q.U2.EUR.DS.EI.DJEURST.HSTA")

varname <- unique(as.character(df$series_name))

networth <- df |> 
  select(value, period) |> 
  mutate(var = as.factor("networth"))

ggplot(data = networth, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  dbnomics() +
  ggtitle("Entrepreneurial net worth (index)")

ggsave(filename = "07_ECB_entrepreneurialnetworth.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

# The Dow Jones index from the ECB is described as
print(varname)

## House prices ----
df <- rdb(ids = "ECB/RPP/Q.I8.N.TD.00.3.00")

varname <- unique(as.character(df$series_name))

houseprice <- df |> 
  select(value, period) |> 
  mutate(var = as.factor("houseprice"))

ggplot(data = houseprice, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  dbnomics() +
  ggtitle("House prices (index)")

ggsave(filename = "08_ECB_houseprices.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

# House prices come from the BIS and are described as
print(varname)

## Final financial database for the Euro Area ----
# We build the final financial database with the six series
final_df <- bind_rows(
  loans_nfc,
  loans_hh,
  lendingrate,
  longrate,
  networth,
  houseprice
)

# Check the last available date for each variable
max_date <- final_df |> 
  filter(var != "houseprice") |> 
  group_by(var) |> 
  summarise(max_date = max(period)) |> 
  arrange(max_date)

kable(max_date)

min_max_date_finance <- min(max_date$max_date)
final_df <- final_df |> 
  filter(period <= min_max_date_finance)

# Filter the data base
plot_df <- final_df

list_var <- list(
  "Loans to NFC"              = "loans_nfc",
  "Loans to HH"               = "loans_hh",
  "Bank lending rate"         = "lendingrate",
  "Entrepreneurial net worth" = "networth",
  "Long-term interest rate"   = "longrate",
  "House prices"              = "houseprice"
)

plot_df$var <- factor(plot_df$var)
levels(plot_df$var) <- list_var

ggplot(data = plot_df, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ var, scales = "free_y", ncol = 3) +
  dbnomics()
  
ggsave(filename = "09_final.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

# You can also download the six final series here: http://shiny.cepremap.fr/data/EA_Finance_rawdata.csv
EA_Finance_rawdata <- final_df |> 
  pivot_wider(names_from = var, values_from = value)

write.csv(EA_Finance_rawdata, file = "data/EA_Finance_rawdata.csv", row.names = FALSE)

## Final CMR database for the Euro Area ----
# We want to build a data base similar to the one in Christiano et al. (2014)
# but on the basis of the Smets and Wouters (2003) data base.

# The data base will begin in 1980 Q1, as the financial series are not
# available before that time.

# You can download the raw series here: http://shiny.cepremap.fr/data/EA_CMR_rawdata.csv

# Import EA_SW_rawdata.csv
EA_SW_rawdata <- read.csv(file = "data/EA_SW_rawdata.csv") |> 
  as_tibble() |> 
  mutate(period = ymd(period))

min_max_date_raw <- max(EA_SW_rawdata$period, na.rm = TRUE)

EA_CMR_rawdata <- EA_SW_rawdata |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  bind_rows(final_df) |> 
  filter(period <= min(min_max_date_raw, min_max_date_finance), period >= "1980-01-01") |> 
  pivot_wider(names_from = var, values_from = value)

write.csv(EA_CMR_rawdata, file = "data/EA_CMR_rawdata.csv", row.names = FALSE)

# Normalize the series by population and price if needed.
# Eventually we have 14 series, similar to the 12 series in Christiano et al. (2014)
# plus the households and house price series.
EA_CMR_data <- EA_CMR_rawdata |> 
  mutate(
    period = gsub(pattern = " ", replacement = "", x = zoo::as.yearqtr(period)),
    gdp_rpc         = 1e+6 * gdp / (pop * 1000),
    conso_rpc       = 1e+6 * conso / (pop * 1000),
    inves_rpc       = 1e+6 * inves / (pop * 1000),
    defgdp          = defgdp,
    wage_rph        = 1e+6 * wage / defgdp / (hours * 1000),
    hours_pc        = 1000 * hours / (pop * 1000),
    pinves_defl     = definves / defgdp,
    loans_nfc_rpc   = 1e+9 * loans_nfc / (pop * 1000) / defgdp,
    loans_hh_rpc    = 1e+9 * loans_hh / (pop * 1000) / defgdp,
    houseprice_defl = houseprice / defgdp,
    networth_rpc    = 1e+6 * networth / (pop * 1000) / defgdp,
    re              = shortrate / 100,
    slope           = (longrate - shortrate) / 100,
    creditspread    = (lendingrate - shortrate) / 100,
    .keep = "none"
  )

write.csv(EA_CMR_data, file = "data/EA_CMR_data.csv", row.names = FALSE)

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

plot_EA_CMR_data <- EA_CMR_data |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  mutate(period = as.Date(zoo::as.yearqtr(period)), var = as.factor(var))

levels(plot_EA_CMR_data$var) <- list_var

ggplot(data = plot_EA_CMR_data, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ var, ncol = 3, scales = "free_y") +
  dbnomics() +
  ggtitle("CMR data for the Euro area")

ggsave(filename = "10_CMR_EA.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

# Download the ready-to-use (normalized) data here: http://shiny.cepremap.fr/data/EA_CMR_data.csv

# END