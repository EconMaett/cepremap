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
EAtot_code <- c("DE", "FI", "FR", "IT", "PT", "AT", "GR", "IE", "NL", "BE", "ES", "XM")

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

# We only retain countries which are available before 1990 to compute the aggregate
available_countries <- filter(.data = loans_nfc, period == "1990-10-01")$country

loans_nfc_countries <- loans_nfc |> 
  filter(country %in% available_countries)

loans_nfc_EA <- loans_nfc |> 
  filter(country == "XM", period >= "1999-01-01") |> 
  mutate(country = "EA")

# Plot credit to NFC
ggplot(data = bind_rows(loans_nfc_countries, loans_nfc_EA), mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = "dodgerblue3") +
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
  geom_line(linewidth = 1.2, color = "dodgerblue3") +
  facet_wrap(facets = ~ country, ncol = 3, scales = "free_y") +
  dbnomics() +
  ggtitle("Loans to households and NPISHs (billions of euro)")

ggsave(filename = "02_BIS_credit_hh.png", path = "figures/10_cmr14-EA-data/", height = 12, width = 12)
graphics.off()

# Next we compare the raw sum over the available countries and the
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
  date_chain = "1989-10-01"
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

varname_nfc

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

varname_hh

# Eventually, we use the EA series in levels after 1999, and the growth rates
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

# To build long series of lending rates, we use data from 
# the OECD's Main Economic Indicators (MEI).

# Historical series from the ECB are available since 2000 Q1.




### Recent data from ECB ----

### Chain historical and recent data ----


## Long-term interest rate ----


## Entrepreneurial net worth ----


## House prices ----


## Final financial database for the Euro Area ----


## Final CMR database for the Euro Area ----


## Appendix ----

### Chaining function -----


# END