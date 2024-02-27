# 06 - Automating update of an international database for the Euro Area ----
# URL: https://macro.cepremap.fr/category/data2.html
library(tidyverse)
library(zoo)
library(rdbnomics)
library(kableExtra)
source(file = "R/utils.R")
# Create an international quarterly data base for the Euro area
# that can be updated automatically:
#   - Foreign demand (without trade between Euro area countries)
#   - Foreign interest rate
#   - Oil prices
#   - Real effective exchange rates
#   - Import and export

## Foreign demand ----
# Build a series of foreign demand without trade between Euro area members.
#   - Growth of imports in volume of main trading partners
#   - Relative importance of trading partner in Euro zone exports
#   - Sum over growth rates of imports weighted by the relative importance

### Imports of goods and services of Euro zone main commercial partners ----
# (Volume, quarterly, seasonally adjusted)
# Get the variation of demand originating from each trading partner of the Euro area.
# Select 14 trading partners that channel most of the Euro zone's exports.

#### General case ----
# Data comes from the OECD Economic Outlook data base.
# We use imports of goods and services in volume
partner_country_iso3 <- c(
  "USA", "GBR", "DNK", "NOR", "SWE", "CAN", "CHE", 
  "JPN", "AUS", "BRA", "IND", "IDN", "KOR", "CHN"
)

partner_country_name <- c(
  "United-States", "United-Kingdom", "Denmark", "Norway", 
  "Sweden", "Canada", "Switzerland", "Japan", "Australia", 
  "Brazil", "India", "Indonesia", "South Korea", "China"
)

imports <- df |> 
  select(period, value, country = Country) |> 
  filter(year(period)>= 1979)
# mutate(country = plyr::mapvalues(country, from = partner_country_iso3, to = partner_country_name))

ggplot(data = imports, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, colour = blue_obs_macro) +
  facet_wrap(facets = ~ country, ncol = 3, scales = "free_y") +
  ggtitle(
    label = "Imports of goods and services",
    subtitle="(volume, seasonally adjusted, national currency)"
    )
ggsave(filename = "01_imports.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

#### China special case ----
# We take annual Chinese imports of goods and services from the 
# IMF WEO database and use a spline interpolation to obtain quarterly values.
df <- rdb(ids = "IMF/WEO:latest/CHN.TM_RPCH.pcent_change")

imports_cn <- df |> select(period, value) |> 
  na.omit() |> 
  arrange(period) |> 
  mutate(value = 100 * cumprod(1 + value / 100)) |> 
  bind_rows(
    tibble(
      period = as.Date("1997-01-01"),
      value = 100
      )
    ) |> 
  arrange(period)

imports_cn_q <- tibble(
  period = seq(
    from = min(imports_cn$period),
    length.out = nrow(imports_cn) * 4,
    by = "quarter"
    )
  ) |> 
  left_join(y = imports_cn, by = join_by(period)) |> 
  mutate(value = na.spline(value), country = "China")

#### Growth rates ----
imports_growth_rate <- imports |> 
  filter(country != "China") |> 
  bind_rows(imports_cn_q) |> 
  arrange(country, period) |> 
  group_by(country) |> 
  mutate(value = value / lag(value, n = 1) - 1) |> 
  ungroup() |> 
  filter(year(period) >= 1980)

ggplot(data = imports_growth_rate, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ country, ncol = 3, scales = "free_y") +
  dbnomics() +
  ggtitle(
    label = "Growth rates of imports of goods and services",
    subtitle = "(% quarter-on-quarter, volume, seasonally adjusted)"
  )

ggsave(filename = "02_imports_gr.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

min_time <- imports_growth_rate |> 
  group_by(country) |> 
  summarise(min_time = min(period)) |> 
  ungroup()

kable(min_time)

### Eurozone exports of goods to main commercial partners ----
# (Values in US Dollars, annual)
# Use the value of exports of goodds, free on board (FOB),
# from the IMF DOT database.

# Exporter countries of the Eurozone
ea_country <- c(
  "AT", "BE", "R1", "FR", "DE", "IT", "LU", "NL", 
  "FI", "GR", "IE", "MT", "PT", "ES", "CY", "SK", 
  "EE", "LV", "LT", "SI"
)

ea_country_name <- c(
  "Austria", "Belgium", "Luxembourg-Belgium", 
  "France", "Germany", "Italy", "Luxembourg", 
  "Netherlands", "Finland", "Greece", "Ireland", 
  "Malta", "Portugal", "Spain", "Cyprus", 
  "Slovak Republic", "Estonia", "Latvia", "Lithuania", 
  "Slovenia"
)

url_ea_country <- paste0(ea_country, collapse = "+")

# Importer countries outside the Eurozone
partner_country <- c(
  "US", "GB", "DK", "NO", "SE", "CA", "CH", "JP", 
  "AU", "BR", "IN", "ID", "KR", "CN"
)

url_partner_country <- paste0(partner_country, collapse = "+")

url_filter <- paste0("A.", url_ea_country, ".TXG_FOB_USD.", url_partner_country)

df <- rdb(provider_code = "IMF", dataset_code = "DOT", mask = url_filter)

bilatx <- df |> 
  as_tibble() |> 
  select(exporter = REF_AREA, importer = COUNTERPART_AREA, value, period) |> 
  mutate(
    exporter = plyr::mapvalues(x = exporter, from = ea_country, to = ea_country_name),
    importer = plyr::mapvalues(x = importer, from = partner_country, to = partner_country_name)
    ) |> 
  filter(period >= "1979-01-01")

# The following list shows for each Eurozone member the
# date from which we have data on exports towards one of the 14
# selected trading partners.
start_sample <- bilatx |> 
  group_by(exporter, importer) |> 
  summarise(min_time = min(year(period))) |> 
  ungroup() |> 
  pivot_wider(names_from = importer, values_from = min_time)

start_sample[, 1:8] |> 
  kable()

start_sample[, c(1, 9:15)] |> 
  kable()

#### Special case: Belgium-Luxembourg ----
# We have data fro Belgium-Luxembourg as a single exporter until 1997.
# So we compute extra-area trade of Belgium and Luxembourg since 1997 to create
# a series for the whole period.
bilatx_belux <- bilatx |> 
  filter(exporter %in% c("Belgium", "Luxembourg")) |> 
  group_by(importer, period) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  mutate(exporter = "Luxembourg-Belgium")

bilatx <- bilatx |> 
  filter(!exporter %in% c("Belgium", "Luxembourg")) |> 
  rbind(bilatx_belux)

#### Special case: Eastern European countries ----
# Before 1992, five countries lack some data:
#   - The Baltic states
#   - Slovenia
#   - The Slovak Republic
# We represent the sum of exports of the Eurozone with and without
# these five countries.
export_15 <- bilatx |> 
  filter(!exporter %in% c("Slovenia", "Slovak Republic", "Estonia", "Latvia", "Lithuania")) |> 
  mutate(var = "Eurozone - 15") |> 
  group_by(var, period) |> 
  summarise(value = sum(value)) |> 
  ungroup()

export_all <- bilatx |> 
  mutate(var = "Eurozone - all") |> 
  group_by(var, period) |> 
  summarise(value = sum(value)) |> 
  ungroup()

plot_export <- rbind(export_15, export_all)

ggplot(data = plot_export, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  dbnomics() +
  ggtitle("Extra-Eurozone exports, with / without Eastern countries")

ggsave(filename = "03_exports-east.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

# Before 2003, the series are very similar.
# We choose to keep the whole data set.

#### Special case: Brazil, China, India, Indonesia ----
# We have incomplete imports of goods and services for
#   - Brazil
#   - China
#   - India
#   - Indonesia
# which lack data before 1997.
# Because these countries mainly developed their imports after 1997,
# we want to check the growth rates of extra-exports with
# and without these partners before 1997.
import_10 <- bilatx |> 
  filter(! importer %in% c("Brazil", "China", "India", "Indonesia")) |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  mutate(var = "Importers - 10")

bilatx |> 
  filter(importer %in% c("Brazil", "China", "India", "Indonesia"))
# Not included anyway!

plot_export2 <- bind_rows(
  mutate(.data = export_all, var = "Importers - all"),
  import_10
  ) |> 
  group_by(var) |> 
  mutate(value2 = value / lag(value) - 1) |> 
  filter(year(period) <= 1997)

ggplot(data = plot_export2, mapping = aes(x = period, y = value2, color = var)) +
  geom_line(linewidth = 1.2) +
  dbnomics() +
  ggtitle("Growth rate of extra-area exports, with 10 and 14 partners")

ggsave(filename = "04_imports-brics.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

#### Weights of the main commercial partners in Eurozone exports ----
# For each commercial partner `i` we compute `\alpha_{i}`,
# the share of EA exports `X` among all EA exports towards these partners,
# at time `i`.

# `\alpha_{i,t} \equals \frac{X_{i,t}}{\sum_{i}X_{i,t}}`

# Sum of exports of Euro area by importer
bilatx <- bilatx |> 
  group_by(importer, period) |> 
  summarise(value = sum(value)) |> 
  ungroup()

# Sum of exports of Euro area to 14 importers
sumX_EA_importer_all <- bilatx |> 
  group_by(period) |> 
  summarise(xsum = sum(value)) |> 
  mutate(exporter = "Eurozone") |> 
  ungroup()

alphas_importer_all <- sumX_EA_importer_all |> 
  left_join(y = bilatx, by = join_by(period)) |> 
  mutate(alpha = value / xsum) |> 
  select(period, country = importer, alpha)

# Sum of exports of Euro area to 10 importers
sumX_EA_importer_10 <- bilatx |> 
  filter(! importer %in% c("Brazil", "China", "India", "Indonesia")) |> 
  group_by(period) |> 
  summarise(xsum = sum(value)) |> 
  mutate(exporter = "Eurozone") |> 
  ungroup()

alphas_importer_10 <- sumX_EA_importer_10 |> 
  left_join(
    y = filter(.data = bilatx, !importer %in% c("Brazil", "China", "India", "Indonesia")),
    by = join_by(period)
  ) |> 
  mutate(alpha = value / xsum) |> 
  select(period, country = importer, alpha)

alphas <- bind_rows(
  filter(.data = alphas_importer_10, year(period) <= 1997),
  filter(.data = alphas_importer_all, year(period) > 1997)
)

ggplot(data = alphas, mapping = aes(x = period, y = alpha)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ country, ncol = 3, scales = "free_y") +
  dbnomics() +
  ggtitle("Share of Eurozone exports among all Eurozone exports")

ggsave(filename = "05_exports-ea.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

### Final index ----
# Sum over the growth rates of imports in volume weighted by the
# relative importance of each trading partner during the previous year.
# Then create a global index.
imports_growth_rate <- imports_growth_rate |> 
  mutate(year = year(period))

alphas <- alphas |> 
  mutate(year = year(period) + 1) |> 
  select(-period)

wd <- alphas |> 
  right_join(y = imports_growth_rate, by = join_by(year, country)) |> 
  mutate(value = alpha * value) |> 
  na.omit() |> 
  select(period, value, country) |> 
  group_by(period) |> 
  summarise(value = sum(value)) |> 
  mutate(value = cumprod(1 + value))

wd_index2010 <- wd |> 
  mutate(year = year(period)) |> 
  filter(year == "2010") |> 
  group_by(year) |> 
  summarise(value = mean(value)) |> 
  ungroup()

wd_index <- wd |> 
  mutate(period, value = 100 * value / wd_index2010$value)

wd_index_growth <- wd_index |> 
  mutate(
    value = value / lag(value, n = 4) - 1,
    var = "2 - Growth rate"
  )

plot_wd <- bind_rows(
  wd_index_growth,
  mutate(.data = wd_index, var = "1 - Level")
  )

ggplot(data = plot_wd, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ var, ncol = 1, scales = "free_y") +
  dbnomics() +
  ggtitle("Foreign demand for the Eurozone, base 100 = 2010")

ggsave(filename = "06_foreign-demand-ea.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

## Foreign interest rate ----
# We use the US federal funds rate overnight as a proxy for the foreign interest rate.
df <- rdb(provider_code = "FED", dataset_code = "H15", mask = "129.FF.O")

shortrate <- df |> 
  mutate(
    period = paste(year(period), quarter(period), sep = "-")
  ) |> 
  group_by(period) |> 
  summarise(value = mean(value)) |> 
  ungroup() |> 
  mutate(period = yq(period)) |> 
  filter(period >= "1980-01-01")

ggplot(data = shortrate, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  dbnomics() +
  ggtitle("Foreign interest rate")

ggsave(filename = "07_foreign-interest-rate.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

## Oil prices ----
# We take Brent crude oil prices from the OECD 
# Economic Outlook (EO) data base.
df <- rdb(ids = "OECD/EO/OTO.WPBRENT.Q")

oil_prices <- df |> 
  select(period, value) |> 
  filter(period >= "1980-01-01")

ggplot(data = oil_prices, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  dbnomics() +
  ggtitle("Crude oil prices")

ggsave(filename = "08_oil-price.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

## Real effective exchange rate ----
# We get the effective exchange rate (eer)
# from the Bank for International Settlements (BIS)
df <- rdb(ids = "BIS/eer/M.R.N.XM")

reer <- df |> 
  mutate(
    period = paste(year(period), quarter(period), sep = "-")
  ) |> 
  group_by(period) |> 
  summarise(value = mean(value)) |> 
  ungroup() |> 
  mutate(period = yq(period)) |> 
  filter(period >= "1980-01-01")

ggplot(data = reer, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  dbnomics() +
  ggtitle("Real Effective Exchange Rate")

ggsave(filename = "09_reer.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

## Extra Euro area imports and exports ----
# TRD database from the ECB
df <- rdb(provider_code = "ECB", dataset_code = "TRD", mask = "M.I8.Y.M+X.TTT.J8.4.VOX")

trade <- df |> 
  mutate(
    period = paste(year(period), quarter(period), sep = "-"),
    value,
    var = if_else(
      condition = grepl(pattern = "Import", x = series_name), 
      true = "imports", 
      false = "exports"
      ),
    .keep = "none"
  ) |> 
  group_by(var, period) |> 
  summarise(value = mean(value)) |> 
  ungroup() |> 
  mutate(period = yq(period))

ggplot(data = trade, mapping = aes(x = period, y = value)) +
  geom_line(linewidth = 1.2, color = blue_obs_macro) +
  facet_wrap(facets = ~ var) +
  dbnomics() +
  ggtitle("Extra euro area imports / exports, in volume, seasonally adjusted")

ggsave(filename = "10_ea-imports-exports.png", path = "figures/06_open-EA-data/", height = 12, width = 12)
graphics.off()

## Final international database for the Euro area ----
# Build the international database for the Euro area
rawdata <- bind_rows(
  mutate(.data = wd_index, var = "world_demand"),
  mutate(.data = shortrate, var = "foreign_rate"),
  mutate(.data = oil_prices, var = "oil_prices"),
  mutate(.data = reer, var = "reer"),
  trade
)

# Check the last data available for each variable
max_date <- rawdata |> 
  group_by(var) |> 
  summarise(max_date = max(period)) |> 
  arrange(max_date)

kable(max_date)

min_max_date <- min(max_date$max_date)

EA_Open_rawdata <- rawdata |> 
  filter(period <= min_max_date) |> 
  pivot_wider(names_from = var, values_from = value)

EA_Open_rawdata |> 
  write.csv(file = "data/EA_Open_rawdata.csv", row.names = FALSE)

# Download the raw series here: http://shiny.cepremap.fr/data/EA_Open_rawdata.csv
sw03 <- read.csv(file = "data/EA_SW_rawdata.csv") |> 
  mutate(period = ymd(period))

EA_Open_data <- EA_Open_rawdata |> 
  inner_join(y = sw03, by = join_by(period)) |> 
  mutate(
    period,
    world_demand,
    oil_prices,
    reer,
    imports,
    exports,
    .keep = "none"
  )

EA_Open_data |> 
  mutate(
    period = gsub(pattern = " ", replacement = "", x = as.yearqtr(period))
  ) |> 
  write.csv(file = "data/EA_Open_data.csv", row.names = FALSE)

# Download the ready-to-use data here: http://shiny.cepremap.fr/data/EA_Open_data.csv

# END

