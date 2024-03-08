# 05 - Foreign Demand for FR, DE, IT and ES ----
# URL: https://macro.cepremap.fr/article/2020-02/foreign-demand-euro-countries-data/

# Quarterly foreign demand for France, Germany, Italy and Spain.
# For each country, we proceed in three steps:
#   1 Calculate the growth of imports in volume of main trading partners;
#   2 Calculate the relative importance of each trading partner in EA exports;
#   3 Sum over the growth rates of imports weighted by the relative importance of each trading partner.
library(tidyverse)
library(zoo)
library(rdbnomics)
library(seasonal)
library(kableExtra)
library(RColorBrewer)
source("R/utils.R")
palette(brewer.pal(n = 9, name = "Set1"))
fig_path <- "figures/05_foreign-demand-ea/"

## France ----

### Main commercial partners imports of goods and services ----
# (Volume, quarterly, seasonally adjusted)

# Compute the variation of the demand originating from each trading partner of France.
# Select 18 trading partners that channel 75 percent of French exports.

#### General case -----
# OECD Economic Outlook (EO) database: Imports of goods and services in volume.
partner_country_iso3 <- c(
  "DEU", "ESP", "USA", "ITA", "GBR", "BEL", "NLD", "CHE", "POL",
  "TUR", "JPN", "SWE", "RUS", "PRT", "CHN", "SGP", "HKG", "DZA"
)
partner_country_name <- c(
  "Germany", "Spain", "United States", "Italy", "United Kingdom", "Belgium",
  "Netherlands", "Switzerland", "Poland", "Turkey", "Japan", "Sweden", "Russia",
  "Portugal", "China", "Singapore", "Hong Kong, China", "Algeria"
)
url_country_iso3 <- paste0(partner_country_iso3, collapse = "+")
url_filter <- paste0(url_country_iso3, ".P7.VOBARSA.Q")
df <- rdb("OECD", "QNA", mask = url_filter)

imports <- df |>
  select(period, value, country = LOCATION) |>
  filter(year(period) >= 1979) |>
  mutate(country = plyr::mapvalues(country, from = partner_country_iso3, to = partner_country_name))
# Warning: CHN, SGP, HKG, DZA not present

ggplot(imports, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle(
    label = "Imports of goods and services",
    subtitle = "(volume, seasonally adjusted, national currency)"
  )
ggsave("01_imports_levels.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Special case: Algeria, China, Hongkong & Singapore ----
# Imports from Algeria, China, Hong Kong & Singapore are not available in our dataset.
# Use the WEO database (IMF) to retrieve this data. 
# As it is annual, use a spline interpolation to obtain a quarterly series.
partner_country_spec_iso3 <- c("CHN", "SGP", "HKG", "DZA")
url_country_spec_iso3 <- paste0(partner_country_spec_iso3, collapse = "+")
url_filter <- paste0(url_country_spec_iso3, ".TM_RPCH")
df <- rdb("IMF", "WEO:latest", mask = url_filter)

imports_spec <- df |>
  select(period, value, country = `weo-country`) |>
  na.omit() |>
  mutate(
    country = case_when(
      country == "HKG" ~ "Hong Kong, China",
      country == "CHN" ~ "China",
      country == "DZA" ~ "Algeria",
      country == "SGP" ~ "Singapore",
      TRUE ~ country
    )
  ) |>
  arrange(country, period) |>
  group_by(country) |>
  mutate(value = 100 * cumprod(1 + value / 100)) |>
  bind_rows(
    tibble(period = as.Date("1997-01-01"), value = 100, country = "China"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Algeria"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Hong Kong, China"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Singapore")
  ) |>
  arrange(country, period) |>
  pivot_wider(names_from = country, values_from = value)

imports_spec_q <- tibble(
  period = seq(
    from = min(imports_spec$period),
    length.out = nrow(imports_spec) * 4,
    by = "quarter"
    )
  ) |>
  left_join(imports_spec, join_by(period)) |>
  pivot_longer(cols = -period, names_to = "country", values_to = "value") |> 
  filter(!(country == "China" & year(period) < 1997)) |>
  group_by(country) |>
  mutate(value = na.spline(value)) |> 
  ungroup()

#### Growth rates ----
imports_growth_rate <- imports |>
  bind_rows(imports_spec_q) |>
  arrange(country, period) |>
  group_by(country) |>
  mutate(value = value / lag(value, 1) - 1) |>
  ungroup() |>
  filter(year(period) >= 1980)

ggplot(filter(imports_growth_rate, year(period) >= 1981), aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "fixed") +
  my_theme() +
  ggtitle(
    label = "Growth rates of imports of goods and services", 
    subtitle = "(% quarter-on-quarter, volume, seasonally adjusted)"
    )
ggsave("02_imports_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()

min_time <- imports_growth_rate |>
  group_by(country) |>
  summarize(min_time = min(period)) |>
  ungroup()

kable(min_time, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for China, Poland and Russia.

### French exports of goods to main commercial partners ----
# (Values in US Dollars, annual)

# To compute the relative importance of each trading partner, 
# we use data series of values of exports of goods (Free on board, in US dollars), 
# from DOT database (IMF), for France towards each country.

# Importer countries
partner_country <- c(
  "DE", "ES", "US", "IT", "GB", "BE", "CN", "NL", "CH", 
  "PL", "TR", "JP", "SG", "HK", "DZ", "SE", "RU", "PT"
  )
url_partner_country <- paste0(partner_country, collapse = "+")
url_filter <- paste0("A.FR.TXG_FOB_USD.", url_partner_country)
df <- rdb("IMF", "DOT", mask = url_filter)

bilatx <- df |>
  separate(series_name, into = c("tu", "ti", "to", "importer"), sep = " – ") |>
  select(importer, value, period) |>
  mutate(
    importer = case_when(
      importer == "Russian Federation" ~ "Russia",
      TRUE ~ importer
      )
    ) |>
  add_column(exporter = "France") |>
  filter(period >= "1979-01-01")

# The following list shows the date from which we have data on French exports towards each one of the trading partners selected.
start_sample <- bilatx |>
  group_by(importer) |>
  summarize(min_time = min(year(period))) |>
  ungroup()

kable(start_sample, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for Belgium and Russia.

#### Special Case: Belgium, China, Poland, Russia ----
# We saw in the previous section that we have incomplete series of imports of goods and services 
# for China, Poland and Russia, and concerning french exports, 
# we have incomplete series only for Belgium and Russia. 
# We want to check the growth rates of exports with and without these partners before 1999.
export_all <- bilatx |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "France - all")

export_14 <- bilatx |>
  filter(!importer %in% c("Belgium", "China", "Poland", "Russia")) |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "France - 14")

plot_export2 <- bind_rows(export_all, export_14) |>
  group_by(var) |>
  mutate(value2 = value / lag(value) - 1) |>
  filter(year(period) <= 1998)

ggplot(plot_export2, aes(period, value2, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Growth rate of exports, with 14 and 18 partners")

ggsave("03_exports_FR_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()
# Before 1999, both series are very similar. 
# So we choose to compute weights of 18 commercial partners after 1999 
# but of only 14 partners before 1999 (without China, Belgium, Poland, and Russia).

#### Weights of main commercial partners in French exports ----
# For each commercial partner i, we compute αi, 
# the share of french exports X among all french exports towards these partners, 
# at time t.

# Sum of French exports by importer
bilatx <- bilatx |> 
  group_by(importer, period) |>
  summarize(value = sum(value)) |>
  ungroup()

# Sum of French exports to 14 importers
sumX_importer_all <- bilatx |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "France") |>
  ungroup()

alphas_importer_all <- left_join(sumX_importer_all, bilatx, join_by(period)) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

# Sum of French exports to 14 importers
sumX_importer_14 <- bilatx |>
  filter(!importer %in% c("Belgium", "China", "Poland", "Russia")) |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "France") |>
  ungroup()

alphas_importer_14 <- left_join(
  x = sumX_importer_14,
  y = filter(bilatx, !importer %in% c("Belgium", "China", "Poland", "Russia")),
  by = join_by(period)
  ) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

alphas <- bind_rows(
  filter(alphas_importer_14, year(period) < 1999),
  filter(alphas_importer_all, year(period) >= 1999)
  )

ggplot(alphas, aes(period, alpha)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle("Relative importance of each trading partner in French exports")

ggsave("04_importance_FR.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Final index ----
# We sum over the growth rates of imports in volume  weighted by the relative 
# importance of each trading partner during the previous year. 
# Then we create a global index.
imports_growth_rate <- imports_growth_rate |> 
  mutate(year = year(period))

alphas <- alphas |> 
  mutate(year = year(period) + 1) |>
  select(-period)

wd <- right_join(
  x = alphas, 
  y = imports_growth_rate, 
  by = join_by(year, country)) |>
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
  summarize(value = mean(value)) |>
  ungroup()

wd_index <- wd |>
  mutate(
    period,
    value = 100 * value / wd_index2010$value
  )

wd_index_growth <- wd_index |>
  mutate(
    value = value / lag(value, 4) - 1,
    var = "2- Growth rate"
  )

plot_wd_FR <- bind_rows(
  wd_index_growth,
  mutate(wd_index, var = "1- Level")
  ) |>
  add_column(country = "France")

## Germany ----

### Main commercial partners imports of goods and services ----
# (Volume, quarterly, seasonally adjusted)

# First of all, we need to compute the variation of the demand 
# originating from each trading partner of Germany. 
# We select 18 trading partners that channel 75 percent of German exports.

#### General case ----
# OECD Economic Outlook (EO) database: Imports of goods and services in volume.
partner_country_iso3 <- c(
  "USA", "FRA", "GBR", "NLD", "CHN", "ITA", "AUT", "POL", "CHE", 
  "BEL", "ESP", "CZE", "SWE", "HUN", "TUR", "RUS", "JPN", "DNK"
  )
partner_country_name <- c(
  "United States", "France", "United Kingdom", "Netherlands", 
  "China", "Italy", "Austria", "Poland", "Switzerland", 
  "Belgium", "Spain", "Czech Republic", "Sweden", 
  "Hungary", "Turkey", "Russia", "Japan", "Denmark"
  )
url_country_iso3 <- paste0(partner_country_iso3, collapse = "+")
url_filter <- paste0(url_country_iso3, ".P7.VOBARSA.Q")
df <- rdb("OECD", "QNA", mask = url_filter)

imports <- df |>
  select(period, value, country = LOCATION) |>
  filter(year(period) >= 1979) |>
  mutate(country = plyr::mapvalues(country, from = partner_country_iso3, to = partner_country_name))
# CHN not present

ggplot(imports, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle("Imports of goods and services", subtitle = "(volume, seasonally adjusted, national currency)")

ggsave("05_imports_DE_levels.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Special case: China ----
# Imports of goods and services from China are not available in our dataset. 
# We decide to use the WEO database (IMF) to retrieve this data. 
# As it is annual, we use a spline interpolation to obtain a quarterly series.
partner_country_spec_iso3 <- c("CHN")
url_country_spec_iso3 <- paste0(partner_country_spec_iso3, collapse = "+")
url_filter <- paste0(url_country_spec_iso3, ".TM_RPCH")
df <- rdb("IMF", "WEO:latest", mask = url_filter)

imports_spec <- df |>
  select(period, value, country = `weo-country`) |>
  mutate(country = "China") |>
  na.omit() |>
  arrange(country, period) |>
  mutate(value = 100 * cumprod(1 + value / 100)) |>
  bind_rows(data.frame(period = as.Date("1997-01-01"), value = 100, country = "China")) |>
  arrange(country, period) |>
  spread(country, value)

imports_spec_q <- tibble(
  period = seq(
    from = min(imports_spec$period),
    length.out = nrow(imports_spec) * 4,
    by = "quarter"
    )
  ) |>
  left_join(imports_spec, join_by(period)) |>
  pivot_longer(cols = -period, names_to = "country", values_to = "value") |> 
  mutate(value = na.spline(value))

#### Growth rates ----
imports_growth_rate <- imports |>
  bind_rows(imports_spec_q) |>
  arrange(country, period) |>
  group_by(country) |>
  mutate(value = value / lag(value, 1) - 1) |>
  ungroup() |>
  filter(year(period) >= 1980)

ggplot(filter(imports_growth_rate, year(period) >= 1981), aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "fixed") +
  my_theme() +
  ggtitle("Growth rates of imports of goods and services", subtitle = "(% quarter-on-quarter, volume, seasonally adjusted)")

ggsave("06_imports_DE_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()

min_time <- imports_growth_rate |>
  group_by(country) |>
  summarize(min_time = min(period)) |>
  ungroup()

kable(min_time, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for China, Czech Republic, Hungary, Poland and Russia.

### German exports of goods to main commercial partners ----
# (Values in US Dollars, annual)
# To compute the relative importance of each trading partner, 
# we use data series of values of exports of goods (Free on board, in US dollars), 
# from DOT database (IMF), for Germany towards each country.

# Importer countries
partner_country <- c(
  "US", "FR", "GB", "NL", "CN", "IT", "AT", "PL", "CH", 
  "BE", "ES", "CZ", "SE", "HU", "TR", "RU", "JP", "DK"
  )
url_partner_country <- paste0(partner_country, collapse = "+")
url_filter <- paste0("A.DE.TXG_FOB_USD.", url_partner_country)
df <- rdb("IMF", "DOT", mask = url_filter)

bilatx <- df |>
  separate(series_name, into = c("tu", "ti", "to", "importer"), sep = " – ") |>
  select(importer, value, period) |>
  mutate(
    importer = case_when(
      importer == "Russian Federation" ~ "Russia",
      TRUE ~ importer
      )
    ) |>
  add_column(exporter = "Germany") |>
  filter(period >= "1979-01-01")

# The following list shows, the date from which we have data on German exports towards each one of the trading partners selected.
start_sample <- bilatx |>
  group_by(importer) |>
  summarize(min_time = min(year(period))) |>
  ungroup()

kable(start_sample, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for Belgium, Czech Republic and Russia.

#### Special case: Belgium, China, Czech Republic, Hungary, Poland, Russia ----
# We saw in the previous section that we have incomplete series 
# for China, Czech Republic, Hungary, Poland and Russia, 
# and concerning German exports, 
# we have incomplete series only for Belgium, Czech Republic and Russia. 
# We want to check the growth rates of exports with and without these partners before 1999.
export_all <- bilatx |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "Germany - all")

export_12 <- bilatx |>
  filter(!importer %in% c("Belgium", "China", "Poland", "Russia")) |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "Germany - 12")

plot_export2 <- bind_rows(export_all, export_12) |>
  group_by(var) |>
  mutate(value2 = value / lag(value) - 1) |>
  filter(year(period) <= 1998)

ggplot(plot_export2, aes(period, value2, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Growth rate of exports, with 12 and 18 partners")

ggsave("07_exports_DE_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()
# Before 1999, both series are very similar. 
# So we choose to compute weights of 18 commercial partners after 1999 
# but of only 12 partners before 1999 (without Belgium, China, Czech Republic, Hungary, Poland and Russia).

#### Weights of main commercial partners in German exports ----
# For each trading partner i, we compute αi, the share of German exports X among all German exports 
# towards these partners, at time t:

# Sum of German exports by importer
bilatx <- bilatx |> 
  group_by(importer, period) |>
  summarize(value = sum(value)) |>
  ungroup()

# Sum of German exports to 12 importers
sumX_importer_all <- bilatx |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "Germany") |>
  ungroup()

alphas_importer_all <- left_join(
  x = sumX_importer_all, 
  y = bilatx, 
  by = join_by(period)) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

# Sum of German exports to 12 importers
sumX_importer_12 <- bilatx |>
  filter(!importer %in% c("Belgium", "China", "Czech Republic", "Hungary", "Poland", "Russia")) |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "Germany") |>
  ungroup()

alphas_importer_12 <- left_join(
  x = sumX_importer_12,
  y = filter(bilatx, !importer %in% c("Belgium", "China", "Czech Republic", "Hungary", "Poland", "Russia")),
  by = join_by(period)
  ) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

alphas <- bind_rows(
  filter(alphas_importer_12, year(period) < 1999),
  filter(alphas_importer_all, year(period) >= 1999)
  )

ggplot(alphas, aes(period, alpha)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle("Relative importance of each trading partner in German exports")

ggsave("08_importance_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Final index ----
# We sum over the growth rates of imports in volume weighted by the relative importance of each trading partner during the previous year. Then we create a global index.
imports_growth_rate <- imports_growth_rate |> 
  mutate(year = year(period))

alphas <- alphas |> 
  mutate(year = year(period) + 1) |>
  select(-period)

wd <- right_join(
  x = alphas, 
  y = imports_growth_rate, 
  by = join_by(year, country)
  ) |>
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
  summarize(value = mean(value)) |>
  ungroup()

wd_index <- wd |>
  mutate(
    period,
    value = 100 * value / wd_index2010$value
  )

wd_index_growth <- wd_index |>
  mutate(
    value = value / lag(value, 4) - 1,
    var = "2- Growth rate"
  )

plot_wd_DE <- bind_rows(
    wd_index_growth,
    mutate(wd_index, var = "1- Level")
  ) |>
  add_column(country = "Germany")

## Italy ----
### Main commercial partners imports of goods and services ----
# (Volume, quarterly, seasonally adjusted)

# First of all, we need to compute the variation of the demand originating from each trading partner of Italy. 
# We select 22 trading partners that channel 75 percent of Italian exports.

#### General case ----
# OECD Economic Outlook (EO): Imports of goods and services in volume.
partner_country_iso3 <- c(
  "DEU", "FRA", "USA", "GBR", "ESP", "CHE", "BEL", "POL", 
  "CHN", "NLD", "TUR", "AUS", "RUS", "ROU", "JPN", "HKG", 
  "ARE", "CZE", "HUN", "SWE", "SAU", "KOR"
  )
partner_country_name <- c(
  "Germany", "France", "United States", "United Kingdom", 
  "Spain", "Switzerland", "Belgium", "Poland", "China", 
  "Netherlands", "Turkey", "Austria", "Russia", "Romania", 
  "Japan", "Hong Kong, China", "United Arab Emirates", 
  "Czech Republic", "Hungary", "Sweden", "Saudi Arabia", "South Korea"
  )
url_country_iso3 <- paste0(partner_country_iso3, collapse = "+")
url_filter <- paste0(url_country_iso3, ".P7.VOBARSA.Q")
df <- rdb("OECD", "QNA", mask = url_filter)

imports <- df |>
  select(period, value, country = LOCATION) |>
  filter(year(period) >= 1979) |>
  mutate(country = plyr::mapvalues(country, from = partner_country_iso3, to = partner_country_name))
# CHN, HKG, ARE, SAU not included

ggplot(imports, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle(
    label = "Imports of goods and services", 
    subtitle = "(volume, seasonally adjusted, national currency)"
    )

ggsave("09_imports_IT_levels.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Special case: China, Hong Kong, Romania, Saudi Arabia, UAE ----
# Imports of goods and services from China, Hong Kong, Romania, Saudi Arabia & United Arab Emirates 
# are not available in our dataset. 
# We decide to use the WEO database (IMF) to retrieve this data 
# (except for Romania whose series have extreme values). 
# As it is annual, we use a spline interpolation to obtain a quarterly series.
partner_country_spec_iso3 <- c("CHN", "HKG", "ARE", "SAU")
url_country_spec_iso3 <- paste0(partner_country_spec_iso3, collapse = "+")
url_filter <- paste0(url_country_spec_iso3, ".TM_RPCH")
df <- rdb("IMF", "WEO:latest", mask = url_filter)

imports_spec <- df |>
  select(period, value, country = `weo-country`) |>
  na.omit() |>
  mutate(
    country = case_when(
      country == "CHN" ~ "China",
      country == "SAU" ~ "Saudi Arabia",
      country == "HKG" ~ "Hong Kong, China",
      # country=="ROU" ~ "Romania",
      country == "ARE" ~ "United Arab Emirates",
      TRUE ~ country
      )
    ) |>
  arrange(country, period) |>
  group_by(country) |>
  mutate(value = 100 * cumprod(1 + value / 100)) |>
  bind_rows(
    tibble(period = as.Date("1997-01-01"), value = 100, country = "China"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Saudi Arabia"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Hong Kong, China"),
    # tibble(period=as.Date("1979-01-01"),value=100, country="Romania"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "United Arab Emirates")
    ) |>
  arrange(country, period) |>
  pivot_wider(names_from = country, values_from = value)

imports_spec_q <- tibble(
  period = seq(
    from = min(imports_spec$period),
    length.out = nrow(imports_spec) * 4,
    by = "quarter"
    )
  ) |>
  left_join(imports_spec, by = join_by(period)) |>
  pivot_longer(cols = -period, names_to = "country", values_to = "value") |> 
  filter(!(country == "China" & year(period) < 1997)) |>
  group_by(country) |>
  mutate(value = na.spline(value)) |> 
  ungroup()

#### Growth rates ----
imports_growth_rate <- imports |>
  bind_rows(imports_spec_q) |>
  arrange(country, period) |>
  group_by(country) |>
  mutate(value = value / lag(value, 1) - 1) |>
  ungroup() |>
  filter(year(period) >= 1980)

ggplot(filter(imports_growth_rate, year(period) >= 1981), aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "fixed") +
  my_theme() +
  ggtitle("Growth rates of imports of goods and services", subtitle = "(% quarter-on-quarter, volume, seasonally adjusted)")

ggsave("10_imports_IT_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()

min_time <- imports_growth_rate |>
  group_by(country) |>
  summarize(min_time = min(period)) |>
  ungroup()

kable(min_time, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for China, Czech Republic, Hungary, Poland, Romania and Russia.

### Italian exports of goods to main commercial partners ----
# (Values in US Dollars, annual)
# To compute the relative importance of each trading partner, 
# we use data series of values of exports of goods (Free on board, in US dollars), 
# from DOT database (IMF), for Italy towards each country.

# Importer countries
partner_country <- c(
  "DE", "FR", "US", "GB", "ES", "CH", "BE", "PL", "CN", "NL", "TR", 
  "AT", "RU", "RO", "JP", "HK", "AE", "CZ", "HU", "SE", "SA", "KR"
  )
url_partner_country <- paste0(partner_country, collapse = "+")
url_filter <- paste0("A.IT.TXG_FOB_USD.", url_partner_country)
df <- rdb("IMF", "DOT", mask = url_filter)

bilatx <- df |>
  separate(series_name, into = c("tu", "ti", "to", "importer"), sep = " – ") |>
  select(importer, value, period) |>
  mutate(
    importer = case_when(
      importer == "Russian Federation" ~ "Russia",
      importer == "Korea, Republic of" ~ "South Korea",
      TRUE ~ importer
      )
    ) |>
  add_column(exporter = "Italy") |>
  filter(period >= "1979-01-01")

# The following list shows, the date from which we have data on Italian exports towards each one of the trading partners selected.
start_sample <- bilatx |>
  group_by(importer) |>
  summarize(min_time = min(year(period))) |>
  ungroup()

kable(start_sample, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for Belgium, Czech Republic, and Russia.

#### Special case: Belgium, China, Czech Republic, Hungary, Poland, Romania, Russia ----
# We saw in the previous section that we have incomplete series of imports of goods and services 
# for China, Czech Republic, Hungary, Poland, Romania and Russia, 
# and concerning Italian exports, we have incomplete series only 
# for Belgium, Czech Republic, and Russia. 
# We want to check the growth rates of exports with and without these partners before 1999.
export_all <- bilatx |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "Italy - all")

export_15 <- bilatx |>
  filter(!importer %in% c("Belgium", "China", "Czech Republic", "Hungary", "Poland", "Romania", "Russia")) |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "Italy - 15")

plot_export2 <- bind_rows(export_all, export_15) |>
  group_by(var) |>
  mutate(value2 = value / lag(value) - 1) |>
  filter(year(period) <= 1998)

ggplot(plot_export2, aes(period, value2, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Growth rate of exports, with 15 and 22 partners")

ggsave("11_exports_IT_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()
# Before 1999, both series are very similar. So we choose to compute weights of 22 commercial partners after 1999 but of only 15 partners before 1999 (without Belgium, China, Czech Republic, Hungary, Poland, Romania and Russia).

#### Weights of main commercial partners in Italian exports ----
# For each commercial partner i, we compute αi, 
# the share of Italian exports X among all Italian exports towards these partners, at time t:

# Sum of Italian exports by importer
bilatx <- bilatx |> 
  group_by(importer, period) |>
  summarize(value = sum(value)) |>
  ungroup()

# Sum of Italian exports to 16 importers
sumX_importer_all <- bilatx |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "Italy") |>
  ungroup()

alphas_importer_all <- left_join(
  x = sumX_importer_all, 
  y = bilatx, 
  by = join_by(period)
  ) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

# Sum of Italian exports to 16 importers
sumX_importer_15 <- bilatx |>
  filter(!importer %in% c("Belgium", "China", "Czech Republic", "Hungary", "Poland", "Romania", "Russia")) |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "Italy") |>
  ungroup()

alphas_importer_15 <- left_join(
  x = sumX_importer_15,
  y = filter(bilatx, !importer %in% c("Belgium", "China", "Czech Republic", "Hungary", "Poland", "Romania", "Russia")),
  by = join_by(period)
  ) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

alphas <- bind_rows(
  filter(alphas_importer_15, year(period) < 1999),
  filter(alphas_importer_all, year(period) >= 1999)
  )

ggplot(alphas, aes(period, alpha)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle("Relative importance of each trading partner in Italian exports")

ggsave("12_importance_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Final index ----
# We sum over the growth rates of imports in volume weighted by the relative importance of each trading partner during the previous year. 
# Then we create a global index.
imports_growth_rate <- imports_growth_rate |> 
  mutate(year = year(period))

alphas <- alphas |> 
  mutate(year = year(period) + 1) |>
  select(-period)

wd <- right_join(
  x = alphas, 
  y = imports_growth_rate, 
  by = join_by(year, country)
  ) |>
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
  summarize(value = mean(value)) |>
  ungroup()

wd_index <- wd |>
  mutate(
    period,
    value = 100 * value / wd_index2010$value
  )

wd_index_growth <- wd_index |>
  mutate(
    value = value / lag(value, 4) - 1,
    var = "2- Growth rate"
  )

plot_wd_IT <- bind_rows(
  wd_index_growth,
  mutate(wd_index, var = "1- Level")
  ) |>
  add_column(country = "Italy")

## Spain ----

### Imports of goods and services of Spain's main commercial partners ----
# (Volume, quarterly, seasonally adjusted)

# First of all, we need to compute the variation of the demand originating from each trading partner of Spain. 
# We select 18 trading partners that channel 75 percent of Spanish exports.

#### General case ----
# OECD Economic Outlook (OE): Imports of goods and services in volume.
partner_country_iso3 <- c(
  "FRA", "DEU", "ITA", "GBR", "PRT", "USA", "NLD", "BEL", "MAR", 
  "TUR", "CHN", "POL", "MEX", "CHE", "DZA", "JPN", "SAU", "BRA"
  )
partner_country_name <- c(
  "France", "Germany", "Italy", "United Kingdom", "Portugal", 
  "United States", "Netherlands", "Belgium", "Morocco", 
  "Turkey", "China", "Poland", "Mexico", "Switzerland", 
  "Algeria", "Japan", "Saudi Arabia", "Brazil"
  )
url_country_iso3 <- paste0(partner_country_iso3, collapse = "+")
url_filter <- paste0(url_country_iso3, ".P7.VOBARSA.Q")
df <- rdb("OECD", "QNA", mask = url_filter)

imports <- df |>
  select(period, value, country = LOCATION) |>
  filter(year(period) >= 1979) |>
  mutate(country = plyr::mapvalues(country, from = partner_country_iso3, to = partner_country_name))
# MAR, CHN, DZA, SAU not included

ggplot(imports, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle("Imports of goods and services", subtitle = "(volume, seasonally adjusted, national currency)")

ggsave("12_imports_ES_levels.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Special case: Algeria, China, Morocco, Saudi Arabia ----
# Data series of imports of goods and services from 
# Algeria, China, Morocco & Saudi Arabia are not available in our dataset. 
# We decide to use the WEO database (IMF) to retrieve this data. 
# As it is annual, we use a spline interpolation to obtain quarterly series.
partner_country_spec_iso3 <- c("DZA", "CHN", "MAR", "SAU")
url_country_spec_iso3 <- paste0(partner_country_spec_iso3, collapse = "+")
url_filter <- paste0(url_country_spec_iso3, ".TM_RPCH")
df <- rdb("IMF", "WEO:latest", mask = url_filter)

imports_spec <- df |>
  select(period, value, country = `weo-country`) |>
  na.omit() |>
  mutate(
    country = case_when(
      country == "SAU" ~ "Saudi Arabia",
      country == "CHN" ~ "China",
      country == "DZA" ~ "Algeria",
      country == "MAR" ~ "Morocco",
      TRUE ~ country
      )
    ) |>
  arrange(country, period) |>
  group_by(country) |>
  mutate(value = 100 * cumprod(1 + value / 100)) |>
  bind_rows(
    tibble(period = as.Date("1997-01-01"), value = 100, country = "China"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Algeria"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Saudi Arabia"),
    tibble(period = as.Date("1979-01-01"), value = 100, country = "Morocco")
  ) |>
  arrange(country, period) |>
  pivot_wider(names_from = country, values_from = value)

imports_spec_q <- tibble(
  period = seq(
    from = min(imports_spec$period),
    length.out = nrow(imports_spec) * 4,
    by = "quarter"
    )
  ) |>
  left_join(imports_spec, by = join_by(period)) |>
  pivot_longer(cols = -period, names_to = "country", values_to = "value") |> 
  filter(!(country == "China" & year(period) < 1997)) |>
  group_by(country) |>
  mutate(value = na.spline(value))

### Growth rates ----
imports_growth_rate <- imports |>
  bind_rows(imports_spec_q) |>
  arrange(country, period) |>
  group_by(country) |>
  mutate(value = value / lag(value, 1) - 1) |>
  ungroup() |>
  filter(year(period) >= 1980)

ggplot(filter(imports_growth_rate, year(period) >= 1981), aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "fixed") +
  my_theme() +
  ggtitle("Growth rates of imports of goods and services", subtitle = "(% quarter-on-quarter, volume, seasonally adjusted)")

ggsave("13_imports_ES_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()

min_time <- imports_growth_rate |>
  group_by(country) |>
  summarize(min_time = min(period)) |>
  ungroup()

kable(min_time, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for Brazil, China, and Poland.

### Spanish exports of goods to main commercial partners ----
# (Values in US Dollars, annual)
# To compute the relative importance of each trading partner, 
# we use data series of values of exports of goods (Free on board, in US dollars), 
# from DOT database (IMF), for Spain towards each country.

# Importer countries
partner_country <- c(
  "FR", "DE", "IT", "GB", "PT", "US", "NL", "BE", "MA", 
  "TR", "CN", "PL", "MX", "CH", "DZ", "JP", "SA", "BR"
  )
url_partner_country <- paste0(partner_country, collapse = "+")
url_filter <- paste0("A.ES.TXG_FOB_USD.", url_partner_country)
df <- rdb("IMF", "DOT", mask = url_filter)

bilatx <- df |>
  separate(series_name, into = c("tu", "ti", "to", "importer"), sep = " – ") |>
  select(importer, value, period) |>
  add_column(exporter = "Spain") |>
  filter(period >= "1979-01-01")

# The following list shows, the date from which we have data on spanish exports towards each one of the trading partners selected.
start_sample <- bilatx |>
  group_by(importer) |>
  summarize(min_time = min(year(period))) |>
  ungroup()

kable(start_sample, format = "html", caption = "min_time") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")
# We have incomplete series only for Belgium.

#### Special case: Belgium, Brazil, China, Poland ----
# We saw in the previous section that we have incomplete series of imports of goods and services 
# for Brazil, China, and Poland, and concerning Spanish exports, 
# we have incomplete series only for Belgium. 
# We want to check the growth rates of exports with and without these partners before 1997.
export_all <- bilatx |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "Spain - all")

export_14 <- bilatx |>
  filter(!importer %in% c("Belgium", "Brazil", "China", "Poland")) |>
  group_by(period) |>
  summarize(value = sum(value)) |>
  ungroup() |>
  mutate(var = "Spain - 14")

plot_export2 <- bind_rows(export_all, export_14) |>
  group_by(var) |>
  mutate(value2 = value / lag(value) - 1) |>
  filter(year(period) <= 1996)

ggplot(plot_export2, aes(period, value2, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Growth rate of exports, with 14 and 18 partners")

ggsave("14_exports_ES_gr.png", path = fig_path, height = 12, width = 12)
graphics.off()
# Before 1999, both series are very similar. So we choose to compute weights of 18 commercial partners after 1997 but of only 14 partners before 1997 (without Belgium, Brazil, China and Poland).

#### Weights of main commercial partners in Spanish exports ----
# For each commercial partner i, we compute αi, 
# the share of Spanish exports X among all Spanish exports towards these partners, at time t:

# Sum of Spanish exports by importer
bilatx <- bilatx |> 
  group_by(importer, period) |>
  summarize(value = sum(value)) |>
  ungroup()

# Sum of Spanish exports to 14 importers
sumX_importer_all <- bilatx |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "Spain") |>
  ungroup()

alphas_importer_all <- left_join(
  x = sumX_importer_all, 
  y = bilatx, 
  by = join_by(period)) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

# Sum of French exports to 14 importers
sumX_importer_14 <- bilatx |>
  filter(!importer %in% c("Belgium", "Brazil", "China", "Poland")) |>
  group_by(period) |>
  summarise(xsum = sum(value)) |>
  mutate(exporter = "Spain") |>
  ungroup()

alphas_importer_14 <- left_join(
  x = sumX_importer_14,
  y = filter(bilatx, !importer %in% c("Belgium", "Brazil", "China", "Poland")),
  by = join_by(period)
  ) |>
  mutate(alpha = value / xsum) |>
  select(period, country = importer, alpha)

alphas <- bind_rows(
  filter(alphas_importer_14, year(period) < 1997),
  filter(alphas_importer_all, year(period) >= 1997)
  )

ggplot(alphas, aes(period, alpha)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~country, ncol = 5, scales = "free_y") +
  my_theme() +
  ggtitle("Relative importance of each trading partner in Spanish exports")

ggsave("15_importance_ES.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Final index ----
# We sum over the growth rates of imports in volume weighted by the relative importance of each trading partner during the previous year. 
# Then we create a global index.
imports_growth_rate <- imports_growth_rate |> 
  mutate(year = year(period))

alphas <- alphas |> 
  mutate(year = year(period) + 1) |>
  select(-period)

wd <- right_join(
  x = alphas, 
  y = imports_growth_rate, 
  by = join_by(year, country)
  ) |>
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
  summarize(value = mean(value)) |>
  ungroup()

wd_index <- wd |>
  mutate(
    period,
    value = 100 * value / wd_index2010$value
  )

wd_index_growth <- wd_index |>
  mutate(
    value = value / lag(value, 4) - 1,
    var = "2- Growth rate"
  )

plot_wd_ES <- bind_rows(
  wd_index_growth,
  mutate(wd_index, var = "1- Level")
  ) |>
  add_column(country = "Spain")

## Foreign Demand ----
foreign_demand <- bind_rows(
  plot_wd_FR,
  plot_wd_DE,
  plot_wd_IT,
  plot_wd_ES
  )

ggplot(foreign_demand, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~var, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle(expression(atop("Foreign demand for France, Germany, Italy and Spain", atop(italic("base 100 = 2010"), ""))))

ggsave("16_.png", path = fig_path, height = 12, width = 12)
graphics.off()

write.csv(world_demand, file = "data/Foreign_demand.csv", row.names = FALSE)
# Find the data here: href="http://shiny.cepremap.fr/data/Foreign_demand.csv
# END