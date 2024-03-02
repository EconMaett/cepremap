# 01 - Macroeconomic data for FR, DE, IT, ES & EA ----
# URL: https://macro.cepremap.fr/article/2021-02/five-countries-data/

# Gather macroeconomic data for FR, DE, IT, ES & EA,
# for the estimation and calibration of general equilibrium models.
# The database can be automatically updated.
library(tidyverse)
library(zoo)
library(rdbnomics)
library(seasonal)
library(kableExtra)

source(file = "R/utils.R")

fig_path <- "figures/01_five-countries-data"

## The Euro Area -----
# Gather the following databases:
#   1. Smets & Wouters (2003) data base
#   2. Financial database for the Euro Area
#   3. Fiscal data base for the Euro Area
#   4. International database for the Euro Area

sw03 <- read_csv(file = "data/EA_SW_rawdata.csv") |>
  filter(period >= "1980-01-01")

fipu <- read_csv(file = "data/EA_Fipu_rawdata.csv")
fina <- read_csv(file = "data/EA_Finance_rawdata.csv")
open <- read_csv(file = "data/EA_Open_rawdata.csv")

EA_rawdata <- sw03 |>
  inner_join(y = fipu, by = join_by(period)) |>
  inner_join(y = fina, by = join_by(period)) |>
  inner_join(y = open, by = join_by(period)) |>
  rename(unempbenef = unemp) |>
  mutate(pop = 1000 * pop) |>
  add_column(country = "EA")

## FR, DE, IT & ES ----
# Sources: Eurostat, IMF (WEO & IFS), BIS, OECD & ECB.

### Data retrieval & seasonal adjustment ----
# Get country data on:
#   - Gross domestic product (GDP)
#   - Consumption (C)
#   - Investment (I)
#   - GDP deflator
#   - Compensation of employees (w)
#   - Hours worked (h)
#   - Investment deflator
#   - Loans to non-financial corporations (NFC)
#   - Entrepreneurial net worth
#   - Short-term interest rate
#   - Lending rate
#   - Total government expenditure
#   - Government consumption
#   - Government transfers
#   - Government interest payments
#   - Government debt
#   - World demand
#   - Total government revenue
#   - Unemployment benefits
#   - Nominal effective exchange rate
#   - Imports
#   - Exports
#   - Population

# Oil prices are assumed to be the same in all countries.

#### Compensation of employees ----
# Eurostat namq_10_a10 database
wage_de_fr <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_a10", mask = "Q.CP_MEUR.SA.TOTAL.D1.DE+FR") |>
  add_column(var = "wage")

wage_es_it <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_a10", mask = "Q.CP_MEUR.SCA.TOTAL.D1.ES+IT") |>
  add_column(var = "wage")

#### Hours worked ----
# Eurostat namq_10_a10_e database
hours <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_a10_e", mask = "Q.THS_HW.TOTAL.SCA.EMP_DC.IT+DE+FR+ES") |>
  add_column(var = "hours")

#### Gross domestic product -----
# Eurostat namq_10_gdp
gdp <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_gdp", mask = "Q.CLV10_MEUR.SCA.B1GQ.IT+DE+FR+ES") |>
  add_column(var = "gdp")

#### Consumption -----
# Eurostat namq_10_gdp
conso <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_gdp", mask = "Q.CLV10_MEUR.SCA.P31_S14_S15.IT+DE+FR+ES") |>
  add_column(var = "conso")

#### Investment -----
# Eurostat namq_10_gdp
inves <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_gdp", mask = "Q.CLV10_MEUR.SCA.P51G.IT+DE+FR+ES") |>
  add_column(var = "inves")

#### GDP deflator -----
# Eurostat namq_10_gdp
defgdp <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_gdp", mask = "Q.PD10_EUR.SCA.B1GQ.IT+DE+FR+ES") |>
  add_column(var = "defgdp")

#### Investment deflator -----
# Eurostat namq_10_gdp
definves <- rdb(provider_code = "Eurostat", dataset_code = "namq_10_gdp", mask = "Q.PD10_EUR.SCA.P51G.IT+DE+FR+ES") |>
  add_column(var = "definves")

#### Population -----
# Eurostat lfsq_pganws, demo_pjanbroad are chained and interpolated
pop_recent <- rdb(provider_code = "Eurostat", dataset_code = "lfsq_pganws", mask = "Q.THS_PER.T.TOTAL.Y15-64.POP.IT+DE+FR+ES") |>
  add_column(var = "pop_recent")

pop_old <- rdb(provider_code = "Eurostat", dataset_code = "demo_pjanbroad", mask = "A.NR.Y15-64.T.IT+DE+FR+ES") |>
  add_column(var = "pop_old")

#### Government consumption ----
# For IT & DE we chain & interpolate quarterly series
# Eurostat gov_10q_ggnfa & gov_10a_main"
pubcons_recent_fr <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.P3.FR") |>
  add_column(var = "pubcons_recent")

pubcons_recent_it_de_es_nsa <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.P3.IT+DE+ES") |>
  add_column(var = "pubcons_recent")

pubcons_old_it_de <- rdb(provider_code = "Eurostat", dataset_code = "gov_10a_main", mask = "A.MIO_EUR.S13.P3.IT+DE") |>
  add_column(var = "pubcons_old")

# gov_10q_ggnfa needs to be seasonally adjusted.
df_nsa_q <- pubcons_recent_it_de_es_nsa |>
  select(period, country = geo, value)

to_deseason <- df_nsa_q |>
  spread(country, value)

deseasoned_q <- bind_rows(
  lapply(
    X = unique(df_nsa_q$country),
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
    })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

df_nsa_q <- df_nsa_q |>
  mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

# Plot government consumption for DE, ES, IT
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~ country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Government Consumption")

ggsave(filename = "01_gov_cons_DE-ES-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on government consumption for DE, ES, IT
pubcons_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "pubcons_recent")

#### Government investment -----
# For IT & DE use two databases from Eurostat that we will chain and 
# interpolate by country in the next section: 
# gov_10q_ggnfa & gov_10a_main.
pubinves_recent_fr <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.P51G.FR") |>
  add_column(var = "pubinves_recent")

pubinves_recent_it_de_es_nsa <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.P51G.IT+DE+ES") |>
  add_column(var = "pubinves_recent")

pubinves_old_it_de <- rdb(provider_code = "Eurostat", dataset_code = "gov_10a_main", mask = "A.MIO_EUR.S13.P51G.IT+DE") |>
  add_column(var = "pubinves_old")

# Seasonally-adjust the gov_10q_ggnfa for IT & DE
df_nsa_q <- pubinves_recent_it_de_es_nsa |>
  select(period, country = geo, value)

to_deseason <- df_nsa_q |>
  spread(country, value)

deseasoned_q <- bind_rows(
  lapply(
    X = unique(df_nsa_q$country),
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
    })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

df_nsa_q <- df_nsa_q |>
  mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

# Plot government investment for DE, ES, IT
ggplot(data = plot_df, mapping = aes(x = period, y = value, colour = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~ country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Government Investment")

ggsave(filename = "02_gov_inves_DE-ES-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on government investment for DE, ES & IT
pubinves_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "pubinves_recent")

#### Government interest payments ----
# IT & DE chain and interpolate Eurostat gov_10q_ggnfa, gov_10a_main
tfs_recent_fr <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.D62PAY.FR") |>
  add_column(var = "tfs_recent")

tfs_recent_it_de_es_nsa <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.D62PAY.IT+DE+ES") |>
  add_column(var = "tfs_recent")

tfs_old_it_de <- rdb(provider_code = "Eurostat", dataset_code = "gov_10a_main", mask = "A.MIO_EUR.S13.D62PAY.IT+DE") |>
  add_column(var = "tfs_old")

# Seasonally-adjust the series for Italy and Germany
df_nsa_q <- tfs_recent_it_de_es_nsa |>
  select(period, country = geo, value)

to_deseason <- df_nsa_q |>
  spread(country, value)

deseasoned_q <- bind_rows(
  lapply(
    X = unique(df_nsa_q$country),
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
    })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

df_nsa_q <- df_nsa_q |>
  mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

ggplot(data = plot_df, mapping = aes(x = period, y = value, colour = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Government Social Transfers")

ggsave(filename = "03_gov_transf_DE-ES-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on government social transfers for DE, ES & IT
tfs_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "tfs_recent")

#### Government interest payments ----
# IT & DE chain & interpolate Eurostat gov_10q_ggnfa, gov_10a_main
intpay_recent_fr <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.D41PAY.FR") |>
  add_column(var = "intpay_recent")

intpay_recent_it_de_es_nsa <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.D41PAY.IT+DE+ES") |>
  add_column(var = "intpay_recent")

intpay_old_it_de <- rdb(provider_code = "Eurostat", dataset_code = "gov_10a_main", mask = "A.MIO_EUR.S13.D41PAY.DE+IT") |>
  add_column(var = "intpay_old")

# Seasonally adjust the data for Italy and Germany
df_nsa_q <- intpay_recent_it_de_es_nsa |>
  select(period, country = geo, value)

to_deseason <- df_nsa_q |>
  spread(country, value)

deseasoned_q <- bind_rows(
  lapply(
    X = unique(df_nsa_q$country),
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
    })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

df_nsa_q <- df_nsa_q |>
  mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

# Plot government interst payments for DE, ES, & IT
ggplot(data = plot_df, mapping = aes(x = period, y = value, colour = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Government Interest Payments")

ggsave(filename = "04_gov_intpay_DE-ES-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on government interest payments for DE, ES & IT
intpay_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "intpay_recent")


#### Total government expenditure ----
# IT & DE & interpolate Eurostat gov_10q_ggnfa, gov_10a_main
totexp_recent_fr <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.TE.FR") |>
  add_column(var = "totexp_recent")

totexp_recent_it_de_es_nsa <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.TE.IT+DE+ES") |>
  add_column(var = "totexp_recent")

totexp_old_it_de <- rdb(provider_code = "Eurostat", dataset_code = "gov_10a_main", mask = "A.MIO_EUR.S13.TE.DE+IT") |>
  add_column(var = "totexp_old")

# Seasonally adjust the data for DE, ES & IT from gov_10q_ggnfa
df_nsa_q <- totexp_recent_it_de_es_nsa |>
  select(period, country = geo, value)

to_deseason <- df_nsa_q |>
  spread(country, value)

deseasoned_q <- bind_rows(
  lapply(
    X = unique(df_nsa_q$country),
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
    })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

df_nsa_q <- df_nsa_q |>
  mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

# Plot total govenrment expenditure for DE, ES & IT
ggplot(data = plot_df, mapping = aes(x = period, y = value, colour = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Total Government Expenditure")

ggsave(filename = "05_gov_exp_DE-ES-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on total government expenditure in DE, ES & IT
totexp_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "totexp_recent")

#### Total government revenue -----
# For IT & DE chain and interpolate Eurostat gov_10q_ggnfa & gov_10a_main
totrev_recent_fr <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.TR.FR") |>
  add_column(var = "totrev_recent")

totrev_recent_it_de_es_nsa <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.TR.IT+DE+ES") |>
  add_column(var = "totrev_recent")

totrev_old_it_de <- rdb(provider_code = "Eurostat", dataset_code = "gov_10a_main", mask = "A.MIO_EUR.S13.TR.DE+IT") |>
  add_column(var = "totrev_old")

# Seasonally adjust gov_10q_ggnfa for DE, ES, & IT
df_nsa_q <- totrev_recent_it_de_es_nsa |>
  select(period, country = geo, value)

to_deseason <- df_nsa_q |>
  spread(country, value)

deseasoned_q <- bind_rows(
  lapply(
    X = unique(df_nsa_q$country),
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
    })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

df_nsa_q <- df_nsa_q |>
  mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

# Plot total government revenue for DE, ES & IT
ggplot(data = plot_df, mapping = aes(x = period, y = value, colour = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Total Government Revenue")

ggsave(filename = "06_gov_rev_DE-ES-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on total government revenue for DE, ES & IT
totrev_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "totrev_recent")

#### Government debt -----
# Use Eurostat gov_10q_ggdebt & IMF WEO
debt_recent <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggdebt", mask = "Q.GD.S13.MIO_EUR.IT+DE+FR+ES") |>
  add_column(var = "debt_recent")

debt_old <- rdb(provider_code = "IMF", dataset_code = "WEO:latest", mask = "DEU+ESP+FRA+ITA.GGXWDG") |>
  add_column(var = "debt_old") |>
  select(geo = "weo-country", period, value, var) |>
  mutate(geo = str_sub(string = geo, start = 1, end = 2)) |>
  filter(year(period) <= max(debt_recent$period))

# Seasonally-adjust the data
df_nsa_q <- debt_recent |>
  select(period, country = geo, value)

to_deseason <- df_nsa_q |>
  spread(country, value)

deseasoned_q <- bind_rows(
  lapply(
    X = unique(df_nsa_q$country),
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
    })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

df_nsa_q <- df_nsa_q |>
  mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

# Plot government debt for DE, ES, FR & IT
ggplot(data = plot_df, mapping = aes(x = period, y = value, colour = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Debt")

ggsave(filename = "07_gov_debt_DE-ES-FR-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on government debt in DE, ES, FR & IT
debt_recent <- deseasoned_q |> 
  filter(Origin == "Adjusted Series") |> 
  select(country, -Origin, value, period) |> 
  mutate(var = "debt_recent")

#### Loans to non-financial corporations -----
# CNFS database from the Bank for International Settlements (BIS)
loans_nfc <- rdb(provider_code = "BIS", dataset_code = "total_credit", mask = "Q.IT+DE+FR+ES.N.A.M.XDC.A") |>
  add_column(var = "loans_nfc") |>
  as_tibble() |>
  select(geo = BORROWERS_CTY, period, value, var)

#### Entrepreneurial net worth ----
# Main Economic Indicators (MEI) of the OECD
networth <- rdb(provider_code = "OECD", dataset_code = "MEI", mask = "FRA+DEU+ITA+ESP.SPASTT01.IXOB.Q") |>
  as_tibble() |>
  select(period, value, geo = LOCATION) |>
  add_column(var = "networth") |>
  mutate(
    geo = case_when(
      geo == "FRA" ~ "FR",
      geo == "DEU" ~ "DE",
      geo == "ESP" ~ "ES",
      geo == "ITA" ~ "IT"
    )
  )

#### Short-term interest rate ----
# OECD Main Economic Indicators (MEI)
shortrate <- rdb(provider_code = "OECD", dataset_code = "MEI", mask = "FRA+DEU+ITA+ESP.IR3TIB01.ST.Q") |>
  as_tibble() |>
  select(period, value, geo = LOCATION) |>
  add_column(var = "shortrate") |>
  mutate(
    geo = case_when(
      geo == "FRA" ~ "FR",
      geo == "DEU" ~ "DE",
      geo == "ESP" ~ "ES",
      geo == "ITA" ~ "IT"
    )
  )

#### Lending rate ----
# Use the ECB MIR and IMF IFS to chain and interpolate by country.
lendingrate_recent <- rdb(provider_code = "ECB", dataset_code = "MIR", mask = "M.IT+DE+FR+ES.B.A2A.A.R.A.2240.EUR.N") |>
  as_tibble() |>
  select(geo = REF_AREA, period, value) |>
  mutate(period = paste(year(period), quarter(period))) |>
  group_by(geo, period) |>
  summarize(value = mean(value, na.rm = TRUE)) |>
  mutate(var = "lendingrate_recent", period = yq(period))

lendingrate_old <- rdb(provider_code = "IMF", dataset_code = "IFS", mask = "Q.IT+DE+FR+ES.FILR_PA") |>
  as_tibble() |>
  add_column(var = "lendingrate_old") |>
  select(geo = REF_AREA, period, value, var)

### World demand ----
# Use the foreign demand specific to the four countries
world_demand <- read_csv(file = "data/Foreign_demand.csv") |>
  rename(geo = country)

#### Unemployment benefits ----
#   1. Determine the quarterly series using the ratio of quarterly social expenditure and annual unemployment benefits
#   2. Remove the seasonal component from the series.
#   3. Retrieve annual unemployment benefits from Eurostat spr_exp_sum.

# 1. Retrieve government social expenditures and calculate quarterly shares for each year
url_filter <- "Q.MIO_EUR.NSA.S13.D62PAY.IT+DE+FR+ES"
df <- rdb(provider_code = "Eurostat", dataset_code = "gov_10q_ggnfa", mask = url_filter)

socialexp <- df |>
  mutate(year = year(period), country = geo) |>
  select(period, value, year, country) |>
  group_by(year, country) |>
  mutate(sum = sum(value), ratio = value / sum) |>
  ungroup() |>
  select(-c("value", "year", "sum"))

# 2. Retrieve the latest annual unemployment benefits,
# put them in a quarterly table and use the previously
# obtained ratio of quarterly social expenditures to compute
# quarterly unemployment benefits.
url_filter <- "Q.MIO_EUR.NSA.S13.D62PAY.IT+DE+FR+ES"
df <- rdb("Eurostat", "gov_10q_ggnfa", mask = url_filter)

socialexp <- df |>
  mutate(year = year(period), country = geo) |>
  select(period, value, year, country) |>
  group_by(year, country) |>
  mutate(
    sum = sum(value),
    ratio = value / sum
  ) |>
  ungroup() |>
  select(-value, -year, -sum)

# 3. Retrieve the latest annual data on unemployment benefits,
# put them in a quarterly table and use the previously calculated
# ratio of quarterly social expenditures to compute quarterly 
# unemployment benefits.
url_filter <- "A.MIO_EUR.S13.GF1005.TE.IT+DE+FR+ES"
df <- rdb("Eurostat", "gov_10a_exp", mask = url_filter)

recent_unempbenef <- df |>
  mutate(year = year(period), country = geo) |>
  select(period, value, year, country) |>
  spread(country, value)

recent_unempbenef_q <- tibble(
    period = seq(min(recent_unempbenef$period),
      length.out = nrow(recent_unempbenef) * 4,
      by = "quarter"
    ),
    year = year(period)
  ) |>
  left_join(recent_unempbenef, by = "year") |>
  select(-period.y, -year) |>
  rename(period = period.x) |>
  gather(country, value, -period)

unempbenef_q <- recent_unempbenef_q |>
  inner_join(socialexp, by = c("period" = "period", "country" = "country")) |>
  mutate(value = value * ratio) |>
  select(-ratio) |>
  na.omit()

# Seasonally adjust the data
to_deseason <- unempbenef_q |>
  spread(country, value)

unempbenef_q_deseasoned <- bind_rows(
  lapply(
    X = unique(unempbenef_q$country), 
    FUN = function(var) {
      deseason(var_arrange = var, source_df = to_deseason)
      })
  ) |>
  mutate(Origin = "Adjusted Series", country = var) |>
  select(-var)

unempbenef_q <- unempbenef_q |> 
  mutate(Origin = "Unadjusted Series")

plot_unempbenef <- bind_rows(unempbenef_q, unempbenef_q_deseasoned) |>
  na.omit()

# Plot unemployment benefits for DE, ES, FR & IT
ggplot(data = plot_unempbenef, mapping = aes(x = period, y = value, colour = Origin)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~ country, scales = "free_y", ncol = 2) +
  my_theme() +
  ggtitle("Unemployment benefits")

ggsave(filename = "08_unempbenef_DE-ES-FR-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

# Save the recent data on unemployment benefits in DE, ES, FR & IT
unempbenef_recent <- unempbenef_q_deseasoned |>
  filter(Origin == "Adjusted Series") |>
  select(geo = country, -Origin, value, period) |>
  mutate(var = "unempbenef_recent")

# 4. Retrieve the annual series from Eurostat spr_exp_sum
# that will later be interpolated and chained to the quarterly series.
url_filter <- "A.UNEMPLOY.MIO_EUR.IT+DE+FR+ES"
df <- rdb(provider_code = "Eurostat", dataset_code = "spr_exp_sum", mask = url_filter)

unempbenef_old <- df |>
  add_column(var = "unempbenef_old") |>
  select(period, value, geo, var)

#### Nominal effective exchange rate ----
# eer from BIS
df <- rdb(provider_code = "BIS", dataset_code = "eer", mask = "M.N.B.IT+DE+FR+ES")

neer <- df |>
  select(period, value, geo = REF_AREA) |>
  mutate(period = paste(year(period), quarter(period))) |>
  group_by(geo, period) |>
  summarize(value = mean(value)) |>
  mutate(period = yq(period), var = "neer")


#### Imports and exports ----
# OECD Economic Outlook (OE) database
df <- rdb(provider_code = "OECD", dataset_code = "EO", mask = "FRA+DEU+ITA+ESP.MGSV+XGSV.Q")

imports_exports_volume <- df |>
  select(period, value, geo = LOCATION, value, var = VARIABLE) |>
  mutate(
    var = case_when(
      var == "MGSV" ~ "imports",
      var == "XGSV" ~ "exports"
    )
  ) |>
  mutate(
    geo = case_when(
      geo == "FRA" ~ "FR",
      geo == "DEU" ~ "DE",
      geo == "ESP" ~ "ES",
      geo == "ITA" ~ "IT"
    )
  )

#### Merging data frames -----
# We merge all data frames with all the series.
# Later we will chain and interpolate the special cases.
df <- bind_rows(
  wage_de_fr,
  wage_es_it,
  hours,
  gdp,
  conso,
  inves,
  defgdp,
  definves,
  pop_recent, pop_old,
  debt_old,
  lendingrate_recent, lendingrate_old,
  networth,
  shortrate,
  loans_nfc,
  totexp_recent_fr, totexp_old_it_de,
  totrev_recent_fr, totrev_old_it_de,
  intpay_recent_fr, intpay_old_it_de,
  pubcons_recent_fr, pubcons_old_it_de,
  pubinves_recent_fr, pubinves_old_it_de,
  tfs_recent_fr, tfs_old_it_de,
  world_demand,
  unempbenef_recent, unempbenef_old,
  neer,
  imports_exports_volume
  ) |>
  select(period, value, country = geo, var) |>
  bind_rows(
    pubcons_recent_it_de_es,
    pubinves_recent_it_de_es,
    tfs_recent_it_de_es,
    debt_recent,
    totexp_recent_it_de_es,
    intpay_recent_it_de_es,
    totrev_recent_it_de_es
  ) |>
  na.omit() |>
  filter(year(period) >= 1991) |>
  arrange(var, period)

# Plot the unchained series for DE, ES, FR, IT
ggplot(data = df, mapping = aes(x = period, y = value, colour = country)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(facets = ~var, ncol = 3, scales = "free_y") +
  my_theme() +
  ggtitle("Unchained Series")

ggsave(filename = "09_unchained_DE-ES-FR-IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

### France: Chaining & Interpolating Data -----
# Before chaining and interpolating the special cases, we first
# gather the data for France and proceed case by case.
df_fr <- df |> 
  filter(country == "FR") |> 
  select(-country)

#### France: Government debt ----
# We first interpolate the annual series to obtain quarterly series,
# then we chain the quarterly series from the two different data bases.
debt_old_a <- df_fr |> 
  filter(var == "debt_old") |> 
  mutate(value = 1000 * value) |> 
  select(-var)

debt_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(debt_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = debt_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "debt")

debt_recent <- df_fr |> 
  filter(var == "debt_recent") |> 
  mutate(var = "debt")

minDateDebtRecent <- min(debt_recent$period)

debt <- chain(
  basis = debt_recent,
  to_rebase = debt_old_q,
  date_chain = minDateDebtRecent
  ) |> 
  mutate(var = "debt")

plot_df <- bind_rows(
  add_column(.data = debt_old_a, var = "debt_old_a"),
  mutate(.data = debt_old_q, var = "debt_old_q"),
  mutate(.data = debt, var = "debt_chained")
  )

# Plot government debt for France
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("France: Government Debt")

ggsave(filename = "10_gov_debt_FR.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Population -----
# First interpolate the annual series to obtain quarterly values,
# then chain the quarterly series from the two different databases.
pop_old_a <- df_fr |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(pop_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = pop_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "pop")

pop_recent <- df_fr |> 
  filter(var == "pop_recent") |> 
  mutate(var = "pop", value = value * 1000)

pop <- chain(
  basis = pop_recent,
  to_rebase = pop_old_q,
  date_chain = "2015-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = pop_old_a, var = "pop_old_a"),
  mutate(.data = pop_old_q, var = "pop_old_q"),
  mutate(.data = pop, var = "pop_chained")
  )

# Plot the population for France
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("France: Population")

ggsave(filename = "11_pop_FR.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### France: Lending rate -----
# Chain the quarterly series from the two different databases.
lendingrate_old <- df_fr |> 
  filter(var == "lendingrate_old") |> 
  mutate(var = "lendingrate")

lendingrate_recent <- df_fr |> 
  filter(var == "lendingrate_recent") |> 
  mutate(var = "lendingrate")

minDateLendingRateRecent <- min(lendingrate_recent$period)

lendingrate <- chain(
  basis = lendingrate_recent,
  to_rebase = lendingrate_old,
  date_chain = minDateLendingRateRecent
  ) |> 
  mutate(var = "lendingrate")

plot_df <- bind_rows(
  mutate(.data = lendingrate_old, var = "lendingrate_old"),
  mutate(.data = lendingrate_recent, var = "lendingrate_recent"),
  mutate(.data = lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for France
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("France: Lending Rate")

ggsave(filename = "12_lendingrate_FR.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### France: Unemployment benefits -----
# Interpolate the annual series to obtain quarterly values,
# then we chain the two quarterly series.
unempbenef_old_a <- df_fr |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = unempbenef_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "unempbenef")

unempbenef_recent <- df_fr |> 
  filter(var == "unempbenef_recent") |> 
  mutate(var = "unempbenef")

unempbenef <- chain(
  basis = unempbenef_recent,
  to_rebase = unempbenef_old_q,
  date_chain ="2015-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(.data = unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(.data = unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for France
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("France: Unemployment Benefits")

ggsave(filename = "13_unempbenef_FR.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### France: Merging French Data -----
# We gather the final series for France in a data frame
FR_rawdata <- df_fr |> 
  filter(! var %in% c(
    "lendingrate_old", "lendingrate_recent",
    "pop_old", "pop_recent",
    "debt_old", "debt_recent",
    "unempbenef_old"," unempbenef_recent"
    )
  ) |> 
  bind_rows(lendingrate, pop, debt, unempbenef) |> 
  mutate(
    var = case_when(
      var == "pubcons_recent" ~ "pubcons",
      var == "pubinves_recent" ~ "pubinves",
      var == "tfs_recent" ~ "tfs",
      var == "totexp_recent" ~ "totexp",
      var == "intpay_recent" ~ "intpay",
      var == "totrev_recent" ~ "totrev",
      TRUE ~ var
      )
    ) |> 
  spread(var, value) |> 
  add_column(country = "FR")

### Spain: Chaining & Interpolating Data ----
# Before chaining and interpolating the special cases,
# we gather all the data for Spain and proceed by case.
df_es <- df |> 
  filter(country == "ES") |> 
  select(-country)

#### Spain: Government debt ----
# First interpolate the annual series to obtain quarterly values,
# then chain the quarterly series from the two data bases.
debt_old_a <- df_es |> 
  filter(var == "debt_old") |> 
  mutate(value = 1000 * value) |> 
  select(-var)

debt_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(debt_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = debt_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "debt")

debt_recent <- df_es |> 
  filter(var == "debt_recent") |> 
  mutate(var = "debt")

minDateDebtRecent <- min(debt_recent$period)

debt <- chain(
  basis = debt_recent,
  to_rebase = debt_old_q,
  date_chain = minDateDebtRecent
  ) |> 
  mutate(var = "debt")

plot_df <- bind_rows(
  add_column(.data = debt_old_a, var = "debt_old_a"),
  mutate(.data = debt_old_q, var = "debt_old_q"),
  mutate(.data = debt, var = "debt_chained")
  )

# Plot the government debt of Spain
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Spain: Government Debt")

ggsave(filename = "14_gov_debt_ES.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Spain: Population -----
# First interpolate the annual series, then chain the quarterly
# values from the two different databases.
pop_old_a <- df_es |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
    period = seq(
      from = as.Date("1991-01-01"),
      length.out = (nrow(pop_old_a) - 1) * 4 + 1,
      by = "quarter"),
    value = NA) |> 
  left_join(y = pop_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "pop")

pop_recent <- df_es |> 
  filter(var == "pop_recent") |> 
  mutate(var = "pop", value = value * 1000)

pop <- chain(
  basis      = pop_recent,
  to_rebase  = pop_old_q,
  date_chain = "1998-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = pop_old_a, var = "pop_old_a"),
  mutate(.data = pop_old_q, var = "pop_old_q"),
  mutate(.data = pop, var = "pop_chained")
  )

# Plot the population for Spain
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Spain: Population")

ggsave(filename = "15_pop_ES.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Spain: Lending rate -----
# Chain the quarterly values from the two different databases.
lendingrate_old <- df_es |> 
  filter(var == "lendingrate_old") |> 
  mutate(var = "lendingrate")

lendingrate_recent <- df_es |> 
  filter(var == "lendingrate_recent") |> 
  mutate(var = "lendingrate")

minDateLendingRateRecent <- min(lendingrate_recent$period)

lendingrate <- chain(
  basis      = lendingrate_recent,
  to_rebase  = lendingrate_old,
  date_chain = minDateLendingRateRecent
  ) |> 
  mutate(var = "lendingrate")

plot_df <- bind_rows(
  mutate(.data = lendingrate_old, var = "lendingrate_old"),
  mutate(.data = lendingrate_recent, var = "lendingrate_recent"),
  mutate(.data = lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for Spain
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Spain: Lending Rate")

ggsave(filename = "16_lendingrate_ES.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Spain: Unemployment benefits -----
# We first interpolate the annual series to obtain quarterly series,
# then we chain the two quarterly series.
unempbenef_old_a <- df_es |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(unempbenef_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "unempbenef")

unempbenef_recent <- df_es |> 
  filter(var == "unempbenef_recent") |> 
  mutate(var = "unempbenef")

unempbenef <- chain(
  basis      = unempbenef_recent,
  to_rebase  = unempbenef_old_q,
  date_chain ="2015-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(.data = unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(.data = unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for Spain
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Unemployment Benefits")

ggsave(filename = "17_unempbenef_ES.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Spain: Merging Spanish Data ----
# Gather all the final series for Spain in a data frame
ES_rawdata <- df_es |> 
  filter(! var %in% c(
    "lendingrate_old", "lendingrate_recent",
    "pop_old", "pop_recent",
    "debt_old", "debt_recent",
    "unempbenef_old", "unempbenef_recent"
    )
  ) |> 
  bind_rows(lendingrate, pop, debt, unempbenef) |> 
  mutate(
    var = case_when(
      var == "pubcons_recent" ~ "pubcons",
      var == "pubinves_recent" ~ "pubinves",
      var == "tfs_recent" ~ "tfs",
      var == "totexp_recent" ~ "totexp",
      var == "totrev_recent" ~ "totrev",
      var == "intpay_recent" ~ "intpay",
      TRUE ~ var
      )
    ) |> 
  spread(var, value) |> 
  add_column(country = "ES")

### Germany: Chaining & Interpolating Data ----
# Before chaining and interpolating the special cases, we
# first gather all the data for Germany and then proceed by case.
df_de <- df |> 
  filter(country == "DE") |> 
  select(-country)

#### Germany: Government debt -----
# We first interpolate the annual series to obtain quarterly series,
# and then chain the quarterly series from the two different databases.
debt_old_a <- df_de |> 
  filter(var == "debt_old") |> 
  mutate(value = 1000 * value) |> 
  select(-var)

debt_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(debt_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = debt_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "debt")

debt_recent <- df_de |> 
  filter(var == "debt_recent") |> 
  mutate(var = "debt")

minDateDebtRecent <- min(debt_recent$period)

debt <- chain(
  basis      = debt_recent,
  to_rebase  = debt_old_q,
  date_chain = minDateDebtRecent
  ) |> 
  mutate(var = "debt")

plot_df <- bind_rows(
  add_column(.data = debt_old_a, var = "debt_old_a"),
  mutate(.data = debt_old_q, var = "debt_old_q"),
  mutate(.data = debt, var = "debt_chained")
  )

# Plot the government debt for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Government Debt")

ggsave(filename = "18_govdebt_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Population -----
# First interpolate the annual series to obtain quarterly series,
# then chain the quarterly series from the two different databases.
pop_old_a <- df_de |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(pop_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = pop_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "pop")

pop_recent <- df_de |> 
  filter(var == "pop_recent") |> 
  mutate(var = "pop", value = value * 1000)

pop <- chain(
  basis      = pop_recent,
  to_rebase  = pop_old_q,
  date_chain = "2015-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = pop_old_a, var = "pop_old_a"),
  mutate(.data = pop_old_q, var = "pop_old_q"),
  mutate(.data = pop, var = "pop_chained")
  )

# Plot the population for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Population")

ggsave(filename = "19_pop_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Lending rate ----
# Chain the quarterly series from the two different databases. 
lendingrate_old <- df_de |> 
  filter(var == "lendingrate_old") |> 
  mutate(var = "lendingrate")

lendingrate_recent <- df_de |> 
  filter(var == "lendingrate_recent") |> 
  mutate(var = "lendingrate")

minDateLendingRateRecent <- min(lendingrate_recent$period)

lendingrate <- chain(
  basis = lendingrate_recent,
  to_rebase = lendingrate_old,
  date_chain = minDateLendingRateRecent
  ) |> 
  mutate(var = "lendingrate")

plot_df <- bind_rows(
  mutate(.data = lendingrate_old, var = "lendingrate_old"),
  mutate(.data = lendingrate_recent, var = "lendingrate_recent"),
  mutate(.data = lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Lending Rate")

ggsave(filename = "20_lendingrate_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Unemployment benefits ----
# Interpolate the annual series to obtain quarterly values,
# and then chain the two quarterly series.
unempbenef_old_a <- df_de |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = unempbenef_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "unempbenef")

unempbenef_recent <- df_de |> 
  filter(var == "unempbenef_recent") |> 
  mutate(var = "unempbenef")

unempbenef <- chain(
  basis = unempbenef_recent,
  to_rebase = unempbenef_old_q,
  date_chain = "2015-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(.data = unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(.data = unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Unemployment Benefits")

ggsave(filename = "21_unempbenef_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Government consumption -----
# Interpolate the annual series to obtain quarterly values,
# then chain the two quarterly series from the different databases.
pubcons_old_a <- df_de |> 
  filter(var == "pubcons_old") |> 
  mutate(value = value / 4)

pubcons_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubcons_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = pubcons_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "pubcons")

pubcons_recent <- df_de |> 
  filter(var == "pubcons_recent") |> 
  mutate(var = "pubcons")

minDatePubConsRecent <- min(pubcons_recent$period)

pubcons <- chain(
  basis = pubcons_recent,
  to_rebase = pubcons_old_q,
  date_chain = minDatePubConsRecent
  )

plot_df <- bind_rows(
  mutate(.data = pubcons_old_a, var = "pubcons_old_a"),
  mutate(.data = pubcons_old_q, var = "pubcons_old_q"),
  mutate(.data = pubcons, var = "pubcons_chained")
  )

# Plot the government consumption for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Government Consumption")

ggsave(filename = "22_gov_cons_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Government investment -----
# Interpolate the annual series to obtain quarterly values,
# then chain the quarterly series from the two different databases.
pubinves_old_a <- df_de |> 
  filter(var == "pubinves_old") |> 
  mutate(value = value / 4)

pubinves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubinves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = pubinves_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "pubinves")

pubinves_recent <- df_de |> 
  filter(var == "pubinves_recent") |> 
  mutate(var = "pubinves")

minDatePubInvesRecent <- min(pubinves_recent$period)

pubinves <- chain(
  basis = pubinves_recent,
  to_rebase = pubinves_old_q,
  date_chain = minDatePubInvesRecent
  )

plot_df <- bind_rows(
  mutate(.data = pubinves_old_a, var = "pubinves_old_a"),
  mutate(.data = pubinves_old_q, var = "pubinves_old_q"),
  mutate(.data = pubinves, var = "pubinves_chained")
  )

# Plot government investment for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Government Investment")

ggsave(filename = "23_gov_inves_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Government social transfers ----
# We first interpolate the annual series to obtain quarterly series,
# then we chain the two series from the two different databases.
tfs_old_a <- df_de |> 
  filter(var == "tfs_old") |> 
  mutate(value = value / 4)

tfs_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(tfs_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = tfs_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "tfs")

tfs_recent <- df_de |> 
  filter(var == "tfs_recent") |> 
  mutate(var = "tfs")

minDateTfsRecent <- min(tfs_recent$period)

tfs <- chain(
  basis = tfs_recent,
  to_rebase = tfs_old_q,
  date_chain = minDateTfsRecent
  )

plot_df <- bind_rows(
  mutate(.data = tfs_old_a, var = "tfs_old_a"),
  mutate(.data = tfs_old_q, var = "tfs_old_q"),
  mutate(.data = tfs, var = "tfs_chained")
  )

# Plot government social transfers for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Government Social Transfers")

ggsave(filename = "24_gov_soctransf_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Total government expenditure ----
# We first interpolate the annual series to obtain quarterly values,
# then we chain the quarterly series from the two different databases.
totexp_old_a <- df_de |> 
  filter(var == "totexp_old") |> 
  mutate(value = value / 4)

totexp_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totexp_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = totexp_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "totexp")

totexp_recent <- df_de |> 
  filter(var == "totexp_recent") |> 
  mutate(var = "totexp")

minDateTotExpRecent <- min(totexp_recent$period)

totexp <- chain(
  basis = totexp_recent,
  to_rebase = totexp_old_q,
  date_chain = minDateTotExpRecent
  )

plot_df <- bind_rows(
  mutate(.data = totexp_old_a, var = "totexp_old_a"),
  mutate(.data = totexp_old_q, var = "totexp_old_q"),
  mutate(.data = totexp, var = "totexp_chained")
  )

# Plot total government expenditure for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Total Government Expenditure")

ggsave(filename = "25_gov_exp_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Total government revenue ----
# First interpolate the annual series to obtain quarterly values,
# then chain the two quarterly series from the two different databases.
totrev_old_a <- df_de |> 
  filter(var == "totrev_old") |> 
  mutate(value = value / 4)

totrev_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totrev_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = totrev_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "totrev")

totrev_recent <- df_de |> 
  filter(var == "totrev_recent") |> 
  mutate(var = "totrev")

minDateTotRevRecent <- min(totrev_recent$period)

totrev <- chain(
  basis = totrev_recent,
  to_rebase = totrev_old_q,
  date_chain = minDateTotRevRecent
  )

plot_df <- bind_rows(
  mutate(.data = totrev_old_a, var = "totrev_old_a"),
  mutate(.data = totrev_old_q, var = "totrev_old_q"),
  mutate(.data = totrev, var = "totrev_chained")
  )

# Plot total government revenue for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Total Government Revenue")

ggsave(filename = "26_gov_rev_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Government interest payments -----
# First interpolate the annual series to obtain quarterly series,
# then chain the two quarterly series from the two databases.
intpay_old_a <- df_de |> 
  filter(var == "intpay_old") |> 
  mutate(value = value / 4)

intpay_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(intpay_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = intpay_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "intpay")

intpay_recent <- df_de |> 
  filter(var == "intpay_recent") |> 
  mutate(var = "intpay")

minDateIntPayRecent <- min(intpay_recent$period)

intpay <- chain(
  basis = intpay_recent,
  to_rebase = intpay_old_q,
  date_chain = minDateIntPayRecent
  )

plot_df <- bind_rows(
  mutate(intpay_old_a, var = "intpay_old_a"),
  mutate(intpay_old_q, var = "intpay_old_q"),
  mutate(intpay, var = "intpay_chained")
  )

# Plot government interst payments for Germany
ggplot(data = plot_df, mapping = aes(x = period, y = value, col = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Germany: Government Interest Payments")

ggsave(filename = "27_gov_intpay_DE.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Germany: Merging German Data ----
# We gather all the final series for Germany in a data frame.
DE_rawdata <- df_de |> 
  filter(! var %in% c(
    "lendingrate_old", "lendingrate_recent",
    "pop_old", "pop_recent",
    "debt_old", "debt_recent",
    "unempbenef_old", "unempbenef_recent", 
    "totexp_recent", "totexp_old", 
    "intpay_recent", "intpay_old",
    "totrev_recent", "totrev_old",
    "pubcons_recent", "pubcons_old",
    "pubinves_recent","pubinves_old",
    "tfs_recent", "tfs_old"
    )
  ) |> 
  bind_rows(lendingrate, pop, debt, unempbenef, totexp, totrev, intpay, pubcons, pubinves, tfs) |> 
  spread(var, value) |> 
  add_column(country = "DE")

### Italy: Chaining & Interpolating Data ----
# Before chaining and interpolating the special cases, we first gather
# all data fro Italy and then proceed case by case.
df_it <- df |> 
  filter(country == "IT") |> 
  select(-country)

#### Italy: Consumption ----
# Quarterly data on consumption is not available 
# for Italy before 1996, but annual data is.
# We interpolate the annual series to obtain quarterly values,
# and then we chain the two quarterly series in 1996-01-01.
conso_old_a <- rdb(provider_code = "Eurostat", dataset_code = "nama_10_gdp", mask = "A.CLV10_MEUR.P31_S14_S15.IT") |> 
  select(period, value) |> 
  add_column(var = "conso_old") |> 
  mutate(value = value / 4)

conso_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(conso_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = conso_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "conso")

conso_recent <- df_it |> 
  filter(var == "conso")

minDateConsoRecent <- min(conso_recent$period)

conso <- chain(
  basis = conso_recent,
  to_rebase = conso_old_q,
  date_chain = minDateConsoRecent
  )

plot_df <- bind_rows(
  mutate(.data = conso_old_a, var = "conso_old_a"),
  mutate(.data = conso_old_q, var = "conso_old_q"),
  mutate(.data = conso, var = "conso_chained")
  )

# Plot consumption for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Consumption")

ggsave(filename = "28_consumption_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Investment ----
# Quarterly data on investment is unavailable befofre 1996,
# but annual data exists.
# We interpolate the annual series to obtain quarterly values,
# and then chain the two quarterly series in 1996-01-01
inves_old_a <- rdb(provider_code = "Eurostat", dataset_code = "nama_10_gdp", mask = "A.CLV10_MEUR.P51G.IT") |> 
  select(period, value) |> 
  add_column(var = "inves_old") |> 
  mutate(value = value / 4)

inves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(inves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = inves_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "inves")

inves_recent <- df_it |> 
  filter(var == "inves")

minDateInvesRecent <- min(inves_recent$period)

inves <- chain(
  basis = inves_recent,
  to_rebase = inves_old_q,
  date_chain = minDateInvesRecent
  )

plot_df <- bind_rows(
  mutate(.data = inves_old_a, var = "inves_old_a"),
  mutate(.data = inves_old_q, var = "inves_old_q"),
  mutate(.data = inves, var = "inves_chained")
  )

# Plot investment for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Investment")

ggsave(filename = "29_investment_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Investment deflator ----
# Quarterly data on investment deflator is unavailable for Italy before 1996,
# but annual data is. We interpolate the annual series to obtain quarterly values
# and chain the two quarterly series in 1996-01-01.
definves_old_a <- rdb(provider_code = "Eurostat", dataset_code = "nama_10_gdp", mask = "A.PD10_EUR.P51G.IT") |> 
  select(period, value) |> 
  add_column(var = "definves_old")

definves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(definves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = definves_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "definves")

definves_recent <- df_it |> 
  filter(var == "definves")

minDateDefInvesRecent <- min(definves_recent$period)

definves<- chain(
  basis = definves_recent,
  to_rebase = definves_old_q,
  date_chain = minDateDefInvesRecent
  )

plot_df <- bind_rows(
  mutate(.data = definves_old_a, var = "definves_old_a"),
  mutate(.data = definves_old_q, var = "definves_old_q"),
  mutate(.data = definves, var = "definves_chained")
  )

# Plot the investment deflator for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Investment Deflator")

ggsave(filename = "30_invesdef_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Government debt ----
# We first interpolate the annual series to obtain quarterly values,
# the we chain the two quarterly series from the two different databases.
debt_old_a <- df_it |> 
  filter(var == "debt_old") |> 
  mutate(value = 1000 * value) |> 
  select(-var)

debt_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(debt_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = debt_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "debt")

debt_recent <- df_it |> 
  filter(var == "debt_recent") |> 
  mutate(var = "debt")

minDateDebtRecent <- min(debt_recent$period)

debt <- chain(
  basis = debt_recent,
  to_rebase = debt_old_q,
  date_chain = minDateDebtRecent
  ) |> 
  mutate(var = "debt")

plot_df <- bind_rows(
  add_column(.data = debt_old_a, var = "debt_old_a"),
  mutate(.data = debt_old_q, var = "debt_old_q"),
  mutate(.data = debt, var = "debt_chained")
  )

# Plot the government debt of Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Government Debt")

ggsave(filename = "31_govdebt_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Population ----
# We first interpolate the annual series in order to obtain quarterly series, 
# and then we chain the quarterly series from the two different databases.
pop_old_a <- df_it |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(pop_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = pop_old_a, by = join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y) |> 
  mutate(value = na.spline(value), var = "pop")

pop_recent <- df_it |> 
  filter(var == "pop_recent") |> 
  mutate(var = "pop", value = value * 1000)

pop <- chain(
  basis = pop_recent,
  to_rebase = pop_old_q,
  date_chain = "2015-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = pop_old_a, var = "pop_old_a"),
  mutate(.data = pop_old_q, var = "pop_old_q"),
  mutate(.data = pop, var = "pop_chained"),
  mutate(.data = pop_recent, var = "pop_recent")
  )

# Plot the population of Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Population")

ggsave(filename = "32_pop_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Lending rate ----
# We chain the quarterly series from the two different databases.
lendingrate_old <- df_it |> 
  filter(var == "lendingrate_old") |> 
  mutate(var = "lendingrate")

lendingrate_recent <- df_it |> 
  filter(var == "lendingrate_recent") |> 
  mutate(var = "lendingrate")

minDateLendingRateRecent <- min(lendingrate_recent$period)

lendingrate <- chain(
  basis = lendingrate_recent,
  to_rebase = lendingrate_old,
  date_chain = minDateLendingRateRecent) |> 
  mutate(var = "lendingrate")

plot_df <- bind_rows(
  mutate(.data = lendingrate_old, var = "lendingrate_old"),
  mutate(.data = lendingrate_recent, var = "lendingrate_recent"),
  mutate(.data = lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Lending Rate")

ggsave(filename = "33_lendingrate_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Unemployment benefits -----
# We first interpolate the annual series in order to obtain quarterly series, 
# and then we chain the two quarterly series.
unempbenef_old_a <- df_it |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = unempbenef_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "unempbenef")

unempbenef_recent <- df_it |> 
  filter(var == "unempbenef_recent") |> 
  mutate(var = "unempbenef")

unempbenef <- chain(
  basis = unempbenef_recent,
  to_rebase = unempbenef_old_q,
  date_chain = "2015-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(.data = unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(.data = unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Unemployment Benefits")

ggsave(filename = "34_unempbenef_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Government consumption ----
# We first interpolate the annual series in order to obtain quarterly series, 
# and then we chain the quarterly series from the two different databases.
pubcons_old_a <- df_it |> 
  filter(var == "pubcons_old") |> 
  mutate(value = value / 4)

pubcons_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubcons_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = pubcons_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "pubcons")

pubcons_recent <- df_it |> 
  filter(var == "pubcons_recent") |> 
  mutate(var = "pubcons")

minDatePubConsRecent <- min(pubcons_recent$period)

pubcons <- chain(
  basis = pubcons_recent,
  to_rebase = pubcons_old_q,
  date_chain = minDatePubConsRecent
  )

plot_df <- bind_rows(
  mutate(.data = pubcons_old_a, var = "pubcons_old_a"),
  mutate(.data = pubcons_old_q, var = "pubcons_old_q"),
  mutate(.data = pubcons, var = "pubcons_chained")
  )

# Plot the government consumption of Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Government Consumption")

ggsave(filename = "35_govcons_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Government investment -----
# Interpolate the annual series in order to obtain quarterly series, 
# and then we chain the quarterly series from the two different databases.
pubinves_old_a <- df_it |> 
  filter(var == "pubinves_old") |> 
  mutate(value = value / 4)

pubinves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubinves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = pubinves_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "pubinves")

pubinves_recent <- df_it |> 
  filter(var == "pubinves_recent") |> 
  mutate(var = "pubinves")

pubinves <- chain(
  basis = pubinves_recent,
  to_rebase = pubinves_old_q,
  date_chain = "2003-01-01"
  )

plot_df <- bind_rows(
  mutate(.data = pubinves_old_a, var = "pubinves_old_a"),
  mutate(.data = pubinves_old_q, var = "pubinves_old_q"),
  mutate(.data = pubinves, var = "pubinves_chained")
  )

# Plot government investment for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Government Investment")

ggsave(filename = "36_gov_inv_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Government social transfers -----
# We first interpolate the annual series in order to obtain quarterly series, 
# and then we chain the quarterly series from the two different databases.
tfs_old_a <- df_it |> 
  filter(var == "tfs_old") |> 
  mutate(value = value / 4)

tfs_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(tfs_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = tfs_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "tfs")

tfs_recent <- df_it |> 
  filter(var == "tfs_recent") |> 
  mutate(var = "tfs")

minDateTfsRecent <- min(tfs_recent$period)

tfs <- chain(
  basis = tfs_recent,
  to_rebase = tfs_old_q,
  date_chain = minDateTfsRecent
  )

plot_df <- bind_rows(
  mutate(.data = tfs_old_a, var = "tfs_old_a"),
  mutate(.data = tfs_old_q, var = "tfs_old_q"),
  mutate(.data = tfs, var = "tfs_chained")
  )

# Plot government social transfers for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Government Social Transfers")

ggsave(filename = "37_gov_soctransf_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Total Government expenditure ----
# We first interpolate the annual series in order to obtain quarterly series, 
# and then we chain the quarterly series from the two different databases.
totexp_old_a <- df_it |> 
  filter(var == "totexp_old") |> 
  mutate(value = value / 4)

totexp_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totexp_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = totexp_old_a, by = join_by(period)) %>% 
  select(-value.x, value = value.y) %>% 
  mutate(value = na.spline(value), var = "totexp")

totexp_recent <- df_it |> 
  filter(var == "totexp_recent") |> 
  mutate(var = "totexp")

minDateTotExpRecent <- min(totexp_recent$period)

totexp <- chain(
  basis = totexp_recent,
  to_rebase = totexp_old_q,
  date_chain = minDateTotExpRecent
  )

plot_df <- bind_rows(
  mutate(.data = totexp_old_a, var = "totexp_old_a"),
  mutate(.data = totexp_old_q, var = "totexp_old_q"),
  mutate(.data = totexp, var = "totexp_chained")
  )

# Plot the total government expenditure for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Total Government Expenditure")

ggsave(filename = "38_gov_exp_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Total government revenue ----
# We first interpolate the annual series in order to obtain quarterly series, 
# and then we chain the quarterly series from the two different databases.
totrev_old_a <- df_it |> 
  filter(var == "totrev_old") |> 
  mutate(value = value / 4)

totrev_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totrev_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = totrev_old_a, by = join_by(period)) |> 
  select(-value.x,value = value.y) |> 
  mutate(value = na.spline(value), var = "totrev")

totrev_recent <- df_it |> 
  filter(var == "totrev_recent") |> 
  mutate(var = "totrev")

minDateTotRevRecent <- min(totrev_recent$period)

totrev <- chain(
  basis = totrev_recent,
  to_rebase = totrev_old_q,
  date_chain = minDateTotRevRecent
  )

plot_df <- bind_rows(
  mutate(.data = totrev_old_a, var = "totrev_old_a"),
  mutate(.data = totrev_old_q, var = "totrev_old_q"),
  mutate(.data = totrev, var = "totrev_chained")
  )

# Plot the total government revenue for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Total Government Revenue")

ggsave(filename = "39_gov_rev_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Government interest payments -----
# We first interpolate the annual series in order to obtain quarterly series, 
# and then we chain the quarterly series from the two different databases.
intpay_old_a <- df_it |> 
  filter(var == "intpay_old") |> 
  mutate(value = value / 4)

intpay_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(intpay_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(y = intpay_old_a, by = join_by(period)) |> 
  select(-value.x, value = value.y) |> 
  mutate(value = na.spline(value), var = "intpay")

intpay_recent <- df_it |> 
  filter(var == "intpay_recent") |> 
  mutate(var = "intpay")

minDateIntPayRecent <- min(intpay_recent$period)

intpay <- chain(
  basis = intpay_recent,
  to_rebase = intpay_old_q,
  date_chain = minDateIntPayRecent
  )

plot_df <- bind_rows(
  mutate(.data = intpay_old_a, var = "intpay_old_a"),
  mutate(.data = intpay_old_q, var = "intpay_old_q"),
  mutate(.data = intpay, var = "intpay_chained")
  )

# Plot the govenrment interest payments for Italy
ggplot(data = plot_df, mapping = aes(x = period, y = value, color = var)) +
  geom_line(linewidth = 1.2) +
  my_theme() +
  ggtitle("Italy: Government Interest Payments")

ggsave(filename = "40_gov_intpay_IT.png", path = fig_path, height = 12, width = 12)
graphics.off()

#### Italy: Merging data ----
# We gather all the final series for Italy in a data frame.
IT_rawdata <- df_it |> 
  filter(! var %in% c(
    "conso", "inves", "definves", "lendingrate_old", "lendingrate_recent",
    "pop_old", "pop_recent",
    "debt_old", "debt_recent",
    "unempbenef_old", "unempbenef_recent", 
    "totexp_recent", "totexp_old", 
    "intpay_recent", "intpay_old",
    "totrev_recent", "totrev_old",
    "pubcons_recent", "pubcons_old",
    "pubinves_recent", "pubinves_old",
    "tfs_recent", "tfs_old"
    )
  ) |> 
  bind_rows(conso, inves, definves, lendingrate, pop, debt, unempbenef, totexp, totrev, intpay, pubcons, pubinves, tfs) |> 
  spread(var, value) |> 
  add_column(country = "IT")


### Final database for the estimation ----

#### Implicit tax rates -----

#### Merge raw data ----

#### Normalize data ----


### Series for the calibration -----

#### Leverage of non-financial corporations ----


### The deprecation of the capital stock ----

#### Step 1: Data for Euro Area countries ----

#### Step 2: Euro Area GDP-weighted average ----


### The share of capital revenues in GDP ----

#### Step 1: Data for Euro Area countries ----

#### Step 2: Euro Area GDP-weighted average ----


### The share of capital in GDP ----

#### Step 1: Data for Euro Area countries ----

#### Step 2: Euro Area GDP-weighted average ----


### The share of crude oil imports in GDP ----


### The share of petrol in private consumption -----


### Share of final consumption in imports ----


### Miscellaneous ----


### Final series for the calibration, and steady state values by country ----


# END
