# 01 - Macroeconomic data for FR, DE, IT, ES & EA ----
# URL: https://macro.cepremap.fr/article/2021-02/five-countries-data/
library(tidyverse)
library(zoo)
library(rdbnomics)
library(seasonal)
library(kableExtra)
library(RColorBrewer)
source("C:/Users/matth/OneDrive/Dokumente/R-scripts/cepremap/R/utils.R")
palette(brewer.pal(n = 5, name = "Set1"))
fig_path  <- "figures/01_five-countries-data"
last_date <- today()
last_update <- paste0("Last update: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

## Gather databases -----
#   1. Smets & Wouters (2003) data base
#   2. Financial database for the Euro Area
#   3. Fiscal data base for the Euro Area
#   4. International database for the Euro Area
sw03 <- read_csv("data/EA_SW_rawdata.csv") |>
  filter(period >= "1980-01-01")

fipu <- read_csv("data/EA_Fipu_rawdata.csv")
fina <- read_csv("data/EA_Finance_rawdata.csv")
open <- read_csv("data/EA_Open_rawdata.csv")

EA_rawdata <- sw03 |>
  inner_join(fipu, join_by(period)) |>
  inner_join(fina, join_by(period)) |>
  inner_join(open, join_by(period)) |>
  rename(unempbenef = unemp) |>
  mutate(pop = 1000 * pop) |>
  add_column(country = "EA")

## FR, DE, IT & ES ----
### Data retrieval & seasonal adjustment ----
#    1. Gross domestic product (GDP)
#    2. Consumption (C)
#    3. Investment (I)
#    4. GDP deflator
#    5. Compensation of employees (w)
#    6. Hours worked (h)
#    7. Investment deflator
#    8. Loans to non-financial corporations (NFC)
#    9. Entrepreneurial net worth
#   10. Short-term interest rate
#   11. Lending rate
#   12. Total government expenditure
#   13. Government consumption
#   14. Government transfers
#   15. Government interest payments
#   16. Government debt
#   17. World demand
#   18. Total government revenue
#   19. Unemployment benefits
#   20. Nominal effective exchange rate (NEER)
#   21. Imports
#   22. Exports
#   23. Population
# Oil prices are assumed to be the same in all countries.

#### Compensation of employees ----
wage_de_fr <- rdb("Eurostat", "namq_10_a10", mask = "Q.CP_MEUR.SA.TOTAL.D1.DE+FR") |>
  add_column(var = "wage")

wage_es_it <- rdb("Eurostat", "namq_10_a10", mask = "Q.CP_MEUR.SCA.TOTAL.D1.ES+IT") |>
  add_column(var = "wage")

#### Hours worked ----
hours <- rdb("Eurostat", "namq_10_a10_e", mask = "Q.THS_HW.TOTAL.SCA.EMP_DC.IT+DE+FR+ES") |>
  add_column(var = "hours")

#### Gross domestic product -----
gdp <- rdb("Eurostat", "namq_10_gdp", mask = "Q.CLV10_MEUR.SCA.B1GQ.IT+DE+FR+ES") |>
  add_column(var = "gdp")

#### Consumption -----
conso <- rdb("Eurostat", "namq_10_gdp", mask = "Q.CLV10_MEUR.SCA.P31_S14_S15.IT+DE+FR+ES") |>
  add_column(var = "conso")

#### Investment -----
inves <- rdb("Eurostat", "namq_10_gdp", mask = "Q.CLV10_MEUR.SCA.P51G.IT+DE+FR+ES") |>
  add_column(var = "inves")

#### GDP deflator -----
defgdp <- rdb("Eurostat", "namq_10_gdp", mask = "Q.PD10_EUR.SCA.B1GQ.IT+DE+FR+ES") |>
  add_column(var = "defgdp")

#### Investment deflator -----
definves <- rdb("Eurostat", "namq_10_gdp", mask = "Q.PD10_EUR.SCA.P51G.IT+DE+FR+ES") |>
  add_column(var = "definves")

#### Population -----
pop_recent <- rdb("Eurostat", "lfsq_pganws", mask = "Q.THS_PER.T.TOTAL.Y15-64.POP.IT+DE+FR+ES") |>
  add_column(var = "pop_recent")

pop_old <- rdb("Eurostat", "demo_pjanbroad", mask = "A.NR.Y15-64.T.IT+DE+FR+ES") |>
  add_column(var = "pop_old")

#### Government consumption ----
pubcons_recent_fr <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.P3.FR") |>
  add_column(var = "pubcons_recent")

pubcons_recent_it_de_es_nsa <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.P3.IT+DE+ES") |>
  add_column(var = "pubcons_recent")

pubcons_old_it_de <- rdb("Eurostat", "gov_10a_main", mask = "A.MIO_EUR.S13.P3.IT+DE") |>
  add_column(var = "pubcons_old")

# Seasoanal adjustment
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

df_nsa_q <- df_nsa_q |> mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q, deseasoned_q) |>
  na.omit()

# Plot government consumption
ggplot(plot_df, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Government Consumption", caption = last_update)

ggsave("01_gov_cons_DE-ES-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on government consumption
pubcons_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "pubcons_recent")

#### Government investment ----
pubinves_recent_fr <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.P51G.FR") |>
  add_column(var = "pubinves_recent")

pubinves_recent_it_de_es_nsa <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.P51G.IT+DE+ES") |>
  add_column(var = "pubinves_recent")

pubinves_old_it_de <- rdb("Eurostat", "gov_10a_main", mask = "A.MIO_EUR.S13.P51G.IT+DE") |>
  add_column(var = "pubinves_old")

# Seasoanal adjustment
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

# Plot government investment
ggplot(plot_df, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Government Investment", caption = last_update)

ggsave("02_gov_inves_DE-ES-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on government investment
pubinves_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "pubinves_recent")

#### Government interest payments ----
tfs_recent_fr <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.D62PAY.FR") |>
  add_column(var = "tfs_recent")

tfs_recent_it_de_es_nsa <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.D62PAY.IT+DE+ES") |>
  add_column(var = "tfs_recent")

tfs_old_it_de <- rdb("Eurostat", "gov_10a_main", mask = "A.MIO_EUR.S13.D62PAY.IT+DE") |>
  add_column(var = "tfs_old")

# Seasoanal adjustment
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

# Plot government social transfers
ggplot(plot_df, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Government Social Transfers", caption = last_update)

ggsave("03_gov_transf_DE-ES-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on government social transfers
tfs_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "tfs_recent")

#### Government interest payments ----
intpay_recent_fr <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.D41PAY.FR") |>
  add_column(var = "intpay_recent")

intpay_recent_it_de_es_nsa <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.D41PAY.IT+DE+ES") |>
  add_column(var = "intpay_recent")

intpay_old_it_de <- rdb("Eurostat", "gov_10a_main", mask = "A.MIO_EUR.S13.D41PAY.DE+IT") |>
  add_column(var = "intpay_old")

# Seasoanal adjustment
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

# Plot government interest payments
ggplot(plot_df, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Government Interest Payments", caption = last_update)

ggsave("04_gov_intpay_DE-ES-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on government interest payments
intpay_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "intpay_recent")

#### Total government expenditure ----
totexp_recent_fr <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.TE.FR") |>
  add_column(var = "totexp_recent")

totexp_recent_it_de_es_nsa <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.TE.IT+DE+ES") |>
  add_column(var = "totexp_recent")

totexp_old_it_de <- rdb("Eurostat", "gov_10a_main", mask = "A.MIO_EUR.S13.TE.DE+IT") |>
  add_column(var = "totexp_old")

# Seasoanal adjustment
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

# Plot total government expenditure
ggplot(plot_df, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Total Government Expenditure", caption = last_update)

ggsave("05_gov_exp_DE-ES-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on total government expenditure
totexp_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "totexp_recent")

#### Total government revenue -----
totrev_recent_fr <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.SCA.S13.TR.FR") |>
  add_column(var = "totrev_recent")

totrev_recent_it_de_es_nsa <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.TR.IT+DE+ES") |>
  add_column(var = "totrev_recent")

totrev_old_it_de <- rdb("Eurostat", "gov_10a_main", mask = "A.MIO_EUR.S13.TR.DE+IT") |>
  add_column(var = "totrev_old")

# Seasoanal adjustment
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

# Plot total government revenue
ggplot(plot_df, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Total Government Revenue", caption = last_update)

ggsave("06_gov_rev_DE-ES-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on total government revenue
totrev_recent_it_de_es <- deseasoned_q |>
  filter(Origin == "Adjusted Series") |>
  select(country, -Origin, value, period) |>
  mutate(var = "totrev_recent")

#### Government debt -----
debt_recent <- rdb("Eurostat", "gov_10q_ggdebt", mask = "Q.GD.S13.MIO_EUR.IT+DE+FR+ES") |>
  add_column(var = "debt_recent")

debt_old <- rdb("IMF", "WEO:latest", mask = "DEU+ESP+FRA+ITA.GGXWDG") |>
  add_column(var = "debt_old") |>
  select(geo = "weo-country", period, value, var) |>
  mutate(geo = str_sub(string = geo, start = 1, end = 2)) |>
  filter(year(period) <= max(debt_recent$period))

# Seasoanal adjustment
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

# Plot government debt
ggplot(plot_df, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 4, scales = "free_y") +
  my_theme() +
  labs(title = "Government debt", caption = last_update)

ggsave("07_gov_debt_DE-ES-FR-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on government debt
debt_recent <- deseasoned_q |> 
  filter(Origin == "Adjusted Series") |> 
  select(country, -Origin, value, period) |> 
  mutate(var = "debt_recent")

#### Loans to non-financial corporations -----
loans_nfc <- rdb("BIS", "total_credit", mask = "Q.IT+DE+FR+ES.N.A.M.XDC.A") |>
  add_column(var = "loans_nfc") |>
  as_tibble() |>
  select(geo = BORROWERS_CTY, period, value, var)

#### Entrepreneurial net worth ----
networth <- rdb("OECD", "MEI", mask = "FRA+DEU+ITA+ESP.SPASTT01.IXOB.Q") |>
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
shortrate <- rdb("OECD", "MEI", mask = "FRA+DEU+ITA+ESP.IR3TIB01.ST.Q") |>
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
lendingrate_recent <- rdb("ECB", "MIR", mask = "M.IT+DE+FR+ES.B.A2A.A.R.A.2240.EUR.N") |>
  as_tibble() |>
  select(geo = REF_AREA, period, value) |>
  mutate(period = paste(year(period), quarter(period))) |>
  group_by(geo, period) |>
  summarize(value = mean(value, na.rm = TRUE)) |>
  mutate(var = "lendingrate_recent", period = yq(period))

lendingrate_old <- rdb("IMF", "IFS", mask = "Q.IT+DE+FR+ES.FILR_PA") |>
  as_tibble() |>
  add_column(var = "lendingrate_old") |>
  select(geo = REF_AREA, period, value, var)

### World demand ----
world_demand <- read_csv("data/Foreign_demand.csv") |>
  mutate(
    country = case_when(
      country == "France"  ~ "FR",
      country == "Germany" ~ "DE",
      country == "Italy"   ~ "IT",
      country == "Spain"   ~ "ES",
      TRUE ~ country
    )
  ) |> 
  rename(geo = country)

#### Unemployment benefits ----
# 1. Calculate the quarterly share from quarterly government social expenditures
url_filter <- "Q.MIO_EUR.NSA.S13.D62PAY.IT+DE+FR+ES"
df <- rdb("Eurostat", "gov_10q_ggnfa", mask = url_filter)

socialexp <- df |>
  mutate(year = year(period), country = geo) |>
  select(period, value, year, country) |>
  group_by(year, country) |>
  mutate(sum = sum(value), ratio = value / sum) |>
  ungroup() |>
  select(-c("value", "year", "sum"))

# 2. Use the ratio to compute quarterly unemployment benefits from annual unemployment benefits
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

# 3. Seasonally adjust the quarterly data
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
ggplot(plot_unempbenef, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~country, nrow = 4, scales = "free_y") +
  my_theme() +
  labs(title = "Unemployment benefits", caption = last_update)

ggsave("08_unempbenef_DE-ES-FR-IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

# Save the recent data on unemployment benefits in DE, ES, FR & IT
unempbenef_recent <- unempbenef_q_deseasoned |>
  filter(Origin == "Adjusted Series") |>
  select(geo = country, -Origin, value, period) |>
  mutate(var = "unempbenef_recent")

# 4. Get annual Eurostat spr_exp_sum data that will be interpolated and chained later.
url_filter <- "A.UNEMPLOY.MIO_EUR.IT+DE+FR+ES"
df <- rdb("Eurostat", "spr_exp_sum", mask = url_filter)

unempbenef_old <- df |>
  add_column(var = "unempbenef_old") |>
  select(period, value, geo, var)

#### (Nominal) effective exchange rate ----
df <- rdb("BIS", "eer", mask = "M.N.B.IT+DE+FR+ES")

neer <- df |>
  select(period, value, geo = REF_AREA) |>
  mutate(period = paste(year(period), quarter(period))) |>
  group_by(geo, period) |>
  summarize(value = mean(value)) |>
  mutate(period = yq(period), var = "neer")

#### Imports and exports ----
df <- rdb("OECD", "EO", mask = "FRA+DEU+ITA+ESP.MGSV+XGSV.Q")

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

# Plot the unchained series
ggplot(df, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ var, ncol = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Unchained Series", caption = last_update)

ggsave("09_unchained_DE-ES-FR-IT.png", path = fig_path, height = 12, width = 10)
graphics.off()

### France: Chaining & Interpolating Data -----
df_fr <- df |> 
  filter(country == "FR") |> 
  select(-country)

#### France: Government debt ----
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
  left_join(debt_old_a, join_by(period)) |> 
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
  add_column(debt_old_a, var = "debt_old_a"),
  mutate(debt_old_q, var = "debt_old_q"),
  mutate(debt, var = "debt_chained")
  )

# Plot government debt for France
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "France: Government Debt", caption = last_update)

ggsave("10_gov_debt_FR.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Population -----
pop_old_a <- df_fr |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(pop_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(pop_old_a, join_by(period)) |> 
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
  mutate(pop_old_a, var = "pop_old_a"),
  mutate(pop_old_q, var = "pop_old_q"),
  mutate(pop, var = "pop_chained")
  )

# Plot the population for France
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "France: Population", caption = last_update)

ggsave("11_pop_FR.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### France: Lending rate -----
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
  mutate(lendingrate_old, var = "lendingrate_old"),
  mutate(lendingrate_recent, var = "lendingrate_recent"),
  mutate(lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for France
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "France: Lending Rate", caption = last_update)

ggsave("12_lendingrate_FR.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### France: Unemployment benefits -----
unempbenef_old_a <- df_fr |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(unempbenef_old_a, join_by(period)) |> 
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
  mutate(unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for France
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "France: Unemployment Benefits", caption = last_update)

ggsave("13_unempbenef_FR.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### France: Merging French Data -----
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

### Spain: Chaining & Interpolating Data ----.
df_es <- df |> 
  filter(country == "ES") |> 
  select(-country)

#### Spain: Government debt ----
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
  left_join(debt_old_a, join_by(period)) |> 
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
  add_column(debt_old_a, var = "debt_old_a"),
  mutate(debt_old_q, var = "debt_old_q"),
  mutate(debt, var = "debt_chained")
  )

# Plot the government debt of Spain
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Spain: Government Debt", caption = last_update)

ggsave("14_gov_debt_ES.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Spain: Population -----
pop_old_a <- df_es |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
    period = seq(
      from = as.Date("1991-01-01"),
      length.out = (nrow(pop_old_a) - 1) * 4 + 1,
      by = "quarter"),
    value = NA) |> 
  left_join(pop_old_a, join_by(period)) |> 
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
  mutate(pop_old_a, var = "pop_old_a"),
  mutate(pop_old_q, var = "pop_old_q"),
  mutate(pop, var = "pop_chained")
  )

# Plot the population for Spain
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Spain: Population", caption = last_update)

ggsave("15_pop_ES.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Spain: Lending rate -----
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
  mutate(lendingrate_old, var = "lendingrate_old"),
  mutate(lendingrate_recent, var = "lendingrate_recent"),
  mutate(lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for Spain
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Spain: Lending Rate", caption = last_update)

ggsave("16_lendingrate_ES.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Spain: Unemployment benefits -----
unempbenef_old_a <- df_es |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(unempbenef_old_a, join_by(period)) |> 
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
  mutate(unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for Spain
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Unemployment Benefits", caption = last_update)

ggsave("17_unempbenef_ES.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Spain: Merging Spanish Data ----
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
df_de <- df |> 
  filter(country == "DE") |> 
  select(-country)

#### Germany: Government debt -----
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
  left_join(debt_old_a, join_by(period)) |> 
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
  add_column(debt_old_a, var = "debt_old_a"),
  mutate(debt_old_q, var = "debt_old_q"),
  mutate(debt, var = "debt_chained")
  )

# Plot the government debt for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Government Debt", caption = last_update)

ggsave("18_govdebt_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Population -----
pop_old_a <- df_de |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(pop_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(pop_old_a, join_by(period)) |> 
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
  mutate(pop_old_a, var = "pop_old_a"),
  mutate(pop_old_q, var = "pop_old_q"),
  mutate(pop, var = "pop_chained")
  )

# Plot the population for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Population", caption = last_update)

ggsave("19_pop_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Lending rate ----
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
  mutate(lendingrate_old, var = "lendingrate_old"),
  mutate(lendingrate_recent, var = "lendingrate_recent"),
  mutate(lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Lending Rate", caption = last_update)

ggsave("20_lendingrate_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Unemployment benefits ----
unempbenef_old_a <- df_de |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(unempbenef_old_a, join_by(period)) |> 
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
  mutate(unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Unemployment Benefits", caption = last_update)

ggsave("21_unempbenef_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Government consumption -----
pubcons_old_a <- df_de |> 
  filter(var == "pubcons_old") |> 
  mutate(value = value / 4)

pubcons_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubcons_old_a) - 1) * 4 + 1,
    by = "quarter"),
  value = NA) |> 
  left_join(pubcons_old_a, join_by(period)) |> 
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
  mutate(pubcons_old_a, var = "pubcons_old_a"),
  mutate(pubcons_old_q, var = "pubcons_old_q"),
  mutate(pubcons, var = "pubcons_chained")
  )

# Plot the government consumption for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Government Consumption", caption = last_update)

ggsave("22_gov_cons_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Government investment -----
pubinves_old_a <- df_de |> 
  filter(var == "pubinves_old") |> 
  mutate(value = value / 4)

pubinves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubinves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(pubinves_old_a, join_by(period)) |> 
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
  mutate(pubinves_old_a, var = "pubinves_old_a"),
  mutate(pubinves_old_q, var = "pubinves_old_q"),
  mutate(pubinves, var = "pubinves_chained")
  )

# Plot government investment for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Government Investment", caption = last_update)

ggsave("23_gov_inves_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Government social transfers ----
tfs_old_a <- df_de |> 
  filter(var == "tfs_old") |> 
  mutate(value = value / 4)

tfs_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(tfs_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(tfs_old_a, join_by(period)) |> 
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
  mutate(tfs_old_a, var = "tfs_old_a"),
  mutate(tfs_old_q, var = "tfs_old_q"),
  mutate(tfs, var = "tfs_chained")
  )

# Plot government social transfers for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Government Social Transfers", caption = last_update)

ggsave("24_gov_soctransf_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Total government expenditure ----
totexp_old_a <- df_de |> 
  filter(var == "totexp_old") |> 
  mutate(value = value / 4)

totexp_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totexp_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(totexp_old_a, join_by(period)) |> 
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
  mutate(totexp_old_a, var = "totexp_old_a"),
  mutate(totexp_old_q, var = "totexp_old_q"),
  mutate(totexp, var = "totexp_chained")
  )

# Plot total government expenditure for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Total Government Expenditure", caption = last_update)

ggsave("25_gov_exp_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Total government revenue ----
totrev_old_a <- df_de |> 
  filter(var == "totrev_old") |> 
  mutate(value = value / 4)

totrev_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totrev_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(totrev_old_a, join_by(period)) |> 
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
  mutate(totrev_old_a, var = "totrev_old_a"),
  mutate(totrev_old_q, var = "totrev_old_q"),
  mutate(totrev, var = "totrev_chained")
  )

# Plot total government revenue for Germany
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Total Government Revenue", caption = last_update)

ggsave("26_gov_rev_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Government interest payments -----
intpay_old_a <- df_de |> 
  filter(var == "intpay_old") |> 
  mutate(value = value / 4)

intpay_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(intpay_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(intpay_old_a, join_by(period)) |> 
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
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Germany: Government Interest Payments", caption = last_update)

ggsave("27_gov_intpay_DE.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Germany: Merging German Data ----
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
df_it <- df |> 
  filter(country == "IT") |> 
  select(-country)

#### Italy: Consumption ----
conso_old_a <- rdb("Eurostat", "nama_10_gdp", mask = "A.CLV10_MEUR.P31_S14_S15.IT") |> 
  select(period, value) |> 
  add_column(var = "conso_old") |> 
  mutate(value = value / 4)

conso_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(conso_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(conso_old_a, join_by(period)) |> 
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
  mutate(conso_old_a, var = "conso_old_a"),
  mutate(conso_old_q, var = "conso_old_q"),
  mutate(conso, var = "conso_chained")
  )

# Plot consumption for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Consumption", caption = last_update)

ggsave("28_consumption_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Investment ----
inves_old_a <- rdb("Eurostat", "nama_10_gdp", mask = "A.CLV10_MEUR.P51G.IT") |> 
  select(period, value) |> 
  add_column(var = "inves_old") |> 
  mutate(value = value / 4)

inves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(inves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(inves_old_a, join_by(period)) |> 
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
  mutate(inves_old_a, var = "inves_old_a"),
  mutate(inves_old_q, var = "inves_old_q"),
  mutate(inves, var = "inves_chained")
  )

# Plot investment for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Investment", caption = last_update)

ggsave("29_investment_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Investment deflator ----
definves_old_a <- rdb("Eurostat", "nama_10_gdp", mask = "A.PD10_EUR.P51G.IT") |> 
  select(period, value) |> 
  add_column(var = "definves_old")

definves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(definves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(definves_old_a, join_by(period)) |> 
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
  mutate(definves_old_a, var = "definves_old_a"),
  mutate(definves_old_q, var = "definves_old_q"),
  mutate(definves, var = "definves_chained")
  )

# Plot the investment deflator for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Investment Deflator", caption = last_update)

ggsave("30_invesdef_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Government debt ----
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
  left_join(debt_old_a, join_by(period)) |> 
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
  add_column(debt_old_a, var = "debt_old_a"),
  mutate(debt_old_q, var = "debt_old_q"),
  mutate(debt, var = "debt_chained")
  )

# Plot the government debt of Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Government Debt", caption = last_update)

ggsave("31_govdebt_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Population ----
pop_old_a <- df_it |> 
  filter(var == "pop_old")

pop_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(pop_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(pop_old_a, join_by(period)) |> 
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
  mutate(pop_old_a, var = "pop_old_a"),
  mutate(pop_old_q, var = "pop_old_q"),
  mutate(pop, var = "pop_chained"),
  mutate(pop_recent, var = "pop_recent")
  )

# Plot the population of Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Population", caption = last_update)

ggsave("32_pop_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Lending rate ----
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
  mutate(lendingrate_old, var = "lendingrate_old"),
  mutate(lendingrate_recent, var = "lendingrate_recent"),
  mutate(lendingrate, var = "lendingrate_chained")
  )

# Plot the lending rate for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Lending Rate", caption = last_update)

ggsave("33_lendingrate_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Unemployment benefits -----
unempbenef_old_a <- df_it |> 
  filter(var == "unempbenef_old") |> 
  mutate(value = value / 4)

unempbenef_old_q <- tibble(
  period = seq(
    from = as.Date("1991-01-01"),
    length.out = (nrow(unempbenef_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(unempbenef_old_a, join_by(period)) |> 
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
  mutate(unempbenef_old_a, var = "unempbenef_old_a"),
  mutate(unempbenef_old_q, var = "unempbenef_old_q"),
  mutate(unempbenef, var = "unempbenef_chained")
  )

# Plot the unemployment benefits for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Unemployment Benefits", caption = last_update)

ggsave("34_unempbenef_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Government consumption ----
pubcons_old_a <- df_it |> 
  filter(var == "pubcons_old") |> 
  mutate(value = value / 4)

pubcons_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubcons_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(pubcons_old_a, join_by(period)) |> 
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
  mutate(pubcons_old_a, var = "pubcons_old_a"),
  mutate(pubcons_old_q, var = "pubcons_old_q"),
  mutate(pubcons, var = "pubcons_chained")
  )

# Plot the government consumption of Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Government Consumption", caption = last_update)

ggsave("35_govcons_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Government investment -----
pubinves_old_a <- df_it |> 
  filter(var == "pubinves_old") |> 
  mutate(value = value / 4)

pubinves_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(pubinves_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(pubinves_old_a, join_by(period)) |> 
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
  mutate(pubinves_old_a, var = "pubinves_old_a"),
  mutate(pubinves_old_q, var = "pubinves_old_q"),
  mutate(pubinves, var = "pubinves_chained")
  )

# Plot government investment for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Government Investment", caption = last_update)

ggsave("36_gov_inv_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Government social transfers -----
tfs_old_a <- df_it |> 
  filter(var == "tfs_old") |> 
  mutate(value = value / 4)

tfs_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(tfs_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(tfs_old_a, join_by(period)) |> 
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
  mutate(tfs_old_a, var = "tfs_old_a"),
  mutate(tfs_old_q, var = "tfs_old_q"),
  mutate(tfs, var = "tfs_chained")
  )

# Plot government social transfers for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Government Social Transfers", caption = last_update)

ggsave("37_gov_soctransf_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Total Government expenditure ----
totexp_old_a <- df_it |> 
  filter(var == "totexp_old") |> 
  mutate(value = value / 4)

totexp_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totexp_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(totexp_old_a, join_by(period)) %>% 
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
  mutate(totexp_old_a, var = "totexp_old_a"),
  mutate(totexp_old_q, var = "totexp_old_q"),
  mutate(totexp, var = "totexp_chained")
  )

# Plot the total government expenditure for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Total Government Expenditure", caption = last_update)

ggsave("38_gov_exp_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Total government revenue ----
totrev_old_a <- df_it |> 
  filter(var == "totrev_old") |> 
  mutate(value = value / 4)

totrev_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(totrev_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(totrev_old_a, join_by(period)) |> 
  select(-value.x, value = value.y) |> 
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
  mutate(totrev_old_a, var = "totrev_old_a"),
  mutate(totrev_old_q, var = "totrev_old_q"),
  mutate(totrev, var = "totrev_chained")
  )

# Plot the total government revenue for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Total Government Revenue", caption = last_update)

ggsave("39_gov_rev_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Government interest payments -----
intpay_old_a <- df_it |> 
  filter(var == "intpay_old") |> 
  mutate(value = value / 4)

intpay_old_q <- tibble(
  period = seq(
    from = as.Date("1995-01-01"),
    length.out = (nrow(intpay_old_a)-1)*4+1,
    by = "quarter"),
  value = NA) |> 
  left_join(intpay_old_a, join_by(period)) |> 
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
  mutate(intpay_old_a, var = "intpay_old_a"),
  mutate(intpay_old_q, var = "intpay_old_q"),
  mutate(intpay, var = "intpay_chained"))

# Plot the government interest payments for Italy
ggplot(plot_df, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Italy: Government Interest Payments", caption = last_update)

ggsave("40_gov_intpay_IT.png", path = fig_path, height = 8, width = 10)
graphics.off()

#### Italy: Merging data ----
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
# Implicit tax rates (ITR) consumption, labor and corporate incomes: https://macro.cepremap.fr/article/2019-11/implicit_tax_rates/

itr <- read_csv("data/ITR_eurodata.csv") |> 
  rename(year = period)

itrq <- tibble(period = EA_rawdata$period) |> 
  mutate(year = year(period)) |> 
  left_join(itr, join_by(year)) |> 
  na.omit() |> 
  select(-year) |> 
  gather(var, value, -period) |> 
  separate(var, c("country", "var")) |> 
  spread(var, value)

#### Merge raw data ----
rawdata_var <- colnames(DE_rawdata)

# real exchange rate or nominal exchange rate?
EA_rawdata_short <- EA_rawdata |> 
  rename(neer = reer) |> 
  select(all_of(rawdata_var), oil_prices)

# Gather the datasets for France, Germany, Italy, Spain and the Euro area in a unique data frame. 
rawdata_df <- bind_rows(EA_rawdata_short, DE_rawdata, ES_rawdata, FR_rawdata, IT_rawdata) |> 
  left_join(itrq, join_by(period, country)) |> 
  arrange(country, period) |>
  filter(year(period) >= 1995, period <= last_date)

save(rawdata_df, file = "data/rawdata.RData")

#### Normalize data ----
data_df <- rawdata_df |> 
  mutate(
    period,
    country,
    gdp_rpc       = 1e+6 * gdp / pop,
    conso_rpc     = 1e+6 * conso / pop,
    inves_rpc     = 1e+6 * inves / pop,
    defgdp        = defgdp,
    wage_rph      = 1e+6 * wage / defgdp / (hours * 1000),
    hours_pc      = (hours * 1000) / pop,
    pinves_defl   = definves / defgdp,
    loans_nfc_rpc = 1e+9 * loans_nfc / pop / defgdp,
    networth_rpc  = 1e+6 * networth / pop / defgdp,
    re            = shortrate / 100,
    creditspread  = (lendingrate - shortrate) / 100,
    pubcons_rpc   = 100 * 1e+6 * pubcons / (defgdp * pop),
    pubinves_rpc  = 100 * 1e+6 * pubinves / (defgdp * pop),
    tfs_rpc       = 100 * 1e+6 * tfs / (defgdp * pop),
    othgov_rpc    = 100 * 1e+6 * (totexp - pubcons - pubinves - tfs - intpay) / (defgdp * pop),
    debt_gdp      = 100 * debt / (defgdp * gdp),
    taun, tauwf, tauwh, tauc,
    world_demand,
    oil_prices,
    neer,
    imports_rpc = imports / pop,
    exports_rpc = exports / pop,
    .keep = "none"
    ) |> 
  gather(var, value, -period, -country)

# The figure below shows the final series for all the listed countries.
plot_data_df <- data_df |> 
  mutate(
    varname = case_when(
      var == "gdp_rpc"       ~ "Real GDP per capita",
      var == "conso_rpc"     ~ "Real consumption \n per capita",
      var == "inves_rpc"     ~ "Real investment \n per capita",
      var == "defgdp"        ~ "GDP deflator",
      var == "wage_rph"      ~ "Real wage per hour" ,
      var == "hours_pc"      ~ "Hours worked per capita",
      var == "pinves_defl"   ~ "Real price of investment",
      var == "loans_nfc_rpc" ~ "Real credit to \n NFC per capita", 
      var == "networth_rpc"  ~ "Real net worth \n per capita",  
      var == "re"            ~ "Short-term \n interest rate (APR)",
      var == "creditspread"  ~ "Credit spread (APP)",
      var == "pubcons_rpc"   ~ "Real public consumption\n per capita",
      var == "pubinves_rpc"  ~ "Real public investment\n per capita",
      var == "tfs_rpc"       ~ "Real social transfers\n per capita",
      var == "othgov_rpc"    ~ "Real other public\n expenditure per capita",
      var == "debt_gdp"      ~ "Debt-to-GDP ratio",
      var == "taun"          ~ "Implicit Tax Rate \n on labor income ",
      var == "tauwh"         ~ "Implicit Tax Rate \n on employees' SSC",
      var == "tauwf"         ~ "Implicit Tax Rate \n on employers' SSC" ,
      var == "tauc"          ~ "Implicit Tax Rate \n on consumption" , 
      var == "world_demand"  ~ "Foreign demand",
      var == "oil_prices"    ~ "Crude oil prices",
      var == "neer"          ~ "Nominal effective exchange rate",
      var == "imports_rpc"   ~ "Real imports per capita",
      var == "exports_rpc"   ~ "Real exports per capita"
      ),
    country_name = case_when(
      country == "FR" ~ "France",
      country == "DE" ~ "Germany",
      country == "IT" ~ "Italy",
      country == "ES" ~ "Spain",
      country == "EA" ~ "Euro Area"
      )
    ) |> 
  na.omit()

# tikz not available for this version or R
# tikz("estimated.tex", width = 5.2, height = 6.4, sanitize = TRUE)

ggplot(plot_data_df, aes(period, value, col = country_name)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~varname, ncol = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Series for the estimation", caption = last_update)

ggsave("41_estimation.png", path = fig_path, height = 8, width = 10)
graphics.off()

df <- data_df |> 
  unite("var", c("country", "var")) |> 
  mutate(period = gsub(" ", "", as.yearqtr(period))) |> 
  spread(var, value) |> 
  select(-c("DE_oil_prices", "FR_oil_prices", "ES_oil_prices", "IT_oil_prices"))

colnames(df)[1] <- ""

write.csv(df, file = "data/data_DE_EA_ES_FR_IT.csv", row.names = FALSE)

# The data can be downloaded directly here: http://shiny.cepremap.fr/data/data_DE_EA_ES_FR_IT.csv

### Series for the calibration -----

#### Leverage of non-financial corporations ----
debt <- rdb("Eurostat", "nasq_10_f_bs", mask = "Q.MIO_EUR.S11.LIAB.F+F3+F4+F6.EA19+IT+DE+FR+ES")

leverage <- debt |> 
  select(value, period, country = geo, var = na_item) |> 
  mutate(var = case_when(
    var == "F"  ~ "total",
    var == "F3" ~ "debt_securities",
    var == "F4" ~ "loans",
    var == "F6" ~ "pensions_reserves"
    )
  ) |> 
  spread(var, value) |> 
  arrange(country, period) |> 
  mutate(
    period,
    country,
    value = (debt_securities + loans + pensions_reserves) / total,
    var="leverage",
    .keep = "none"
    ) |> 
  na.omit()

ggplot(leverage, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Leverage", caption = last_update)

ggsave("42_leverage.png", path = fig_path, height = 8, width = 10)
graphics.off()

### The deprecation of the capital stock ----
# Penn World Table Feenstra et al. (2015): https://www.rug.nl/ggdc/productivity/pwt/
list_country <- list(
  "France"    = "FRA",
  "Germany"   = "DEU",
  "Italy"     = "ITA",
  "Spain"     = "ESP",
  "Euro Area" = "EA"
  )

# `haven` package to read Stata DTA files: https://haven.tidyverse.org/reference/read_dta.html
df <- haven::read_dta("https://www.rug.nl/ggdc/docs/pwt100.dta") |> 
  mutate(country = countrycode, period = as.Date(as.yearqtr(year))) |> 
  filter(year(period) >= 1995 & currency_unit == "Euro" & !grepl("MNE", country))

#### Step 1: Data for Euro Area countries ----
delta <- df |> 
  select(country, period, value = delta) |> 
  add_column(var = "delta")

#### Step 2: Euro Area GDP-weighted average ----
# Get weights from output-side real GDP at chained PPPs (in mil. 2011US$) for each country.
gdp <- df |> 
  select(country, period, value = rgdpo)

EA_gdp <- gdp |> 
  group_by(period) |> 
  summarize(value = sum(value)) |> 
  ungroup()

weights <- gdp |> 
  left_join(EA_gdp, join_by(period)) |> 
  mutate(country, period, weight = value.x / value.y, .keep = "none")

# Apply the weights to the country data to build the Euro Area GDP-weighted average. 
delta_EA <- delta |> 
  left_join(weights, join_by(country, period))

delta_EA <- delta_EA |> 
  mutate(period, value = value * weight, .keep = "none") |> 
  group_by(period) |> 
  summarize(value = sum(value)) |> 
  add_column(country = "EA", var = "delta")

delta_countries <- delta |> 
  filter(grepl("FRA|DEU|ITA|ESP", country))

delta_FIN <- bind_rows(delta_countries, delta_EA)

# Plot depreciation rate of the capital stock
ggplot(delta_FIN, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Depreciation rate of the capital stock", caption = last_update)

ggsave("43_depreciation.png", path = fig_path, height = 8, width = 10)
graphics.off()

### The share of capital revenues in GDP ----
# Proceed in two steps. 

#### Step 1: Data for Euro Area countries ----
# Obtain the share of labor compensation in GDP also from the Penn World Table, 
# for the countries that compose the Euro Area. 
# We deduce then the share of capital revenues in GDP
alpha <- df |> 
  select(country, period, value = labsh) |> 
  mutate(value = 1 - value) |> 
  add_column(var = "alpha")

#### Step 2: Euro Area GDP-weighted average ----
# Now we apply the GDP-weights to our country data in order to build the Euro Area GDP-weighted average. 
# The figure below shows the final series for France, Germany, Italy, Spain and the Euro Area.
alpha_EA <- alpha |> 
  left_join(weights, join_by(country, period))

alpha_EA <- alpha_EA |> 
  mutate(period, value = value * weight, .keep = "none") |> 
  group_by(period) |> 
  summarize(value = sum(value)) |> 
  add_column(country = "EA", var = "alpha")

alpha_countries <- alpha |> 
  filter(grepl("FRA|DEU|ITA|ESP", country))

alpha_FIN <- bind_rows(alpha_countries, alpha_EA)

# Plot the share of capital revenues in GDP
ggplot(alpha_FIN, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Share of capital revenues in GDP", caption = last_update)

ggsave("44_capital_revenues.png", path = fig_path, height = 8, width = 10)
graphics.off()

### The share of capital in GDP ----

#### Step 1: Data for Euro Area countries ----
# We obtain the stock of capital in GDP also from the Penn World Table, 
# for the countries that compose the Euro Area. 
capital <- df |> 
  select(country, period, gdp = rgdpna, capital = rnna) |> 
  mutate(value = capital / gdp) |> 
  select(-capital, -gdp) |> 
  add_column(var = "capital_gdp")

#### Step 2: Euro Area GDP-weighted average ----
# Apply the GDP-weights to our country data in order to build the Euro Area GDP-weighted average. 
capital_EA <- capital |> 
  left_join(weights, join_by(country, period))

capital_EA <- capital_EA |> 
  mutate(period, value = value * weight, .keep = "none") |> 
  group_by(period) |> 
  summarize(value = sum(value)) |> 
  add_column(country = "EA", var = "capital_gdp")

capital_countries <- capital |> 
  filter(grepl("FRA|DEU|ITA|ESP", country))

capital_FIN <- bind_rows(capital_countries, capital_EA)

# Plot the stock of capital in GDP
ggplot(capital_FIN, aes(period, value, color = country)) +
  geom_line(lwd = 1.2)+
  my_theme() +
  labs(title = "Stock of capital in GDP", caption = last_update)

ggsave("45_capital_shares.png", path = fig_path, height = 8, width = 10)
graphics.off()

### The share of crude oil imports in GDP ----
oil_import_value <- rdb("Eurostat", "nrg_ti_coifpm", mask = "M.TOTAL.VAL_THS_USD.DE+FR+IT+ES+EU_V") |> 
  select(period, oil_import = value, country = geo) |> 
  mutate(year = year(period)) |> 
  group_by(country, year) |> 
  summarise(oil_import = sum(oil_import)) |> 
  mutate(period = as.Date(paste0(year, "-01-01"))) |> 
  select(-year) |> 
  mutate(
    country = case_when(
      country == "EU_V" ~ "EA",
      TRUE ~ country
      )
    )

# in USD
ea_gdp_usd <- rdb("IMF", "WEOAGG:latest", mask = "998.NGDPD.us_dollars") |> 
  select(period, gdp = value, country = `weo-countries-group`)

gdp_usd <- rdb("IMF", "WEO:latest", mask = "FRA+DEU+ITA+ESP.NGDPD.us_dollars") |> 
  select(period, gdp = value, country = `weo-country`) |> 
  bind_rows(ea_gdp_usd) |> 
  mutate(country = case_when(
    country == "FRA" ~ "FR",
    country == "DEU" ~ "DE",
    country == "ITA" ~ "IT",
    country == "ESP" ~ "ES",
    country == "998" ~ "EA",
    TRUE ~ country
    )
  )

oil <- oil_import_value |> 
  left_join(gdp_usd, join_by(country, period)) |> 
  mutate(
    period,
    value = oil_import / (gdp * 1000000),
    var = "oil_imports_gdp",
    .keep = "none"
    )

ggplot(oil, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Share of oil imports in GDP", caption = last_update)

ggsave("46_oil_import_shares.png", path = fig_path, height = 8, width = 10)
graphics.off()

### The share of petrol in private consumption -----
petrol_weight <- rdb("Eurostat", "prc_hicp_inw", mask = "A.CP07222.FR+DE+IT+ES+EA") |> 
  select(value, country = geo, period) |> 
  mutate(value = value / 1000) |> 
  rename(petrol_weight = value) |> 
  filter(year(period) >= 2015)

hhconso <- rdb("Eurostat", "nama_10_gdp", mask = "A.PC_GDP.P31_S14.FR+DE+IT+ES+EA") |> 
  select(period, conso_gdp = value, country = geo) |> 
  filter(year(period) >= 2015)

petrol_conso <- hhconso |> 
  left_join(petrol_weight, join_by(country, period)) |> 
  na.omit() |> 
  mutate(
    period,
    country,
    value = conso_gdp * petrol_weight / 100,
    var = "petrol_conso",
    .keep = "none"
    )

ggplot(petrol_conso, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Petrol consumption to GDP ratio", caption = last_update)

ggsave("47_petrol_cons_shares.png", path = fig_path, height = 8, width = 10)
graphics.off()

### Share of final consumption in imports ----
imported_conso <- rdb("Eurostat", "naio_10_cp1700", mask = "A.MIO_EUR.IMP.P3+P51G.TOTAL.DE+FR+IT+ES+EA19") |> 
  select(period, value, country = geo, var = induse) |> 
  spread(var, value) |> 
  mutate(value = P3 / (P3 + P51G)) |> 
  select(-c("P3", "P51G")) |> 
  filter(year(period) >= 2010) |> 
  mutate(
    country = case_when(
      country == "EA19" ~ "EA",
      TRUE ~ country
      ),
    var = "imported_conso"
    )

ggplot(imported_conso, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  labs(title = "Share of final consumption in total imports", caption = last_update)

ggsave("48_cons_imp_shares.png", path = fig_path, height = 8, width = 10)
graphics.off()

### Miscellaneous ----
ea_gdp <- rdb(ids = "Eurostat/namq_10_gdp/Q.CP_MEUR.SCA.B1GQ.EA19") |> 
  select(period, gdp = value)

df <- rdb("ECB", "TRD", mask = "M.I9.Y.M+X.TTT.J9.4.VAL")
# Original: M.I8.Y.M+X.TTT.J8.4.VAL

ea_trade <- df |> 
  mutate(
    period = paste(year(period), quarter(period), sep = "-"),
    value,
    var = if_else(grepl("Import", series_name), "imports", "exports"),
    .keep = "none") |> 
  group_by(var, period) |> 
  summarize(value = sum(value)) |> 
  ungroup() |> 
  mutate(period = yq(period)) |> 
  spread(var, value) |> 
  left_join(ea_gdp, join_by(period)) |> 
  mutate(
    period,
    imports_gdp = imports / (gdp * 1000),
    exports_gdp = exports / (gdp * 1000),
    .keep = "none"
    ) |> 
  gather(var, value, -period) |> 
  add_column(country = "EA")

df <- rdb("OECD", "EO", mask = "FRA+DEU+ITA+ESP.XGS+MGS+GDP.A")

imports_exports <- df |> 
  select(var = VARIABLE, period, value, country = LOCATION) |> 
  spread(var, value) |> 
  mutate(
    period, 
    country,
    imports_gdp = MGS / GDP,
    exports_gdp = XGS / GDP,
    .keep = "none"
    ) |> 
  gather(var, value, -period, -country) |> 
  mutate(country = case_when(
    country == "FRA" ~ "FR",
    country == "DEU" ~ "DE",
    country == "ITA" ~ "IT",
    country == "ESP" ~ "ES",
    TRUE ~ country
    )
  ) |> 
  bind_rows(ea_trade)

alpha_delta_capital <- bind_rows(alpha_FIN, delta_FIN, capital_FIN) |> 
  mutate(
    country = case_when(
      country == "FRA" ~ "FR",
      country == "DEU" ~ "DE",
      country == "ITA" ~ "IT",
      country == "ESP" ~ "ES",
      TRUE ~ country
      )
    )

leverage2 <- leverage |> 
  mutate(country = case_when(
    country == "EA19" ~ "EA",
    TRUE ~ country))

share <- rdb("Eurostat", "nama_10_gdp", mask = "A.CP_MPPS_EU27_2020.B1GQ.DE+FR+IT+ES+EA19")
# Original: A.CP_MPPS.B1GQ.DE+FR+IT+ES+EA19

share2 <- share |> 
  select(value, country = geo, period) |> 
  spread(country, value) |> 
  mutate(
    period,
    FR = FR / EA19,
    DE = DE/EA19,
    IT = IT / EA19,
    ES = ES / EA19,
    .keep = "none") |> 
  gather(country, value, -period) |> 
  mutate(var = "share")

### Final series for the calibration, and steady state values by country ----
rawdata <- bind_rows(EA_rawdata_short, FR_rawdata, ES_rawdata, IT_rawdata, DE_rawdata) |> 
  left_join(itrq, join_by(country, period)) |> 
  select(period, country, pubcons, pubinves, tfs, totexp, totrev, intpay, gdp, inves, tauk) |> 
  filter(period <= max(rawdata_df$period)) |> 
  gather(var, value, -country, -period) |> 
  bind_rows(plot_data_df) |> 
  select(-c("varname", "country_name"))

hours_pc_meanEA <- rawdata |> 
  filter(var == "hours_pc", country == "EA") |> 
  summarise(value = mean(value, na.rm = TRUE)) |> 
  first()

rawdata_growth_ratio <- rawdata |> 
  spread(var, value) |> 
  arrange(country) |> 
  mutate(
    period,
    country,
    defgdp_growth   = defgdp / lag(defgdp, 4) - 1,
    gdp_rpc_growth  = gdp_rpc / lag(gdp_rpc, 4) - 1,
    definves_growth = pinves_defl / lag(pinves_defl, 4) - 1,
    hours_pc_index  = hours_pc / hours_pc_meanEA$value,
    tfs_gdp         = tfs / (defgdp / 100 * gdp),
    pubcons_gdp     = pubcons / (defgdp / 100 * gdp),
    pubinves_gdp    = pubinves / (defgdp / 100 * gdp),
    totexp_gdp      = totexp / (defgdp / 100 * gdp),
    otherexp_gdp    = (totexp - tfs - pubcons - pubinves - intpay) / (defgdp / 100 * gdp),
    intpay_gdp      = intpay / (defgdp / 100 * gdp),
    totrev_gdp      = totrev / (defgdp / 100 * gdp),
    inves_gdp       = inves / gdp,
    shortrate       = re,
    tauk, taun, tauwh, tauwf, tauc,
    .keep = "none"
  ) |> 
  gather(var, value, -period, -country) |> 
  bind_rows(
    alpha_delta_capital,
    leverage2,
    imports_exports,
    imported_conso,
    share2,
    oil,
    petrol_conso
  ) |> 
  filter(year(period) >= 1995, year(period) <= 2019) |> 
  mutate(
    varname = case_when(
      var == "alpha"           ~ "Share of capital \n revenue in GDP",
      var == "defgdp_growth"   ~ "GDP deflator \n growth rate",
      var == "definves_growth" ~ "Price of investment \n growth rate",
      var == "delta"           ~ "Depreciation rate \n of the capital stock" ,
      var == "capital_gdp"     ~ "Capital stock in GDP",
      var == "exports_gdp"     ~ "Exports-to-GDP ratio" ,
      var == "gdp_rpc_growth"  ~ "Real GDP per capita \n growth rate" ,
      var == "hours_pc_index"  ~ "Hours worked per \n capita index",
      var == "imports_gdp"     ~ "Imports-to-GDP ratio" ,
      var == "imported_conso"  ~ "Share of final consumption \n in total imports",
      var == "oil_imports_gdp" ~ "Oil imports to GDP ratio",
      var == "petrol_conso"    ~ "Petrol consumption \n to GDP ratio ",
      var == "intpay_gdp"      ~ "Government interest \n payments to GDP ratio",
      var == "inves_gdp"       ~ "Investment-to-GDP ratio",
      var == "leverage"        ~ "Leverage of non \n financial corporations",
      var == "share"           ~ "Share of PPP GDP \n in Euro area PPP GDP",
      var == "otherexp_gdp"    ~ "Other government \n expenditures to GDP ratio",
      var == "pubcons_gdp"     ~ "Government consumption \n to GDP ratio",
      var == "pubinves_gdp"    ~ "Government investment \n to GDP ratio",
      var == "shortrate"       ~ "Short-term \n interest rate (APR)",
      var == "taun"            ~ "Implicit Tax Rate \n on labor income " ,
      var == "tauwh"           ~ "Implicit Tax Rate \n on employees' SSC",
      var == "tauwf"           ~ "Implicit Tax Rate \n on employers' SSC",
      var == "tauc"            ~ "Implicit Tax Rate \n on consumption",
      var == "tauk"            ~ "Implicit Tax Rate \n on corporate income",
      var == "tfs_gdp"         ~ "Government social \n transfers to GDP ratio",
      var == "totexp_gdp"      ~ "Total government \n expenditure to GDP ratio",
      var == "totrev_gdp"      ~ "Total government \n revenue to GDP ratio"
    ),
    country = case_when(
      country == "FR" ~ "France",
      country == "DE" ~ "Germany",
      country == "IT" ~ "Italy",
      country == "ES" ~ "Spain",
      country == "EA" ~ "Euro Area"
    )
  )

# R package `tikz` is not available for this version of R
# tikz("calibrated.tex", width = 5.2, height = 6.4, sanitize = TRUE)

ggplot(rawdata_growth_ratio, aes(period, value, color = country)) +
  geom_line(lwd = 1.2)+
  facet_wrap(~varname, ncol = 3, scales = "free_y") +
  my_theme() +
  labs(title = "Series for the calibration", caption = last_update)

ggsave("49_final.png", path = fig_path, height = 12, width = 12)
graphics.off()

calibration <- rawdata_growth_ratio |> 
  mutate(Parameter = varname) |> 
  group_by(country, Parameter) |> 
  summarise(mean = round(mean(value, na.rm = TRUE), digits = 3)) |> 
  ungroup() |> 
  spread(country, mean)

kable(x = calibration, format = "html", caption = "Calibration") |> 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), 
    position = "center",
    full_width = FALSE
    )

# Create a .tex file called "calibration.tex"
print(
  xtable::xtable(
    x = calibration, 
    caption = c("Calibration"), 
    type = "latex", 
    digits = c(0, 0, 3, 3, 3, 3, 3)), 
  include.rownames = FALSE, 
  file = "data/calibration.tex"
  )

# END