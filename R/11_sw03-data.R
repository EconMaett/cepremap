# 10 - Smets & Wouters (2003) database -----
# URL: https://macro.cepremap.fr/article/2015-10/sw03-data/
library(tidyverse)
library(zoo)
library(kableExtra)
library(rdbnomics)
library(mFilter)
library(RColorBrewer)
source("R/utils.R")
palette(brewer.pal(n = 9, name = "Dark2"))
fig_path <- "figures/11_sw03-data/"
# Smets & Wouters (2003) use eight time series:
#    1. GDP
#    2. GDP deflator
#    3. Consumption
#    4. Investment
#    5. Employment
#    6. Wage
#    7. Working-age population
#    8. Interest rate

# Add three more:
#    9. Hours worked
#   10. Consumption deflator
#   11. Investment deflator

# Merge data from:
#   - The Area-Wide Model (AWM) proposed by Fagan et al. (2001).
#   - The Conference Board
#   - The European Central Bank (ECB)
#   - EUROSTAT

# The first three sources range from 1970 Q1 to the end of the 1990s.
# Updates will be fed with EUROSTAT data for the eleven series from DBnomics.

# All series seasonally and working days adjusted, except for the interest rate and 
# the population. We also smooth the population series.

# We use the updated version of the AWM database composed of 19 Euro area countries.
# We keep this convention in the definition of the Euro area.

## Historical data (1970 - end of the 1990s) ----
# Three sources are used to construct the database until the 1990s: 
#   1. The Area-Wide Model (AWM) database
#   2. The Conference Board
#   3. The European Central Bank (ECB)

### AWM database ----
# The Area-Wide Model (AWM) database was originally proposed by Fagan et al. (2001).

# We use an updated version of the database available on the Euro Area 
# Business Cycle Network (EABCN) website: https://eabcn.org/page/area-wide-model

# The website stopped being updated since 2019 and the AWM model was decommissioned in 2010.

# The historical database with data available until 2017 Q4 can be downloaded 
# from: https://eabcn.org/sites/default/files/awm19up18.csv

# We find nine out of the eleven series mentioned before on the EABCN website.
# The exceptions are the hours worked and the population series.
link_to_awm <- "https://eabcn.org/sites/default/files/awm19up18.csv"

if (! "awm19up18.csv" %in% list.files(path = "data/")) {
  download.file(url = link_to_awm, destfile = "data/awm19up18.csv", method = "auto")
}

awm <- read.csv(file = "data/awm19up18.csv", sep = ",")
# The variables of the database are described in the ECB working paper No. 52: 
# 'An Area-wide Model (AWM) for the euro area' by Gabrial Fagan, Jérôme Henry and Ricardo Mestre (January 2001)
# https://www.ecb.europa.eu/pub/pdf/scpwps/ecbwp042.pdf?a1cb4280848b9c3557120a146468f3ab
awm <- awm |> 
  mutate(
    gdp       = YER, # GDP (Real)
    defgdp    = YED, # GDP Deflator
    conso     = PCR, # Private Consumption (Real)
    defconso  = PCD, # Consumption Deflator
    inves     = ITR, # Gross Investment (Real)
    definves  = ITD, # Gross Investment Deflator
    wage      = WIN, # Compensation to Employees (Nominal)
    shortrate = STN, # Short-Term Interest Rate (Nominal)
    employ    = LNN, # Total Employment (Persons)
    period    = as.Date(zoo::as.yearqtr(X)),
    .keep = "none"
  ) |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value")
# The database ends in 2017 Q4 ("2017-10-01").

### First special case: Hours worked ----
# In 2003 no time series for hours worked existed, so the authors used a formula linking 
# employment to the hours worked in their model. 
# Today, Eurostat provides a quarterly series starting in 2000 Q1.

# We build a historical hours worked time series with data from The Conference Board 
# until 1999 Q4 and then use the data from Eurostat.

# Find The Conference Board's Total Economy Database (TED) at:
# https://www.conference-board.org/data/economydatabase/index.cfm
ted <- "TED---Output-Labor-and-Labor-Productivity-1950-2015.xlsx"
link_to_confboard <- paste0("https://www.conference-board.org/retrievefile.cfm?filename=", ted, "&type=subsite")

if (! ted %in% list.files(path = "data/")) {
  download.file(url = link_to_confboard, destfile = "data/ted", mode = "wb")
}

EA19_names <- c(
  "Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France",
  "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania",
  "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovak Republic",
  "Slovenia", "Spain"
)

# Read in the excel file and clean it
hours_confboard <- readxl::read_excel(
  path  = paste0("data/", ted), 
  sheet = "Total Hours Worked", 
  skip  = 2
  ) |> 
  rename(country = Country) |> 
  filter(country %in% EA19_names) |> 
  select(-1) |>
  pivot_longer(cols = -country, names_to = "period", values_to = "value") |> 
  mutate(period = as.Date(paste0(period, "-07-01"))) |> 
  filter(period >= "1970-07-01" & period <= "2012-07-01")

# Plot the data
ggplot(hours_confboard, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~ country, ncol = 4, scales = "free_y") +
  my_theme() +
  ggtitle("Hours worked")

ggsave("01_TED_EA19_hours.png", path = fig_path, height = 12, width = 24)
graphics.off()
# There are still two problems with such a series:
#   1. the series does not cover all the 19 member states of the Euro area for the whole period
#   2. the data are at annual frequency, not quarterly

#### Complete the hours worked series before 1990 ----
# Data for the 19 Euro area member states are only available since the 1990s.
hours_confboard |> 
  group_by(period) |> 
  summarise(number_countries = length(country)) |> 
  tail(n = -12) |> 
  kable()
# At the time of writing, the hours worked for five Central European countries, 
# Estonia, Latvia, Lithuania, Slovak Republic, Slovenia are missing between 1970 and 1990.

# Use growth rates of the sum of hours worked series for the 14 countries available before 1990 
# to complete the series of the sum of hours worked over the 19 countries after 1990.

# This is appropriate since in 1990 the 14 economies of the Euro area 
# represented more than 95% of the total hours worked.

# sum over the 14 countries
EA14_names <- c(filter(hours_confboard, period == "1970-07-01" & !is.na(value))$country)

hours_confboard_14 <- hours_confboard |>
  filter(country %in% EA14_names) |> 
  group_by(period) |> 
  summarise(value = sum(value), var = "hours")

# sum over the whole countries
hours_confboard_tot <- hours_confboard |>
  group_by(period) |> 
  summarise(value = sum(value), var = "hours")

# Use the custom `chain()` function
hours_confboard_chained <- chain(
  to_rebase  = hours_confboard_14, 
  basis      = hours_confboard_tot, 
  date_chain = "1990-07-01")

#### Convert the annual hours worked series to quarterly data before 2000 -----
# Once the annual data have been completed since 1970 have been completed,
# the issue of converting the annual data into quarterly data remains.

# Create a data frame with the period for the quarters, and NAs whenever
# there is no value available.
hours_confboard_chained_q <- tibble(
  period = seq(
    from = as.Date("1970-07-01"), 
    to   = as.Date("2012-07-01"), 
    by   = "quarter"),
  value = NA
  ) |> 
  left_join(hours_confboard_chained, join_by(period)) |> 
  select(-value.x) |> 
  rename(value = value.y)

# Several methods of interpolation are tested:
#   - constant quarterly growth rate over one year
#   - cubic spline
#   - Kalman filter
hours <- hours_confboard_chained_q

# Constant quarterly growth rates over one year with `zoo::na.approx()`.
# The missing values (NAs) are replaced by linear interpolation via `stats::approx()`
hours_approx <- hours |>
  mutate(value = zoo::na.approx(value), var = "hours_approx")

# Cubic spline interpolation
hours_spline <- hours |> 
  mutate(value = zoo::na.spline(value), var = "hours_spline")

# Kalman filter and smoother to interpolate the missing values
# Convert the data frame to a time series `ts` object first
hoursts <- ts(data = hours$value, start = c(1970, 4), frequency = 4)
# Note that we have passed the data to Q4, not Q3...

# Use the function `stats::tsSmooth()` for fixed-interval smoothing.
# The function demands an object of class `stats::StrucTS()`
struct_ts <- StructTS(x = hoursts, type = "trend")
smooth_ts <- tsSmooth(object = struct_ts)

# "level" only
smoothed_hoursts <- tsSmooth(object = StructTS(hoursts, type = "trend"))[, 1]

# help("StructTS") # Fit structural model for a time series by maximum likelihood.

# Structural time series models are also called linear Gaussian state-space models for univariate
# time series based on a decomposition into different components.

# The simplest model is the *local level* model, specified by type = "level".

# The *local linear trend model*, type = "trend" has the same measurement equation, 
# but with a time-varying slope

# help("tsSmooth") # Performs fixed-interval smoothing on a univariate time series via a state-space model.
# It gives the best estimate of the state at each time period point based on the whole observed series.

# Note that the `stats` R package also comes with the functions
#   - `KalmanLike()`
#   - `KalmanRun()`
#   - `KalmanSmooth()`
#   - `KalmanForecast()` 
#   - `makeARIMA()`

# Put the smoothed series into a data frame
hours_StructTS <- hours |> 
  mutate(value = smoothed_hoursts, var = "hours_kalman")

# Combine the three smoothing methods
hours_filtered <- bind_rows(hours_approx, hours_spline, hours_StructTS)

# Calculate quarterly growth rates with the log-lag approximation for that.
hours_filtered_levgr <- hours_filtered |> 
  mutate(value = log(value) - log(lag(value))) |> 
  tibble(ind2 = "2- Growth rates") |> 
  bind_rows(tibble(hours_filtered, ind2 = "1- Levels")) |> 
  filter(period >= "1971-01-01")

# Plot the interpolated levels and QoQ (sequential) growth rates
ggplot(hours_filtered_levgr, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, ncol = 1, scales = "free_y") +
  my_theme() + 
  ggtitle("Hours worked")

ggsave("02_TED_EA19_hours-approx.png", path = fig_path, height = 12, width = 24)
graphics.off()

# Retain the Kalman filter method of interpolation to avoid the jump each first quarter in the growth rate
# implied by the linear method, and the high volatility implied by the spline method.
hours <- hours_StructTS |> 
  mutate(var = "hours")

#### Compare the different series of hours worked ----
# Graphically check that the interpolation of annual hours worked with the Kalman filter 
# is consistent with the raw data available in the most recent period.

# Convert The Conference Board TED annual hours worked series into 2000 basis index.
valref <- filter(hours_confboard_chained, period == "2000-07-01")$value

hours_confboard_ind <- hours_confboard_chained |> 
  mutate(
    period = period,
    var    = "Annual hours (original, The Conference Board)",
    value  = value / valref,
    .keep = "none")

# Retrieve the quarterly hours worked series from Eurostat
df <- rdb(ids = "Eurostat/namq_10_a10_e/Q.THS_HW.TOTAL.SCA.EMP_DC.EA19")

eurostat_data <- df |> 
  select(period, value) |> 
  mutate(var = "Quarterly hours (original, Eurostat)")

valref <- eurostat_data |> 
  filter(year(period) == 2000) |> 
  summarise(value = mean(value))

eurostat_data_ind <- eurostat_data |> 
  mutate(value = value / valref$value)

# Convert the quarterly hours worked series interpolated
# from annual values using the Kalman filter into 2000 basis index
valref <- hours |>
  filter(year(period) == 2000) |> 
  summarise(value = mean(value))

hours_ind <- hours |> 
  mutate(
    period = period,
    var    = "Quarterly hours (interpolated)",
    value  = value / valref$value,
    .keep = "none")

# Combine the three indexed series
check <- bind_rows(
  hours_confboard_ind,
  eurostat_data_ind,
  hours_ind)

# Plot the three time series
ggplot(check, aes(period, value, group = var, linetype = var, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Comparison of hours worked series")

ggsave("03_TED_EA19_hours-comparison.png", path = fig_path, height = 12, width = 24)
graphics.off()

### Second special case: Population ----
# The second special case is the population series.
# Quarterly population series only exist for the Euro Area after the year 2005.

# Take historical population with annual frequency from Eurostat by country until 2005 
# and then add the original Euro area quarterly population series from Eurostat.

# Build URLs to the DBnomcis API to retrieve annual population series for the 19 Euro Area countries.
EA19_code <- c("AT", "BE", "CY", "DE_TOT", "EE", "IE", "EL", "ES", "FX", 
               "IT", "LT", "LV", "LU", "NL", "PT", "SK", "FI", "MT", "SI")

url_country <- paste0("A.NR.Y15-64.T.", paste0(EA19_code, collapse = "+"))
df <- rdb("Eurostat", "demo_pjanbroad", mask = url_country)

pop_eurostat_bycountry <- df |>
  as_tibble() |> 
  select(geo, period, value) |> 
  rename(country = geo) |> 
  filter(period >= "1970-01-01", period <= "2013-01-01")

pop_eurostat_bycountry |> 
  mutate(value = value / 1e6) |> 
  ggplot(mapping = aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~ country, ncol = 4, scales = "free_y") +
  my_theme() +
  ggtitle("Working-age population (in millions)")

ggsave("04_Eurostat_population.png", path = fig_path, height = 12, width = 24)
graphics.off()
# There remain two problems with this time series:
#   1) the series does not cover all 19 Euro area member countries for the whole period
#   2) the data are annual, not quarterly

#### Complete the population series before 1982 ----
# Between 1970 and 1981 (included), EUROSTAT only provides the population
# for the then 16 Euro area member countries, excluding Malta, Slovenia, and Cyprus.
pop_eurostat_bycountry |> 
  group_by(period) |> 
  summarise(number_countries = length(country)) |> 
  kable()

# As those 16 countries represent more than 95% of the population
# of the EA19 over the last decades, we chain the series of the 
# sum of the population for 19 countries to the series of the
# sum for 16 countries between 1970 and 1982.

# Sum the annual population for the EA16
EA16_code <- filter(pop_eurostat_bycountry, period == "1970-01-01")$country

pop_a_16 <- pop_eurostat_bycountry |>
  filter(country %in% EA16_code) |> 
  group_by(period) |> 
  summarise(value = sum(value), var = "pop")

# Sum the annual population for all the available countries
pop_a_tot <- pop_eurostat_bycountry |> 
  group_by(period) |> 
  summarise(value = sum(value), var = "pop")

# Use the custom `chain()` function
pop_chained <- chain(
  to_rebase  = pop_a_16, 
  basis      = pop_a_tot, 
  date_chain = "1982-01-01")

#### Convert the annual population series to quarterly data before 2000 ----
# Once the annual data have been completed since 197',
# they need to be disaggregated into quarterly data.
pop_chained_q <- tibble(
  period = seq(
    from = as.Date("1970-01-01"),
    to   = as.Date("2013-01-01"),
    by   = "quarter"
    ),
  value = NA
  ) |> 
  left_join(pop_chained, by = "period") |> 
  select(-value.x) |> 
  rename(value = value.y)

# Again we test three methods to interpolate quarterly values
#   - constant quarterly growth rate over one year
#   - cubic spline
#   - Kalman filter
pop <- pop_chained_q

pop_approx <- pop |> 
  mutate(value = na.approx(value), var = "pop_approx")

pop_spline <- pop |> 
  mutate(value = na.spline(value), var = "pop_spline")

popts <- ts(data = pop$value, start = c(1970, 1), frequency = 4)
smoothed_popts <- tsSmooth(object = StructTS(popts, type = "trend", optim.control = list(maxit = 1e6)))[, 1]

pop_StructTS <- pop |> 
  mutate(value = smoothed_popts, var = "pop_kalman")

# Combine the data frames
pop_filtered <- bind_rows(pop_approx, pop_spline, pop_StructTS)

# Add QoQ growth rates with log-lag approximations
pop_filtered_levgr <- pop_filtered |> 
  mutate(value = log(value) - log(lag(value))) |> 
  tibble(ind2 = "2- Growth rates") |> 
  bind_rows(tibble(pop_filtered, ind2 = "1- Levels")) |> 
  filter(period >= "1970-04-01")

# Plot the three filtering methods
ggplot(pop_filtered_levgr, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, ncol = 1, scales = "free_y") +
  my_theme() +
  ggtitle("Population")

ggsave("05_Eurostat_population-approx.png", path = fig_path, height = 12, width = 24)
graphics.off()

# Retain the Kalman filter method of interpolation to avoid the jump each first quarter 
# in the growth rate implied by the constant linear growth method, 
# and the high volatility implied by the cubic spline method.
pop <- pop_StructTS |> 
  mutate(var = "pop")

#### Compare the different series for population ----
# Graphically check that the interpolation of the annual population with 
# the Kalman filter is consistent with he raw data available in the most recent period.

# Note that the interpolation is used only before 2005, but we present interpolated 
# data after this date in the plots only to check the consistency of the filtering method,
# but we will not use it afterwards.

# Convert annual population series to 2005 basis index
valref <- filter(pop_chained, period == "2005-01-01")$value

pop_a_ind <- pop_chained |> 
  mutate(
    period = period,
    var    = "Annual population (original, Eurostat)",
    value  = value / valref)

# URL for quarterly population series from Eurostat
df <- rdb(ids = "Eurostat/lfsq_pganws/Q.THS_PER.T.TOTAL.Y15-64.POP.EA20")
# Original: Eurostat/lfsq_pganws/Q.THS.T.TOTAL.Y15-64.POP.EA19

eurostat_data <- df |> 
  select(period, geo, value) |> 
  rename(var = geo) |> 
  mutate(var = "Quarterly population (original, Eurostat)") |> 
  filter(period >= "2005-01-01")

valref <- eurostat_data |> 
  filter(year(period) == 2005) |> 
  summarise(value = mean(value))

eurostat_data_ind <- eurostat_data |> 
  mutate(value = value / valref$value)

# Convert the interpolated population series in 2005 basis index
valref <- pop |> 
  filter(year(period) == 2005) |> 
  summarise(value = mean(value))

pop_ind <- pop |> 
  mutate(
    period = period,
    var    = "Quarterly population (interpolated)",
    value  = value / valref$value,
    .keep = "none")

# Combine the data
check <- bind_rows(pop_a_ind, eurostat_data_ind, pop_ind)

# Plot the data
ggplot(check, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Population")

ggsave("06_Eurostat_population-comparison.png", path = fig_path, height = 12, width = 24)
graphics.off()

### Recent data (since the end of the 1990s) ----
# Once the historical database is created, all the variables can be found on
# Eurostat without any transformation, through DBnomics.
old_data <- bind_rows(awm, hours, pop)

#### URL GDP / Consumption / Investment & Prices -----
variable_list <- c("B1GQ", "P31_S14_S15", "P51G")
measure_list  <- c("CLV10_MEUR", "PD10_EUR")

url_var  <- paste0(variable_list, collapse = "+")
url_meas <- paste0(measure_list, collapse = "+")

url_filter <- paste0("Q.", url_meas, ".SCA.", url_var, ".EA19")
df <- rdb("Eurostat", "namq_10_gdp", mask = url_filter)

d1 <- df |> 
  select(period, value, unit, na_item, series_name) |> 
  rename(var = na_item) |> 
  mutate(
    var = if_else(
      condition = var =="B1GQ" & unit == "CLV10_MEUR",
      true = "gdp",
      false = if_else(
        condition = var == "B1GQ",
        true = "defgdp",
        false = if_else(
          condition = var == "P31_S14_S15" & unit == "CLV10_MEUR",
          true = "conso",
          false = if_else(
            condition = var == "P31_S14_S15",
            true = "defconso",
            false = if_else(
              condition = var == "P51G" & unit == "CLV10_MEUR",
              true = "inves",
              false = "definves"
              )
            )
          )
        )
      )
    ) |> 
  mutate(period, var, value, series_name, .keep = "none")

##### Wages -----
df <- rdb(ids = "Eurostat/namq_10_a10/Q.CP_MEUR.SCA.TOTAL.D1.EA19")

d2 <- df |> 
  select(period, unit, value, series_name) |> 
  rename(var = unit) |> 
  mutate(var = "wage")

#### Hours & employment -----
url_meas   <- "THS_HW+THS_PER"
url_filter <- paste0("Q.", url_meas, ".TOTAL.SCA.EMP_DC.EA19")

df <- rdb("Eurostat", "namq_10_a10_e", mask = url_filter)

d3 <- df |> 
  select(period, unit, value, series_name) |> 
  rename(var = unit) |> 
  mutate(var = if_else(condition = var == "THS_HW", true = "hours", false = "employ")) |> 
  mutate(period, var, value, series_name, .keep = "none")

##### Quarterly 3-month rates -----
df <- rdb(ids = "Eurostat/irt_st_q/Q.IRT_M3.EA")

d4 <- df |> 
  select(period, geo, value, series_name) |> 
  rename(var = geo) |> 
  mutate(var = "shortrate")

#### Quarterly population series -----
df <- rdb(ids = "Eurostat/lfsq_pganws/Q.THS_PER.T.TOTAL.Y15-64.POP.EA20")

d5 <- df |> 
  select(period, geo, value, series_name) |> 
  rename(var = geo) |> 
  mutate(var = "pop") |> 
  filter(period >= "2005-01-01")

# Combine the data
recent_data <- bind_rows(d1, d2, d3, d4, d5)

var_names <- unique(recent_data$series_name)

recent_data <- recent_data |> 
  select(-series_name)

# Check the last available date for each variable
max_date <- recent_data |> 
  group_by(var) |> 
  summarize(max_date = max(period)) |> 
  arrange(max_date)

kable(max_date)

min_max_date <- min(max_date$max_date)
recent_data  <- recent_data |> 
  filter(period <= min_max_date)

### Final database ----
# Now we can create the final database and chain the 11 historical and recent series
# Keep recent data from Eurostat unchained and rebase the historical data.
# Check the first date available for each variable in the recent database.
min_date <- recent_data |> 
  group_by(var) |> 
  summarize(max_date = min(period)) |> 
  arrange(max_date)

kable(min_date)

# All the variable except the population (GDP, consumption, investment, their deflators,
# interest rates, hours, wage, and employment) are chained at 1999 Q1,
# the official date of the creation of the Euro area.
vars <- c("gdp", "conso", "inves", "defgdp", "defconso", "definves", "shortrate", "hours", "wage", "employ")

new_df <- recent_data |> 
  filter(var %in% vars)

old_df <- awm |> 
  filter(var %in% vars) |> 
  bind_rows(hours)

df1 <- chain(
  basis      = new_df,
  to_rebase  = old_df,
  date_chain = "1999-01-01")

### Population special case ----
# Note: Apparently data frames and tibbles can store time series `ts` objects now

#### Chain and smooth the population series ----
# Population is a special case because we need to chain recent data with the 
# historical series in 2005, the beginning of the quarterly population series.

# We also want to make the series as smooth as possible for normalization.

# First we chain the two series
recent_pop_q   <- filter(recent_data, var == "pop")
min_date_pop_q <- min(recent_pop_q$period)

pop <- pop_StructTS |> 
  mutate(var = "pop")

pop <- chain(
  basis = recent_pop_q,
  to_rebase = pop,
  date_chain = min_date_pop_q)

plot_pop <- pop |> 
  mutate(value = log(value) - log(lag(value))) |> 
  data.frame(ind2 = "Growth rates") |> 
  filter(period >= "1970-04-01")

ggplot(plot_pop, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, ncol = 1, scales = "free_y") +
  my_theme() +
  ggtitle("Quarterly Population")

ggsave("07_Eurostat_population-growthrates.png", path = fig_path, height = 12, width = 24)
graphics.off()

# The last years of the series exhibit high levels of volatility
# because they were not interpolated with the Kalman filter,
# and thus we apply the Hodrick-Prescott (HP) filter to the series.
popts <- ts(data = pop$value, start = c(1970, 1), frequency = 4)
smoothed_popts <- hpfilter(popts, freq = 1600)$trend

pop_StructTS <- pop |> 
  mutate(value = as.numeric(smoothed_popts), var = "Smoothed population")

plot_pop <- pop |> 
  mutate(var = "Original population")

pop_filtered <- bind_rows(plot_pop, pop_StructTS)

pop_filtered_levgr <- pop_filtered |> 
  mutate(value = log(value) - log(lag(value))) |> 
  tibble(ind2 = "2- Growth rates") |> 
  bind_rows(tibble(pop_filtered, ind2 = "1- Levels")) |> 
  filter(period >= "1970-04-01")

ggplot(pop_filtered_levgr, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, ncol = 1, scales = "free_y") +
  my_theme() +
  ggtitle("Population")

ggsave("08_Eurostat_population-smoothed.png", path = fig_path, height = 12, width = 24)
graphics.off()

# We retain the smoothed series obtained from the Hodrick-Prescott filter.
pop <- pop_StructTS |> 
  mutate(var = "pop")

# Now we can produce the final update of the Smets and Wouters (2003) database.
final_df <- bind_rows(df1, pop)
plot_df  <- final_df

list_var <- list(
  "Real GDP [1]"             = "gdp",
  "Real consumption [2]"     = "conso",
  "Real investment [3]"      = "inves",
  "GDP deflator [4]"         = "defgdp",
  "Consumption deflator [5]" = "defconso",
  "Investment deflator [6]"  = "definves",
  "Real wage [7]"            = "wage",
  "Hours worked [8]"         = "hours",
  "Employment [9]"           = "employ",
  "Interest rate [10]"       = "shortrate",
  "Population [11]"          = "pop"
)

plot_df$var <- factor(plot_df$var)
levels(plot_df$var) <- list_var

ggplot(plot_df, aes(period, value)) +
  geom_line(lwd = 1.2, color = blue_obs_macro) +
  facet_wrap(~ var, ncol = 3, scales = "free_y") +
  my_theme()
  
ggsave("09_final-database.png", path = fig_path, height = 12, width = 24)
graphics.off()

# You can download the 11 series directly here: shiny.cepremap.fr/data/EA_SW_rawdata.csv
EA_SW_rawdata <- final_df |> 
  pivot_wider(names_from = var, values_from = value)

write.csv(EA_SW_rawdata, file = "data/EA_SW_rawdata.csv", row.names = FALSE)

# Ready-to-use (normalized) data: shiny.cepremap.fr/data/EA_SW_data.csv
EA_SW_data <- final_df |> 
  mutate(period = gsub(" ", "", zoo::as.yearqtr(period))) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  mutate(
    period = period,
    gdp_rcp     = 1e+6 * gdp / (pop * 1000),
    conso_rpc   = 1e+6 * conso / (pop * 1000),
    inves_rpc   = 1e+6 * inves / (pop * 1000),
    defgdp      = defgdp,
    wage_rph    = 1e+6 * wage / defgdp / (hours * 1000),
    hours_pc    = hours * 10000 / (pop * 1000),
    pinves_defl = definves / defgdp,
    pconso_defl = defconso / defgdp,
    shortrate   = shortrate / 100,
    employ      = employ * 1000 / (pop * 1000),
    .keep = "none"
  )

write.csv(EA_SW_data, file = "data/EA_SW_data.csv", row.names = FALSE)
# END