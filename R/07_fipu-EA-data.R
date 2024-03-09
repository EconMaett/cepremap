# 07 - Fiscal database for the Euro Area ----
# URL: https://macro.cepremap.fr/article/2019-11/fipu-EA-data/
library(tidyverse)
library(zoo)
library(kableExtra)
library(rdbnomics)
library(seasonal)
library(gridExtra)
library(RColorBrewer)
source("R/utils.R")
palette(brewer.pal(n = 3, name = "Set1"))
fig_path <- "figures/07_EA_Fipu_data/"
# Build the database following Paredes et al. (2014).
# The following series are included:
#    1. Direct taxes
#    2. Indirect taxes
#    3. Social security contributions by employees
#    4. Social security contributions by employers
#    5. Government consumption
#    6. Government investments
#    7. Government transfers
#    8. Government subsidies
#    9. Government compensation of employees
#   10. Unemployment benefits
#   11. Government debt
#   12. Interest payments
#   13. Total revenues
#   14. Total expenditures.

## Historical data ----
# Replicate the series proposed in Paredes et al. (2014).
# Social contribution by contributors starts in 1991.
# https://macro.cepremap.fr/article/2019-11/fipu-EA-data/#ppp14
ppp <- readxl::read_excel(path = "data/PPP_raw.xls", sheet = 1, skip = 1)

ppp <- ppp |> 
  mutate(
    period = as.Date(as.yearqtr(`MILL. EURO, RAW DATA, NON-SEAS. ADJUSTED, SMOOTHED ESTIMATES`, "%YQ%q")),
    totexp   = TOE,             # Total expenditures
    pubcons  = GCN,             # General government consumption expenditures
    pubinves = GIN,             # General government investment
    tfs      = THN,             # Social payments
    unemp    = `of which UNB`,  # Unemployment benefits (among social payments)
    salaries = COE,             # Compensation of employees
    subs     = SIN,             # Subsidies
    intpay   = INP,             # General government interest payments
    totrev   = TOR,             # Total revenue
    indirtax = TIN,             # Total indirect taxes
    dirtax   = DTX,             # Total direct taxes
    scr      = as.numeric(SCR), # Social contribution by employers
    sce      = as.numeric(SCE), # Social contribution by employees and self-employed
    sct      = SCT,             # Total social security contributions
    debt     = MAL,             # Euro area general government debt
    .keep = "none"
  ) |> 
  filter(!is.na(period))

# Assume the ratio of social contributions between employers and households
# remains stable before 1991 to reconstruct the series before that time.
# Calculate the ratio of social contributions for the first data point
prcnt <- ppp |> 
  mutate(scr_sct = scr / sct, .keep = "none") |> 
  na.omit() |> 
  first() |> 
  as.numeric()

# Use this ratio to reconstruct the series before 1991
scr_sce_before91 <- ppp |> 
  filter(is.na(scr)) |> 
  select(period, sct, scr, sce) |> 
  mutate(period, scr = prcnt * sct, sce = sct - scr, .keep = "none") |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value")

# Add this series to the existing ppp data base
ppp <- ppp |> 
  select(-sct) |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  bind_rows(scr_sce_before91) |> 
  arrange(var, period) |> 
  filter(!is.na(value)) # Remove the NAs in scr, sce before 1991

max_date <- ppp |> 
  group_by(var) |> 
  summarise(max_date = max(period)) |> 
  arrange(max_date)

kable(max_date)

## Recent data ----
# The historical data base ends in 2013. For the latest data, we add
# data from EUROSTAT through DBnomics.

### Special case: Social contributions ----
#### Download annual data ----
var_taken <- c(
  "D613", # Annual Households' actual social contributions (D613) for general govt only (S13)
  "D612", # Annual Employers' imputed social contributions
  "D611"  # Annual Employers' actual social contributions (D611) for general govt only (S13)
)

url_variables <- paste0(var_taken, collapse = "+")
url_filter    <- paste0("A.MIO_EUR.S13.", url_variables, ".EA19")
df            <- rdb("Eurostat", "gov_10a_taxag", mask = url_filter)

data_1 <- df |> 
  as_tibble() |> 
  select(period, var = na_item, value) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  mutate(sce = D613 + D612, scr = D611) |> 
  select(-c("D611", "D612", "D613")) |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  mutate(year = year(period))

# The series of actual social contributions presents two problems:
#   (i)  they are not at quarterly frequency
#   (ii) they are only available up to 2019
# We use the two series of quarterly net total social contributions
# and quarterly employers contributions for the total economy.

#### From annual to quarterly data ----
# Quarterly net social contributions, receivable (D61REC) for general govt only (S13)
df <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.D61REC.EA19")

qsct <- df |> 
  mutate(period, var = "sct", value, .keep = "none") |> 
  mutate(year = year(period))

max_time <- qsct |> 
  summarise(max_date = max(period))

# Use the series of quarterly total net social contributions to find
# the share of each contributor for each year.

# Use this share and the quarterly value of the total net social contributions
# to deduce the quarterly value of the net social contributions of 
# each contributor.

# Calculate total amount of sct by year
qsct_a <- qsct |> 
  group_by(year) |> 
  summarise(value_a = sum(value))

qsct <- qsct |> 
  left_join(qsct_a, join_by(year))

# Convert the data from annual to quarterly
qsce_uncomplete <- data_1 |> 
  filter(var == "sce") |> 
  full_join(qsct, join_by(year)) |> 
  mutate(
    period = period.y,
    var    = var.x,
    value  = value.y * value.x / value_a,
    .keep = "none"
  ) |> 
  filter(!is.na(value))

# Convert data from annual to quarterly
qscr_uncomplete <- data_1 |> 
  filter(var == "scr") |> 
  full_join(qsct, join_by(year)) |> 
  mutate(
    period = period.y,
    var    = var.x,
    value  = value.y * value.x / value_a,
    .keep = "none"
  ) |> 
  filter(!is.na(value))

# Plot the series to compare the quarterly and annual series
plot_treatment <- bind_rows(qscr_uncomplete, qsce_uncomplete) |> 
  mutate(
    Origin = "Deduced quarterly series",
    value = 4 * value # To compare quarterly and annual levels
  ) |> 
  bind_rows(mutate(data_1, Origin = "Original annual series")) |> 
  mutate(
    var = if_else(
      condition = var == "sce", 
      true      = "Social contribution of households", 
      false     = "Social contribution of employers" 
        )
  ) |> 
  select(-year)

ggplot(plot_treatment, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ var, ncol = 2, scales = "free_y") +
  my_theme()

ggsave("01_SCR-SCT.png", path = fig_path, height = 4, width = 6)
graphics.off()

#### Most recent data ----
# Use the series of total employers contribution for the total economy
# along with the share of each contributor in total contributions to
# deduce the latest contributions of households and employers

# Quarterly employers SSC for total economy
df <- rdb("Eurostat", "namq_10_gdp", mask = "Q.CP_MEUR.NSA.D12.EA19")

qscr_toteco <- df |> 
  as_tibble() |> 
  mutate(period, var = "scr", value, .keep = "none") |> 
  mutate(year = year(period))

# Use the recent data on employers total contributions to chain forward
# the social contributions of employers
qscr <- chain(
  to_rebase  = qscr_toteco,
  basis      = qscr_uncomplete,
  date_chain = max(qscr_uncomplete$period), 
  is_basis_the_recent_data = FALSE
  ) |> 
  arrange(period)

# Assume the ratio of social contributions by the contributors remains
# constant over time to deduce the social contributions of households
qsce <- bind_rows(qsce_uncomplete, select(qsct, period, value, var), qscr) |> 
  filter(period <= max_time$max_date) |> 
  pivot_wider(names_from = var, values_from = value, values_fill = 0) |> 
  mutate(
    period = period,
    sce    = if_else(
      condition = period <= max(qsce_uncomplete$period), 
      true      = sce, 
      false     = sct - scr
        ),
    .keep = "none"
  ) |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  arrange(period)

# The series of employers contribution are different in levels.
# We are only interested in  contributions of employers for the
# general government, but not for the total economy.

# Since the patterns of the two series are very similar, we
# chain them by taking the variations from social contributions of
# employers for the total economy and apply them to the level of
# actual social contributions for the general government.
plot_treatment <- bind_rows(qscr_uncomplete, qsce_uncomplete) |> 
  mutate(Origin = "Quarterly series") |> 
  bind_rows(
    mutate(qscr_toteco, Origin = "Original quarterly series (for total economy)"),
    mutate(bind_rows(qsce, qscr), Origin = "Chained series")
    ) |> 
  mutate(
    var = if_else(
      condition = var == "sce",
      true      = "Contribution of households",
      false     = "Contribution of employers"
    )
  ) |> 
  select(-year)

ggplot(plot_treatment, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ var, ncol = 2, scales = "free_y") +
  my_theme() +
  ggtitle("Social contribution forward chaining")

ggsave("02_SC-chain.png", path = fig_path, height = 4, width = 6)
graphics.off()

### Special case: Unemployment benefits ----
# Retrieve government social expenditures and compute their quarterly share for every year.
socialexp <- rdb("Eurostat", "gov_10q_ggnfa", mask = "Q.MIO_EUR.NSA.S13.D62PAY.EA19") |> 
  as_tibble() |> 
  mutate(year = year(period)) |> 
  select(period, value, year) |> 
  group_by(year) |> 
  mutate(sum = sum(value), ratio = value / sum) |> 
  ungroup() |> 
  select(-c("value", "year", "sum"))

# Retrieve the latest annual data on unemployment benefits,
# put them in a quarterly table and use the previous ratio
# of quarterly social expenditures to compute the quarterly 
# unemployment benefits
df <- rdb("Eurostat", "gov_10a_exp", mask = "A.MIO_EUR.S13.GF1005.TE.EA19")

recent_unemp <- df |> 
  as_tibble() |> 
  mutate(year = year(period)) |> 
  select(period, value, year)

recent_unemp_q <- tibble(
  period = seq(from = min(recent_unemp$period), length.out = nrow(recent_unemp) * 4, by = "quarter"),
  year   = year(period)
  ) |> 
  left_join(recent_unemp, join_by(year)) |> 
  select(-c("period.y", "year")) |> 
  rename(period = period.x)

unemp_q <- recent_unemp_q |> 
  inner_join(y = socialexp, join_by(period)) |> 
  mutate(
    value = value * ratio,
    var   = "unemp"
  ) |> 
  select(-ratio)

# We compare the historical and the new quarterly series.
data_plot <- ppp |> 
  filter(var == "unemp") |> 
  mutate(var = "From PPP") |> 
  bind_rows(mutate(unemp_q, var = "New measure")) |> 
  filter(year(period) >= 2007)

ggplot(data_plot, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Unemployment benefits series comparison")

ggsave("03_unemp-comp.png", path = fig_path, height = 4, width = 6)
graphics.off()

## Chaining recent data with historical ----
# Fetch the remaining series from DBnomics, none of which have to be trated before use.
# Then add the social contributions by contributors.

# List of variables that can be taken on the first dataset
var_taken <- c(
  "P3",     # Public consumption
  "P51G",   # Public gross fixed capital formation
  "D62PAY", # Social transfers
  "D1PAY",  # Compensation of employees
  "D3PAY",  # Subsidies
  "D2REC",  # Indirect taxes on production
  "D5REC",  # Direct taxation on income and wealth
  "D41PAY", # Interest payments
  "TE",     # Total expenditures
  "TR"      # Total revenue
)

# Build the URL, fetch the data and convert it to a tibble
url_variables <- paste0(var_taken, collapse = "+")
url_filter    <- paste0("Q.MIO_EUR.NSA.S13.", url_variables, ".EA19")
data_1        <- rdb("Eurostat", "gov_10q_ggnfa", mask = url_filter)

# Government consolidated gross debt is in a different dataset
data_2 <- rdb("Eurostat", "gov_10q_ggdebt", mask = "Q.GD.S13.MIO_EUR.EA19")

# Bind the two data frames
recent_data <- bind_rows(data_1, data_2) |> 
  mutate(value, period, var = as.factor(na_item), .keep = "none")

# Harmonize the DBnomic series variable names with PPP
var_names <- c("pubcons", "pubinves", "tfs", "salaries", "subs", "indirtax", "dirtax", "intpay", "totexp", "totrev", "debt")

recent_data$var <- plyr::mapvalues(recent_data$var, from = c(var_taken, "GD"), to = var_names)

# Include the series of social contributions
var_names <- c(var_names, "sce", "scr", "unemp")

recent_data <- recent_data |> 
  bind_rows(qsce, qscr, unemp_q)

# Check the last available date for each series.
max_date <- recent_data |> 
  group_by(var) |> 
  summarize(max_date = max(period)) |> 
  arrange(max_date)

kable(max_date)

# All that is left to do is to chain the data frame of recent series
# with the historical data base of Paredes et al. (2014).

# Once the data is chained we use the `seasonal` R package to remove
# the seasonal component of each series.

# Hereafter we present the treatment on each variable and check graphically
# that what we obtain is consistent with what we expect.
chained_data <- recent_data %>%
  chain(
    basis      = ., # Necessitates use of tidyverse pipe !
    to_rebase  = filter(ppp, var %in% var_names), 
    date_chain = "2007-01-01"
    ) %>%
  arrange(var, period) %>%
  select(period, var, value) %>%
  mutate(Origin = "Chained")

to_deseason <- chained_data |>
  select(-Origin) |> 
  pivot_wider(
    names_from = var, values_from = value,
    values_fn = mean
    )

deseasoned <- bind_rows(
  lapply(
    X = unique(chained_data$var), 
    FUN = function(var) deseason(source_df = to_deseason, var_arrange = var) |> 
      mutate(Origin = "Final series")
      )
  )

ppp <- ppp |> 
  mutate(Origin = "Historical data")

to_plot <- bind_rows(chained_data, deseasoned, ppp)

### Total revenue and expenditures ----
plot_totrev <- to_plot |> 
  filter(var == "totrev", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "totrev", Origin != "Final series"), ind2 = "1 - Chain series")
  )

plot_totexp <- to_plot |> 
  filter(var == "totexp", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "totexp", Origin != "Final series"), ind2 = "1 - Chain series")
  )

p1 <- ggplot(plot_totrev, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Total revenue")

p2 <- ggplot(plot_totexp, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Total expenditures")

grid.arrange(arrangeGrob(p1, p2, ncol = 2))
ggsave("04_revenue-expenditures.png", path = fig_path, height = 4, width = 6)
graphics.off()

### Public direct spending ----
# The chained series of public consumption strongly resembles the historical series.
# The chained series of investment has a visually significant difference
# in the levels compared to the historical one.
# We use the `chain()` method to build a reasonabl eproxy for the
# series of public investment, albeit at a certain loss.
plot_cons <- to_plot |> 
  filter(var == "pubcons", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "pubcons", Origin != "Final series"), ind2 = "1 - Chain series")
  )

plot_inves <- to_plot |> 
  filter(var == "pubinves", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "pubinves", Origin != "Final series"), ind2 = "1 - Chain series")
  )

p1 <- ggplot(plot_cons, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Public consumption")

p2 <- ggplot(plot_inves, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Public investment")

grid.arrange(arrangeGrob(p1, p2, ncol = 2))
ggsave("05_consumption-investment.png", path = fig_path, height = 4, width = 6)
graphics.off()

### Specific spending ----
# The chained series seems consistent with the historical series.
# Our manipulation does not entail much loss.
plot_salaries <- to_plot |> 
  filter(var == "salaries", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "salaries", Origin != "Final series"), ind2 = "1 - Chain series")
  )

plot_subs <- to_plot |> 
  filter(var == "subs", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "subs", Origin != "Final series"), ind2 = "1 - Chain series")
  )

p1 <- ggplot(plot_salaries, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Compensation of employees")

p2 <- ggplot(plot_subs, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Subsidies")

grid.arrange(arrangeGrob(p1, p2, ncol = 2))
ggsave("06_compensation-subsidies.png", path = fig_path, height = 4, width = 6)
graphics.off()

### Taxes ----
# The chained series seems consistent with the historical series.
plot_indir <- to_plot |> 
  filter(var == "indirtax", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "indirtax", Origin != "Final series"), ind2 = "1 - Chain series")
  )

plot_dir <- to_plot |> 
  filter(var == "dirtax", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "dirtax", Origin != "Fianl series"), ind2 = "1 - Chain series")
  )

p1 <- ggplot(plot_indir, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Indirect taxation")

p2 <- ggplot(plot_dir, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Direct taxation")

grid.arrange(arrangeGrob(p1, p2, ncol = 2))
ggsave("07_indirect-direct-taxation.png", path = fig_path, height = 4, width = 6)
graphics.off()

### Debt and interest payments ----
# The chained series of general government debt deviates slightly
# from the historical one but the deviation is thin.
plot_debt <- to_plot |> 
  filter(var == "debt", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "debt", Origin != "Final series"), ind2 = "1 - Chain series")
  )

plot_intpay <- to_plot |> 
  filter(var == "intpay", Origin != "Historical data") |> 
  mutate(ind2 = "2 - Remove seasonal component") |> 
  bind_rows(
    tibble(filter(to_plot, var == "intpay", Origin != "Final series"), ind2 = "1 - Chain series")
  )

p1 <- ggplot(plot_intpay, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Interest payments")

p2 <- ggplot(plot_debt, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("General government debt")

grid.arrange(arrangeGrob(p1, p2, ncol = 2))
ggsave("08_interest-debt.png", path = fig_path, height = 4, width = 6)
graphics.off()

### Total social transfers and unemployment benefits ----
plot_unemp <- to_plot |>
  filter(var == "unemp", Origin != "Historical data") |>
  mutate(ind2 = "2 - Remove seasonal component") |>
  bind_rows(tibble(filter(to_plot, var == "unemp", Origin != "Final series"), ind2 = "1 - Chain series"))

plot_transf <- to_plot |>
  filter(var == "tfs", Origin != "Historical data") |>
  mutate(ind2 = "2 - Remove seasonal component") |>
  bind_rows(tibble(filter(to_plot, var == "tfs", Origin != "Final series"), ind2 = "1 - Chain series"))

p1 <- ggplot(plot_transf, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Social transfers")

p2 <- ggplot(plot_unemp, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ ind2, scales = "free_y", ncol = 1) +
  my_theme() +
  ggtitle("Unemployment benefits")

grid.arrange(arrangeGrob(p1, p2, ncol = 2))
ggsave("09_transfers-benefits.png", path = fig_path, height = 4, width = 6)
graphics.off()

## Building the final database ----
### Comparing the obtained series with PPP ----
# Check that the final database resembles the seasonally adjusted one
# of Paredes et al. (2014).
pppSA <- readxl::read_excel(path = "data/PPP_seasonal.xls", sheet = 1, skip = 1)

pppSA <- pppSA |> 
  mutate(
    period = as.Date(as.yearqtr(`MILL. EURO, RAW DATA, SEASONALLY ADJUSTED, SMOOTHED ESTIMATES`, format = "%YQ%q")),
    totexp   = TOE,             # Total expenditures
    pubcons  = GCN,             # General government consumption expenditure
    pubinves = GIN,             # General government investment
    tfs      = THN,             # Social payments
    salaries = COE,             # Compensation of employees
    subs     = SIN,             # Subsidies
    unemp    = `of whichUNB`,   # Unemployment benefits (among social payments)
    intpay   = INP,             # General government interest payments
    totrev   = TOR,             # Total revenue
    indirtax = TIN,             # Total indirect taxes
    dirtax   = DTX,             # Total direct taxes
    scr      = as.numeric(SCR), # Social contribution by employers
    sce      = as.numeric(SCE), # Social contribution by employees and self-employed
    debt     = MAL,             # Euro area general government debt
    .keep = "none"
    ) |> 
  filter(!is.na(period))

plot_compare <- pppSA |> 
  pivot_longer(cols = -period, names_to = "var", values_to = "value") |> 
  na.omit() |> 
  mutate(Origin = "ppp") |> 
  bind_rows(deseasoned) |> 
  mutate(var = as.factor(var))

xlab_plot <- c(
  "Euro Area government debt",
  "Direct taxes",
  "Indirect taxes",
  "Interest payments",
  "Public consumption",
  "Public investment",
  "Compensation of employees",
  "Households' social contribution",
  "Employers' social contribution",
  "Subsidies",
  "Social transfers",
  "Total expenditures",
  "Total revenues",
  "Unemployment benefits"
)

plot_compare$var <- plyr::mapvalues(plot_compare$var, from = levels(plot_compare$var), to = xlab_plot)

ggplot(plot_compare, aes(period, value, color = Origin)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ var, ncol = 3, scales = "free_y") +
  my_theme() +
  ggtitle("Fiscal database for the Euro Area")

ggsave("10_final.png", path = fig_path, height = 4, width = 6)
graphics.off()

### Final fiscal database for the Euro area ----
# Find the raw series here: http://shiny.cepremap.fr/data/EA_Fipu_rawdata.csv
EA_Fipu_rawdata <- deseasoned |> 
  select(-Origin) |> 
  pivot_wider(names_from = var, values_from = value)

EA_Fipu_rawdata |> 
  write.csv("data/EA_Fipu_rawdata.csv", row.names = FALSE)

# Then we normalize the data by population and price 
# to reproduce the Smets and Wouters (2003) data base
# https://macro.cepremap.fr/article/2019-11/fipu-EA-data/#Smet03
sw03 <- read.csv(file = "data/EA_SW_rawdata.csv") |> 
  mutate(period = ymd(period)) |> 
  filter(period >= "1980-01-01")

EA_Fipu_data <- EA_Fipu_rawdata |> 
  inner_join(sw03, join_by(period)) |> 
  mutate(
    period = period,
    pubcons_rpc  = 100 * 1e+6 * pubcons / (defgdp * pop * 1000),
    pubinves_rpc = 100 * 1e+6 * pubinves / (defgdp * pop * 1000),
    salaries_rpc = 100 * 1e+6 * salaries / (defgdp * pop * 1000),
    subs_rpc     = 100 * 1e+6 * subs / (defgdp * pop * 1000),
    dirtax_rpc   = 100 * 1e+6 * dirtax / (defgdp * pop * 1000),
    indirtax_rpc = 100 * 1e+6 * indirtax / (defgdp * pop * 1000),
    tfs_rpc      = 100 * 1e+6 * tfs / (defgdp * pop * 1000),
    sce_rpc      = 100 * 1e+6 * sce / (defgdp * pop *1000),
    scr_rpc      = 100 * 1e+6 * scr / (defgdp * pop * 1000),
    debt_rpc     = 100 * 1e+6 * debt / (defgdp * pop * 1000),
    .keep = "none"
  )

EA_Fipu_data |> 
  write.csv("data/EA_Fipu_data.csv", row.names = FALSE)

# Find the normalized data here: http://shiny.cepremap.fr/data/EA_Fipu_data.csv
# END