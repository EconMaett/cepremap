# 08 - Implicit Tax Rates on Consumption & Labor in Europe ----
# URL: https://macro.cepremap.fr/article/2019-11/implicit_tax_rates/
# Implicit tax rates (ITR) on consumption, labor and corporate income
# for FR, IT, ES, DE & EA since 1995.
# Reference: European Commission's report on Taxation trends (2019).
library(tidyverse)
library(zoo)
library(rdbnomics)
library(kableExtra)
library(RColorBrewer)
source("R/utils.R")
palette(brewer.pal(n = 9, name = "Set1"))
fig_path <- "figures/08_ITR/"
year_max <- 2023
list_country <- list(
  "France"    = "FR",
  "Germany"   = "DE",
  "Italy"     = "IT",
  "Spain"     = "ES",
  "Euro Area" = "EA19"
  )

## Implicit tax rate on consumption ----
# Implicit tax rate (ITR) on consumption are all consumption taxes
# divided by the final consumption expenditure of households (domestic concept).

# The DG Taxation & Customs Union of the European Commission proposes
# a detailed methodology on its website.

# We follow this methodology using Eurostat's data for the EA19.
# Some specifities for France, Germany, Italy, and Spain are added.

# The ITR is calculated in four steps.
#   1. Gather all data on consumption taxes (numerator)
#   2. Add country-specific taxes
#   3. Retrieve final consumption (denominator)
#   4. Apply Euro Area GDP-weighted average

# Eurostat's "Statistics Explained" "Tax revenue statistics"
# URL: https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Tax_revenue_statistics

### Step 1: Taxes on consumption ----
# Consider the following aggregates (values in parenthesis represent the ESA 2010 nomenclature):
#   1. Value added type taxes (D211)
#   2. Taxes and duties on imports excluding VAT (D212)
#   3. Taxes on products except VAT and import duties (D214), excluding:
#       a. Stamp taxes (D214B)
#       b. Taxes on financial and capital transactions (D214C)
#       c. Export duties and monetary compensatory amounts on exports (D214K)
#   4. From other taxes on production:
#      a. Taxes on international transactions (D29D)
#      b. Taxes on pollution (D29F)
#      c. Under-compensation of VAT (flat rate system) (D29G)
#   5. From other current taxes:
#      a. Poll taxes (D59B)
#      b. Expenditure taxes (D59C)
#      c. Payments by households for licences (D59D)
# Eurostat: Main national accounts tax aggregates (gov_10a_taxag)
url_country <- paste(c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "EL", "IE", "IT",
                       "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES"), collapse = "+")

url_taxes <- paste(c("D211", "D212",
                     "D214", "D214B", "D214C", "D214K",
                     "D29D", "D29F", "D29G",
                     "D59B", "D59C", "D59D"), collapse = "+")

url_filter <- paste0("A.MIO_NAC.S13_S212.", url_taxes, ".", url_country)

ITR_cons_num <- rdb("Eurostat", "gov_10a_taxag", mask = url_filter) |> 
  filter(year(period) >= 1995 & year(period) < year_max) |> 
  select(country = geo, period, var = na_item, value)

# Check the beginning and the end of the data series we have just obtained:
ITR_cons_min_d <- ITR_cons_num |> 
  na.omit() |> 
  group_by(country, var) |> 
  summarize(mindate = min(year(period))) |> 
  spread(var, mindate)

kable(ITR_cons_min_d, format = "html", caption = "Taxes on consumption: beginning of the sample") |> 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed",  "responsive"), 
    position = "center", font_size = 12) |> 
  footnote(
    general = "The following taxes are not applicable for some countries (in a determined period of time): D214B, D214C, D214K, D29D, D29F, D29G, D59B, D59C, D59D. Thus we replace their NA values by 0 for the calculation.", 
    number = c(
      "D214B is not applicable for: EE, LV, SI. ", 
      "D214C is not applicable for: EE, LV, SI; it is applicable for FI since 1997. ", 
      "D214K is not applicable for: EE, FI, IT, LT, MT PT, SI,SK. ", 
      "D29D is not applicable for: EE, FI, IT, LT, LV, MT, PT, SI, SK. ", 
      "D29F is applicable since: 1996 for FI, 2007 for MT, and 2006 for PT.", 
      "D29G is not applicable for: EE, FI, IT, LT, LV, MT, PT; it is applicable for SI since 1999.", 
      "D59B is not applicable for: EE, FI, IT, LT, LV, MT, SI, SK; it is applicable for PT since 2015.", 
      "D59C is not applicable for: EE, FI, IT, LT, LV, PT, SI, SK.", 
      "D59D is not applicable for: EE; it is applicable for LV since 2007. "
      )
    )

ITR_cons_max_d <- ITR_cons_num |> 
  na.omit() |> 
  group_by(country, var) |> 
  summarize(max_date = max(year(period))) |> 
  spread(var, max_date)

kable(ITR_cons_max_d, format = "html", caption = "Taxes on consumption: end of the sample") |> 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed",  "responsive"), 
    position = "center", font_size = 12) |> 
  footnote(
    general = "The following taxes are not applicable for some countries (in a determined period of time): D214B, D214C, D214K, D29D, D29F, D29G, D59B, D59C, D59D. Thus we replace their NA values by 0 for the calculation.", 
    number = c(
      "D214B is not applicable for: EE, LV, SI;  it is applicable for FI until 2002, and for SK until 2004.", 
      "D214C is not applicable for: EE, LV, SI.", 
      "D214K is not applicable for: EE, FI, IT, LT, MT PT, SI,SK.  it is applicable for MT until 1999.", 
      "D29D is not applicable for: EE, FI, IT, LT, LV, MT, PT, SI, SK.", 
      "D29G is not applicable for: EE, FI, IT, LT, LV, MT, PT.", 
      "D59B is not applicable for: EE, FI, IT, LT, LV, MT, SI, SK.", 
      "D59C is not applicable for: EE, FI, IT, LT, LV, PT, SI, SK;  it is applicable for MT until 1995", 
      "D59D is not applicable for: EE."
      )
    )

### Step 2: Taxes on consumption - Specificities for France, Germany, Italy, and Spain ----
# Follow the national classification (NTLs - national tax lists):
#   France: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/data_on_taxation/fr-national-tax.xlsx
#   1. From council tax (D59A):
#      a. (C05):part raised on consumption.
#   2. From capital transfers from general government to relevant 
#      sectors representing taxes and social contributions assessed 
#      but unlikely to be collected (D995):
#      a. part raised on consumption.
#   Germany: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/data_on_taxation/de-national-tax.xlsx
#   1. From other current taxes n.e.c. (D59F):
#      a. (C03): tax on radio and TV.
#   Italy: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/data_on_taxation/it-national-tax.xlsx
#   1. From stamp taxes (D214B):
#      a. (C01): excise duty on tobacco.
#      b. (C02): excise duty on spirits.
#      c. (C03): receipts from sale of denaturing agents and government seals
#   2. From other current taxes on production n.e.c. (D29H):
#      a. (C02): other taxes on production.
#      b. (C05): fees to national & local economic bodies.
#   Spain: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/data_on_taxation/es-national-tax.xlsx
#   1. From taxes on products except VAT and import duties (D214) exclude also:
#      a. (D214L_C01): tax on building, equipment and works.
#   2. From other taxes on production assessed but unlikely to be collected (D995B):
#      a. part raised on consumption.
# National Tax Lists: https://ec.europa.eu/taxation_customs/business/economic-analysis-taxation/data-taxation_en, 
# Eurostat Tax Revenue Statistics: https://ec.europa.eu/eurostat/statistics-explained/index.php/Tax_revenue_statistics
conso_specificities <- readxl::read_xlsx(path = "data/s_conso.xlsx") |> 
  mutate(period = as.Date(period)) |> 
  select(country, period, var, value = total) |> 
  filter(year(period) >= 1995 & year(period) < year_max) |> 
  spread(var, value)

ITR_cons_num <- ITR_cons_num |> 
  spread(var, value) |> 
  left_join(conso_specificities, by = c("country", "period"))

ITR_cons_num[is.na(ITR_cons_num)] <- 0

ITR_cons_num <- ITR_cons_num |> 
  mutate(
    period,
    country,
    value = D211 + D212 + (D214 - D214B - D214C - D214K) + (D29D + D29F + D29G) + (D59B + D59C + D59D) + spec,
    .keep = "none"
    ) |> 
  add_column(var = "tx_cons")

### Step 3: Final consumption expenditure of households on the economic territory ----
# Denominator: Final consumption expenditure of households 
# on the economic territory - domestic concept (P31_S14_DC). 
# Eurostat: Final consumption aggregates by durability (nama_10_fcs).
url_filter <- paste0("A.CP_MNAC.", "P31_S14_DC", ".", url_country)

ITR_cons_den <- rdb("Eurostat", "nama_10_fcs", mask = url_filter) |> 
  filter(year(period) >= 1995 & year(period) < year_max) |> 
  select(country = geo, period, value, var = na_item)

ITR_cons_minmax_d <- ITR_cons_den |> 
  na.omit() |> 
  group_by(country) |> 
  summarize(min_date = min(year(period)), max_date = max(year(period)))

kable(ITR_cons_minmax_d, format = "html", 
      caption = "Final consumption expenditure of households: beginning and end of the sample", 
      table.attr = "style=\"width:100%\"") |> 
  kable_styling(position = "center", font_size = 12)

ITR_consumption <- bind_rows(ITR_cons_num, ITR_cons_den) |> 
  spread(var, value) |> 
  mutate(country, period, value = tx_cons / P31_S14_DC, .keep = "none")

### Step 4: Euro Area GDP-weighted average -----
# Build the GDP-weighted average for the Euro Area. 
# Get the weights from the GDP of each country (million purchasing power standards). 
# Data series for each country is available since 1995.
url_filter <- paste0("A.CP_MPPS_EU27_2020.", "B1GQ", ".", url_country)
# previously: paste0("A.CP_MPPS.", "B1GQ", ".", url_country)

gdp <- rdb("Eurostat", "nama_10_gdp", mask = url_filter) |> 
  select(period, value, country = geo) |> 
  add_column(var = "gdp") |> 
  filter(year(period) >= 1995 & year(period) < year_max)

EA_gdp <- gdp |> 
  group_by(period) |> 
  summarize(value = sum(value))

weights <- gdp |> 
  left_join(EA_gdp, by = join_by(period)) |> 
  ungroup() |> 
  mutate(country, period, weight = value.x / value.y, .keep = "none")

# Apply the weights to the country data to get the Euro Area GDP-weighted average. 
ITR_consumption_EA_na <- ITR_consumption |> 
  left_join(weights, by = join_by(country, period))

ITR_consumption_EA_na[is.na(ITR_consumption_EA_na)] <- 0

ITR_consumption_EA <- ITR_consumption_EA_na |> 
  mutate(period, value = value * weight, .keep = "none") |> 
  group_by(period) |> 
  summarize(value = sum(value)) |> 
  add_column(country = "EA19")

ITR_consumption_4 <- ITR_consumption |> 
  filter(grepl(pattern = 'FR|DE|IT|ES', x = country))

ITR_consumption_FIN <- bind_rows(ITR_consumption_4, ITR_consumption_EA) |> 
  add_column(var = "Consumption tax")

ITR_consumption_FIN$country <- factor(ITR_consumption_FIN$country)                  

levels(ITR_consumption_FIN$country) <- list_country

# Plot the final series for France, Germany, Italy, Spain and the Euro Area.
ggplot(ITR_consumption_FIN, aes(period, value, color = country)) +
  geom_line(lwd = 1.2)+
  my_theme() +
  ggtitle("Implicit Tax Rate on Consumption")

ggsave("01_ITR_consumption.png", path = fig_path, width = 12, height = 8)
graphics.off()

## Implicit tax rate on labor ----
# ITR on employed labor is the sum of all direct  and indirect taxes 
# and employees’ and employers’ social contributions levied on employed 
# labor income, divided by the total compensation of employees working in the economic territory. 

# DG Taxation & Customs Union methodology here: https://ec.europa.eu/taxation_customs/sites/taxation/files/taxation_trends_report_2017_-_methodology.pdf
#   1. Gather all data on employed labor taxes (the numerator), 
#   2. Add the specificities for France, Germany, Italy and Spain, 
#   3. Deal with the special case of the personal income tax. 
#   4. Retrieve data on the total compensation of employees & wage bill and payroll taxes, 
#   5. Constitute the Euro Area GDP-weighted average. 


### Step 1: Taxes on employed labor ----
# Consider as taxes on employed labor the following aggregates:
#   1. From taxes on income:
#      a. Taxes on individual or household income including holding gains 
#         - part raised on labor income (D51A_C1). 
#         We deduce the part raised on labor income in the next step
#   2. From other current taxes:
#      a. Total wage bill and payroll taxes (D29C)
#   3. From employers’ actual social contributions:
#      a. Compulsory employers’ actual social contributions (D611C)
#   4. From households’ actual social contributions:
#      a. Compulsory employees’ actual social contributions (D613CE)
# Eurostat “Main national accounts tax aggregates” (gov_10a_taxag).
url_taxes  <- paste(c("D51A_C1", "D29C", "D611C", "D613CE"), collapse = "+")
url_filter <- paste0("A.MIO_NAC.S13_S212.", url_taxes, ".", url_country)

ITR_lab_num_raw <- rdb("Eurostat", "gov_10a_taxag", mask = url_filter)

ITR_lab_num1 <- ITR_lab_num_raw |> 
  mutate(country = geo, period, var = na_item, value, .keep = "none") |> 
  filter(year(period) >= 1995 &  year(period) < year_max & !grepl(pattern = 'D51A_C1', x = var))

### Step 2: Taxes on employed labor - specifities for France, Germany, Italy and Spain ----
# Follow the national classification (NTLs - national tax lists), as follows:
#   France: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/data_on_taxation/fr-national-tax.xlsx
#   1. From contributions on the value added of the corporations (D29A):
#      a. part raised on employers’ labor tax.
#   2. From capital transfers from general government to relevant sectors representing taxes and social contributions assessed but unlikely to be collected (D995):
#      a. part raised on employers’ labor tax.
#      b. part raised on employees’ labor tax.
#   3. From taxes on individual or household income including holding gains - part raised on labor income (D51A_C1), exclude the following for the calculation of the personal income tax:
#      a. (D51A_C02): used dividend imputation (positive).
#      b. (D51A_C05): civil servants contribution to the unemployment insurance regime.
#      c. (D51A_C06): other social levies.
#      d. (D51A_C08): levies on the income from financial assets (PRCM).
#   4. From taxes on individual or household income including holding gains - part raised on labor income (D51A_C1), include the following for the calculation of the Employees’ SSC:
#      a. (D51A_C05): civil servants contribution to the unemployment insurance regime.
#   Spain: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/data_on_taxation/es-national-tax.xlsx
#   1. Taxes on income assessed but unlikely to be collected (D995C).
#      a. part raised on personal income tax.

# For Italy and Germany, it is also possible to apply some specificities, 
# e.g. for Germany the trade tax (D51M_C4) can be excluded from D51A_C1, 
# and for Italy part of the Revenue from IRAP tax can be included to labor and employers’ SSC. 

# However, the general formula was already sufficient to recover the general trend of the implicit tax rate. 
# To find more information about other EU country specificities, 
# check the National Tax Lists online in the DG Taxation and Customs Union website, 
# URL: https://ec.europa.eu/taxation_customs/business/economic-analysis-taxation/data-taxation_en

# Eurostat Tax Revenue Statistics: https://ec.europa.eu/eurostat/statistics-explained/index.php/Tax_revenue_statistics
labor_specificities <- readxl::read_xlsx(path = "data/s_labour.xlsx") |> 
  mutate(period = as.Date(period)) |> 
  select(country, period, corr_pit = total_split1, corr_leyrs = total_leyrs, corr_lees = total_lees) |> 
  filter(year(period) >= 1995 & year(period) < year_max)

### Step 3: Personal income tax, part raised on labor income ----
# The methodological problem in calculating the ITR on labor relies in this part: 

# The personal income tax is broad-based and relates to multiple sources of income 
# (i.e. employed labor, self-employed labor, income from capital and income in the form of social benefits and pensions received). 
# For constituting this ITR, it is then only necessary to use the part raised on labor income. 

# The DG Taxation & Customs Union uses disaggregated taxpayers’ data 
# in order to allocate the personal income tax revenue across different sources of income. 

# It provides data for estimating the part of the revenue from personal income tax 
# that can be attributed to labor income. 

# The tables below can be found in the reports on Taxation Trends in the European Union in its 
#   2020: https://ec.europa.eu/taxation_customs/sites/taxation/files/taxation_trends_report_2020.pdf
#   2019: https://ec.europa.eu/taxation_customs/sites/taxation/files/taxation_trends_report_2019.pdf
#   2018: https://ec.europa.eu/taxation_customs/sites/taxation/files/taxation_trends_report_2018.pdf
#   2017: https://ec.europa.eu/taxation_customs/sites/taxation/files/taxation_trends_report_2017.pdf
#   2016: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/tax_structures/2016/econ_analysis_report_2016.pdf
#   2014: https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/gen_info/economic_analysis/tax_structures/2014/report.pdf
# editions (pages 297, 292, 292, 276, 330 and 303 respectively). 

# We will chain the tables by averaging the data points when they differ from table to table, 
# in order to get the (1995-2018) series on the percentage of personal income tax revenue 
# allocated to employed labor income. 

# We use these percentages to obtain the final series on personal income tax raised on labor income.
url_country_pit <- paste(c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "EL", "IE", "IT",
                           "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES"), collapse = "|")

pit_2020_raw <- read_csv(file = "data/pit_2020.csv") |> 
  filter(grepl(pattern = url_country_pit, x = country))

pit_2020_raw_d <- pit_2020_raw |> 
  gather(period, value, -country) |> 
  mutate(period = year(period)) |> 
  spread(period, value)

kable(
  x = pit_2020_raw_d, format = "html", 
  caption = "Personal income tax revenue allocated to employed labor income, in % of total revenue of personal income tax (2004–2018)",
  table.attr = "style=\"width:100%\"") |> 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
    position = "center", 
    font_size = 12
    )

pit_2020 <- pit_2020_raw |> 
  gather(period, r_2020, -country)

pit_2019 <- read_csv(file = "data/pit_2019.csv") |> 
  filter(grepl(pattern = url_country_pit, x = country)) |> 
  gather(period, r_2019, -country)

pit_2018 <- read_csv(file = "data/pit_2018.csv") |> 
  filter(grepl(pattern = url_country_pit, x = country)) |> 
  gather(period, r_2018, -country)

pit_2017 <- read_csv(file = "data/pit_2017.csv") |> 
  filter(grepl(pattern = url_country_pit, x = country)) |> 
  gather(period, r_2017, -country)

pit_2016 <- read_csv(file = "data/pit_2016.csv") |> 
  filter(grepl(pattern = url_country_pit, x = country)) |> 
  gather(period, r_2016, -country)

pit_2014 <- read_csv(file = "data/pit_2014.csv") |> 
  filter(grepl(pattern = url_country_pit, x = country)) |> 
  gather(period, r_2014, -country)

pit <- pit_2014 |> 
  full_join(pit_2016, by = join_by(country, period)) |> 
  mutate(r_2016 = if_else(is.na(r_2016), r_2014, r_2016)) |> 
  full_join(pit_2017, by = join_by(country, period)) |> 
  mutate(r_2017 = if_else(is.na(r_2017), r_2016, r_2017)) |> 
  full_join(pit_2018, by = join_by(country, period)) |> 
  mutate(r_2018 = if_else(is.na(r_2018), r_2017, r_2018)) |> 
  full_join(pit_2019, by = join_by(country, period)) |> 
  mutate(r_2019 = if_else(is.na(r_2019), r_2018, r_2019)) |> 
  full_join(pit_2020, by = join_by(country, period)) |> 
  mutate(r_2020 = if_else(is.na(r_2020), r_2018, r_2020)) |> 
  mutate(country, period = ymd(period), a_weight = r_2020 / 100, .keep = "none")

ITR_lab_num2 <- ITR_lab_num_raw |> 
  mutate(country = geo, period, var = na_item, value, .keep = "none") |> 
  filter(year(period) >= 1995 & year(period) < year_max & var == "D51A_C1") |> 
  left_join(labor_specificities, by = join_by(country, period)) |>
  left_join(pit, by = join_by(country, period))

ITR_lab_num2[is.na(ITR_lab_num2)] <- 0

ITR_lab_num2 <- ITR_lab_num2 |> 
  mutate(country, period, var, value = ((value + corr_pit) * a_weight), .keep = "none")

ITR_lab_num <- bind_rows(ITR_lab_num1, ITR_lab_num2)

### Step 4: Compensation of employees, wage bill and payroll taxes ----

# The total compensation of employees (D1) is part of the denominator of the ratio. 
# It is defined as total remuneration, in cash or in kind, 
# payable by an employer to an employee in return for work done. 

# It consists of gross wages (in cash or in kind) and thus also the 
# amount paid as social insurance contributions and wage withholding tax. 

# In addition, employers’ social contributions (including imputed social contributions) 
# as well as to private pensions and related schemes are included. 

# Personal income taxes and social contributions paid by EU civil servants 
# to the EU Institutions are not included. 

# Compensation of employees is thus a broad measure of the gross economic 
# income from employment before any charges are withheld. 

# Eurostat “GDP and main components (output, expenditure and income)” (nama_10_gdp).
url_filter <- paste0("A.CP_MNAC.", "D1", ".", url_country)

ITR_lab_den <- rdb("Eurostat", "nama_10_gdp", mask = url_filter) |> 
  mutate(country = geo, period, value, var = na_item, .keep = "none") |> 
  filter(year(period) >= 1995 & year(period) < year_max)

ITR_labor_na <- bind_rows(ITR_lab_num, ITR_lab_den) |> 
  spread(var, value)

ITR_lab_min_d <- ITR_labor_na |> 
  gather(var, value, -country, -period) |> 
  na.omit() |> 
  group_by(country, var) |> 
  summarize(min_date = min(year(period))) |> 
  spread(var, min_date)

kable(ITR_lab_min_d, format = "html", caption = "Taxes on employed labor: beginning of the sample") |> 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed",  "responsive"), 
    position = "center", font_size = 12) |> 
  footnote(
    general = "The following aggregates are not applicable for some countries (in a determined period of time): D51A_C1, D29C, D611C, D613CE. Thus we replace their NA values by 0 for the calculation.", 
    number = c(
      "D1 has not been collected for IE before 1998. We will omit these points in the estimation.",
      "D29C is not applicable for: EE, MT and SK; it is applicable for LT since 2002 and for LV since 2003.", 
      "D611C is not available, exists but has not been transmitted/collected for PT.", 
      "D613C is applicable for EE since 2002; it is not available, exists but has not been collected for PT before 2010"
      )
    )

ITR_labor_na <- ITR_labor_na |> 
  left_join(labor_specificities, by = join_by(country, period))

ITR_labor_na[is.na(ITR_labor_na)] <- 0 

ITR_labor <- ITR_labor_na |> 
  mutate(
    country,
    period,
    ITR_labor = (D51A_C1 + D29C + D611C + D613CE + corr_leyrs + corr_lees) / (D1 + D29C),
    ITR_pi = D51A_C1 / (D1 + D29C),
    ITR_essc = (D613CE + corr_lees) / (D1 + D29C),
    ITR_esscprt = (D611C + D29C + corr_leyrs) / (D1 + D29C),
    .keep = "none")

ITR_lab_max_d  <- ITR_labor_na |> 
  gather(var, value, -country, -period) |> 
  na.omit() |> 
  group_by(country, var) |> 
  summarize(max_date = max(year(period))) |> 
  spread(var, max_date)

kable(ITR_lab_max_d, format = "html", caption = "Taxes on employed labor: end of the sample") |> 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed",  "responsive"), 
    position = "center", font_size = 12) |> 
  footnote(
    general = "The following aggregates are not applicable for some countries (in a determined period of time): D51A_C1, D29C, D611C, D613CE. Thus we replace their NA values by 0 for the calculation.", 
    number = c("D1 has not been collected for IE before 1998. We will omit these points in the estimation.","D29C is not applicable for: EE, MT and SK; it is applicable for LT since 2002 and for LV since 2003.", "D611C is not available, exists but has not been transmitted/collected for PT.", "D613C is applicable for EE since 2002; it is not available, exists but has not been collected for PT before 2010")
    )

### Step 5: Euro Area GDP-weighted average ----

# After setting up the ITR on labor for the 19 countries that compose the Euro Area, 
# it is possible to build the GDP-weighted average for the Euro Area. 

# We use the same weights that were established for the ITR on consumption. 
# The chart below shows the final series for France, Germany, Italy, Spain and the Euro Area.
ITR_labor_EA <- ITR_labor |> 
  left_join(weights, by = c("country" = "country", "period" = "period")) |> 
  filter(ITR_labor < 1) |> 
  mutate(
    period,
    ITR_labor  = ITR_labor * weight,
    ITR_pi      = ITR_pi * weight,
    ITR_essc    = ITR_essc * weight,
    ITR_esscprt = ITR_esscprt * weight,
    .keep = "none"
    ) |> 
  group_by(period) |> 
  summarize(
    ITR_labor  = sum(ITR_labor),
    ITR_pi      = sum(ITR_pi),
    ITR_essc    = sum(ITR_essc),
    ITR_esscprt = sum(ITR_esscprt)
    ) |> 
  add_column(country = "EA19")

ITR_labor_4 <- ITR_labor |> 
  filter(grepl(pattern = 'FR|DE|IT|ES', x = country))

ITR_labor_FIN1 <- bind_rows(ITR_labor_4, ITR_labor_EA)

ITR_labor_FIN1$country <- factor(ITR_labor_FIN1$country)

levels(ITR_labor_FIN1$country) <- list_country

ggplot(ITR_labor_FIN1, aes(period, ITR_labor, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Implicit Tax Rate on Labor")

ggsave("02_ITR_Labor.png", path = fig_path, width = 12, height = 8)
graphics.off()

# For the analysis, it is possible to recover the evolution 
# of the personal income tax, the employees’ SSC and the employers’ SSC 
# as a share of the ITR on labor. The chart below shows this evolution:
ITR_labor_shares <- ITR_labor_FIN1 |> 
  mutate(
    country,
    period,
    w_pi      = ITR_pi / ITR_labor,
    w_essc    = ITR_essc / ITR_labor,
    w_esscprt = ITR_esscprt / ITR_labor,
    .keep = "none"
    ) |> 
  gather(var, value, -period, -country)

list_var <- list(
  "Personal Income Tax"              = "w_pi",
  "Employees' SSC"                   = "w_essc",
  "Employers' SSC and payroll taxes" = "w_esscprt"
  )

ITR_labor_shares$var <- factor(ITR_labor_shares$var)

levels(ITR_labor_shares$var) <- list_var

ggplot(ITR_labor_shares, aes(period, value, color = var)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ country , scales = "fixed", ncol = 2) +
  my_theme() +
  ggtitle("Personal Income Tax, Employees' SSC and Employers' SSC & payroll taxes \n (as a share of the ITR on Labor)")

ggsave("03_ITR_SSC.png", path = fig_path, width = 12, height = 8)
graphics.off()

# The chart below shows the evolution of the composition of the ITR on labor:
ITR_labor_FIN <- ITR_labor_FIN1 |> 
  select(-ITR_labor) |> 
  gather(var, value, -period, -country)

list_var <- list("Labor income tax" = "ITR_pi",
                 "Employees' SSC" = "ITR_essc",
                 "Employers' SSC and payroll taxes" = "ITR_esscprt")

ITR_labor_FIN$var <- factor(ITR_labor_FIN$var)

levels(ITR_labor_FIN$var) <- list_var

ggplot(ITR_labor_FIN, mapping = aes(fill = var, value, x = period)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ country , scales = "fixed", ncol = 3) +
  my_theme() +
  ggtitle("Composition of the Implicit Tax Rate on Labor")

ggsave("04_ITR_Labor.png", path = fig_path, width = 12, height = 8)
graphics.off()

# And the last chart shows all the ITRs for a multi-country comparison:
ggplot(ITR_labor_FIN, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ var , ncol = 3) +
  my_theme() +
  ggtitle("Implicit Tax Rates on Labor (%)")

ggsave("05_ITR_Labor.png", path = fig_path, width = 12, height = 8)
graphics.off()

## Implicit tax rate on corporate income ----

# The methodology for calculating the ITR on corporate income that proposes 
# the DG Taxation & Customs Union may exceed the statutory corporate tax rate, 
# for instance, on the payment by corporation of taxes referring to profits earned earlier, 
# or on taxes paid on capital gains (which are not included in the ITR denominator due to a lack of statistics). 
# That is why, in this section we choose the top statutory corporate income tax rate (including surcharges) as a proxy. 

# Data can be downloaded directly from the DG Taxation & Customs Union here. 
# URL: https://ec.europa.eu/taxation_customs/sites/taxation/files/taxation_trends_report_2019_statutory_rates.xlsx

# Concerning the GDP-weighted average for the Euro Area, 
# we will use these country tax rates and the same weights that were established for the ITR on consumption to constitute it. 
# The chart below shows the final series for France, Germany, Italy, Spain and the Euro Area.
ITR_corporate_income <- read_csv(file = "data/ITR_corporate_income.csv") |> 
  gather(period, value, -country) |> 
  mutate(country, period = ymd(period), value = value / 100, .keep = "none") |> 
  filter(year(period) >= 1995 & year(period) < year_max)

ITR_corporate_income_EA <- ITR_corporate_income |> 
  left_join(weights, by = join_by(country, period)) |> 
  mutate(period, value = value * weight, .keep = "none") |> 
  group_by(period) |> 
  summarize(value = sum(value)) |> 
  add_column(country = "EA19")

ITR_corporate_income_4 <- ITR_corporate_income |> 
  filter(grepl(pattern = 'FR|DE|IT|ES', x = country))

ITR_corporate_income_FIN <- bind_rows(ITR_corporate_income_4, ITR_corporate_income_EA) |> 
  add_column(var = "Corporate income tax")

ITR_corporate_income_FIN$country <- factor(ITR_corporate_income_FIN$country)  

levels(ITR_corporate_income_FIN$country) <- list_country

ggplot(ITR_corporate_income_FIN, aes(period, value, color = country)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  ggtitle("Implicit Tax Rate on Corporate Income")

ggsave("06_ITR_Corporate.png", path = fig_path, width = 12, height = 8)
graphics.off()

## Average values ----

# We summarize the average values of the implicit tax rates in the following table:
ITR <- bind_rows(ITR_corporate_income_FIN, ITR_labor_FIN, ITR_consumption_FIN) |> 
  na.omit() |> 
  mutate(value = round(value, digits = 3))

ss_ITR <- ITR |> 
  group_by(var, country) |> 
  summarize(steady_state = mean(value)) |> 
  mutate(steady_state = round(steady_state, digits = 3)) |> 
  spread(country, steady_state) |> 
  ungroup() 

kable(ss_ITR, format = "html", caption = "Implicit tax rates - average values",
      table.attr = "style=\"width:100%\"") |> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center")

ss_ITR_plot <- ss_ITR |> 
  gather(country, value, -var)

ggplot(ss_ITR_plot, aes(country, value, fill = country)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ var , scales = "free_y", ncol = 3) +
  xlab(NULL) + ylab(NULL) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    strip.text = element_text(size = 12),
    legend.position = "none"
    ) +
  ggtitle("Implicit tax rates - average values")

ggsave("07_ITR_Average.png", path = fig_path, width = 12, height = 8)
graphics.off()

# We can download ready-to-use data for France, Germany, Italy, Spain and the Euro Area in csv format here.
# URL: http://shiny.nomics.world/data/ITR_eurodata.csv
list_tau <- list(
  "taun"  = "Labor income tax" ,
  "tauwh" = "Employees' SSC",
  "tauwf" = "Employers' SSC and payroll taxes",
  "tauc"  = "Consumption tax", 
  "tauk"  = "Corporate income tax"
  )

list_country <- list(
  "FR" = "France",
  "DE" = "Germany",
  "IT" = "Italy",
  "ES" = "Spain",
  "EA" = "Euro Area"
  )

ITR$country <- factor(ITR$country)                  

levels(ITR$country) <- list_country

ITR$var <- factor(ITR$var)

levels(ITR$var) <- list_tau

ITR_eurodata <- ITR |> 
  unite("var", c("country","var")) |> 
  mutate(period = year(period)) |> 
  spread(var, value)

write.csv(ITR_eurodata, file = "data/ITR_eurodata.csv", row.names = FALSE)

## Comparison ----
# The Directorate-General for Taxation & Customs Union of the European Commission 
# provides data on the implicit tax rates on consumption and labor 
# since 2005, using detailed revenue data provided by member states. 

# You can find this data here. 
# URL: https://ec.europa.eu/taxation_customs/business/economic-analysis-taxation/data-taxation_en

# We compare in this section our series with the ones of the European Commission in the charts below.

# The main steps, together with some specificities, 
# allow to recover the general trend of the implicit tax rates. 
# Some differences remain, and they stem mainly from the last update of National Tax Lists, 
# which was on June 2020, 
# URL: https://ec.europa.eu/eurostat/statistics-explained/index.php/Tax_revenue_statistics
# whereas the last update of the Taxation Trends Report was on February 2020. 
# URL: https://ec.europa.eu/taxation_customs/business/economic-analysis-taxation/data-taxation_en

# Other minimal differences could stem from taxes assessed but unlikely to be collected when the data was released, 
# or eventually discretionary adjustments or specificities applied by the DG Taxation and Customs Union.
labor_comp <- ITR_labor_FIN1 |> 
  select(country, period, value = ITR_labor) |> 
  add_column(var = "Labor tax")

conso_labor_comp <- bind_rows(labor_comp, ITR_consumption_FIN) |> 
  add_column(data_s = "Updated")

eucom <- read_csv(file = "data/eucom.csv") |> 
  rename(value = eucom) |> 
  mutate(value = value / 100)

eucom$country <- factor(eucom$country)                  

levels(eucom$country) <- list_country

comparison <- bind_rows(conso_labor_comp, eucom)

comparison1 <- comparison |> 
  filter(var == "Consumption tax")

ggplot(comparison1, aes(period, value, color = data_s)) +
  geom_line(lwd = 1.2) +
  facet_wrap(~ country , scales = "fixed", ncol = 3) +
  my_theme() +
  ggtitle(expression(atop("Consumption tax", atop(italic("Comparison: European Commission vs. Updated Data"), ""))))

ggsave("08_comparison_1.png", path = fig_path, width = 12, height = 8)
graphics.off()

comparison2 <- comparison |> 
  filter(var == "Labor tax")

ggplot(comparison2, aes(period, value, color = data_s)) +
  geom_line(lwd = 1.2)+
  facet_wrap(~ country , scales = "fixed", ncol = 3) +
  my_theme() +
  ggtitle(expression(atop("Labor tax", atop(italic("Comparison: European Commission vs. Updated Data"), ""))))

ggsave("09_comparison_2.png", path = fig_path, width = 12, height = 8)
graphics.off()
# END