# 03 - Reproduce economic indicators from "The Economist" ----

# URL: https://macro.cepremap.fr/article/2020-10/economic-indicators/

# To update the table, just download the code and re-run it.
# https://git.nomics.world/macro/indicators

if (!"pacman" %in% install.packages()[, "Package"]) { 
  install.packages("pacman", repos = "http://cran.r-project.org")
}

pacman::p_load(tidyverse, rdbnomics, zoo, knitr, kableExtra, formattable)

current_year     <- year(Sys.Date()) - 1
last_year        <- current_year - 2
before_last_year <- current_year - 3

country_list <- c(
  "United States", "China", "Japan", "Britain", "Canada", 
  "Euro area", "Austria", "Belgium", "France", "Germany", "Greece", 
  "Italy", "Netherlands", "Spain", "Czech Republic", 
  "Denmark", "Norway", "Poland", "Russia", "Sweden", "Switzerland", "Turkey", 
  "Australia", "Hong Kong", "India", "Indonesia", "Malaysia",
  "Pakistan", "Philippines", "Singapore", "South Korea", "Taiwan", "Thailand", 
  "Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Peru",
  "Egypt", "Israel", "Saudi Arabia", "South Africa"
)


## Download ----

### GDP ----

#### OECD ----
# GDP from the Main Economic Indicators (MEI) of the OECD
# MEASURE Measure
# GPSA:   Growth rate previous period, s.a.          
# GYSA:   Growth rate same period previous year, s.a.
gdp <- rdb(
  provider_code = "OECD", 
  dataset_code  = "MEI", 
  ids = ".NAEXKP01.GPSA+GYSA.Q"
)

# Note: China (People's Republic of) is included in OECD country list

#### IMF ----
# Non-OECD countries Hong Kong, Philippines, Saudi Arabia, and Singapore
# from the International Financial Statistics (IFS) of the IMF
gdp_level_hk_ph_th_sa_sg <- rdb(
  provider_code = "IMF",
  dataset_code  = "IFS",
  mask = "Q.HK+PH+TH+SA+SG.NGDP_R_SA_XDC"
  ) |> 
  rename(Country = `Reference Area`) |> 
  mutate(
    Country = case_when(Country == "Hong Kong, China" ~ "Hong Kong", TRUE ~ Country)
  )

gdp_qoq_hk_ph_th_sa_sg <- gdp_level_hk_ph_th_sa_sg |> 
  arrange(Country, period) |> 
  group_by(Country) |> 
  mutate(
    value = (value / lag(value) - 1) * 100,
    MEASURE = "GPSA"
  )

gdp_yoy_hk_ph_th_sa_sg <- gdp_level_hk_ph_th_sa_sg |> 
  arrange(Country, period) |> 
  mutate(quarter = quarter(period)) |> 
  group_by(Country, quarter) |> 
  mutate(value = (value / lag(value) - 1) * 100, MEASURE = "GYSA")


#### Bank of Indonesia ----
# GDP for Malaysia and Taiwan from the Bank of Indonesia (BI)
gdp_my <- rdb(ids = "BI/TABEL9_1/21.Q") |> 
  mutate(Country = "Malaysia", MEASURE = "GYSA")

gdp_tw <- rdb(ids = "BI/TABEL9_1/17.Q") |> 
  mutate(Country = "Taiwan", MEASURE = "GYSA")

#### IMF WEO ----
# GDP for Egypt, Pakistan and Peru from the IMF World Economic Outlook (WEO).
gdp_eg_pk_pe <- rdb(
  provider_code = "IMF", 
  dataset_code  = "WEO:latest", 
  mask = "EGY+PAK+PER.NGDP_RPCH"
  ) |> 
  rename(Country = `WEO Country`) |> 
  mutate(MEASURE = "GYSA") |> 
  filter(year(period) < current_year)


#### China ----
# gdp_level_cn <- rdb(ids = "OECD/MEI/CHN.NAEXCP01.STSA.Q")

# Get GDP QoQ
# gdp_qoq_cn <- gdp_level_cn |> 
#   arrange(period) |> 
#   mutate(value = (value / lag(value) - 1) * 100, MEASURE = "GPSA")

# Get GDP YoY
# gdp_yoy_cn <- gdp_level_cn |> 
#   arrange(period) |> 
#   mutate(quarter = quarter(period)) |> 
#   group_by(quarter) |> 
#   mutate(value = (value / lag(value) - 1) * 100, MEASURE = "GYSA")


#### Argentina ----
# Argentina's GDP from Eurostat
gdp_level_ar <- rdb(ids = "Eurostat/naidq_10_gdp/Q.SCA.KP_I10.B1GQ.AR") |> 
  rename(Country = `Geopolitical entity (reporting)`)

gdp_qoq_ar <- gdp_level_ar |> 
  arrange(period) |> 
  mutate(value = (value / lag(value) - 1) * 100, MEASURE = "GPSA")

gdp_yoy_ar <- gdp_level_ar |> 
  arrange(period) |> 
  mutate(quarter = quarter(period)) |> 
  group_by(quarter) |> 
  mutate(value = (value / lag(value) - 1) * 100, MEASURE = "GYSA")

gdp <- bind_rows(
  gdp,
  gdp_qoq_hk_ph_th_sa_sg,
  gdp_yoy_hk_ph_th_sa_sg,
  gdp_my,
  gdp_tw,
  gdp_eg_pk_pe,
  # gdp_qoq_cn,
  # gdp_yoy_cn,
  gdp_qoq_ar,
  gdp_yoy_ar
)


### Industrial Production ----

#### OECD Monthly ----
indprod <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  ids = ".PRINTO01.GYSA.M"
)

#### OECD Quarterly ----
# Switzerland and Australia publish Industrial Production
# on a quarterly level only?
indprod_ch_au <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  ids = "AUS+CHE.PRINTO01.GYSA.Q"
  )

# China, Egypt, Mexico, Malaysia 
indprod_cn_eg_mx_my <- rdb(
  provider_code = "IMF",
  dataset_code  = "IFS",
  mask = "M.CN+EG+MX+MY.AIP_PC_CP_A_PT"
  ) |> 
  rename(Country = `Reference Area`)

# Indonesia, Pakistan, Peru, Philippines, Singapore, South Africa
indprod_id_pk_pe_ph_sg_za <- rdb(
  provider_code = "IMF",
  dataset_code  = "IFS",
  mask = "M.ID+PK+PE+PH+SG+ZH.AIPMA_PC_CP_A_PT"
  ) |> 
  rename(Country = `Reference Area`)

# Argentina, Hong Kong, Saudi Arabia, Thailand
indprod_ar_hk_sa_th <- rdb(
  provider_code = "IMF",
  dataset_code  = "IFS",
  mask = "Q.AR+HK+SA+TH.AIPMA_PC_CP_A_PT"
  ) |> 
  rename(Country = `Reference Area`) |> 
  mutate(
    Country = case_when(Country == "Hong Kong, China" ~ "Hong Kong", TRUE ~ Country)
  )

indprod <- bind_rows(
  indprod,
  indprod_ch_au,
  indprod_cn_eg_mx_my,
  indprod_id_pk_pe_ph_sg_za,
  indprod_ar_hk_sa_th
)

### CPI ----
cpi <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  ids = ".CPALTT01.GY.M"
  )
  
# Australia
cpi_au <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  ids = "AUS.CPALTT01.GY.Q"
  )
  
# Taiwan
cpi_tw <- rdb(ids = "BI/TABEL9_2/17.Q") |> 
  mutate(Country = "Taiwan")
  
# Other
cpi_other <- rdb(
  provider_code = "IMF",
  dataset_code  = "IFS",
  mask = "M.EG+HK+MY+PE+PH+PK+SG+TH.PCPI_PC_CP_A_PT"
  ) |> 
  rename(Country = `Reference Area`) |> 
  mutate(
    Country = case_when(Country == "Hong Kong, China" ~ "Hong Kong", TRUE ~ Country)
  )

cpi <- bind_rows(
  cpi,
  cpi_au,
  cpi_tw,
  cpi_other
)

### Unemployment ----
unemp <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  ids = ".LRHUTTTT.STSA.M"
  )
  
# Switzerland
unemp_ch <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  mask = "CHE.LMUNRRTT.STSA.M"
)

# Brazil
unemp_br <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  mask = "BRA.LRUNTTTT.STSA.M"
)

# South Africa & Russia
unemp_za_ru <- rdb(
  provider_code = "OECD",
  dataset_code  = "MEI",
  mask = "ZAF+RUS.LRUNTTTT.STSA.Q"
)

# China
# Unemployment for China from the German Bundesbank (BUBA)
# Labor Market and Population of foreign countries [BBXL3]
# Note: Bundesbank does not provide data for China anymore.
# Use the National Bureau of Statistics of China (NBS)
# The Urban Surveyed Unemployment Rate [NBS/M_A0E01] 
unemp_cn <- rdb(ids = "NBS/M_A0E01/A0E0101") |> 
  mutate(Country = "China")

# Saudi Arabia
unemp_sa <- rdb(ids = "ILO/UNE_DEAP_SEX_AGE_RT/SAU.BA_627.AGE_AGGREGATE_TOTAL.SEX_T.A") |> 
  rename(Country = `Reference area`) |> 
  filter(year(period) < current_year)

# India
unemp_in <- rdb(ids = "ILO/UNE_2EAP_SEX_AGE_RT/IND.XA_1976.AGE_YTHADULT_YGE15.SEX_T.A") |> 
  rename(Country = `Reference area`) |> 
  filter(year(period) < current_year)

# Indonesia & Pakistan
unemp_id_pk <- rdb(
  provider_code = "ILO",
  dataset_code  = "UNE_DEAP_SEX_AGE_EDU_RT",
  mask = "IDN+PAK..AGE_AGGREGATE_TOTAL.EDU_AGGREGATE_TOTAL.SEX_T.Q"
  ) |> 
  rename(Country = `Reference area`)

# Other
unemp_other <- rdb(
  provider_code = "ILO",
  dataset_code  = "UNE_DEA1_SEX_AGE_RT",
  mask = "ARG+EGY+HKG+MYS+PER+PHL+SGP+THA+TWN..AGE_YTHADULT_YGE15.SEX_T.Q"
  ) |> 
  rename(Country = `Reference area`) |> 
  mutate(
    Country = case_when(
      Country == "Hong Kong, China" ~ "Hong Kong", 
      Country == "Taiwan, China" ~ "Taiwan",
      TRUE ~ Country
      )
  )

unemp <- bind_rows(
  unemp,
  unemp_br,
  unemp_za_ru,
  unemp_ch,
  unemp_cn,
  unemp_sa,
  unemp_in,
  unemp_id_pk,
  unemp_other
)


### Forecast GDP CPI

# EA
forecast_gdp_cpi_ea <- rdb(
  provider_code = "IMF",
  dataset_code  = "WEOAGG:latest",
  mask = "163.NGDP_RPCH+PCPIPCH"
)

forecast_gdp_cpi <- rdb(
  provider_code = "IMF",
  dataset_code  = "WEO:latest",
  mask = ".NGDP_RPCH+PCPIPCH"
  ) |> 
  bind_rows(forecast_gdp_cpi_ea) |> 
  # mutate(
  #   Country = `WEO Country`,
  #   var = `WEO Subject`,
  #   value,
  #   period, 
  #   .keep = "used"
  #   ) |> 
  transmute(
    Country = `WEO Country`,
    var = `WEO Subject`,
    value,
    period
    ) |> 
  mutate(
    Country = str_trim(Country),
    var = str_trim(var)
  ) |> 
  mutate(
    Country = case_when(
      Country == "United Kingdom" ~ "Britain",
      Country == "Hong Kong SAR" ~ "Hong Kong",
      Country == "Korea" ~ "South Korea",
      Country == "Taiwan Province of China" ~ "Taiwan",
      TRUE ~ Country
    ),
    var = case_when(
      var == "Gross domestic product, constant prices" ~ "GDP",
      var == "Inflation, average consumer prices" ~ "CPI",
      TRUE ~ var
    )
  )

forecast_gdp_cpi <- left_join(
  data.frame(Country = country_list),
  forecast_gdp_cpi, 
  by = "Country"
)


## Transform ----
gdp_yoy_latest_period <- gdp |> 
  filter(MEASURE == "GYSA") |> 
  filter(!is.na(value)) |> 
  summarise(period = max(period))

gdp_yoy_latest <- gdp |> 
  filter(MEASURE == "GYSA") |> 
  inner_join(gdp_yoy_latest_period, by = join_by(period)) |> 
  mutate(
    var = "GDP",
    measure = "latest"
  )

gdp_qoq_latest_period <- gdp |> 
  filter(MEASURE == "GPSA") |> 
  filter(!is.na(value)) |> 
  group_by(Country) |> 
  summarise(period = max(period))

gdp_qoq_latest <- gdp |> 
  filter(MEASURE == "GPSA") |> 
  inner_join(gdp_qoq_latest_period, by = join_by(Country, period)) |> 
  mutate(
    var = "GDP",
    measure = "quarter"
  )

gdp_2024_2025 <- forecast_gdp_cpi |> 
  filter(var == "GDP" & (period == "2024-01-01" | period == "2025-01-01")) |> 
  mutate(measure = as.character(year(period)))

indprod_latest_period <- indprod |> 
  filter(!is.na(value)) |> 
  group_by(Country) |> 
  summarise(period = max(period))

indprod_latest <- indprod |> 
  inner_join(indprod_latest_period, by = join_by(Country, period)) |> 
  mutate(
    var = "indprod",
    measure = "latest"
  )

cpi_latest_period <- cpi |> 
  filter(!is.na(value)) |> 
  group_by(Country) |> 
  summarise(period = max(period))

cpi_latest <- cpi |> 
  inner_join(cpi_latest_period, by = join_by(Country, period)) |> 
  mutate(
    var = "CPI",
    measure = "latest"
  )

cpi_2024 <- forecast_gdp_cpi |> 
  filter(var == "CPI" & period == "2024-01-01") |> 
  mutate(measure = as.character(year(period)))

unemp_latest_period <- unemp |> 
  filter(!is.na(value)) |> 
  group_by(Country) |> 
  summarise(period = max(period))

unemp_latest <- unemp |> 
  inner_join(unemp_latest_period, by = join_by(Country, period)) |> 
  mutate(
    var = "unemp",
    measure = "latest"
  )


## Merge ----
df_all <- bind_rows(
  gdp_yoy_latest,
  gdp_qoq_latest,
  gdp_2024_2025,
  indprod_latest,
  cpi_latest,
  cpi_2024,
  unemp_latest
  ) |> 
  mutate(
    value = ifelse(
      test = value >= 0, 
      yes = paste0("+", sprintf("%.1f", round(value, 1))), 
      no = sprintf("%.1f", round(value, 1))
        )
    ) |> 
  unite(measure, c(var, measure))


df_latest <- df_all |> 
  filter(measure %in% c("GDP_latest", "indprod_latest", "CPI_latest", "unemp_latest")) |> 
  mutate(
    value = case_when(
      `@frequency` == "quarterly" ~ paste(value, " Q", quarter(period), sep = ""),
      `@frequency` == "monthly" ~ paste(value, " ", month(period, label = TRUE, abbr = TRUE, locale = "en_US.utf8"), sep = ""),
      `@frequency` == "annual" ~ paste(value, " Year", sep = ""),
      TRUE ~ value)
    ) |>
  mutate(
    value = text_spec(
      format = ifelse(
        test = year(period) == last_year, 
        yes = paste0(value, footnote_marker_symbol(3)), 
        no = ifelse(
          test = year(period) == before_last_year, 
          yes = paste0(value, footnote_marker_symbol(4)), 
          no = value)
        ),
      link = paste("https://db.nomics.world", provider_code, dataset_code, series_code, sep = "/"), 
      color = "#333333", 
      escape = FALSE, 
      extra_css = "text-decoration:none"
      )
    )

df_final <- df_all |> 
  filter(measure %in% c("GDP_quarter", "GDP_2024", "GDP_2025", "CPI_2024")) |> 
  bind_rows(df_latest) |> 
  mutate(
    Country = case_when(
      Country == "United Kingdom" ~ "Britain",
      Country == "Euro area (20 countries)" ~ "Euro area",
      Country == "China (People's Republic of)" ~ "China",
      Country == "Korea" ~ "South Korea",
      TRUE ~ Country
    )
  ) |> 
  select(Country, value, measure) |> 
  spread(measure, value) |> 
  select(Country, GDP_latest, GDP_quarter, GDP_2024, GDP_2025, indprod_latest, CPI_latest, CPI_2024, unemp_latest)


df_final <- left_join(
  x = data.frame(Country = country_list), 
  y = df_final, 
  by = "Country"
)

## Display ----

names(df_final)[1] <- ""
names(df_final)[2] <- "latest"
names(df_final)[3] <- paste0("quarter", footnote_marker_symbol(1))
names(df_final)[4] <- paste0("2024", footnote_marker_symbol(2))
names(df_final)[5] <- paste0("2025", footnote_marker_symbol(2))
names(df_final)[6] <- "latest"
names(df_final)[7] <- "latest"
names(df_final)[8] <- paste0("2024", footnote_marker_symbol(2))
names(df_final)[9] <- "latest"


df_final |> 
  kable(
    row.names = FALSE,
    escape = FALSE,
    align = c("l", rep("c", 8)),
    caption = "Economic data (% change on year ago)"
  ) |> 
  kable_styling(
    bootstrap_options = c("striped", "hover", "responsive"),
    fixed_thead = TRUE,
    font_size = 13
  ) |> 
  add_header_above(
    header = c(" " = 1, "Gross domestic product" = 4, "Industrial production" = 1, "Consumer prices" = 2, "Unemployment rate, %" = 1)
    ) |> 
  column_spec(column = 1, bold = TRUE) |> 
  row_spec(
    row = seq(from = 1, to = nrow(df_final), by = 2), 
    background = "#D5E5EB"
    ) |> 
  row_spec(
    row = c(5, 14, 22, 33, 39),
    extra_css = "border-bottom: 1.2px solid"
  ) |> 
  footnote(
    general = "DBnomics (Eurostat, ILO, IMF, OECD and national sources). Click on the figures in the `latest`columns to see the full time series.",
    general_title = "Source: ",
    footnote_as_chunk = TRUE,
    symbol = c("% change on previous quarter, annual rate ", "IMF estimation/forecast", paste0(last_year), paste0(before_last_year))
    )




# END