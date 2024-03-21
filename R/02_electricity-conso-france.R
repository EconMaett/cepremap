# 02 - Electricity consumption in France ----
# URL: https://macro.cepremap.fr/article/2020-12/electricity-conso-france/
# High-frequency data to gauge the health of the economy in real time.
# Electricity consumption from ENEDIS (Électricité réseau distribution France)
# URL: https://data.enedis.fr/explore/dataset/bilan-electrique-jour/information/
library(tidyverse)
library(rdbnomics)
library(ghibli)
library(gghighlight)
library(gridExtra)
library(RcppRoll)
source("R/utils.R")
fig_path <- "figures/02_electricity-conso-france"
last_update <- paste0("Last update: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
# Access data through DBnomics. Consumption categories:
# - pme: Petites et moyennes entreprises (PME) | Small and medium enterprises (SME)
# - resid: Redsidential consumption
# - hva: High Voltage A
# - conso: Total consumption
# - temp: Smoothed realized temperature in France
pme   <- rdb(ids = "ENEDIS/ELECTRICITY_BALANCE/Profiled_SME_SMI_consumption.FRA.PROFILED_SME_SMI_CONSUMPTION.ALL.ALL.D")
resid <- rdb(ids = "ENEDIS/ELECTRICITY_BALANCE/Profiled_residential_consumption.FRA.PROFILED_RESIDENTIAL_CONSUMPTION.ALL.ALL.D")
hva   <- rdb(ids = "ENEDIS/ELECTRICITY_BALANCE/Total_HVA_consumption.FRA.TOTAL_HVA_CONSUMPTION.ALL.ALL.D")
conso <- rdb(ids = "ENEDIS/ELECTRICITY_BALANCE/Total_consumption.FRA.TOTAL_CONSUMPTION.ALL.ALL.D")
temp  <- rdb(ids = "ENEDIS/ELECTRICITY_BALANCE/Smoothed_realized_temperature.FRA.ALL.ALL.SMOOTHED_REALIZED_TEMPERATURE.D")

# Combine the five data sets, apply right-aligned seven-day moving average
elec <- bind_rows(pme, resid, hva, conso, temp) |> 
  select(period, value, var = cons) |> 
  group_by(var) |> 
  mutate(value = RcppRoll::roll_mean(value, n = 7, align = "right", fill = NA)) |> 
  mutate(
    value = as.numeric(value) / 1e+09,
    var = case_when(
      var == "PROFILED_RESIDENTIAL_CONSUMPTION" ~ "Residential",
      var == "PROFILED_SME_SMI_CONSUMPTION" ~ "Small and medium enterprises",
      var == "TOTAL_HVA_CONSUMPTION" ~ "Large enterprises",
      var == "TOTAL_CONSUMPTION" ~ "Total consumption",
      var == "ALL" ~ "Average temperature"
      )
    ) |> 
  ungroup() |> 
  filter(period >= "2016-01-01") |> 
  filter(period <= "2020-12-31") |> 
  separate(col = period, into = c("year", "month_day"), sep = "-", extra = "merge")

# Get average outside of 2020
elec_mean <- elec |> 
  filter(year != "2020") |> 
  group_by(var, month_day) |> 
  summarise(value = mean(value)) |> 
  ungroup() |> 
  mutate(year = "average \n2016-2019")

elec2 <- bind_rows(elec, elec_mean) |> 
  filter(month_day >= "03-01", month_day <= "11-30")

# Prepare graphics
subtitle_vec     <- "in gigawatt, 7-day moving average"
title_total_vec  <- "Total electricity consumption in France"
title_client_vec <- "Electricity consumption by customer category in France"

breaks_vec <- c(
  "01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
  "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"
  )

labels_vec <- c(
  "01-01" = "Jan.", "02-01" = "Feb.", "03-01" = "Mar.", 
  "04-01" = "Apr.", "05-01" = "May", "06-01" = "June", 
  "07-01" = "July", "08-01" = "Aug.", "09-01" = "Sept.", 
  "10-01" = "Oct.", "11-01" = "Nov.", "12-01" = "Dec."
  )

# Plot total consumption in years 2016 to 2020
ggplot(filter(elec2, var == "Total consumption"), aes(month_day, value, color = year, group = year)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_discrete(breaks = breaks_vec, labels = labels_vec) +
  scale_color_manual(values = ghibli_palette("MononokeMedium")) +
  gghighlight(
    year != "average \n2016-2019",
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "transparent")
    ) +
  theme(legend.position = c(0.85, 0.75)) +
  labs(title = title_total_vec, subtitle = subtitle_vec, caption = last_update)

ggsave("01_elec-total.png", path = fig_path, width = 8.5, height = 7)
graphics.off()

# Plot total consumption in 2020 and the average 2016-2020
ggplot(filter(elec2, var == "Total consumption"), aes(month_day, value, color = year, group = year)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_discrete(breaks = breaks_vec, labels = labels_vec) +
  scale_color_manual(values = ghibli_palette("MononokeMedium")[5:1]) +
  gghighlight(
    year %in% c("average \n2016-2019","2020"),
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "transparent")
    ) +
  theme(legend.position = c(0.88, 0.64)) +
  labs(title = title_total_vec, subtitle = subtitle_vec, x = NULL, y = NULL, caption = last_update)

ggsave("02_elec-total.png", path = fig_path, width = 8.5, height = 7)
graphics.off()

# Data shows under-consumption of electricity in the spring 
# and over-consumption of electricity in the fall of 2020.

# Prepare min|max for `geom_ribbon()` and binary variable `section_id`
figures <- elec2 |> 
  filter(var == "Total consumption", year %in% c("2020", "average \n2016-2019")) |> 
  pivot_wider(names_from = year, values_from = value) |> 
  mutate(
    min        = pmin(`2020`, `average \n2016-2019`),
    max        = pmax(`2020`, `average \n2016-2019`),
    above      = `2020`>=`average \n2016-2019`,
    changed    = is.na(lag(above)) | lag(above) != above,
    section_id = cumsum(changed),
    diff       = `2020` - `average \n2016-2019`
    )

sum <- figures |> 
  filter(month_day <= "10-31") |> 
  group_by(above) |> 
  summarise(sum = sum(diff)) |> 
  ungroup()

# Plot total consumption and highlight areas where consumption is
# below or above the average.
plot_elec_total_3 <- ggplot(filter(elec2, var == "Total consumption"), aes(month_day, value, color = year, group = year)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_discrete(breaks = breaks_vec, labels = labels_vec) +
  scale_color_manual(values = ghibli_palette("MononokeMedium")[5:1]) +
  gghighlight(
    year %in% c("average \n2016-2019", "2020"),
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "transparent")
    ) +
  geom_ribbon(
    data = filter(figures, above == FALSE), 
    aes(month_day, ymin = min, ymax = max, group = section_id),
    fill = "#06141FFF", alpha = 0.2, inherit.aes = FALSE
    ) +
  geom_ribbon(
    data = filter(figures, above == TRUE), 
    aes(month_day, ymin = min, ymax = max, group = section_id), 
    fill = "#CD4F38FF", alpha = 0.2, inherit.aes = FALSE
    ) +
  annotate("text", x = "05-10", 45, label = "sum of cons. < \naverage = -438\n(Mar.-July)") +
  annotate("text", x = "09-10", 45, label = "sum of cons. > \naverage = 215\n(Aug.-Oct.)") +
  theme(legend.position = c(0.88, 0.64)) +
  labs(title = title_total_vec, subtitle = subtitle_vec, x = NULL, y = NULL, caption = last_update)

plot_elec_total_3

# But variations in electricity consumption seem to be correlated 
# with temperatures variations.
temp2 <- elec2 |> 
  filter(var == "Average temperature", year %in% c("2020", "average \n2016-2019")) |> 
  mutate(value = value * 1e+09)

figures <- temp2 |> 
  pivot_wider(names_from = year, values_from = value) |> 
  mutate(
    min        = pmin(`2020`, `average \n2016-2019`),
    max        = pmax(`2020`, `average \n2016-2019`),
    above      = `2020` >= `average \n2016-2019`,
    changed    = is.na(lag(above)) | lag(above) != above,
    section_id = cumsum(changed),
    diff       = `2020` - `average \n2016-2019`
    ) |> 
  ungroup()

moy_spread_mar_july <- figures |> 
  filter(month_day <= "07-31") |> 
  select(`2020`, `average \n2016-2019`, diff) |> 
  pivot_longer(cols = everything(), names_to = "var", values_to = "value") |> 
  group_by(var) |> 
  summarise(mean = mean(value))

moy_spread_aout_oct <- figures |> 
  na.omit() |> 
  filter(month_day >= "08-01", month_day <= "10-31") |> 
  select(`2020`, `average \n2016-2019`, diff) |> 
  pivot_longer(cols = everything(), names_to = "var", values_to = "value") |> 
  group_by(var) |> 
  summarise(mean = mean(value))  

plot_elec_temp <- ggplot(data = temp2, aes(month_day, value, color = year, group = year))+
  geom_line(lwd = 1.2) +
  my_theme() +
  scale_x_discrete(breaks = breaks_vec, labels = labels_vec) +
  scale_color_manual(values = ghibli_palette("MononokeMedium")[5:1]) +
  geom_ribbon(data = filter(figures, above == FALSE), aes(month_day, ymin = min, ymax = max, group = section_id), fill = "#06141FFF", alpha = 0.2, inherit.aes = FALSE) +
  geom_ribbon(data = filter(figures, above == TRUE), aes(month_day, ymin = min, ymax = max, group = section_id), fill = "#CD4F38FF", alpha = 0.2, inherit.aes = FALSE) +
  annotate("text", x = "05-10", 24, label = "average of temperature deviations \nto the mean (Mar.-July) = +0.4") +
  annotate("text", x = "10-10", 24, label = "average of temperature deviations \nto the mean (Aug-Oct.) = +0.1") +
  theme(legend.position = "none") +
  labs(
    title = "Average temperature",
    subtitle = "in degrees, 7-day moving average",
    x = NULL, y = NULL,
    caption = last_update
    )

plot_elec_temp

g <- grid.arrange(plot_elec_total_3, plot_elec_temp, nrow = 2)

ggsave("03_elec-total.png", plot = g, path = fig_path, width = 8.5, height = 7)
graphics.off()

# We then look at electricity consumption by customer category for every year
ggplot(filter(elec2, !var %in% c("Total consumption", "Average temperature")), aes(month_day, value, color = year, group = year)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_discrete(breaks = breaks_vec, labels = labels_vec) +
  scale_color_manual(values = ghibli_palette("MononokeMedium")) +
  gghighlight(
    year != "average \n2016-2019",
    use_direct_label = FALSE,
    calculate_per_facet = TRUE,
    unhighlighted_params = list(color = "transparent")) +
  facet_wrap(facets = ~var, ncol = 2, scales = "free_y") +
  theme(legend.position = c(0.75, 0.25), axis.text = element_text(size = 10)) +
  labs(title = title_client_vec, subtitle = subtitle_vec, x = NULL, y = NULL, caption = last_update)

ggsave("04_elec-compo.png", path = fig_path, width = 8.5, height = 7)
graphics.off()

# and for 2020 and the average before
ggplot(filter(elec2,!var %in% c("Total consumption", "Average temperature")), aes(month_day, value, color = year, group = year)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_discrete(breaks = breaks_vec, labels = labels_vec) +
  scale_color_manual(values = ghibli_palette("MononokeMedium")[5:1]) +
  gghighlight(
    year %in% c("average \n2016-2019","2020"),
    use_direct_label = FALSE,
    calculate_per_facet = TRUE,
    unhighlighted_params = list(color = "transparent"))  +
  facet_wrap(facets = ~ var, ncol = 2, scales = "free_y") +
  theme(legend.position = c(0.78,0.14), axis.text = element_text(size = 10)) +
  labs(title = title_client_vec, subtitle = subtitle_vec, x = NULL, y = NULL, caption = last_update)

ggsave("05_elec-compo.png", path = fig_path, width = 8.5, height = 7)
graphics.off()

# Residential electricity consumption appears to capture a significant 
# portion of the variations due to temperature.
figures <- elec2 |> 
  filter(
    !var %in% c("Total consumption","Average temperature"), 
    year %in% c("2020","average \n2016-2019")
    ) |> spread(year,value) |> 
  group_by(var) |> 
  mutate(
    min        = pmin(`2020`, `average \n2016-2019`),
    max        = pmax(`2020`, `average \n2016-2019`),
    above      = `2020`>=`average \n2016-2019`,
    changed    = is.na(lag(above)) | lag(above) != above,
    section_id = cumsum(changed),
    diff       = `2020` - `average \n2016-2019`
    )

# 1st lockdown : 56 days
percent_first_lockdown <- elec2 |> 
  filter(
    !var %in% c("Total consumption", "Average temperature"),
    year %in% c("2020", "average \n2016-2019")
    ) |> 
  group_by(var) |> 
  filter(month_day >= "03-17", month_day <= "05-11") |> 
  group_by(year, var) |> 
  summarise(sum = sum(value)) |> 
  spread(year,sum) |> 
  mutate(
    average_percent = (`2020` - `average \n2016-2019`) / `average \n2016-2019`,
    average_per_day = (`2020` - `average \n2016-2019`) / 56
    ) 

# 2nd lockdown : 27 days
percent_second_lockdown <- elec2 |> 
  filter(
    !var %in% c("Total consumption", "Average temperature"),
    year %in% c("2020", "average \n2016-2019")
    ) |> 
  group_by(var) |> 
  filter(month_day >= "11-01", month_day <= "11-27") |> 
  group_by(year, var) |> 
  summarise(sum = sum(value)) |> 
  spread(year, sum) |> 
  mutate(
    average_percent = (`2020` - `average \n2016-2019`) / `average \n2016-2019`,
    average_per_day = (`2020` - `average \n2016-2019`) / 27
    ) 

ann_text <- tibble(
  x     = c("04-01", "04-01", "11-17", "11-17"),
  xend  = c("04-01", "04-01", "11-17", "11-17"),
  y     = c(12.5, 5.4, 5.6, 12.9),
  yend  = c(10.1, 4.2, 4.55, 12.15),
  var   = c(
    "Large enterprises", "Small and medium enterprises", 
    "Small and medium enterprises","Large enterprises"
    ),
  label = c(
    "on average -20% \nduring 1st lockdown",
    "on average -27% \nduring 1st lockdown",
    "on average -18% \nduring 2nd lockdown\n(November)",
    "on average -6% \nduring 2nd lockdown\n(November)"),
  ylab  = c(13.2, 5.9, 3.6, 11),
  xlab  = c("05-10", "05-30", "10-10", "10-10")
  )

ggplot(
  data = filter(elec2, !var %in% c("Total consumption", "Average temperature")), 
  aes(month_day, value, color = year, group = year)) +
  geom_line(lwd = 1.2) +
  my_theme() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_discrete(breaks = breaks_vec, labels = labels_vec) +
  scale_color_manual(values = ghibli_palette("MononokeMedium")[5:1]) +
  gghighlight(
    year %in% c("average \n2016-2019","2020"),
    use_direct_label = FALSE,
    calculate_per_facet = TRUE,
    unhighlighted_params = list(color = "transparent")) +
  facet_wrap(facets = ~ var, ncol = 2, scales = "free_y") +
  geom_ribbon(
    data = filter(figures, above == FALSE), 
    aes(month_day, ymin = min, ymax = max, group = section_id), 
    fill = "#06141FFF", 
    alpha = 0.2, 
    inherit.aes = FALSE
    ) +
  geom_ribbon(
    data = filter(figures, above == TRUE),
    aes(month_day, ymin = min, ymax = max, group = section_id), 
    fill = "#CD4F38FF", 
    alpha = 0.2, 
    inherit.aes = FALSE
    ) +
  geom_segment(
    data = ann_text,
    aes(x, xend = xend, y, yend = yend),
    inherit.aes = FALSE,
    arrow = arrow(angle = 40, length = unit(0.05, "inches")), size = 1, color = "darkred") +
  geom_text(
    data = ann_text,
    aes(xlab, ylab, label = label),
    inherit.aes = FALSE,
    linewidth = 3.6,
    color = "darkred"
    ) +
  theme(legend.position = c(0.78, 0.14), axis.text = element_text(size = 10)) +
  labs(title = title_client_vec, subtitle = subtitle_vec, x = NULL, y = NULL, caption = last_update)

ggsave("06_elec-compo.png", path = fig_path, width = 8.5, height = 7)
graphics.off()

# Large enterprises and SMEs experienced a net decrease 
# (-20% and -27% respectively) during the 1st lockdown. 

# The 2nd lockdown mainly impacts the electricity consumption of SMEs 
# (-18% on average in November), even if large enterprises are also affected (-6% on average).

# END