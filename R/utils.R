# User-defined functions ----

## reorder_cols ----
reorder_cols <- function(x) {
  cols <- c(
    "provider_code", "dataset_code", "dataset_name",
    "series_code", "series_name", 
    "original_period", "period",
    "original_value", "value",
    "@frequency"
  )
  
  if ("unit" %in% colnames(x)) {
    cols <- c(cols, "unit", "Unit")
  }
  
  if ("geo" %in% colnames(x)) {
    cols <- c(cols, "geo", "Country")
  }
  
  if ("freq" %in% colnames(x)) {
    cols <- c(cols, "freq", "Frequency")
  }
  
  cols_add <- setdiff(colnames(x), cols)
  cols <- c(cols, cols_add)
  
  cols <- cols[cols %in% colnames(x)]
  
  cols <- match(x = cols, table = colnames(x))
  
  select(.data = x, all_of(cols))
}

## scale_color_discrete ----
scale_color_discrete <- function(...) {
  scale_color_brewer(palette = "Set1")
}

## dbnomics -----
# The original used "transparent" instead of "white" backgrounds.
dbnomics <- function() {
  list(
    scale_x_date(expand = c(0, 0)),
    scale_y_continuous(labels = function(x) { format(x, big.mark = " ") }),
    xlab(""),
    ylab(""),
    theme_bw(),
    theme(
      legend.position   = "bottom",
      legend.direction  = "vertical",
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.title      = element_blank()
    ),
    annotate(
      geom     = "text",
      label    = "DBnomics <http://db.nomics.world>",
      x        = structure(.Data = Inf, class = "Date"),
      y        = -Inf,
      hjust    =  1.1,
      vjust    = -0.4,
      col      = "grey",
      fontface = "italic"
    )
  )
}

## theme ----
theme <- theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 15),
    title            = element_text(size = 16),
    panel.border     = element_blank(),
    panel.grid.major = element_line(linewidth = 1),
    legend.key       = element_rect(colour = "white"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 10),
    axis.text        = element_text(size = 10),
    plot.title       = element_text(hjust = 0.5)
    )

## blue_obs_macro ----
blue_obs_macro <- "#0D5BA4"

## display_table ----
display_table <- function(DT) {
  kable(DT) |> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |> 
    kable_paper() |> 
    column_spec(column = 1:ncol(DT), width_min = "4cm") |> 
    scroll_box(width = "100%", height = "500px")
}

## chain ----
# To chain tow datasets, we build a custom `chain()` function whose
# inputs must be two data frames with three standard columns
# `period`, `var`, and `value`.

# It returns a data frame composed of chained values,
# i.e. the data frame "to rebase" will be chained to the "basis" data frame.

# More specifically, the function:
# - computes the growth rates from `value` in the data frame of the 1st argument
# - multiplies it with the value of a reference chosen in `value` in the data frame of the 2nd argument
# - at the `date` specified in the 3rd argument.
chain <- function(to_rebase, basis, date_chain) {
  
  date_chain <- as.Date(date_chain, format = "%Y-%m-%d")
  
  valref <- basis |> 
    filter(period == date_chain) |> 
    mutate(
      var,
      value_ref = value,
      .keep = "none"
    )
  
  res <- to_rebase |> 
    filter(period <= date_chain) |> 
    arrange(desc(period)) |> 
    group_by(var) |> 
    mutate(
      growth_rate = c(1, value[-1] / lag(value)[-1])
    ) |> 
    full_join(y = valref, by = "var") |> 
    group_by(var) |> 
    mutate(
      period,
      value = cumprod(growth_rate) * value_ref,
      .keep = "none"
    ) |> 
    ungroup() |> 
    bind_rows(filter(basis, period > date_chain)) |> 
    arrange(period)
  
  return(res)
}

## deseason ----
# Convert data frame into `ts` time object, apply methods 
# of `seasonal` R package, converts back to data frame.
deseason <- function(source_df = data_large, var_arrange = "pubcons", ...) {
  
  local_data <- source_df
  
  detrend <- local_data[var_arrange] |> 
    ts(start = c(1980, 1), frequency = 4) |> 
    seas(na.action = na.exclude)
  
  res <- as.numeric(final(detrend))
  
  res <- tibble(
    value  = res,
    period = local_data$period,
    var    = var_arrange
  ) |> 
    arrange(period) |> 
    mutate(period, var, value, .keep = "none")
  
  return(res)
}