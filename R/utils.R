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
  
  dplyr::select(.data = x, dplyr::all_of(cols))
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
    scale_y_continuous(labels = function(x) { format(x, big.mark = " ")}),
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
      geom  = "text",
      label = "DBnomics <http://db.nomics.world>",
      x = structure(.Data = Inf, class = "Date"),
      y = -Inf,
      hjust = 1.1,
      vjust = -0.4,
      col = "grey",
      fontface = "italic"
    )
  )
}


## display_table ----
display_table <- function(DT) {
  kable(DT) |> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |> 
    kable_paper() |> 
    kableExtra::column_spec(column = 1:ncol(DT), width_min = "4cm") |> 
    kableExtra::scroll_box(width = "100%", height = "500px")
}


## Chaining function ----
# To chain tow datasets, we build a custom `chain()` function whose
# inputs must be two data frames with three standard columns
# `period`, `var`, and `value`.

# It returns a data frame composed of chained values,
# i.e. the dataframe "to rebase" will be chained to the "basis" dataframe.

# More specifically, the function:
# - computes the growth rates from `value` in the dataframe of the 1st argument
# - multiplies it with the value of a reference chosen in `value` in the dataframe of the 2nd argument
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
