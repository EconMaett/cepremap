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
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key        = element_blank(),
      panel.background  = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA),
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

