# taskschedluer ----
taskscheduleR::taskscheduler_create(
  taskname = "01_five-countries-data.R",
  rscript = fs::path_abs("R/01_five-countries-data.R"),
  schedule = "ONCE",
  starttime = format(Sys.time() + 2*60, "%H:%M")
)

fs::path_abs("") # C:/Users/matth/OneDrive/Dokumente/R-scripts/cepremap

taskscheduleR::taskscheduler_delete(taskname = "01_five-countries-data.R")


