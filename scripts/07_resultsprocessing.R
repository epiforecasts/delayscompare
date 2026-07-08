library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts <- "latest"
raw_dir <- here("results", "raw")
agg_dir <- here("results", "agg")

# Aggregate the 6x6 GT x INC grid for one (disease, scenario) into results/agg/.
# read_latest() re-binds each cell's per-timepoint files.
aggregate_scenario <- function(disease, scen) {
  types <- c("samples", "id", "R", "diagnostics", "warnings", "timing")
  acc <- setNames(vector("list", length(types)), types)

  for (gt in 1:6) for (inc_val in 1:6) {
    cell <- paste0(disease, "_", scen, "_", rt_opts)
    suffix <- paste0("_gt", gt, "_inc", inc_val)
    got <- tryCatch({
      list(
        id       = read_latest(raw_dir, paste0(cell, "_id", suffix)),
        samples  = read_latest(raw_dir, paste0(cell, "_samples", suffix)),
        R        = read_latest(raw_dir, paste0(cell, "_R", suffix)),
        diagnostics = tryCatch(read_latest(raw_dir, paste0(cell, "_diagnostics", suffix)), error = function(e) NULL),
        warnings = tryCatch(read_latest(raw_dir, paste0(cell, "_warnings", suffix)), error = function(e) NULL),
        timing   = tryCatch(read_latest(raw_dir, paste0(cell, "_timing", suffix)), error = function(e) NULL)
      )
    }, error = function(e) {
      message("Missing: ", disease, " ", scen, " gt=", gt, " inc=", inc_val)
      NULL
    })
    if (is.null(got)) next

    for (ty in types) {
      x <- got[[ty]]
      if (!is.null(x) && is.data.frame(x) && nrow(x) > 0) {
        x$gt <- gt
        x$inc <- inc_val
        acc[[ty]] <- c(acc[[ty]], list(x))
      }
    }
  }

  if (length(acc$id) == 0) {
    message("No cells found for ", disease, " ", scen)
    return(invisible())
  }
  for (ty in types) {
    if (length(acc[[ty]]) > 0) {
      save_latest(bind_rows(acc[[ty]]), agg_dir,
                  paste0(disease, "_", scen, "_", rt_opts, "_", ty))
    }
  }
  message("Saved: ", disease, " ", scen, " (", length(acc$id), " cells)")
}

for (disease in c("ebola", "covid", "cholera")) {
  message("\n=== Processing ", disease, " ===\n")
  for (scen in c("const_low", "const_high", "inc", "dec", "resim", "casestudy")) {
    aggregate_scenario(disease, scen)
  }
}

message("\n=== Results processing complete ===\n")
