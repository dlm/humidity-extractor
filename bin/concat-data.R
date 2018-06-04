append_file <- function(out_fname, x, first) {
  x_data <- read.csv(x)
  write.table(
    x_data,
    out_fname,
    append=!first,
    col.names=first,
    row.names=FALSE,
    sep=","
  )
}


aggregate_files <- function(input_dir, output_file) {
  first <- TRUE
  x <- lapply(
    list.files(input_dir, full.names=TRUE, recursive=TRUE),
    function(fname) {
      append_file(output_file, fname, first)
      first <<- FALSE
    }
  )
}

input_dir <- "noaa_data"
output_file <- "noaa_agg_data.csv"
aggregate_files(input_dir, output_file)
