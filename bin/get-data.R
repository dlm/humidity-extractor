require("RCurl")


fetch_years <- function(url) {
  url_data <- getURL(noaa_ftp, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly=TRUE)
  files <- strsplit(url_data, "\n")[[1]]
  files_as_numbers <- as.numeric(files)
  years <- as.character(files_as_numbers[!is.na(files_as_numbers)])
  return(years)
}

noaa_ftp <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/"
years <- fetch_years(noaa_ftp)
print(years)


