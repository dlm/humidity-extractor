library("RCurl")
library("weathermetrics")

fetch_years <- function(url) {
  url_data <- RCurl::getURL(url, ftp.use.epsv=TRUE, dirlistonly=TRUE)
  files <- strsplit(url_data, "\n")[[1]]
  files_as_numbers <- as.numeric(files)
  years <- as.character(files_as_numbers[!is.na(files_as_numbers)])
  return(years)
}

make_year_url <- function(url, year) {
  year <- paste(url, year, "/", sep="")
  return(year)
}

get_station_list <- function(url, file_loc=9) {
  # If using getURL doesn't work reliably, check out
  # this stack overflow post https://goo.gl/HZp3qP
  url_data <- RCurl::getURL(url, ftp.use.epsv=TRUE)
  files <- strsplit(url_data, "\n")[[1]]
  ss <- strsplit(files, "\\s+")
  file_names <- rapply(ss, function(x) { x[file_loc] })
  return(file_names)
}

make_station_url <- function(url, year) {
  year <- paste(url, year, sep = "")
  return(year)
}

make_station_name <- function(file_name) {
  station_name <- gsub(".gz", "", file_name)
  return(station_name)
}

download_station_file <- function(url) {
  dst <- paste(tempfile(), ".gz", sep = "")
  download.file(url, dst)
  return(dst)
}

load_station_data <- function(path, sentinal=-9999) {
  data <- read.table(path,
    colClasses = c(rep("integer", 6), rep("NULL",6)),
    col.names = c("year", "month", "day", "hour", "air_temp", "dew_point", rep("x", 6))
  )

  # replace sentinal values with NA to make it easier to work with data
  data$dew_point[data$dew_point == sentinal] <- NA
  data$air_temp[data$air_temp == sentinal] <- NA

  # massage the data to year, temp, dew point, relative humidity
  data$date <- sprintf("%4d-%02d-%02d", data$year, data$month, data$day)
  data$air_temp <- data$air_temp / 10.0
  data$dew_point <- data$dew_point / 10.0
  data$relative_humidity <- weathermetrics::dewpoint.to.humidity(
    t = data$air_temp,
    dp = data$dew_point,
    temperature.metric = 'celsius'
  )

  return(data[,c("date", "air_temp", "dew_point", "relative_humidity")])
}

aggregate_station_data <- function(data, station) {
  aggregated_data <- aggregate(
    data[c("air_temp", "dew_point", "relative_humidity")],
    by = list(data$date),
    FUN = function(x) c(min=min(x), max=max(x), avg=mean(x))
  )
  aggregated_data$station <- rep(station, nrow(aggregated_data))
  colnames(aggregated_data)[1] <- "year"
  return(aggregated_data)
}


noaa_ftp <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/"
year_i <- 90
station_file_i <- 25

years <- fetch_years(noaa_ftp)
print(years)

year <- years[year_i]
year_url <- make_year_url(noaa_ftp, year)
station_files <- get_station_list(year_url)

station_file_name <- station_files[station_file_i]
station_url <- make_station_url(year_url, station_file_name)
station_name <- make_station_name(station_file_name)
station_url <- make_station_url(year_url, station_file_name)

local_file_name <- download_station_file(station_url)
station_data <- load_station_data(local_file_name)
final_data <- aggregate_station_data(station_data, station_name)
