suppressMessages(library(tidyverse))
library(rvest)
library(glue)

find_routes <- function(airport_code = "LGW", value_year = 2023, value_month = 11){
  
  value_date_start <- make_date(year = value_year, month = value_month, day = 1)
  value_date_end <- rollforward(value_date_start)
  
  value_link <- glue("https://www.flightsfrom.com/{airport_code}/destinations?dateMethod=month&dateFrom={value_date_start}&dateTo={value_date_end}")
  
  airports <- read_html(value_link) |>
    html_nodes(".airport-content-destination-list-name") |> 
    html_text() |> 
    str_trim()
  
  times <- read_html(value_link) |>
    html_nodes(".airport-content-destination-list-time") |> 
    html_text() |> 
    str_trim()
  
  data_raw <- tibble(airports = airports, times = times, origin = airport_code, value_year = value_year, value_month = value_month) |> 
    rownames_to_column(var = "size_relative")
  
  return(data_raw)
  
}

# I then scrape this data for the months and origin airports that I need.

# data_airports <- tibble(value_airports = c("LGW", "LHR"))

# value_date_original <- make_date(year = 2023, month = 11, day = 1)

# data_parameters <- tibble(
#   date_start = value_date_original %m+% months(0:11),
#   date_year = year(date_start),
#   date_month = lubridate::month(date_start)
# ) |> 
#   cross_join(data_airports) |> 
#   select(value_airports, date_year, date_month)

# data_raw <- pmap(
#   .l = data_parameters, 
#   .f = ~find_routes(airport_code = ..1, value_year = ..2, value_month = ..3)
# ) |> 
#   bind_rows()

# readr::write_rds(x = data_raw, file = "data_raw.rds", compress = "xz")

# Finally, I tidy this data into a table of destinations from Heathrow and Gatwick for a given month.

data_destinations <- readRDS(file = "data_raw.rds") |> 
  rowwise() |> 
  mutate(
    code = str_extract(airports, '[[:upper:]][[:upper:]][[:upper:]]'),
    position = str_locate_all(airports, '[[:upper:]][[:upper:]][[:upper:]]')
  ) |> 
  separate(col = times, into = c('split', 'detail'), sep = ": ") |> 
  separate(col = detail, into = c('hours', 'mins'), sep = "h ") |> 
  rowwise() |> 
  mutate(
    mins = as.integer(str_sub(mins, start = 1L, end = -2L)),
    hours = as.integer(hours),
    minutes = (60 * hours) + mins,
    position1 = position[[1]] - 2, 
    position2 = position[[2]] + 2,
    city = str_trim(str_to_lower(str_sub(string = airports, start = 1L, end = position1))),
    country = str_trim(str_to_lower(str_sub(string = airports, start = position2, end = -1L))),
    value_month_start = make_date(value_year, value_month, 1)
  ) |> 
  select("code_dest" = code, minutes, city, country, value_month, value_year, value_month_start) |> 
  ungroup() |> 
  mutate(
    country = case_when(
      country == "a" ~ "usa",
      country == "r" ~ "china", 
      .default = country
    )
  ) |> 
  group_by(value_month_start, code_dest) |> 
  slice(1) |> 
  ungroup()


# I then add longitude and latitude data for each airport.

data_airport_detail <- read_csv(file = "airports.csv") |> 
  select(iata_code, type, scheduled_service, latitude_deg, longitude_deg, iso_country) |> 
  filter(
    !is.na(iata_code),
    type %in% c("large_airport", "medium_airport"),
    scheduled_service == "yes"
  ) |> 
  select(-type, -scheduled_service)

data_destinations_detail <- data_destinations |> 
  left_join(
    y = data_airport_detail, 
    by = join_by(code_dest == iata_code)
  ) |> 
  select(-value_year, -value_month_start) |> 
  arrange(code_dest, value_month)


# Doing so generates the following data on distinct airport destinations.

data_destinations_distinct <- data_destinations_detail |> 
  distinct(code_dest, longitude_deg, latitude_deg) |> 
  arrange(code_dest)


### Temperature data
  
# I now import the temperature data.

data_temp <- read_csv(
  file = "global_temperatures_by_city.csv",
  col_select = -AverageTemperatureUncertainty
) |> 
  mutate(
    value_year = year(dt),
    value_month = month(dt)
  ) |> 
  filter(
    value_year >= 2000
  ) |> 
  summarise(
    AvTemp_monthly_mean = mean(AverageTemperature, na.rm = TRUE), 
    .by = c(City, Country, Latitude, Longitude, value_month)
  ) 


# I then clean the longitude and latitude for these cities.

data_temp_locations <- data_temp |> 
  separate_wider_regex(
    cols = Longitude, 
    c(
      long_level = ".*", 
      long_heading = "[:upper:]$"
    ), 
    too_few = "align_start", 
    cols_remove = FALSE
  ) |> 
  separate_wider_regex(
    cols = Latitude, 
    c(
      lat_level = ".*", 
      lat_heading = "[:upper:]$"
    ), 
    too_few = "align_start", 
    cols_remove = FALSE
  ) |> 
  mutate(
    lon = if_else(
      long_heading == "E", 
      as.numeric(long_level), 
      -1 * as.numeric(long_level)
    ),
    lat = if_else(
      lat_heading == "N", 
      as.numeric(lat_level), 
      -1 * as.numeric(lat_level)
    )
  ) |> 
  distinct(Country, City, lon, lat) |> 
  arrange(Country, City)


### Joining destinations with temperatures
  
# Given all this data, I can now find the closest `City` (in the temperature data of `data_temp_locations`) to each `code_dest` entry in the `data_destinations_detail` data of unique destinations. If you like, this is akin to a spatial join based upon geographical proximity, as [shown here](https://stackoverflow.com/questions/27321856/closest-point-to-a-path).

library(RANN)

lookup_cities <- data_temp_locations |> 
  select(Country, City) |> 
  rownames_to_column(var = "value_ID") |> 
  mutate(value_ID = as.integer(value_ID), .before = Country)

closest <- nn2(
  data = data_temp_locations[, c("lon", "lat")], 
  query = data_destinations_distinct[, c("longitude_deg", "latitude_deg")], 
  k = 1)

airport_city_lookup <- tibble(
  airport_origin = data_destinations_distinct$code_dest,
  most_proximate = as.integer(closest[["nn.idx"]]),
  distance = as.numeric(closest[["nn.dists"]])
) |> 
  left_join(lookup_cities, by = join_by(most_proximate == value_ID)) |> 
  select(airport_origin, "country_proxy" = Country, "city_proxy" = City)

# With this lookup table, I can now connect the destinations with the temperatures of their nearest city in a given month.

data_hols <- data_destinations_detail |> 
  left_join(
    airport_city_lookup, 
    by = join_by(code_dest == airport_origin)
  ) |> 
  left_join(
    data_temp, 
    by = join_by(
      country_proxy == Country, 
      city_proxy == City, 
      value_month == value_month
    )
  ) |> 
  select(code_dest, country, city, value_month, minutes, "temperature" = AvTemp_monthly_mean, longitude_deg, latitude_deg) |> 
  arrange(country, city, value_month) |> 
  sf::st_as_sf(
    coords = c("longitude_deg", "latitude_deg"), 
    crs = sf::st_crs(4326)
  )

save(data_hols, file = "data_hols.RData")
