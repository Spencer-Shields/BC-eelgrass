library(httr2)
library(tidyverse)

#------------------
# use the Integrated Water Level Service (IWLS) API from the Canadian Hydrographic Service to get water levels around Vancouver Island
#------------------

BASE_URL <- "https://api-iwls.dfo-mpo.gc.ca/api/v1"

iwls_base <- request(BASE_URL) |>
  req_headers(accept = "application/json") |>
  req_user_agent("my-tides-project")


# Query active stations with data in a given window
# Filter by bounding box for Vancouver Island (rough bounds)
all_stations <- iwls_base |>
  req_url_path_append("stations") |>
  req_url_query(
    dateStart = "2025-04-01T00:00:00Z",
    dateEnd   = "2025-04-07T23:59:59Z"
  ) |>
  req_perform() |>
  resp_body_json() |>
  map(\(x) as_tibble(x)) |>
  list_rbind()

# Filter to Vancouver Island region
vi_stations <- all_stations |>
  filter(
    latitude  <= 51.0,
    longitude <= -122
  ) |>
  select(id, code, officialName, latitude, longitude, operating) |>
  distinct()

print(vi_stations)

# station_id <- "5cebf1df3d0f4a073c4bbb44"  # Example: Victoria
station_id = vi_stations$id[1]

meta <- iwls_base |>
  req_url_path_append("stations", station_id, "metadata") |>
  req_perform() |>
  resp_body_json()

# View available time series codes
meta$timeSeries |> map_dfr(\(ts) tibble(
  code     = ts$code,
  nameEn   = ts$nameEn
))

# Check which station this is
vi_stations |> filter(id == station_id) |> select(code, officialName)

# Get high/low tide predictions for a given range of times
hilo <- iwls_base |>
  req_url_path_append("stations", station_id, "data") |>
  req_url_query(
    `time-series-code` = "wlp-hilo",
    from = "2026-06-01T00:00:00Z",
    to   = "2026-09-15T00:00:00Z"
  ) |>
  req_perform() |>
  resp_body_json() |>
  map_dfr(\(x) tibble(
    time       = x$eventDate,
    level_m    = x$value,
    qcFlagCode = x$qcFlagCode
  )) |>
  mutate(
    time      = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    tide_type = if_else(level_m == max(level_m) | level_m > lag(level_m, default = 0) & level_m > lead(level_m, default = 0),
                        "High", "Low")
  )

print(hilo)
