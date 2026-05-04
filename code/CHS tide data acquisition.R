library(httr2)
library(tidyverse)

#------------------
# use the Integrated Water Level Service (IWLS) API from the Canadian Hydrographic Service to get water levels around Vancouver Island
#------------------

tides_f = 'data/tide_station_data.csv'

if(!file.exists(tides_f)){
  
  BASE_URL <- "https://api-iwls.dfo-mpo.gc.ca/api/v1"
  
  iwls_base <- request(BASE_URL) |>
    req_headers(accept = "application/json") |>
    req_user_agent("my-tides-project")
  
  
  # 1. Fetch EVERY station in the PAC (Pacific) region
  all_pac_stations <- iwls_base |>
    req_url_path_append("stations") |>
    req_url_query(`chs-region-code` = "PAC") |>
    req_perform() |>
    resp_body_json()
  
  #2. Convert to tibble, filter to only include stations around vancouver Island
  vi_stations <- all_pac_stations |>
    map_dfr(\(x) {
      # Extract available time series codes into a single string for easy filtering
      ts_codes <- x$timeSeries |> 
        map_chr(\(ts) ts$code) |> 
        paste(collapse = ", ")
      
      tibble(
        id              = x$id %||% NA_character_,
        code            = x$code %||% NA_character_,
        officialName    = x$officialName %||% NA_character_,
        alternativeName = x$alternativeName %||% NA_character_,
        latitude        = x$latitude %||% NA_real_,
        longitude       = x$longitude %||% NA_real_,
        operating       = x$operating %||% FALSE,
        type            = x$type %||% NA_character_,
        available_ts    = ts_codes
      )
    }) |>
    # Apply spatial filter for Vancouver Island
    filter(
      latitude >= 48.2, latitude <= 51.5,
      longitude >= -129.5, longitude <= -122.5
    #only include stations with high-low tide event data (filters out current stations)
    , str_detect(available_ts, 'wlp-hilo')
    )
  
  #3. Iterate over staton IDs, get times of high/low tides, combine all into dataframe 
  library(progressify)
  
  tide_station_data_df = lapply(1:nrow(vi_stations), function(i){
    
    station_id = vi_stations$id[i]
    
    cat('\nProcessing',i,',',station_id)
    
    meta <- iwls_base |>
      req_url_path_append("stations", station_id, "metadata") |>
      req_perform() |>
      resp_body_json()
    
    # Get tide data in dataframe
    tide_events_df <- iwls_base |>
      req_url_path_append("stations", station_id, "data") |>
      req_url_query(
        `time-series-code` = "wlp-hilo",
        from = "2026-05-31T00:00:00Z",
        to   = "2026-09-30T00:00:00Z"
      ) |>
      req_perform() |>
      resp_body_json() |>
      map_dfr(\(x) tibble(
        time       = x$eventDate,
        level_m    = x$value,
        qcFlagCode = x$qcFlagCode
      )) 
    
    if(nrow(tide_events_df) != 0){
    
    tide_events_df = tide_events_df |>
      mutate(
        time      = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        date = as.Date(time),
        time_only = format(time, '%H:%M:%S')
         # ,tide_type = if_else(level_m == max(level_m) | level_m > lag(level_m, default = 0) & level_m > lead(level_m, default = 0),
         #                    "High", "Low")
      )
    
    
    Sys.sleep(2)  # ensures ≤ 30 requests per minute
    
    
    station_info = vi_stations[i,]
    
    tide_events_df = cbind(tide_events_df,station_info)
    
    return(tide_events_df)
    } else {
      cat(' SKIPPING')
      return(NULL)
    }
  }) |> 
    progressify::progressify() |>
    bind_rows()
  
  write_csv(tide_station_data_df, tides_f)
  
} else {
  
  tide_station_data_df = read_csv(tides_f)
  
}

cat('Tide data: tide_station_data_df')
