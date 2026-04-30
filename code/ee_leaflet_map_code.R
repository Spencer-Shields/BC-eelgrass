library(tidyverse)


#----load trip schedule data from google sheets----
library(googlesheets4)

gs4_auth(email = FALSE, cache = FALSE,
         scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

schedule_df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1_uK-_hyLVkg2MFWS-8iTIfdCl7BAdpsPL8jZ5MvK_Eg/edit",
  sheet = "0.3"
)

schedule_df <- schedule_df |>
  mutate(
    Leg = sapply(Leg, \(x) if (is.null(x)) NA else as.character(x)),
    `Leg day` = sapply(`Leg day`, \(x) if (is.null(x)) NA else as.character(x))
  ) |>
  filter(!is.na(`End location`))

#----make schedule into sf object with points for stops/campsites----

library(sf)

schedule_sf <- schedule_df |>
  separate(`End lat lon`, into = c("End_lat", "End_lon"), sep = ", ", convert = TRUE, remove =F) |>
  st_as_sf(coords = c('End_lon', 'End_lat'), crs = 4326) |>
  mutate(Date = as.Date(Date),
         `Daily distance (km)` = round(`Distance (km)`,1),
         `Cumulative distance (km)` = round(`Cumulative distance (km)`,1)
  ) |>
  select(-`Distance (nm)`)

#----load other data----

#marine trails data
bcmt_sf = read_sf('data/BCMT Sites (Proximity).geojson')
if(st_crs(bcmt_sf) != st_crs(schedule_sf)){
  bcmt_sf = st_transform(bcmt_sf, st_crs(schedule_sf))
}

bcmt_df = bcmt_sf |> st_drop_geometry()

#eelgrass sampling location polygons
eg_sf = read_sf('data/eelgrass_sampling_locations.shp')
if(st_crs(eg_sf) != st_crs(schedule_sf)){
  eg_sf = st_transform(eg_sf, st_crs(schedule_sf))
}

eg_df = eg_sf |> st_drop_geometry()

#----make leaflet map with crosstalk dataframe----
library(leaflet)
library(crosstalk)
library(DT)

#helper function to make popups
make_popup <- function(row) {
  vals <- as.list(row)
  paste0(
    "<div style='max-height:200px; overflow-y:auto;'>",
    paste(
      paste0("<b>", names(vals), ":</b> ", vals),
      collapse = "<br>"
    ),
    "</div>"
  )
}

eg_sf$popup <- unname(apply(eg_df, 1, make_popup))
bcmt_sf$popup <- unname(apply(bcmt_df, 1, make_popup))

{
  
  #make shared data for map and table
  shared_schedule <- SharedData$new(schedule_sf)
  
  #make leaflet map
  leaflet_map <- leaflet() |>
    addTiles() |>
    #expedition eelgrass trip plan data
    addCircleMarkers(
      data = shared_schedule,
      group = "Expedition stops",
      popup = ~paste0(
        "<b>", `End location`, "</b><br>",
        `End lat lon`, "<br>",
        "Day ", `Trip day`, " — ", Date, "<br>",
        `Daily agenda`
      ),
      radius = 6,
      color = "black",
      weight = 1.5,
      fillColor = "steelblue",
      fillOpacity = 1,   # unselected points are faint
      opacity = 1        # border also faint when unselected
    ) |>
    #eelgrass sampling locations
    addPolygons(
      data = eg_sf,
      group = 'Eelgrass sampling locations',
      popup = eg_sf$popup,
      fillColor = 'purple',
      color = 'black'
    ) |>
    #bc marine trails data
    addCircleMarkers(
      data = bcmt_sf,
      popup = bcmt_sf$popup,
      group = "BC Marine Trails sites",
      fillColor = "firebrick",
      color='black',
      weight = 1.5,
      radius = 4,
      fillOpacity = 0.5
    ) |>
    #bc parks and protected areas
    addWMSTiles(
      baseUrl = "https://openmaps.gov.bc.ca/geo/pub/WHSE_TANTALIS.TA_PARK_ECORES_PA_SVW/ows",
      layers = "pub:WHSE_TANTALIS.TA_PARK_ECORES_PA_SVW",
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        version = "1.3.0"
      ),
      attribution = "DataBC",
      group = "BC Parks and Protected Areas"
    )|>
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = 'kilometers',
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = 'hectares',
      activeColor = "#FFA500",
      completedColor = "#FFDBBB"
    ) |>
    #control layer visibility
    addLayersControl(
      overlayGroups = c("Expedition stops", "Eelgrass sampling locations", "BC Marine Trails sites", "BC Parks and Protected Areas"),
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    hideGroup("BC Marine Trails sites") |>
    hideGroup("BC Parks and Protected Areas")|>
    hideGroup('Eelgrass sampling locations')
  
  
  #make table that points to leaflet map
  table <- datatable(
    shared_schedule,
    extensions = "Scroller",
    style = "bootstrap",
    width = "100%",
    rownames = F,
    options = list(
      deferRender = TRUE,
      scrollY = 300,
      scroller = TRUE
    )
  )
  
  #combine table and map
  highlight_css <- htmltools::tags$style("
  .leaflet-interactive.crosstalk-map-selected {
    fill: yellow !important;
    fill-opacity: 1 !important;
    stroke: red !important;
    stroke-width: 3px !important;
  }
")
  
  combined <- bscols(
    widths = 12,
    htmltools::tagList(highlight_css, leaflet_map),
    table
  )
  combined
}


#----export html file for webmap----

htmltools::save_html(combined, 'expedition_map.html')
