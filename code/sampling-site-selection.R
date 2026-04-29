###    SAMPLING SITE SELECTION    ###

library(tidyverse)
library(sf)
library(pbapply)
library(tidyterra)

#----load eelgrass data----

#BC CRIMS
{
  eg_crims_f = "data/BCGW_02001F02_1773528403579_6052/CRIMS_EELGRASSES.gdb"
  st_layers(eg_crims_f)
  eg_crims = st_read(eg_crims_f)

}


#NETforce data
{
  eg_datasets = list.files('data', pattern = '\\.gdb$', full.names = T)
  eg_datasets = eg_datasets[str_detect(eg_datasets, 'National_Eelgrass_Dataset')]
  nshelf_f = eg_datasets[1]
  sshelf_f = eg_datasets[2]
  st_layers(nshelf_f) #see layers
  
  #define function for loading data
  eg_load = function(file, aoi=NULL){
    lyrs = st_layers(file)[['name']]
    
    dat = lapply(lyrs, function(n){
      d = st_read(file, layer=n) |>
        st_make_valid()
      
      if(!is.null(aoi)){
        d = d |> st_intersection(aoi)
      }
      
      return(d)
    })
    names(dat) = lyrs
    return(dat)
  }
  
  # ns_dat = eg_load(file=nshelf_f, aoi) #the North Shelf data does not intersect with the area of interest at all
  ss_dat = eg_load(file = sshelf_f, aoi) #south shelf data
  
  eg_aerimg = ss_dat[[2]] #Aerial image dataset
  eg_aervid = ss_dat[[1]] #Aerial video dataset
  
}

#----load marine trails data----

mt_sites_df = 'data/csv_datasets/BCMT Sites (Proximity).csv' |> 
  read_csv() |>
  #filter out values with
  filter(!is.na(Latitude)&!is.na(Longitude))


mt_sites <- st_as_sf(
  mt_sites_df,
  coords = c("Longitude", "Latitude"), # which columns to use
  crs = 4326,                          # WGS84 (lat/long)
  # coords = c('x', 'y'),
  # crs = 3005,
  remove = FALSE                       # keep original columns
)
mt_sites = mt_sites |> 
  filter(!Description %in% c('Day Use',
                             'Launch Site',
                             'nan',
                             'Closed Site',
                             'Ferry Service'))

mt_sites = st_transform(mt_sites, st_crs(eg_crims))


#----calculate distance of each eelgrass meadow from each marine trails site----

#for each eelgrass meadow, calculate distance (m) to each marine trails site
dists_l = pblapply(1:nrow(eg_crims), function(i){
  eg = eg_crims[i,]
  dists = st_distance(eg, mt_sites) |> as.numeric()
  dists_df_ = data.frame(EELGRASS_ID = eg_crims$EELGRASS_ID[i], mt_sites_Title = mt_sites$Title, distance = dists)
})
dists_df = bind_rows(dists_l)

#----filter potential sites----

max_dist = 3000 #define maximum distance (m) that an eelgrass meadow can be from a campsite

dists_df_filt = dists_df |> filter(distance <= max_dist)

#get number of eelgrass meadows that are close to campsites
length(unique(dists_df_filt$EELGRASS_ID))

#filter sf object to only include meadows that are close to marine trails sites
close_eg = eg_crims |> filter(EELGRASS_ID %in% dists_df_filt$EELGRASS_ID)

#----load vancouver island polygon for plotting map----

#point centered on Vancouver island
geojson_text = '{
  "type": "FeatureCollection",
  "features": [
    {
      "type": "Feature",
      "properties": {},
      "geometry": {
        "coordinates": [
          -125.43198326189508,
          49.549616517761166
        ],
        "type": "Point"
      }
    }
  ]
}'

geojson_text2 = '{
  "type": "FeatureCollection",
  "features": [
    {
      "type": "Feature",
      "properties": {},
      "geometry": {
        "coordinates": [
          -125.6,
          49.6
        ],
        "type": "Point"
      }
    }
  ]
}'

geojson_text3 = '{
  "type": "FeatureCollection",
  "features": [
    {
      "type": "Feature",
      "properties": {},
      "geometry": {
        "coordinates": [
          -126.0,
          49.6
        ],
        "type": "Point"
      }
    }
  ]
}'
aoi_point3 = st_read(geojson_text3)

aoi_point = st_read(geojson_text)
aoi_point2 = st_read(geojson_text2)

aoi = aoi_point3 |>
  st_transform(3005) |>
  st_buffer(dist=210000) |>
  st_transform(4326) |>
  st_make_valid()

aoi_box = st_bbox(aoi)

aoi = aoi_box |> st_as_sfc(crs=4326) |> st_as_sf()

land <- ne_download(scale = 10, type = "land", category = "physical",
                    returnclass = "sf") |>
  st_make_valid()

sf_use_s2(use_s2=F)
bc_land = st_crop(land, aoi) |> st_transform(crs = st_crs(close_eg))
aoi = aoi |> st_transform(crs = st_crs(close_eg))


#----plot locations of eelgrass meadows----
ggplot() +
  #plot area of interest
  geom_sf(data=aoi, color='black', fill='darkgrey')+
  #plot basemap
  geom_sf(data=bc_land, color='black', fill='black')+
  #plot eelgrass meadows
  geom_sf(data = close_eg |> st_centroid(), color = 'red')

#----leaflet map----

library(leaflet)
library(sf)

# Convert to WGS84 (leaflet requires lat/lng)
aoi_wgs <- st_transform(aoi, 4326)
close_eg_centroids_wgs <- st_transform(st_centroid(close_eg), 4326)
close_eg_wgs = st_transform(close_eg, 4326)
mt_sites_wgs = mt_sites |> filter(Title %in% dists_df_filt$mt_sites_Title) |> st_transform(4326)

leaflet() |>
  # Google Satellite basemap
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}") |>
  # # Plot area of interest
  addPolygons(
    data = close_eg_wgs,
    color = "black",
    fillColor = "darkgrey",
    fillOpacity = 0.7,
    weight = 1
  ) |>
  # Plot eelgrass meadow centroids
  addCircleMarkers(
    data = close_eg_centroids_wgs,
    color = "red",
    radius = 5,
    fillOpacity = 1
  ) |>
  #add marine trails sites
  addCircleMarkers(
    data = mt_sites_wgs,
    color = "purple",
    radius = 3,
    fillOpacity = 1
  )
