library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(geodata)
library(osmextract)
library(jsonlite)
library(pbapply)
library(ggmap)
library(terra)
library(tidyterra)

#----define sf object for AOI----

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

#----get coastline polygons that fall within AOI----

# world = ne_countries(scale='large') |> st_make_valid()
# if(st_crs(world) != st_crs(aoi)){
#   world = st_transform(world, crs=st_crs(aoi))
# }
# 
# aoi_bm = st_intersection(world, aoi)

# coast_box = oe_get("British Columbia", #accidentally got whole province
#                    layer = "multipolygons",
#                    boundary = aoi,
#                    boundary_type = 'clipsrc'
#                    )
# coast = coast_box |> st_make_valid()
# coast_aoi = st_intersection(coast, aoi)

land <- ne_download(scale = 10, type = "land", category = "physical",
                    returnclass = "sf") |>
  st_make_valid()

sf_use_s2(use_s2=F)
bc_land = st_crop(land, aoi)

# can_coast = st_read('data/lhy_000h16a_e/lhy_000h16a_e.shp') |>
#   st_make_valid() #|>
#   st_transform(crs=st_crs(aoi)) |>
#   st_make_valid()
# coast = st_intersection(
#   can_coast,
#   aoi
# )



#----load seagrass data----

#canada gov data
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
  
  #get point location data
  eg_aerimg_pt = st_centroid(eg_aerimg)
  eg_aervid_pt = st_centroid(eg_aervid)
}


#iNaturalist
{
  eg_inat = read.csv("data/iNat_eelgrass_since2020_ResearchGrade.csv/observations-618804.csv") |>
    st_as_sf(coords = c('longitude', 'latitude'), crs=4326) |>
    mutate(observed_on_Date = as.Date(observed_on)) |>
    filter(place_country_name != 'United States')
  
  sf_use_s2(F)
  eg_inat = st_crop(eg_inat, aoi)
}

#combine observations into harmonized dataset
{
  eg_harm = rbind(
    
    eg_aerimg_pt |>
      filter(Eelgrass_Observation == 'Presence' & Eelgrass_Density %in% c('Dense', 'Moderate')) |>
      rename(geometry = Shape) |>
      mutate(dataset = 'NETForce aerial images') |>
      select(Year, dataset)
    ,
    eg_aervid_pt |>
      filter(Eelgrass_Observation=='Presence' & Eelgrass_Distribution == 'Continuous') |>
      rename(geometry=Shape) |>
      mutate(dataset = 'NETForce aerial video') |>
      select(Year, dataset)
    ,
    eg_inat |>
      mutate(dataset = 'iNaturalist', Year = year(observed_on_Date)) |>
      filter(Year>=2020) |>
      select(Year, dataset)
  )
}

#----eelgrass suitability from Ashley----
eg_suit = rast('data/eelgrass_predictions_ss.tif')
eg_suit = crop(eg_suit, project(vect(aoi), eg_suit), mask=T) |>
  project(aoi)

high_p_mask = ifel(eg_suit > 0.2|eg_suit<0.1, NA, 1)
eg_suit_highp = mask(eg_suit, high_p_mask)

#----define seagrass sampling sites----
#data taken from BC eelgrass atlas: https://cmnmaps.ca/eelgrassbc/

eg_sites = tribble(
  ~lat, ~lon, ~id,
  49.401968, -123.472727, 'Keats1',
  50.124528, -124.704226, 'Prideaux_Haven1',
  50.037849, -125.264958, 'CR_millpond1',
  50.249021, -125.200306, 'Quadra1'
) |>
  st_as_sf(coords = c('lon', 'lat'),
           crs=4326,
           remove=F)

#----plot----

base_plot = ggplot() +
  #plot area of interest
  geom_sf(data=aoi, color='black', fill='darkgrey')+
  #plot basemap
  geom_sf(data=bc_land, color='black', fill='black')

#eelgrass observations
obs_plot = base_plot +
  #plot harmonized eelgrass dataset
  geom_sf(data=eg_harm, aes(color=dataset), alpha=0.6, size=1)+
  
  ##AOI centroids
  # geom_sf(data=aoi_point, color='gold')+
  # geom_sf(data=aoi_point2, color='orange')+
  # geom_sf(data=aoi_point3, color='red')+
  
  #labels, themes, titles
  labs(color='Dataset')+
  ggtitle('Eelgrass observations') +
  # theme_void()+
  coord_sf()

obs_plot

#eelgrass suitability (binary, only values less than 0.2)
suit_plot_highp = base_plot+
  # geom_spatraster(data = eg_suit_highp, aes(fill=est_p))+
  geom_spatraster(data = high_p_mask, aes(fill=est_p))+
  scale_fill_gradient(
    low = "red", high = "red",  # everything red
    na.value = NA,
    guide = "none"
  ) +
  ggtitle("Estimated probability of occurence")
suit_plot

suit_plot2 = ggplot()+
  geom_spatvector(data=crop(vect(aoi),eg_suit), fill='darkgrey')+
  geom_spatvector(data=crop(vect(bc_land),eg_suit), color='black', fill='black')+
  geom_spatraster(data= eg_suit, aes(fill=est_p))+
  scale_fill_viridis_c(na.value=NA) +
  ggtitle('Est. probability of eelgrass occurence')
suit_plot2

library(patchwork)
obs_plot/suit_plot2

#eelgrass suitability

# ggmap::get_cloudmademap(bbox = c(
#   left = aoi_box[['xmin']]
#   ,right = aoi_box[['xmax']]
#   ,bottom = aoi_box[['ymin']]
#   ,top = aoi_box[['ymax']])
#                      , zoom=5, maptype = 'stamen_terrain')
# 
# ggmap::get_googlemap(center = c(lon = st_coordinates(aoi_point)[[1]]
#                                 , lat = st_coordinates(aoi_point)[[2]]),
#                      zoom = 10)

# ggplot() +
#   #plot area of interest
#   geom_sf(data=aoi|>st_as_sfc()|>st_as_sf(), color='black', fill='darkgrey')+
#   #plot basemap
#   geom_sf(data=bc_land, color='black', fill='black')+
#   # #plot government eelgrass datasets
#   # geom_sf(data = eg_aerimg_pt, color='green', alpha=0.3)+
#   # # geom_sf(data=ss_dat[[2]] |>
#   # #           filter(Eelgrass_Density %in% c('Dense', 'Moderate'))
#   # #           , aes(fill=Eelgrass_Density, color=Eelgrass_Density))+ #CORI aerial satellite images
#   # # geom_sf(data=ss_dat[[1]] |>
#   # #           filter(Eelgrass_Observation %in% c('Presence'))
#   # #           , aes(color=Eelgrass_Observation))+ #CORI aerial video
#   # # geom_sf(data=eg_sites, color='red')+
#   # #add inaturalist data
#   # geom_sf(data=inat_eg, color='hotpink', alpha=0.3)+
#   # # scale_color_manual(label='iNaturalist')
#   theme_void()+
#   coord_sf()
