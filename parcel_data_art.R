##################
# DATA IMPORTS
##################
sf_parcels_shape <- 
  st_read("https://data.sfgov.org/api/geospatial/acdm-wktn?method=export&format=GeoJSON") %>%
  filter(active == "true") %>% 
  select(
    apn = blklot,
    zoning = zoning_code,
    zoning_desc = zoning_district
  )

sf_secured <- readRDS("sf_secured.rds")
datakey <- readRDS("datakey.rds")
usecode <- readRDS("usecode.rds")

sf_tracts <- tracts(state = "CA", 
                    county = "San Francisco",
                    cb = T,
                    progress_bar = F) %>%
  st_transform(4326)

sample_tracts <- c("030900",
                   "030800")

sf_subset <- sf_tracts %>%
  filter(TRACTCE %in% sample_tracts)


  leaflet() %>% 
  addMapboxTiles(
    style_id = "streets-v11",
    username = "mapbox"
  ) %>% 
  addPolygons(
    data = sf_tracts,
    fillColor = "blue",
    color = "black",
    weight = 0.5,
    label = ~TRACTCE
  )


# bay_parcels <- 
#   st_read("https://opendata.arcgis.com/datasets/0584361f051849639950307b449795db_0.geojson") %>%
#   select(county_id, parcel_id)


# Link parcels and property data on block lot id
sf_parcels <- sf_parcels_shape %>% 
  left_join(
    sf_secured %>% 
      mutate(
        apn = RP1PRCLID %>% 
          str_replace(" ","")
      )
  )

subset_parcels <- sf_parcels %>% 
  st_centroid() %>% 
  .[sf_subset, ] %>% 
  st_set_geometry(NULL) %>% 
  left_join(sf_parcels %>% select(apn)) %>% 
  st_as_sf() %>%
  st_transform(., 7131) # 7131

# sf_parcels %>% 
#   st_set_geometry(NULL) %>%
#   group_by(zoning) %>%
#   summarize(count = n())




# Map sample parcels
#parcels <- 
  tm_shape(subset_parcels) +
  tm_polygons(col = "MAP_COLORS", 
              palette = "Accent") +
  tm_layout(bg.color = "cornsilk", inner.margins = c(0, .02, .02, .02))

tmap_save(parcels, "parcels2.png", height = 7, width = 7) # height interpreted in pixels


subset_parcels %>% 
  leaflet() %>% 
  addMapboxTiles(
    style_id = "streets-v11",
    username = "mapbox"
  ) %>% 
  addPolygons(
    fillColor = "blue",
    color = "black",
    weight = 0.5,
    label = ~zoning
  )

#-------------------
## TEXAS COMAL PARCEL
comal_path <- c("C:/Users/Franc/Documents/Shapefile_Repository/tx_geog/stratmap19-landparcels_48091_vector/shp/")

comal_parcels <- st_read(paste0(comal_path, 
                                "stratmap19-landparcels_48091_comal_201903.shp")
                         )


leaflet() %>% 
  addMapboxTiles(
    style_id = "streets-v11",
    username = "mapbox"
  ) %>% 
  addPolygons(
    data = comal_parcels %>% filter(SITUS_ZIP == "78132") %>% st_transform(., crs = 4326),
    fillColor = "blue",
    color = "black",
    weight = 0.5,
    label = ~OWNER_NAME
  )
