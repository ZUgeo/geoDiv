# --- lib ---

lib = c("dplyr", "stringr", "purrr", "leaflet", "htmltools", "RColorBrewer")
lapply(lib, require, character.only = T)

# --- load data ----

formatted.data.circle.markers = read.csv2("formatted_data_circle_markers.csv", stringsAsFactors = F) %>% 
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>% 
  split(as.factor(.$id))
formatted.data.regular.markers = read.csv2("formatted_data_regular_markers.csv", stringsAsFactors = F) %>% 
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>% 
  split(as.factor(.$id))

# --- vis ---
#create special icons
univIcon = makeIcon(
  iconUrl = "univ-icon.png",
  iconWidth = 15,
  iconHeight = 15,
  popupAnchorY = -10,
  popupAnchorX = .1
)

zeppelinIcon = makeIcon(
  iconUrl = "zeppelin-icon.png",
  iconWidth = 25,
  iconHeight = 20,
  popupAnchorY = -10,
  popupAnchorX = .1
)

#create general map
m = leaflet() %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  fitBounds(-90, -180, 90, 180)

#set starting view
m = m %>% setView(lng = 10.5, lat = 51, zoom = 6)

#add circle markers
names(formatted.data.circle.markers) %>%
  walk( function(df) {
    pal = colorNumeric(
      palette = c("#ff3333", "#ff0000", "#cc0000", "#990000", "#660000"),
      domain = formatted.data.circle.markers[[df]]$n
    )
    
    m <<- m %>%
      addCircleMarkers(data = formatted.data.circle.markers[[df]],
                       lng = ~lon, 
                       lat = ~lat,
                       popup = ~content,
                       radius = ~ifelse(n <= 2, n*2, n),
                       color = ~pal(n),
                       group = df,
                       stroke = F,
                       fillOpacity = .5)
  })

#add regular markers
m = m %>% 
  addMarkers(data = formatted.data.regular.markers[["Messen"]],
             lng = ~lon,
             lat = ~lat,
             popup = ~content,
             icon = zeppelinIcon,
             group = "Messen") %>%
  addMarkers(data = formatted.data.regular.markers[["Partneruniversitäten"]],
             lng = ~lon,
             lat = ~lat,
             popup = ~content,
             icon = univIcon,
             group = "Partneruniversitäten") %>%
  addMarkers(data = formatted.data.regular.markers[["Alumniprofile"]],
             lng = ~lon,
             lat = ~lat,
             popup = ~content,
             group = "Alumniprofile")

#add layer controls
m %>%
  addLayersControl(
    overlayGroups = c(names(formatted.data.circle.markers), names(formatted.data.regular.markers)),
    options = layersControlOptions(collapsed = T)) %>%
  hideGroup(c(names(formatted.data.circle.markers)[-1], names(formatted.data.regular.markers)))