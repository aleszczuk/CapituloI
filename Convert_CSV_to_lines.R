library(sp)
library(maptools)
library(leaflet)
library(raster)
library(sf)
library(units)
library(smoothr)

# Esta funcion convierte un una tabla de puntos en lineas para verlo como un shape

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  library(sp)
  library(maptools)
  library(leaflet)
  library(raster)
  library(sf)
  library(units)
  library(smoothr)
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

'
data <- datos1[,c(39,40,41,42,28)]
names(data)
data <- sp::SpatialPoints(data, proj4string = CRS("+proj=longlat"))
v_lines <- points_to_line(data = data,long = "lon_pos_red", lat =  "lat_pos_red",id_field =  "Ciclo.1" , sort_field = "shape_id" )
class(v_lines)
sf<- st_as_sf(v_lines)
proj4string(v_lines) <- CRS("+proj=tmerc +lat_0=-90 +lon_0=-54 +k=1 +x_0=7500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
st_write(sf, "shapefile_out.shp", driver="ESRI Shapefile")
smooth(v_lines, method = "chaikin")
ProyPosgar <- spTransform(v_lines,CRS("+proj=tmerc +lat_0=-90 +lon_0=-54 +k=1 +x_0=7500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
v_lines <-sp::SpatialLinesDataFrame(v_lines)
v_lines <- CRS("+proj=longlat +datum=WGS84")
v_lines <- CRS("+proj=tmerc +lat_0=-90 +lon_0=-54 +k=1 +x_0=7500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ProyPosgar <- spTransform(v_lines,CRS("+proj=tmerc +lat_0=-90 +lon_0=-54 +k=1 +x_0=7500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
leaflet(data = v_lines) %>%
  addTiles() %>%
  addPolylines()
'