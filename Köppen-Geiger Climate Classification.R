###########################################################################################
##
## R source code to read and visualize Köppen-Geiger Climate Classification in Robinson projection
##
###########################################################################################

#Package Loading --------------------------------------------------------
library(raster)
library(rasterVis)
library(rworldxtra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(extrafont)

x11(width = 60, height = 40)


# Read raster files -------------------------------------------------------
period <- '1986-2010'
r <- raster(paste('KG_', period, '.grd', sep=''))


# Color palette for climate classification --------------------------------
climate.colors <- c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", 
                    "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", 
                    "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", 
                    "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", 
                    "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", 
                    "#64FFFF", "#FFF4D4")



# Prepare raster for classification ---------------------------------------
r0 <- r[1:32]; r[1:32] <- seq(1,32,1)
r <- ratify(r); rat <- levels(r)[[1]]


# Climate classification labels -------------------------------------------
rat$climate <- c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 
                 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 
                 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 
                 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean')

r[1:32] <- r0; levels(r) <- rat

# Define Robinson projection ----------------------------------------------
target_crs_robinson <- "ESRI:54030"

# Project raster to Robinson ----------------------------------------------
r_robinson <- projectRaster(r, crs = target_crs_robinson, method = "ngb")

# Convert to data frame for ggplot2 ---------------------------------------
r_df <- as.data.frame(r_robinson, xy = TRUE, na.rm = TRUE)
names(r_df) <- c("x", "y", "KG")
r_df$KG <- as.factor(r_df$KG)


# Download and prepare basemap data --------------------------------------
world_countries <- ne_countries(scale = 'medium', returnclass = 'sf')
world_oceans <- ne_download(scale = 'medium', type = 'ocean', 
                            category = 'physical', returnclass = 'sf')
world_countries_robinson <- st_transform(world_countries, crs = target_crs_robinson)
world_oceans_robinson <- st_transform(world_oceans, crs = target_crs_robinson)


# Create graticules with finer spacing ------------------------------------
graticules_robinson <- st_graticule(
  lat = seq(-90, 90, by = 10),
  lon = seq(-180, 180, by = 10),
  crs = st_crs(4326)
) |> st_transform(crs = target_crs_robinson)


# Create degree labels for parallels and meridians ------------------------
create_degree_labels <- function() {
  # Longitude labels (meridians) - EVERY OTHER line at 20° intervals
  lon_breaks <- seq(-180, 180, by = 20)
  lon_labels <- ifelse(lon_breaks == 0, "0°",
                       ifelse(lon_breaks == 180, "180°",
                              ifelse(lon_breaks > 0, paste0(lon_breaks, "°E"), 
                                     paste0(lon_breaks, "°W"))))  # negative values
  
  # Latitude labels (parallels) - ALL lines at 10° intervals
  lat_breaks <- seq(-80, 80, by = 10)  # Exclude poles for better placement
  lat_labels <- ifelse(lat_breaks == 0, "0°",
                       ifelse(lat_breaks > 0, paste0(lat_breaks, "°N"), 
                              paste0(lat_breaks, "°S")))  # negative values
  

# Convert to sf and transform to Robinson ---------------------------------
  
  # Position meridian labels at TOP (north) and BOTTOM (south)
  lon_points_top <- st_sfc(
    lapply(lon_breaks, function(lon) {
      st_point(c(lon, 85))  # Position at top of the map
    }),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  lon_points_bottom <- st_sfc(
    lapply(lon_breaks, function(lon) {
      st_point(c(lon, -85))  # Position at bottom of the map
    }),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  # Position parallel labels at LEFT (west) and RIGHT (east)
  lat_points_left <- st_sfc(
    lapply(lat_breaks, function(lat) {
      st_point(c(-179, lat))  # Position left of the map
    }),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  lat_points_right <- st_sfc(
    lapply(lat_breaks, function(lat) {
      st_point(c(179, lat))  # Position right of the map
    }),
    crs = 4326
  ) |> st_transform(crs = target_crs_robinson)
  
  return(list(
    lon_labels_top = st_sf(geometry = lon_points_top, label = lon_labels),
    lon_labels_bottom = st_sf(geometry = lon_points_bottom, label = lon_labels),
    lat_labels_left = st_sf(geometry = lat_points_left, label = lat_labels),
    lat_labels_right = st_sf(geometry = lat_points_right, label = lat_labels)
  ))
}


# Generate labels ---------------------------------------------------------
degree_labels <- create_degree_labels()


# Create the map with Köppen-Geiger Climate classification ------------------------
ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = KG)) +
  
  geom_sf(data = world_oceans_robinson, fill = "white", color = NA, alpha = 0.3) +
  geom_sf(data = world_countries_robinson, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = graticules_robinson, color = "black", linewidth = 0.5, alpha = 0.4) +
  
  # Add meridian labels (longitude) at TOP - every other line
  geom_sf_text(data = degree_labels$lon_labels_top, 
               aes(label = label),
               color = "black", size = 2.8, family = "Times New Roman",
               fontface = "bold",
               nudge_y = 500000) +
  
  # Add meridian labels (longitude) at BOTTOM - every other line
  geom_sf_text(data = degree_labels$lon_labels_bottom, 
               aes(label = label),
               color = "black", size = 2.8, family = "Times New Roman",
               fontface = "bold",
               nudge_y = -500000) +
  
  # Add parallel labels (latitude) at LEFT
  geom_sf_text(data = degree_labels$lat_labels_left,
               aes(label = label), 
               color = "black", size = 3, family = "Times New Roman",
               fontface = "bold",
               nudge_x = -800000) +
  
  # Add parallel labels (latitude) at RIGHT
  geom_sf_text(data = degree_labels$lat_labels_right,
               aes(label = label), 
               color = "black", size = 3, family = "Times New Roman",
               fontface = "bold",
               nudge_x = 800000) +
  
  scale_fill_manual(
    name = "Köppen-Geiger Classification",
    values = climate.colors,
    labels = rat$climate,
    na.value = "black",
    drop = FALSE
  ) +
  ggtitle(paste("Köppen-Geiger Climate Classification", period)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold")
  ) +
  guides(fill = guide_legend(
    nrow = 4,
    byrow = TRUE,
    title.position = "top",
    title.hjust = 0.5
  ))


# Save the map ------------------------------------------------------------
ggsave(paste0("Koppen_Geiger_", period, "_new.png"), 
       width = 16, height = 10, dpi = 300, bg = "white")
ggsave(paste0("Koppen_Geiger_", period, "_Robinson.pdf"), 
       width = 16, height = 10, device = cairo_pdf)


# Find climate class for specific location (e.g., Mashhad) ----------------
lon <- 59.6067; lat <- 36.2972
coords <- data.frame(lon = lon, lat = lat)
coordinates(coords) <- c("lon", "lat")
proj4string(coords) <- CRS("epsg:4326")
coords_robinson <- spTransform(coords, CRS(target_crs_robinson))

KG_value <- extract(r_robinson, coords_robinson)
climate_class <- rat$climate[KG_value]

print(paste("Climate class for Mashhad:", climate_class))