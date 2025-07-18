library(sf)

fc_euclidean_dist <- function(data) {
  # Convert the data frame to an sf object using the geographic CRS (EPSG:4326)
  data_sf_geo <- st_as_sf(data, coords = c("LONG", "LAT"), crs = 4326)
  
  # Transform the geographic coordinates (EPSG:4326)
  # projected coordinate system for KPP in Thailand (EPSG:32647 - UTM Zone 47N)
  data_sf_utm <- st_transform(data_sf_geo, crs = 32647)
  
  # Calculate the pairwise Euclidean distances between points in meters
  distance_matrix_m <- st_distance(data_sf_utm, by_element = FALSE)
  
  # Convert the distance values from meters to kilometers
  # then reshape the matrix into a data frame.
  distance_df <- as.data.frame(matrix(as.numeric(distance_matrix_m), nrow = nrow(data_sf_utm)) / 1000)
  
  # Assign row and column names to the distance matrix using your 'studyno' identifier.
  rownames(distance_df) <- data$studyno
  colnames(distance_df) <- data$studyno
  
  return(distance_df)
}
