extent_to_line <- function(extent_i) {

  x_points <- c(extent_i[1], extent_i[2], extent_i[2], extent_i[1], extent_i[1])
  y_points <- c(extent_i[3], extent_i[3], extent_i[4], extent_i[4], extent_i[3])
  
  points_crop_matrix <- matrix(c(x_points,y_points), ncol = 2)
  
  e <- st_linestring(points_crop_matrix)
  p <- st_cast(st_make_grid(e, crs = 31370, n = 1), to = "LINESTRING")
  
  outp <- plot(p)
  return(outp)
  
}
