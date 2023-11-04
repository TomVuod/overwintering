# TO DO: handle the case with only two reference points (colony 17-31)

find_reference_coordinates <- function(point_distances, C_up = TRUE){
  Ax <- Ay <- By <- 0
  Bx <- point_distances['A_B']
  Cx <- (point_distances['A_B']^2 + point_distances['A_C']^2 - point_distances['B_C']^2)/point_distances['A_B']/2
  Cy <- sqrt(point_distances['A_C']^2 - Cx^2)*(-1+2*C_up)
  x <- c(Ax, Bx, Cx)
  x <- x - min(x)
  y <- c(Ay, By, Cy)
  y <- y - min(y)

  list(A=c(Ax, Ay), B=c(Bx, By), C=c(Cx, Cy))
}

# TO DO: handle the case with only two reference points (colony 17-31)
distance_loss <- function(focal_point, ref_points, distances){
  sum(mapply(function(ref_point, distance, focal_point) (sqrt(sum((ref_point - focal_point)^2))-distance)^2,
             ref_points, distances, MoreArgs = list(focal_point=focal_point)))
}

# TO DO: handle the case with only two reference points (colony 17-31)
distances_to_coordinates <- function(reference_points_dist){
  res <- list()
  for(i in seq_len(nrow(reference_points_dist))){
    colony <- reference_points_dist[i, "colony"]
    arr <- reference_points_dist[i, "arrangement"]==1
    distances <- setNames(reference_points_dist[i, c("A_B", "A_C", "B_C")], c("A_B", "A_C", "B_C"))
    coords <- find_reference_coordinates(distances, arr)
    res <- setNames(c(res, list(coords)), c(names(res), colony))
  }
  res
}

add_point_coordinates <- function(spatial_data, reference_points){
  res <- data.frame()
  for(i in seq_len(nrow(spatial_data))){
    colony <- as.character(spatial_data$colony[i])
    reference_points_colony <- reference_points[[colony]]
    distances <- spatial_data[i,c("Distance to A", "Distance to B", "Distance to C")]
    if(sum(is.na(distances))==3)
      x <- y <- NA
    else{
      corrds <- optim(initial_pos=c(3,3), distance_loss, ref_points=ref_points_colony, distances=distances)
      x <- coords$par[1]
      y <- coords$par[2]
    }
    res <- rbind(res, data.frame(position_x=x, position_y=y))
  }
  cbind(spatial_distribution, res)
}
