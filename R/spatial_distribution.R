rotation <- function(coords,angle){
  c(x=coords["x"]*cos(angle)-coords["y"]*sin(angle),
    y=coords["x"]*sin(angle)+coords["y"]*cos(angle))
}

find_reference_coordinates <- function(point_distances, C_up = TRUE){
  Ax <- Ay <- By <- 0
  Bx <- point_distances['A_B']
  Cx <- (point_distances['A_B']^2 + point_distances['A_C']^2 - point_distances['B_C']^2)/point_distances['A_B']/2
  Cy <- sqrt(point_distances['A_C']^2 - Cx^2)*(-1+2*C_up)
  x <- c(Ax, Bx, Cx)
  x <- as.numeric(x - min(x))
  y <- c(Ay, By, Cy)
  y <- as.numeric(y - min(y))

  list(A=c(x=x[1], y=y[1]), B=c(x=x[2], y=y[2]), C=c(x=x[3], y=y[3]))
}

# TO DO: handle the case with only two reference points (colony 17-31)
distance_loss <- function(focal_point, ref_points, distances){
  sum(mapply(function(ref_point, distance, focal_point) (sqrt(sum((ref_point - focal_point)^2))-distance)^2,
             ref_points, distances, MoreArgs = list(focal_point=focal_point)))
}

# TO DO: handle the case with only two reference points (colony 17-31)
#' @export
distances_to_coordinates <- function(reference_points_dist){
  res <- list()
  for(i in seq_len(nrow(reference_points_dist))){
    colony <- reference_points_dist[i, "Colony"]
    arr <- reference_points_dist[i, "arrangement"]==1
    distances <- setNames(reference_points_dist[i, c("A_B", "A_C", "B_C")], c("A_B", "A_C", "B_C"))
    coords <- find_reference_coordinates(unlist(distances), arr)
    res <- setNames(c(res, list(coords)), c(names(res), colony))
  }
  res
}

#' @export
add_point_coordinates <- function(spatial_data, reference_points){
  res <- data.frame()
  for(i in seq_len(nrow(spatial_data))){
    colony <- as.character(spatial_data$Colony[i])
    reference_points_colony <- reference_points[[colony]]
    distances <- spatial_data[i,c("Distance to A", "Distance to B", "Distance to C")]
    if(sum(is.na(distances))==3)
      x <- y <- NA
    else if(sum(is.na(distances))==2){ # the case of colony 17-31
      # calculate angle of the |AC| vector
      angle <- atan(reference_points_colony$C["y"]/reference_points_colony$C["x"])
      # put C point on the x axis
      C_point <- rotation(reference_points_colony$C,-angle)
      x <- (C_point["x"]^2 + distances[1]^2 - distances[3]^2)/C_point["x"]/2
      y <- -sqrt(point_distances['A_C']^2 - Cx^2)
      coords <- rotation(c(x=x, y=y), angle)
      x <- coords["x"]
      y <- coords["y"]
    }
    else{
      coords <- optim(c(3,3), distance_loss, ref_points=reference_points_colony, distances=distances)
      x <- coords$par[1]
      y <- coords$par[2]
    }
    res <- rbind(res, data.frame(position_x=as.numeric(x), position_y=as.numeric(y)))
  }
  cbind(spatial_data, res)
}
