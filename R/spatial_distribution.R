rotation <- function(coords,angle){
  c(x=as.vector(coords["x"]*cos(angle)-coords["y"]*sin(angle)),
    y=as.vector(coords["x"]*sin(angle)+coords["y"]*cos(angle)))
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

distance_loss <- function(focal_point, ref_points, distances){
  sum(mapply(function(ref_point, distance, focal_point) (sqrt(sum((ref_point - focal_point)^2))-distance)^2,
             ref_points, distances, MoreArgs = list(focal_point=focal_point)))
}

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

#' Add coordinates on horizontal plane
#' 
#' Add coordinates of the ant clusters mapped onto the horizontal plane. Calculations
#' are performed based on the distances from the reference points to the cluster
#' center projected upward to the ground level. Because the exact geometrical solution is
#' often unavailable due to the distance measurement errors, the approximate solution
#' is achieved through the minimizing the loss function, which is proportional to the
#' differences between measured distances and those implied by the choice of the candidate center position. 
#' 
#'  @param spatial_data A data frame with the columns containing data on the distances
#'  of the reference points to the cluster center shifted upward to the ground level.
#'  @param reference_points A named list with the plane coordinates of the reference points.
#'  List elements correspond to the different colonies. 
#' @export
add_point_coordinates <- function(spatial_data, reference_points){
  res <- data.frame()
  for(i in seq_len(nrow(spatial_data))){
    colony <- as.character(spatial_data$Colony[i])
    reference_points_colony <- reference_points[[colony]]
    distances <- unlist(spatial_data[i,c("Distance to A", "Distance to B", "Distance to C")])
    if(sum(is.na(distances))==3)
      x <- y <- NA
    else if(sum(is.na(distances))==1){ # the case of colony 17-31
      # calculate angle of the |AC| vector
      angle <- atan(reference_points_colony$C["y"]/reference_points_colony$C["x"])
      # put C point on the x axis to make the calculations easier
      C_point <- rotation(reference_points_colony$C,-angle)
      x <- (C_point["x"]^2 + distances[1]^2 - distances[3]^2)/C_point["x"]/2
      y <- -sqrt(distances[1]^2 - x^2)
      # rotate back to original position
      coords <- rotation(c(x=as.vector(x), y=as.vector(y)), angle)
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


#' @export
determine_vertical_distribution <- function(spatial_distribution){
  spatial_distribution$Worker_density <- spatial_distribution$'Worker number'/(spatial_distribution$'Depth to'-spatial_distribution$'Depth from')
  spatial_distribution <- split(spatial_distribution, spatial_distribution$Colony)
  res <- list()
  for(Colony in names(spatial_distribution)){
    colony_data <- spatial_distribution[[Colony]]
    valid_rows <- apply(as.matrix(colony_data[,c("Depth from", "Depth to", "Worker number")]),1,function(x) !any(is.na(x)))
    colony_data <- colony_data[valid_rows,]
    if(nrow(colony_data)==0) next
    start_depth <- ifelse(Colony=="18-14",-5,0)
    colony_size <- sum(colony_data$'Worker number')
    colony_data_vert_distr <- data.frame()
    for(i in start_depth:55){
      n_workers_segment <- sum(colony_data[colony_data$'Depth from'<=i&colony_data$'Depth to'>i,"Worker_density"])
      colony_data_vert_distr <- rbind(colony_data_vert_distr, data.frame(Colony=Colony, 
                                   Depth_from=i,
                                   Depth_to=i+1,
                                   Worker_number=n_workers_segment,
                                   prop=n_workers_segment/colony_size))
    }
    res <- setNames(c(res, list(colony_data_vert_distr)), c(names(res), Colony))
  }
  res
}
