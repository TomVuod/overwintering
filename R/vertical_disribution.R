# assume uniform distribution of ants along the vertical axis
expand_vert_distr <- function(rowline){
  if(any(is.na(rowline[,c("depth_lower_limit", "depth_upper_limit")]))) return(data.frame())
  depths <- rowline$depth_upper_limit : (rowline$depth_lower_limit-1)
  work_numb <- rowline$worker_number / (rowline$depth_lower_limit - rowline$depth_upper_limit)
  data.frame(depth = depths, worker_number = work_numb, colony = rowline$colony[1])
}

# aggregate worker number data according to vertical stratification (resolution: 1 cm)
aggr_vertical <- function(data = underground_distribution){
  results <- data.frame()
  for (i in 1:nrow(data)){
    results <- rbind(results, expand_vert_distr(data[i,]))
  }
  dplyr::group_by(results, colony, depth) %>%
    dplyr::summarise(worker_number = sum(worker_number)) %>%
    dplyr::arrange(colony, depth) %>%
    split(.$colony) %>%
    lapply(function(x) {x$cum_numb = cumsum(x$worker_number); x}) %>%
    lapply(function(x) {x$proportion = x$worker_number/sum(x$worker_number); x}) %>%
    do.call(rbind, .)
}

stratification_data <- aggr_vertical(underground_distribution)
