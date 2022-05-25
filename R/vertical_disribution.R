# assume uniform distribution of ants along the vertical axis
expand_vert_distr <- function(rowline){
  if(any(is.na(rowline[,c("depth_lower_limit", "depth_upper_limit")]))) return(data.frame())
  depths <- (rowline$depth_upper_limit+1): rowline$depth_lower_limit
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

#' @importFrom purrr map_dfr keep
# fit linear model of the relationship between log10 colony size and the maximal depth
# reached by a colony
# some little corrections have been made after publication so coefficients are now up to date
maximal_depth_regr <- function(data = stratification_data, colony_metadata_ = colony_metadata){
  max_depths <- stratification_data %>% split(.$colony) %>%
    lapply(function(x) data.frame(colony = x$colony[1], depth = max(x$depth))) %>%
    do.call(rbind,.)
  col_sizes <- colony_metadata_ %>%
    map_dfr(function(x) {data.frame(colony = x$colony_ID, size = log10(x$colony_size))})
  model_data <- dplyr::inner_join(max_depths, col_sizes)
  lm(depth~size, data = model_data)
}

# fit linear model of the relationship between log10 colony size and the maximal depth
# mean head width of workers
head_width_regr <- function(colony_metadata_ = colony_metadata){
  plot_data <- colony_metadata_ %>%
    map_dfr(function(x) data.frame(size = log10(x$colony_size),
                                   mean_head_width = mean(x$head_width)/10^3,
                                   colony = x$colony_ID))
  # exclude the smallest colony (see publication)
  plot_data <- dplyr::filter(plot_data, colony != "18-61")
  lm(plot_data$mean_head_width ~ plot_data$size)
}
