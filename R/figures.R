# Figure 5 in the publication
figure_5 <- function(){
  library(ggplot2)
  library(dplyr)
  track_CI_change() %>%
    ggplot(aes(x = duration)) +
    geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit, fill = CI), alpha = 0.5) +
    scale_fill_manual("Confidence interval", labels = c("Eprical data", "Null model"), values = c('#8d7500','#341202')) +
    geom_line(aes(y = maximum_likelihood, color = CI), lwd=1) +
    scale_color_manual("Maximum likelihood", labels = c("Eprical data", "Null model"), values = c('#8d7500','#341202')) +
    facet_grid(.~colony, scales = "free_x") +
    ylab("Proportion of ants from the\nupper nest segment")
}


# Figure 6 in the publication
# note that the results of the Fig. 6 are subject to some small random variation
# to avoid this effect you can set the initial value of the generator seed
#' @importFrom purrr map_chr map walk
#' @importFrom stringi stri_extract
figure_6 <- function(data = experiment_course, colony_data = ant_removal,
                     sample_size = 1e4){

  # calculate slope values after probability distribution sampling
  empirical_slopes <- calculate_regr_coeffs(data = data, colony_data = colony_data,
                                            sample_size = sample_size)
  randomized_slopes <- calculate_regr_coeffs(data = data, colony_data = colony_data,
                                             sample_size = sample_size, randomize = TRUE)

  HDI_values <- list()

  calculate_HDI <- function(colony_data) {
    # data saved in the parent frame object
    HDI_values[[colony_data$colony[1]]] <<- HDInterval::hdi(colony_data$slope, n =500)
  }

  # add mask to select slope values which fall into the highest density interval
  add_HDI_mask <- function(colony_data) {
    col <- colony_data$colony[1]
    HDI_mask <- colony_data$x >= HDI_values[[col]][1] & colony_data$x <= HDI_values[[col]][2]
    cbind(colony_data, data.frame(HDI_mask = HDI_mask))
  }

  # density of the slope values
  calculate_density <- function(colony_data){
    dens <- density(colony_data$slope, n = 500)
    data.frame(x = dens$x, y = dens$y, colony = colony_data$colony[1])
  }

  set_colony_name <- . %>% dplyr::rename_with(function(x) "slope", tidyselect::everything()) %>%
    dplyr::mutate(colony = map_chr(rownames(.), stri_extract, regex = "[0-9]{2}-[0-9]{2}"))

  process_data <- . %>%
    unlist() %>%
    as.data.frame() %>%
    set_colony_name() %>%
    split(.$colony) %>%
    walk(calculate_HDI) %>%
    map(calculate_density) %>%
    map(add_HDI_mask) %>%
    do.call(rbind, .)

  empirical_slopes <- empirical_slopes %>%
    process_data() %>%
    dplyr::mutate(model = "empirical")

  randomized_slopes <- randomized_slopes %>%
    process_data() %>%
    dplyr::mutate(model = "null")

  plot_data <- rbind(empirical_slopes, randomized_slopes)
  ggplot(plot_data, aes(x = x, y =y )) +
    geom_line(aes(lty = model)) +
    geom_area(aes(x = x, y = y, group = model),
              data = subset(plot_data, HDI_mask), alpha = 0.5) +
    facet_grid(.~colony) +
    labs(linetype = "Legend", x = "Slope value",y = "Count (density)") +
    theme(panel.background = element_rect(fill = "white", colour = "#c8c8c8")) +
    theme(panel.grid.major = element_line(colour = "#c8c8c8",size = 0.05)) +
    theme(axis.text.x = element_text(angle = 90))
}
