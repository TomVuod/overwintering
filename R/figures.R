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

# note that the results of the Fig. 6 are subject to some small random variation
# to avoid this effect you can set the initial value of the generator seed
#' @importFrom purrr map_chr
#' @importFrom stringi stri_extract
figure_6 <- function(data = experiment_course, colony_data = ant_removal,
                     sample_size = 1e4){
  empirical_slopes <- calculate_regr_coeffs(data = data, colony_data = colony_data,
                                            sample_size = sample_size)
  randomized_slopes <- calculate_regr_coeffs(data = data, colony_data = colony_data,
                                             sample_size = sample_size, randomize = TRUE)
  check_values <- function(x){
    interv <- HDInterval::hdi(x$slope)
    x$slope >= interv[1] & x$slope <= interv[2]
  }

  add_hdi_mask <- . %>% split(., .$colony) %>% lapply(check_values) %>%
    unlist(use.names = FALSE)

  set_colony_name <- . %>% as.data.frame(.) %>%
    dplyr::rename_with(function(x) "slope", tidyselect::everything())  %>%
    dplyr::mutate(colony = map_chr(rownames(.), stri_extract, regex = "[0-9]{2}-[0-9]{2}"))

  empirical_slopes <- empirical_slopes %>% unlist() %>% set_colony_name() %>%
    dplyr::mutate(HDI_mask = add_hdi_mask(.), model = "empirical")
  randomized_slopes <- randomized_slopes %>% unlist() %>% set_colony_name() %>%
    dplyr::mutate(HDI_mask = add_hdi_mask(.),model = "null")
  plot_data <- rbind(empirical_slopes, randomized_slopes)
  ggplot(plot_data, aes(x = slope)) +
    geom_density(aes(lty = model)) +
    facet_grid(.~colony)
}
