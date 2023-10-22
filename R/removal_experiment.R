#' Probability mass distribution
#'
#' Calculate probability mass distribution for the proportion of ants from
#' the upper part of the nest among ants removed from outside the nest
#'
#' @param removed_1 The number of marked ants from the upper part of the nest which
#' were removed
#' @param removed_2 The number of the marked ants from the lower part of the nest which
#' were removed
#' @param unmarked The number of the removed ants which had not been marked
#' @param marked_1 The total number of the marked ants from the upper part of the nest
#' @param marked_2 The total number of the marked ants from the lower part of the nest
#' @param N1 The total number of ants from the upper part of the nest
#' @param N2 The total number of ants from the lower part of the nest
#' @param ... Additional arguments to be omitted
#' @param null_model logical; if set to TRUE the probability mass distribution
#' for the null model is calculated; the null model assumes an equal probability of
#' being removed for an ant from the upper and lower part of the nest; if set to FALSE
#' the probability takes into account the numbers of marked ans which have been removed
#' from the outside of nest and the model assumes that a marked ant has the same
#' probability of being removed as an unmarked one.
#' @return A data frame with the all possible proportions of ants from the upper part
#' of the nest among all ants active outside the nest and the corresponding probabilities.
#' @examples
#' data(time_series_results)
#' data(colony_stats)
#' dataset <- dplyr::left_join(time_series_results,
#' dplyr::select(colony_stats, colony, N1, N2, marked_1, marked_2))
#' do.call(get_prob_mass, as.list(unlist(dataset[2,])))
#' @importFrom stats dhyper
#' @export
get_prob_mass <- function(removed_1, removed_2, unmarked, marked_1, marked_2,
                        N1, N2, ..., null_model = FALSE) {
  removed_total <- removed_1 + removed_2 + unmarked
  probabs <- numeric(0)
  if(null_model)
    ant_seq <- 0:removed_total
  else
    # find all possible numbers of removed ants from upper part of the nest (marked or not)
    ant_seq <- max(removed_total - N2, removed_1):min(removed_total - removed_2, N1)
  for (removed_upper_all in ant_seq){
    if(null_model)
      # calculate the probabilities of obtaining all possible numbers of marked
      # ants from the upper part of the nest given the sample size and assuming that
      # the probability is equal to group size divided by colony size
      probab <- dhyper(removed_upper_all, N1, N2, removed_total)
    else
      # calculate posterior distribution using the Bayes rule
      probab<-dhyper(removed_1, marked_1, N1-marked_1, removed_upper_all)*
        dhyper(removed_2, marked_2, N2-marked_2, (removed_total - removed_upper_all))
    probabs <- c(probabs, probab)
  }
  # normalize sum of the probabilities to 1
  return(data.frame(prob = probabs/sum(probabs),
                    upper_part_proportion = ant_seq/removed_total))
}

#' Interval for the proportion of ants
#'
#' Calculate confidence/credible interval for a vector of probabilities deriving
#' from the probability mass distribution
#'
#' @param data Data frame returned by \code{get_prob_mass}
#' @param alpha Numeric value setting the maximum value for the type 1 error
#' @return A numeric vector with elements corresponding to lower limit, maximum
#' likelihood, and the upper limit for the proportion of ants coming from the
#' upper part of the nest.
calculate_CI <- function(data, alpha){
  if (!is.data.frame(data)) stop(paste0(sQuote("Data"), " shoud be a data frame"))
  if (any(is.na(data$prob))) stop("NA probability values")
  if (round(sum(data$prob), 5) != 1) stop("Probability distribution should sum up to 1")
  if (!is.numeric(alpha)) stop("Alpha should be numeric")
  if (length(alpha) != 1) stop("Alpha shlould be of length one")
  if (alpha > 1) stop("Alpha should not be greater than one")
  max_prob_index <- which.max(data$prob)
  r <- l <-  max_prob_index
  cumprob <- data$prob[max_prob_index]
  while (round(cumprob, 4) < (1 - alpha)){
    new_limits <- c(l-1, r+1)
    new_limits <- new_limits[new_limits > 0 & new_limits <= nrow(data)]
    selected_index <- new_limits[which.max(data$prob[new_limits])]
    l <- min(l, selected_index)
    r <- max(r, selected_index)
    cumprob <- sum(data$prob[l:r])
  }
  c(lower_limit = data$upper_part_proportion[l],
    maximum_likelihood = data$upper_part_proportion[max_prob_index],
    upper_limit = data$upper_part_proportion[r])
}

#' Calculate credible interval
#'
#' Calculate credible interval for data obtained over the course of experiment
#'
#' This function uses \code{time_series_results} and \code{colony_stats} datasets to
#' calculate how confidence interval for the null hypothesis and credible interval for
#' actual data changes over the course of experiment; this issue was examined following
#' the suggestion of the reviewer
#' @return an updated version of of \code{time_series_results} data frame with CI values
#' added (long format)
#' @importFrom dplyr %>%
#' @export
track_CI_change <- function(){
  data("time_series_results", package = "overwintering", envir = enviroment())
  data("colony_stats", package = "overwintering", envir = enviroment())
  removal_experiment_results <- time_series_results %>% dplyr::left_join(select(colony_stats, N1, N2, marked_1, marked_2, colony))
  removal_experiment_results <- removal_experiment_results %>% dplyr::mutate(N1b = N1 - round(Dead_unm*(N1/(N1+N2))) - Upper_part_dead,
                          N2 = N2 - round(Dead_unm*(N2/(N1+N2)))- Lower_part_dead, # adjust for dead ants
                          N1 = N1b) %>%
    dplyr::mutate(marked_1 = marked_1 - Upper_part_dead, marked_2 = marked_2 - Lower_part_dead)
  CI_limits <- pmap(removal_experiment_results, get_prob_mass) %>%
    lapply(calculate_CI, alpha = 0.05) %>%
    do.call(rbind, .) %>%
    cbind(removal_experiment_results[,c("colony", "Date")]) %>%
    dplyr::mutate(CI = "empirical")
  CI_limits_null <- pmap(removal_experiment_results, get_prob_mass, null_model = TRUE)  %>%
    lapply(calculate_CI, alpha = 0.05) %>%
    do.call(rbind, .) %>%
    cbind(removal_experiment_results[,c("colony", "Date")]) %>%
    dplyr::mutate(CI = "null")
  return(rbind(CI_limits, CI_limits_null))
}

#' Sample probability distribution
#'
#' Take samples from the set with defined probabilities
#'
#' @param data data.frame returned by the \code{get_prob_mass}
#' @param sample_size int defines the number of draws from the probability
#' distribution
#' @return the values of the random variable whose distribution was sampled; in
#' this case these are the proportions of ants from the upper nest segment
#' @export
sample_distribution <- function(data, sample_size = 1e4){
  stopifnot(is.data.frame(data),
            all((c("prob", "upper_part_proportion") %in% colnames(data))))
  draw_res <- apply(rmultinom(sample_size, 1, data$prob),2,as.logical)
  apply(draw_res, 2, function(x) data$upper_part_proportion[x])
}


cond_sample <-function(x, permutation = TRUE){
  if (!permutation) return(x)
  sample(x)
}

#' Slopes based on random variables
#'
#' Calculate slopes of the linear regression of proportion of ants on time. The
#' proportion for each time point and colony is sampled from the distribution
#' returned by the \code{get_prob_mass} function.
#'
#' @param randomize A logical indicating whether slopes should be calculated using
#' permuted data, see below.
#' @param sample_size A numeric indicating the number of samples taken from the
#' distribution of the possible proportion of ants from the upper part of the nest
#' among all ants being active outside the nest, see below.
#' @details
#' This function uses \code{get_prob_mass} function on experiment data provided in the 
#' \code{time_series_results} and \code{colony_stats} datasets to calculate the probability distribution
#' of the proportion of ants captured outside the nest which come from the upper
#' part of the nest. The inference is based on the numbers of marked ants among all
#' captured individuals with marking label indicating the nest segment (upper or lower).
#' The probability distributions, obtained separately for each colony and time point,
#' is sampled many times, producing multiple data vectors for linear regression. These
#' vectors can additionally be permuted to have the null distribution of regression
#' slopes.
#' @importFrom purrr pmap_dbl pmap
#' @importFrom dplyr %>%
calculate_regr_coeffs <- function(randomize = FALSE,
                                  sample_size = 1e4) {
  data("time_series_results", package = "overwintering", envir = enviroment())
  data("colony_stats", envir = environment(), package = "overwintering")
  data_combined <- dplyr::left_join(time_series_results, dplyr::select(colony_stats, "N1", "N2", "marked_1", "marked_2", "colony"))
  exp_data <- split(data_combined, data_combined$colony)
  coeffs <- list()
  for (i in 1:length(exp_data)){
    # calculate the number of days from the start of the measurements
    timing <- as.numeric(exp_data$Date - min(exp_data$Date))
    coeffs[[i]] <- pmap(exp_data[[i]], get_prob_mass) %>%
      lapply(sample_distribution, sample_size = sample_size) %>%
      do.call(cbind, .) %>%
      as.data.frame() %>%
      pmap_dbl(~lm(cond_sample(c(...), permutation = randomize)~timing)$coeff[2])
    names(coeffs)[i] <- as.character(exp_data[[i]]$colony[1])
  }
  coeffs
}
