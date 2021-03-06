#' Probability mass distribution
#'
#' Calculate probability mass distribution for the number of individuals from
#' the one of two groups based on the hypergeometric distribution
#'
#' @param removed_1 The number of marked and removed ants
#' which come from the upper part of the nest
#' @param removed_2 The number of ants removed which come from the lower part of the nest
#' @param unmarked The number of removed ants which had not been marked
#' @param marked_1 The number from the upper part of the nest which had been marked
#' @param marked_2 The number from the lower part of the nest which had been marked
#' @param N1 The total number of ants from the upper part of the nest
#' @param N2 The total number of ants from the lower part of the nest
#' @param ... Additional arguments to be omitted
#' @param null_model logical; if set to TRUE the probability mass distribution
#' for the null model is calculated; the null model assumes an equal probability of
#' being removed for an ant from the upper and lower part of the nest
#' @return Vector of the length three accounting for the lower limit, minimum likelihood, and
#' the upper limit for the proportion of ants coming from the upper part of the nest
#' @examples
#' data(experiment_course)
#' data(ant_removal)
#' dataset <- dplyr::left_join(experiment_course,
#' select(ant_removal, colony, N1, N2, marked_1, marked_2))
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
      # ants from one of the groups given the sample size and given that
      # the probability is equal to group 1 size/colony size
      probab <- dhyper(removed_upper_all, N1, N2, removed_total)
    else
      # calculate posterior distribution using the Bayes rule
      probab<-dhyper(removed_1, marked_1, N1-marked_1, removed_upper_all)*
        dhyper(removed_2, marked_2, N2-marked_2, (removed_total - removed_upper_all))
    probabs[length(probabs)+1] <- probab
  }
  # normalize sum of the probabilities to 1
  return(data.frame(prob = probabs/sum(probabs),
                    upper_part_proportion = ant_seq/removed_total))
}

#' Vector of indices for the confidence interval
#'
#' Calculate confidence/credible interval for a vector of probabilities deriving
#' from the probability mass distribution
#'
#' @param data Data frame returned by \code{get_prob_mass}
#' @param alpha Numeric value setting the maximal value for the type 1 error
#' @return Indices of the passed vector corresponding to the confidence interval
#' limits
calculate_CI <- function(data, alpha){
  if (!is.data.frame(data)) stop(paste0(sQuote("Data"), " shoud be a data frame"))
  if (any(is.na(data$prob))) stop("NA probability values")
  if (round(sum(data$prob), 5) != 1) stop("Probability distribution should sum up to 1")
  if (!is.numeric(alpha)) stop("Alpha should be numeric")
  if (length(alpha) != 1) stop("Alpha shlould be of length one")
  if (alpha > 1) stop("Alpha should not be greater than one")
  max_prob_index<-which.max(data$prob)
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
  c(lower_limit = data$upper[l], maximum_likelihood = data$upper[max_prob_index],
    upper_limit = data$upper[r])
}

#' Calculate credible interval
#'
#'  Calculate credible interval for data obtained over the course of experiment
#'
#'  This function uses \code{experiment_course} and \code{ant_removal} datasets to
#'  calculate how condifence interval for the null hypothesis and credible interval for
#'  actual data changes over the course of experiment; this issue was examined following
#'  the suggestion of the reviewer
#'  @return an updated version of of \code{experiment_course} data frame with CI values
#'  added (long format)
#'  @examples
#'  track_CI_change()
#'  @importFrom dplyr %>%
#'  @export
track_CI_change <- function(data = experiment_course, summary_data = ant_removal){
  data <- data %>% dplyr::left_join(select(summary_data, N1, N2, marked_1, marked_2, colony))
  data <- data %>% dplyr::mutate(N1b = N1 - round(Dead_unm*(N1/(N1+N2))) - Upper_part_dead,
                          N2 = N2 - round(Dead_unm*(N2/(N1+N2)))- Lower_part_dead,
                          N1 = N1b) %>%
    dplyr::mutate(marked_1 = marked_1 - Upper_part_dead, marked_2 = marked_2 - Lower_part_dead)
  CI_limits <- pmap(data, get_prob_mass) %>%
    lapply(calculate_CI, alpha = 0.05) %>%
    do.call(rbind, .) %>%
    cbind(data[,c("colony", "Date")]) %>%
    dplyr::mutate(CI = "empirical")
  CI_limits_null <- pmap(data, get_prob_mass, null_model = TRUE)  %>%
    lapply(calculate_CI, alpha = 0.05) %>%
    do.call(rbind, .) %>%
    cbind(data[,c("colony", "Date")]) %>%
    dplyr::mutate(CI = "null")
  return(rbind(CI_limits, CI_limits_null))
}

#' Sample probability distribution
#'
#' Take sample from the probability distribution
#'
#' @param data data.frame returned by the \code{caclulate_CI}
#' @param sample_size int defines the number of draw from the probability
#' distribution
#' @return the values of the random variable whose distribution was sampled; in
#' this case these are the proportions of ants from the upper nest segment
#' @export
sample_distribution <- function(data, sample_size = 1e4){
  stopifnot(is.data.frame(data),
            all((c("prob", "upper_part_proportion") %in% colnames(data))))
  draw_res <- apply(rmultinom(sample_size, 1, data$prob),2,as.logical)
  apply(draw_res, 2, function(x) data$upper[x])
}

cond_sample <-function(x, permutation = TRUE){
  if (!permutation) return(x)
  sample(x)
}

# random sample of regression slopes based on the results
# of the samples from the probability distribution
#' @importFrom purrr pmap_dbl pmap
calculate_regr_coeffs <- function(data = experiment_course,
                                  colony_data = ant_removal,
                                  randomize = FALSE,
                                  sample_size = 1e4) {
  data <- dplyr::left_join(data, dplyr::select(ant_removal, "N1", "N2", "marked_1", "marked_2", "colony"))
  exp_data <- split(data, data$colony)
  coeffs <- list()
  for (i in 1:length(exp_data)){
    timing <- exp_data[[i]]$duration
    coeffs[[i]] <- pmap(exp_data[[i]], get_prob_mass) %>%
      lapply(sample_distribution, sample_size = sample_size) %>%
      do.call(cbind, .) %>%
      as.data.frame() %>%
      pmap_dbl(~lm(cond_sample(c(...), permutation = randomize)~timing)$coeff[2])
    names(coeffs)[i] <- as.character(exp_data[[i]]$colony[1])
  }
  coeffs
}
