#' @export
#' Hypergeometric distribution confidence interval
#'
#' Caclulate confidence interval for the number  of ants from the upper part of the nest
#' taking into consideration all possible assignemnts of unmarked ants to either
#' upper or lower part group
#'
#' @param alpha A number between 0 and 1 corresponding to the type 1 error
#' @param removal_total The total number of ants removed from the arena
#' @param removed_1 The number of marked and removed ants
#' which come from the upper part of the nest
#' @param removed_2 The number of ants removed which come from the lower part of the nest
#' @param marked_1 The number of marked ants from the upper part of the nest
#' @param marked_2 The number of marked ants from the lower part of the nest
#' @param N1 The total number of ants from the upper part of the nest
#' @param N2 The total number of ants from the lower part of the nest
#' @return Vector of the length three accouting for the lower limit, maimum likelihood, and
#' the upper limit for the number of ants coming from the upper part of the nest
#' @examples
#' data(ant_removal)
#' do.call(hgeom_numbers, as.list(unlist(ant_removal[1,]))))
hgeom_numbers<-function(alpha=0.05, Unmarked, removed_1,
                                      removed_2, marked_1, marked_2, removed_total,
                        N1, N2, ...) {
  # iterate over all possible number of ants coming from either lower or upper
  # part of the nest
  probabs <- numeric(0)
  # find all possible numbers of removed ants from upper part of the nest (marked or not)
  ant_seq <- max(removed_total - N2, removed_1):min(removed_total - removed_2, N1)
  for (i in ant_seq){
    removed_upper_all<-i
    removed_lower_all<-removed_total-i
    # calculate posterior distribution using the Bayes rule
    probab<-dhyper(removed_1, marked_1, N1-marked_1, removed_upper_all)*
      dhyper(removed_2, marked_2, N2-marked_2, removed_lower_all)
    probabs[length(probabs)+1] <- probab
  }
  # normalize sum fo the probabilities to 1
  probabs <- probabs/sum(probabs)
  CI_bounds <- calculate_CI(probabs, alpha = alpha)
  return(c(lower = ant_seq[CI_bounds['l']], ml = ant_seq[which.max(probabs)],
           upper = ant_seq[CI_bounds['r']]))
}

#' Vector of indices for the confidence interval
#'
#' Calculate confidence interval for a vector of probabilities deriving from
#' the probability mass function
#'
#' @param data Vector of numbers representing probabilities; should sum up to 1
#' @param alpha Numeric value setting the mamximal value for the type 1 error
#' @return Indices of the passed vector correspoding to the confidence interval
#' limits
#' @examples
#' x <- runif(20)
#' x <- sum(x)
#' caclulate_CI(x, 0.05)
calculate_CI<-function(data, alpha){
  if (any(is.na(data))) stop("NA probability values")
  if (!is.numeric(data)) stop("Data shoud be a numeric vector")
  if (round(sum(data), 5) != 1) stop("Data vector should sum to 1")
  if (!is.numeric(alpha)) stop("Alpha should be numeric")
  if (length(alpha) != 1) stop("Alpha shlould be of length one")
  if (alpha > 1) stop("Alpha should not be greater than one")
  max_prob_index<-which.max(data)
  r<-max_prob_index
  l<-max_prob_index
  right_edge<-FALSE
  left_edge<-FALSE
  cum_prob1<-data[max_prob_index]
  #print(sprintf('n1: %d', n1))
  cumprob=data[max_prob_index]
  counter=1
  while (counter<=length(data)){
    if (round(sum(cumprob), 4) >= (1 - alpha)) break
    if (r==length(data)) right_edge<-TRUE
    if (l==1) left_edge<-TRUE
    comparison<-numeric()
    if (left_edge == FALSE) {
      l2<-l-1
      comparison<-c(comparison, l2)
    }
    if (right_edge == FALSE) {
      r2<-r+1
      comparison<-c(comparison, r2)
    }
    comparison2<-numeric()
    for (i in comparison){
      comparison2<-c(comparison2, data[i])
    }
    index_pop <- comparison[which.max(comparison2)]
    cumprob <- cumprob + data[index_pop]
    if (index_pop>max_prob_index) r<-r+1
    else l<-l-1
    counter=counter+1
  }
  c(r = r, l = l)
}


null_distribution_CI <- function(alpha = 0.05, removed_total, N1, N2, ...){
# caluculate confidence interval for the proportion of ants of both groups among all caught ants
# given that an ant from any group has equal probability to be represented in a sample
# corrected version (after publication; hypergeometric distribution insted of binomial)
  probabs <- c()
  upper_part_numbers <- 0:removed_total
  for (i in upper_part_numbers){
    #calculate the probabilities of obtaining all possible numbers of makred and from one of the
    #groups given the sample size and given that the probability is equal to group 1 size/colony size
    probabs[length(probabs)+1] <- dhyper(i, N1, N2, removed_total)
  }
  CI_limits<-calculate_CI(probabs, alpha=alpha)
  return(c(lower = upper_part_numbers[CI_limits['l']],
           ml = upper_part_numbers[which.max(probabs)],
           upper = upper_part_numbers[CI_limits['r']]))
}

#' @importFrom dplyr %>%

track_CI_change<-function(data = experiment_course, summary_data = ant_removal){
  data <- data %>% dplyr::left_join(select(summary_data, N1, N2, marked_1, marked_2, colony))
  data <- data %>% dplyr::mutate(N1b = N1 - round(Dead_unm*(N1/(N1+N2))) - Upper_part_dead,
                          N2 = N2 - round(Dead_unm*(N2/(N1+N2)))- Lower_part_dead,
                          N1 = N1b) %>%
    dplyr::mutate(marked_1 = marked_1 - Upper_part_dead, marked_2 = marked_2 - Lower_part_dead)
  CI_limits <- pmap_dfr(data, hgeom_numbers)  %>% cbind(data[,c("N1", "N2", "colony", "Date", "removed_total")]) %>%
    mutate(lower_limit = lower/(removed_total), upper_limit = upper/(removed_total), max_likelihood_val = ml/(removed_total)) %>%
    select(colony, Date, lower_limit, upper_limit, max_likelihood_val) %>% dplyr::mutate(CI = "empirical")
  CI_limits_null <- pmap_dfr(data, null_distribution_CI) %>%
    cbind(data[,c("N1", "N2", "colony", "Date", "removed_total")]) %>%
    mutate(lower_limit = lower/(removed_total), upper_limit = upper/(removed_total), max_likelihood_val = ml/(removed_total)) %>%
    select(colony, Date, lower_limit, upper_limit, max_likelihood_val) %>% dplyr::mutate(CI = "null")
  return(rbind(CI_limits, CI_limits_null))
}
