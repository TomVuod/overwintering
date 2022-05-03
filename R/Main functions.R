#' Caclulate confidence interval for the number  of ants from the upper part of the nest
#' taking into consideration all possible assignemnts of unmarked ants to either
#' upper or lower part group
#' 
#' @param alpha A number between 0 and 1 corresponding to the type 1 error
#' @param removal_total The total number of ants removed from the arena
#' @param removed_1 The number of ants removed which come from the upper part of the nest
#' @param removed_2 The number of ants removed which come from the lower part of the nest
#' @param marked_1 The number of ants marked and removed ants
#'  which come from the upper part of the nest
#' @param marked_2 The number of ants marked and removed ants
#'  which come from the lower part of the nest
#' @param N1 The total number of ants from the upper part of the nest
#' @param N2 The total number of ants from the lower part of the nest
#' @return Vector of the length three accouting for the lower limit, maimum likelihood, and
#' the upper limit for the number of ants coming from the upper part of the nest
#' @examples 
#' data(ant_removal)
#' do.call(hgeom_numbers, as.list(unlist(ant_removal[1,]))))
hgeom_numbers<-function(alpha=0.05, removed_total, removed_1,
                                      removed_2, marked_1, marked_2, 
                        N1, N2, ...) {
  # iterate over all possible number of ants coming from either lower or upper
  # part of the nest
  probabs <- c(0)
  # find all possible numbers of removed ants from upper part of the nest (marked or not)
  ant_seq <- max(removed_total - marked_2 ,removed_1):min(removed_total - removed_2, marked_1)
  for (i in ant_seq){
    proportion1[counter]<-round(i/n, digits=3)
    removed_upper_all<-i
    removed_lower_all<-removed_total-i
    # from the Bayes rule we know that P(N1|r1,r2)=(P(r1|N1)*P(r2|N1)*P(N1))/(P(r1)*P(r2))
    probab<-dhyper(removed_1, marked_1, N1-marked_1, removed_upper_all)*
      dhyper(removed_2, marked_2, N2-marked_2, removed_lower_all)
    probabs[length(probabs)+1] <- probab 
  }
  # normalize sum fo the probabilities to 1
  probabs <- probabs/sum(probabs)
  CI_bounds <- calculate_CI<-function(probabs, alpha = alpha)
  return(c(lower = ant_seq[CI_bounds['l']], ml= ant_seq[which.max(probabs)], 
           upper = ant_seq[CI_bounds['r']]))
}


calculate_CI<-function(data, ...){ 
  if (any(is.na(data))) stop("NA probability values")
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
    if (sum(cumprob) >= (1 - alpha)) break
    if (r==length(data)) right_edge<-TRUE 
    if (l==1) left_edge<-TRUE 
    comparison<-numeric()
    if (left_edge!=TRUE) {
      l2<-l-1
      comparison<-c(comparison, l2)
    }
    if (right_edge!=TRUE) {
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

# caluculate confidence interval for the proportion of ants of both groups among  all caught ants
# given that an ant from any group has equal probability to be represented in a sample


null_distribution_CI <- function(alpha = 0.05, ...){
# corrected version (after publication; hypergeometric distribution insted of binomial)
  probabs <- c()
  upper_part_numbers <- 0:removed_total
  for (i in upper_part_numbers){
    #calculate the probabilities of obtaining all possible numbers of makred and from one of the
    #groups given the sample size and given that the probability is equal to group 1 size/colony size
    probabs[length(probabs)+1] <- dhyper(i, N1, N2, removed_total)
  }
  CI_limits<-coalculate_CI(probabs, alpha=alpha)
  return(l = CI_limits['l'], ml = upper_part_numbers[which.max(probabs)], r = CI_limits['r'])
}

track_CI_change<-function(data = experiment_course, summary_data = ant_removal){
  library(purrr)
  data <- data %>% left_join(select(summary_data, N1, N2, marked_1, marked_2))
  data <- data %>% mutate(N1b = N1 - round(Dead_unm*(N1/(N1+N2))) - Upper_part_dead,
                          N2 = N2 - round(Dead_unm*(N2/(N1+N2)))- Lower_part_dead,
                          N1 = N1b) %>%
    mutate(marked_1 = marked_1 - Upper_part_dead, marked_2 = marked_2 - Lower_part_dead)
    
  CI_limits <- pmap_dfr(data, hgeom_numbers) %>% cbind(data[,c("N1", "N2", "Colony", "Date")]) %>%
    mutate(lower_limit = l/(N1+N2), upper_limit = r/(N1+N2), max_likelihood_val = ml/(N1+N2)) %>%
    select(Colony, Date, lower_limit, upper_limit, max_likelihood_val)
  CI_limits_null <- pmap_dfr(data, null_distribution_CI) %>% 
    cbind(data[,c("N1", "N2", "Colony", "Date")]) %>%
    mutate(lower_limit_null = l/(N1+N2), upper_limit_null = r/(N1+N2), max_likelihood_val_null = ml/(N1+N2)) %>%
    select(lower_limit_null, upper_limit_null, max_likelihood_val_null)
  return(cbind(CI_limits, CI_limits_null))
}