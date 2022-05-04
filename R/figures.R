figure_5 <- function(){
  library(ggplot2)
  track_CI_change() %>%
    ggplot(aes(x = Date)) +
    geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit, fill = CI), alpha = 0.5) +
    scale_fill_manual("Confidence interval", labels = c("Eprical data", "Null model"), values = c('#8d7500','#341202')) +
    geom_line(aes(y = max_likelihood_val, color = CI), lwd=1) +
    scale_color_manual("Maximum likelihood", labels = c("Eprical data", "Null model"), values = c('#8d7500','#341202')) +
    facet_grid(.~colony, scales = "free_x") 
}

