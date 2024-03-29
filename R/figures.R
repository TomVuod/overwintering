#' Figure 4
#'
#' Plot figure 4 from the publication.
#' @param data A data frame with the results of the colony-level measurements and
#' mark-remove experiment. For the details on how to prepare the input data run \code{vignette("Data_workflow")}
#' @param w1 A one-length numeric vector specifying the width of the bars
#' @param sep A one-length numeric vector specifying the width of the gap between bars.
#' @export
figure_4 <- function(data, w=15, sep=12){
  par(mfrow=c(1,5))
  for (i in 1:5){
    r1<-(data[i,]$removed_1/data[i,]$removed_total)*100
    r2<-(data[i,]$removed_2/data[i,]$removed_total)*100
    N_2<-100-(data[i,]$max_prob*100)
    n2<-(data[i,'N2']/(data[i,'N1']+data[i,'N2']))*100
    m1<-100*(data[i,'marked_1']/(data[i,'N1']+data[i,'N2']))
    m2<-100*(data[i,'marked_2']/(data[i,'N1']+data[i,'N2']))
    par(mar=rep(0.3,4))
    plot.new()
    plot.window(c(-14,(2*w+sep+12)), c(-7,140))

    rect(0,0,w,m2, border=NA, lwd=1.5, col="#009F10")
    rect(0,m2,w,n2, border=NA, lwd=1.5, col='#BCE78B')
    rect(0,n2,w,100-m1, border=NA, lwd=1.5, col="#E6B97D")
    rect(0,100-m1,w,100, border=NA, lwd=1.5, col="#FF6904")
    rect(0,0,w,100, border=rgb(0,0,0), lwd=1.5, col=rgb(1,1,1, alpha=0))

    rect(w+sep,0,2*w+sep,r2, border=NA, lwd=1.5,  col='#009F10')
    rect(w+sep,r2,2*w+sep,N_2, border=NA, lwd=1.5, col="#BCE78B")
    rect(w+sep,N_2,2*w+sep,(100-r1), border=NA, lwd=1.5, col="#E6B97D")
    rect(w+sep,100-r1,2*w+sep,100, border=NA, lwd=1.5,col="#FF6904")
    rect(w+sep,0,2*w+sep,100, border=rgb(0,0,0), lwd=1.5, col=rgb(1,1,1,alpha=0))

    lines(c(w+0.5*sep, w+0.5*sep), c(100-data[i,]$CI_min_null*100, 100-data[i,]$CI_max_null*100), lwd=1.5)
    lines(c(w+0.5*sep-1, w+0.5*sep+1), rep(100-data[i,]$CI_min_null*100,2), lwd=1.5)
    lines(c(w+0.5*sep-1, w+0.5*sep+1), rep(100-data[i,]$CI_max_null*100,2), lwd=1.5)

    lines(c(w+sep+w*0.5, w+sep+w*0.5), c(100-data[i,]$CI_min*100, 100-data[i,]$CI_max*100), lwd=1.5)
    lines(c(w+sep+w*0.5-1, w+sep+w*0.5+1), rep(100-data[i,]$CI_min*100,2), lwd=1.5)
    lines(c(w+sep+w*0.5-1, w+sep+w*0.5+1), rep(100-data[i,]$CI_max*100,2), lwd=1.5)

    for (j in 0:10){
      lines(c(-1.5, 0),c(j*10,j*10), lwd=1.4)
      lines(c(sep+2*w, sep+2*w+1.5),c(j*10,j*10), lwd=1.4)
      if (i==1)
        text(-8, j*10, as.character(round(j/10, digits=1)), cex=1.9)
      if (i==5)
        text(sep+2*w+8, j*10, as.character(round(1-(j/10), digits=1)), cex=1.9)

    }
    text(w*0.5-1, 105, 'colony', cex=1.9)
    text(sep+1.5*w+1, 105, 'foragers', cex=1.9)
    text(w+0.5*sep, 111, sprintf('Colony #%s', i), cex=1.95)
    text(w*0.5, -8, as.character(data[i,'N1']+data[i,'N2']), cex=1.9)
    text(sep+1.5*w, -8, as.character(data[i,]$removed_total), cex=1.9)
  }
}



#' Figure 5
#'
#' Plot figure 5 from the publication.
#' @export
figure_5 <- function(){
  library(ggplot2)
  library(dplyr)
  data("time_series_results", package = "overwintering", envir = environment())
  data("colony_stats", package = "overwintering", envir = environment())
  track_CI_change(time_series_results, colony_stats) %>%
    ggplot(aes(x = duration)) +
    geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit, fill = CI), alpha = 0.5) +
    scale_fill_manual("Confidence interval", labels = c("Emprical data", "Null model"), values = c('#8d7500','#341202')) +
    geom_line(aes(y = maximum_likelihood, color = CI), lwd=1) +
    scale_color_manual("Maximum likelihood", labels = c("Emprical data", "Null model"), values = c('#8d7500','#341202')) +
    facet_grid(.~colony, scales = "free_x") +
    ylab("Proportion of ants from the\nupper nest segment")+
    xlab("Time (days)")
}

#' Figure 6
#'
#' Plot figure 6 from the publication. Note that the results of the Fig. 6 are
#' subject to some small random variation to avoid this effect you can set the
#' initial value of the generator seed.
#' @param sample_size A numeric indicating the number of samples to be taken from
#' the probability distribution of the proportion of ants from the upper nest segment
#' @importFrom purrr map_chr map walk
#' @importFrom stringi stri_extract
#' @export
figure_6 <- function(sample_size = 1e4){
  data("time_series_results", package = "overwintering", envir = environment())
  data("colony_stats", package = "overwintering", envir = environment())
  # calculate slope values after probability distribution sampling
  empirical_slopes <- calculate_regr_coeffs(time_series_results=time_series_results,
                                            colony_stats=colony_stats,
                                            sample_size=sample_size)
  randomized_slopes <- calculate_regr_coeffs(time_series_results=time_series_results,
                                             colony_stats=colony_stats,
                                             sample_size=sample_size,
                                             randomize=TRUE)

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
              data = subset(plot_data, HDI_mask), alpha = 0.5,
              position = 'identity') +
    facet_grid(.~colony) +
    labs(linetype = "Legend", x = "Slope value",y = "Count (density)") +
    theme(panel.background = element_rect(fill = "white", colour = "#c8c8c8")) +
    theme(panel.grid.major = element_line(colour = "#c8c8c8",size = 0.05)) +
    theme(axis.text.x = element_text(angle = 90))
}

#' Figure 3 legend
#'
#' Plot the legend for Figure 3 from the publication.
#' @export
plot_legend_fig_3 <- function(){
  plot.new()
  plot.window(c(0,14), c(-5,30))
  for(i in 0:200){
    # define color value for the gray scale representing
    # ant distribution along depth gradient
    val=exp(-i/200)-((1-(exp(-i/200)))/(1-exp(-1)))*exp(-1)

    if(i==0){text(2.8, 0.05, paste(as.character(i*0.05),'%',
                                   sep=''), cex=1.2)
      rect(c(0.3,0.3), c(-0.5,-0.5), c(2,2), c(0.1, 0.1),
           col=rgb(val, val, val), lwd=0.1, border=rgb(val, val, val))
      lines(c(2,2.15), c(0.05,0.05))
    }
    else {
      if(i==200)
      {text(3.4, i*0.1+0.05, paste('>= ',as.character(i*0.05),
                                   '%',
                                   sep=''), cex=1.2)
        rect(c(0.3,0.3), c(20,20), c(2,2), c(20.6,20.6),
             col=rgb(val, val, val), lwd=0.1, border=rgb(val, val, val))
        lines(c(2,2.15), c(i*0.1+0.05,i*0.1+0.05))
      }
      else{
        if (i%%20==0)
        {text(2.8, i*0.1+0.05, paste(as.character(i*0.05), '% ',
                                     sep=''), cex=1.2)
          lines(c(2,2.15), c(i*0.1+0.05,i*0.1+0.05))
        }
        rect(c(0.3,0.3), c(i*0.1,i*0.1), c(2,2), c(i*0.1+0.1,i*0.1+0.1),
             col=rgb(val, val, val), lwd=0.1, border=rgb(val, val, val))

      }
    }
  }
  rect(c(0.3,0.3), c(-0.5,-0.5), c(2,2), c(20.6, 20.6),
       col=rgb(0,0,0, alpha=0), lwd=1.2, border='black')
  # add symbols indicating queen and median depth
  points(1.15, -2, col='#bc0900', cex=2.2,pch=21,bg='#fb8b06')
  text(2.3, -2, 'queen', cex=1.2, pos=4)
  for (i in seq(0.3,2, by=0.14)){
    points(i, -4, pch=15, cex=0.3, col='#00AB25')
  }
  text(2.3, -4.8, 'mean depth \nof workers', cex=1.2, pos=4)
}


vertical_distribution_plot_colony <- function(colony, colony_metadata, vert_distribution){
  queen_depths <- colony_metadata[[colony]]$queens_depth
  distr_data <- vert_distribution[[colony]]
  # calculate mean worker position along depth gradient
  mean_depth <- sum(distr_data$prop*(distr_data$Depth_from+0.5))
  if (colony=='18-14'){ # in this colony part of workers overwintered in a tree sump
    for (i in 1:nrow(distr_data)){
      # adjust color to normalized ant density along vertical axis
      val = exp(-distr_data$prop[i]*20)-((1-(exp(-distr_data$prop[i]*20)))/(1-exp(-2)))*exp(-2)
      if (val<0) val=0 # if the proportion is greater than 10% the color is black
      rect(c(0,0), c(65-i,65-i), c(10,10), c(66-i,66-i), border=rgb(val, val, val), col=rgb(val, val, val), lwd=0.1)
      if (val<0.5) text_col=1
      else text_col=0
      if (distr_data$prop[i]>0)
        text(5,65.5-i,as.character(round(distr_data$prop[i]*100, digits=1)),cex=0.9,col=rgb(text_col,text_col,text_col))
    }
    text(5, 68, colony, cex=1.7)
    rect(c(0,0), c(0,0), c(10,10), c(65,65), lwd=1.3)
    text(5, -3, as.character(colony_metadata[[colony]]$colony_size), cex=1.9)
  }
  else{
    for (i in 1:nrow(distr_data)){
      val=exp(-distr_data$prop[i]*10)-((1-(exp(-distr_data$prop[i]*10)))/(1-exp(-1)))*exp(-1)
      if (val<0) val=0
      if (val<0.5) text_col=1
      else text_col=0

      rect(c(0,0), c(60-i,60-i), c(10,10), c(61-i,61-i), border=rgb(val, val, val), col=rgb(val, val, val), lwd=0.1)
      if (distr_data$prop[i]>0.0007)
        text(5,60.5-i,as.character(round(distr_data$prop[i]*100, digits=1)),cex=0.9,col=rgb(text_col,text_col,text_col))
    }
    text(5, -3, as.character(colony_metadata[[colony]]$colony_size), cex=1.9)
    text(5, 63, colony, cex=1.7)
    rect(c(0,0), c(0,0), c(10,10), c(60,60), lwd=1.3)
  }
  for (i in seq(0.2,10, by=0.8)){
    points(c(i), c(60-mean_depth), pch=15, cex=0.5, col='#00AB25')
  }
  for (i in seq_along(queen_depths)){
    if (i<length(queen_depths)){
      if(queen_depths[i]==queen_depths[i+1])
        points(c(1.5, 8.5), rep(60-queen_depths[i],2),
               col='#bc0900', cex=3,pch=21,bg='#fb8b06')
      else points(2, 60-queen_depths[i], col='#bc0900', cex=3,pch=21,bg='#fb8b06')}
    else {
      if (i>1){
        if(queen_depths[i-1]==queen_depths[i])
          next
        else
          points(2, 60-queen_depths[i], col='#bc0900', cex=3,pch=21,bg='#fb8b06')
      }
      else{
        points(2, 60-queen_depths[i], col='#bc0900', cex=3,pch=21,bg='#fb8b06')
      }
    }
  }
}

#' Figure 3
#'
#' Plot the main part of the Figure 3 from the publication.
#' @param vert_distribution A list of data frames with the data on the density of ants along
#' vertical axis.
#' @param colony_metadata A list of lists with the various information about a colony. Retirved by
#' calling \code{data(colony_metadata)}
#' @details
#' Note that the total number of workers for part of colonies are greater that those
#' reported in the publication. The reason for this is that here we take into account all the
#' excavated workers whereas in the publication the sum was calculated from only
#' those individuals whose position was determined. As mentioned in the publication,
#' for some of the workers I failed to determine the depth at which they stayed before
#' excavation.
#' @export
vertical_distribution_plot <- function(vert_distribution, colony_metadata){
  par(mfrow=c(1,12))
  par(mar=rep(0.3,4))
  plot.new()
  plot.window(c(-6,4), c(-5,72))
  lines(c(3,3), c(0,60), lwd=1)
  for (i in 0:6){
    lines(c(2.5,3), c(60-(i*10), 60-(i*10)), lwd=1)
    text(0,60-(i*10), as.character(i*10), cex=1.9)
  }
  text(-4, 33, 'depth [cm]', cex=2.1, srt=90)
  text(-1, -3, 'Number of \n workers', cex=1.6)
  text(-1.4, 63, 'Colony ID', cex=1.6)
  # render bars to represent vertical distribution of ants
  for (colony in names(vert_distribution)){
    par(mar=rep(0.3,4))
    plot.new()
    plot.window(c(-2,11), c(-5,72))
    vertical_distribution_plot_colony(colony, colony_metadata, vert_distribution)
  }
}
