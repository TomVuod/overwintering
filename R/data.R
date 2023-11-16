#' A data frame summarizing ant removal experiment across colonies
#' @format Columns description:
#'   \describe{
#'     \item{colony}{colony ID}
#'     \item{N1}{number of workers from the upper part of the nest}
#'     \item{N2}{number of workers from the lower part of the nest}
#'     \item{marked_1}{number of marked workers from the upper part of the nest}
#'     \item{marked_2}{number of marked workers from the lower part of the nest}
#'     \item{removed_1}{number of removed workers from the uper part of the nest
#'     which have been marked}
#'     \item{removed_2}{number of removed workers from the lower part of the nest
#'     which have been marked}
#'     \item{removed total}{total number of removed workers}
#'   }
"colony_stats"

#' Slopes obtained as a result of the permutation of the time series data
"permutated_slopes"


#' A data frame presenting results of the ant collection event. Ants were
#' collected from the arena - they were active outside the nest.
#' @format Columns description:
#'  \describe{
#'    \item{Date}{Collection date.}
#'    \item{Colony}{Colony ID.}
#'    \item{Unmarked}{Cumulative number of collected ants which have not
#'    been marked.}
#'    \item{Red}{Cumulative number of the ants marked with red paint.}
#'    \item{Yellow}{Cumulative number of the ants marked with yellow paint.}
#'    \item{Dead_unm}{Cumulative number of the dead unmarked ants.}
#'    \item{Dead_red}{Cumulative number of the dead ants marked with red color.}
#'    \item{Dead_yellow}{Cumulative number of the dead ants marked with yellow
#'     color.}
#'    \item{Removed_1}{Cumulative number of the removed marked ants from the upper
#'    part of the nest.}
#'    \item{Removed_2}{Cumulative number of the removed marked ants from the lower
#'    part of the nest.}
#'    \item{Upper_part_dead}{Cumulative number of the marked dead ants from the
#'    upper part of the nest.}
#'    \item{Lower_part_dead}{Cumulative number of the marked dead ants from the
#'    lower part of the nest.}
#'  }
"time_series_results"

#' Colony metadata
#'
#' A list of additional data on the colonies
#'
#' @format Each list item contains the data on a sigle colony. This is a list made up
#' of the following components:
#' \describe{
#'  \item{colony_ID}{The unique colony label.}
#'  \item{colony_size}{The number of workers in the colony.}
#'  \item{head_width}{A numeric vectors representing head widths of the sampled workers
#'  in micrometers.}
#'  \item{measurement_target}{Indicates the source from which measured ants were sampled.
#'  'Colony' means that ants were sampled from directly from the living population of the
#'  colony whereas 'corpses' means ants were sampled from dead individuals
#'  collected after transfering colonies to the laboratory.}
#'  \item{queens_depth}{A numeric vector indicating the depths at which queens have been found.
#'  The vector length is equivalent to the number of queens.}
#' }
"colony_metadata"

#' Worker spatial distribution within the nest
#' 
#' A data frame presenting results of the F. fusca colonies excavation during the winter.
#' @format Columns description:
#'  \describe{
#'    \item{Colony}{Colony ID.}
#'    \item{Point_ID}{ID of the place (nest part) where ants were collected.}
#'    \item{Date}{Collection date. Note that different part of the colonies were 
#'    often collected in different days.}
#'    
#'    \item{Distance to A}{Distance in cm from the central point of the excacated part of the nest
#'     to the reference point A.}
#'    \item{Distance to B}{Distance in cm from the central point of the excacated part of the nest
#'     to the reference point B.}
#'    \item{Distance to C}{Distance in cm from the central point of the excacated part of the nest
#'     to the reference point C.}
#'    \item{Radius}{The maximum distance from the central point of the excavated part of the nest
#'    where ants were found and collected. Measured on horizontal plane.}
#'    \item{Deprt from}{The minimum depth at which ants from the excavated part of the nest
#'    were found and collected.}
#'    \item{Deprt to}{The maximum depth at which ants from the excavated part of the nest
#'    were found and collected.}
#'    \item{Worker number}{Sum of the worker excavated from the corresponding part of the nest.}
#'  }
"spatial_distribution"


#' Reference points 
#' 
#' Data frame presenting distances between points used as reference to define the position of the
#' places (nest parts) where ants were found. Arrangement equal to one means that the order of the points
#' going clockwise was: A -> C -> B.
#' 
"reference_points" 