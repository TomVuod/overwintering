#' A list of data frames representing data on different colonies.
#'
#' @format Each data frame contains following columns:
#'   \describe{
#'     \item{colony}{colony ID}
#'     \item{depth}{depth range, in cm}
#'     \item{worker_number}{worker number within given depth range}
#'     \item{cumulative_number}{sum of worker number from actual and
#'       upper depth ranges}
#'     }
"vert_distribution"

#' A data frame summarising ant removal experiment
#' @format Columns description:
#'   \describe{
#'     \item{N1}{number of workers from the upper part of the nest}
#'     \item{N2}{number of workers from the lower part of the nest}
#'     \item{marked_1}{number of marked workers from the upper part of the nest}
#'     \item{marked_2}{number of marked workers from the lower part of the nest}
#'     \item{removed_1}{number of removed workers from the uper part of the nest
#'     which have been marked}
#'     \item{removed_2}{number of removed workers from the lower part of the nest
#'     which have been marked}
#'     \item{removed total}{total number of removed workers}
#'     \item{colony}{colony ID}
#'   }
"ant_removal"

#' Slopes obtained as a result of the permutation of the time series data
"permutated_slopes"


#' A data frame presenting results of the ant collection event. Ants were
#' collected from the arena - they were active outside the nest.
#' @format Columns description:
#'  \describe{
#'    \item{Date}{Collection date}
#'    \item{Colony}{Colony ID}
#'    \item{Unmarked}{Cumulative number of collected ants which have not
#'    been marked}
#'    \item{Red}{Cumulative number of the ants marked with red paint}
#'    \item{Yellow}{Cumulative number of the ants marked with yellow paint}
#'    \item{Dead_unm}{Cumulative number of the dead unmarked ants}
#'    \item{Dead_red}{Cumulative number of the dead ants marked with red color}
#'    \item{Dead_yellow}{Cumulative number of the dead ants marked with yellow
#'     color}
#'    \item{Removed_1}{Cumulative number of the removed marked ants from the upper
#'    part of the nest}
#'    \item{Removed_2}{Cumulative number of the removed marked ants from the lower
#'    part of the nest}
#'    \item{Upper_part_dead}{Cumulative number of the marked dead ants from the
#'    upper part of the nest}
#'    \item{Lower_part_dead}{Cumulative number of the marked dead ants from the
#'    lower part of the nest}
#'  }
"experiment_course"
#' Colony metadata
#'
#' A list of additional data on the colonies
#'
#' @format Each list item contains the data on a sigle colony. This is a list made up
#' of the following components:
#' \describe{
#'  \item{colony_ID}{The unique colony label}
#'  \item{colony_size}{The number of workers in the colony}
#'  \item{head_width}{A numeric vectors representing head widths of the sampled workers
#'  in micrometers}
#'  \item{measurement_source}{Indicates from which source ants devoted to be measured
#'  were samples. 'Colony' means that ants were samples from directly from the living
#'  population of the colony whereas 'corpses' means ants samples from dead individuals
#'  collected after tranfering colonies to the laboratory}
#'  \item{queens_depth}{A numeric vector indicating the depths at which queens have been found.
#'  The vector length is equivalent to the number of queens.}
#' }
"colony_metadata"
