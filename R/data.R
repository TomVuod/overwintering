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
