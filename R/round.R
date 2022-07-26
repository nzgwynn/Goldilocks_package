## Because of the way that shiny returns values from interactive graphs
## zoom function we have to fidget with the numbers
## a bit to get to the appropriate
#' Interactive portion of the Shiny app that allows
#' the zooming to work by modifying the limits of the
#' zoom plot and rounds them
#'
#' @param Min numeric that is the minimum of the x value from brushing
#' the original plot. The limits of the original plot are `[0,1]` so Min is
#' inside that interval
#' @param Max numeric that is the maximum of the x value
#' from brushing the original. The limits of the original plot are
#' `[0,1]` so Min is inside that interval and greater than Min.
#' @param N numeric of the number of variables used for matching
#'
#' @return a vector of length two that tells the zoom plot the variables to display
#' @noRd
#'
#' @examples
#'
#' Round(Min = .0245, Max = .1534, N = 20)
#'
Round <- function(Min, Max, N){
  ## The x values returned from the graph
  SEQ = c(0, (1:(N))*(1/(N-1)))

  ## returns min and max
  c(max(which(SEQ<=Min)), min(which(SEQ>=Max)))
}
