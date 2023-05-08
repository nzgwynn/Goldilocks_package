## This is used for the zoom graph
## So that when we double click on the
## Zoom graph the one we click on is
## the one where the colours come from.
#' Title
#'
#' @param Min numeric that is the minimum of the x value from brushing
#' the original plot. The limits of the original plot are in `[0,1]` so Min is
#' inside that interval
#' @param Max numeric that is the maximum of the x value
#' from brushing the original. The limits of the original plot are
#' in `[0,1]` so Max is inside that interval and greater than Min.
#' @param DC numeric of where on the zoom plot the double click occurred
#' @param K numeric of the number of variables used for matching
#'
#' @return the variable that the plot should use to recolor the plot
#' @noRd
#'
#' @examples
#' Round1(Min = .2045, Max = .5234, DC = .43 , K = 20)
#'
#'
Round1 <- function(Max, Min, DC, K){
  ## Variables in the plot
  A <- Round(Min, Max, K)

  ## Finds the placement of the variables on the zoom plot
  SEQ <- (A[1]:A[2] - A[1])*1/(A[2]-A[1])

  ## Chooses the one closest to the double click
  B <- abs(DC - SEQ)

  ## Outputs the min value transformed to the original plot, not the zoom plot
  which(B == min(B)) + A[1] - 1
}
