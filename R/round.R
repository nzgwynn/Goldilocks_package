## Because of the way that shiny returns values from interactive graphs
## zoom function we have to fidget with the numbers
## a bit to get to the appropriate
#' Title
#'
#' @param Min
#' @param Max
#' @param N
#'
#' @return
#' @export
#'
#' @examples
Round <- function(Min, Max, N){
  ## The x values returned from the graph
  SEQ = c(0, (1:(N))*(1/(N-1)))

  ## returns min and max
  c(max(which(SEQ<=Min)), min(which(SEQ>=Max)))
}
