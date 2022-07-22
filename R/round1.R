## This is used for the zoom graph
## So that when we double click on the
## Zoom graph the one we click on is
## the one where the colours come from.
Round1 <- function(Max, Min, DC, K){
  ## Variables in the plot
  A = Round(Min, Max, K)

  ## Finds the placement of the variables on the zoom plot
  SEQ = (A[1]:A[2] - A[1])*1/(A[2]-A[1])

  ## Chooses the one closest to the double click
  B = abs(DC - SEQ)

  ## Outputs the min value transformed to the original plot, not the zoom plot
  which(B == min(B)) + A[1] - 1
}
