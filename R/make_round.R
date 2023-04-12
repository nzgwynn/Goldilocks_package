## Function that draws the plot and saves it in a file
## Graphing Ks
#' Interactive portion of the shiny app that
#' recolors the plot
#'
#' @param column a string that says which column should be used to color the parallel coordinates plot
#' @param data a dataframe made in make.Ks that need to be colored
#' @param M a number which is the Max number of colors allowed in YlGnBu palette from RColorBrewer is 9
#'
#' @return a dataframe that includes a column with G used to color a plot
#' @noRd
#' @examples
#' df <- data.frame(x = seq(10, 5, length = 5), y = seq(30, 50, length = 5))
#' result_x <- make.order(column = "x", data = df)
#'
make.order = function(column, data, M = 9){
  # M = 9 Max number of colors allowed in YlGnBu pallette from RColorBrewer

  data = data[order(data[, column]),]

  ## Grouping for color
  G =
    factor(rep(1:M,
               each =
                 (dim(data)[1]%/%M + 1))[1:dim(data)[1]])

  data = cbind(data, G)
}
