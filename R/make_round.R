## Function that draws the plot and saves it in a file
## Graphing Ks
#' Title
#'
#' @param column
#' @param data
#'
#' @return
#' @export
#'
#' @examples
make.order = function(column, data){
  data = data[order(data[, column]),]
  ## Grouping for color
  G = factor(rep(1:M, each = (dim(data)[1]%/%M + 1))[1:dim(data)[1]])
  data = cbind(data, G)
}
