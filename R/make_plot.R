#' The inputs from the Matching tab are used to run make_Ks,
#' then this plot is made.
#'
#'
#' @param data a dataframe output from the make.Ks function
#' @param I a string that is the name of one of the variables
#' which will be used to color the plot, default is the first column
#'
#'
#' @return this makes a parallel coordinate plot
#' @export
#'
#' @examples
#' data <- data.frame(Var_1 = seq(0, 1, length = 9),
#'                  Var_2 = seq(0, 5, length = 9),
#'                  Var_3 = seq(0, 1, length = 9),
#'                  Var_4 = seq(0, 10, length = 9),
#'                  G = as.factor(1:9))
#'
#' I <- matrix(c(c("Var_1", "Var_2", "Var_3", "Var_4"),
#'               rep(1, 4),
#'               c("Var_1", "Var_2", "Var_3", "Var_4"),
#'               rep(0, 4),
#'               c(rep(5, 3), 10)), nrow = 4)
#' colnames(I) = c("cols", "w", "L", "Mins", "Maxs")
#'
#' make.plot(data = data, I = I)
#'
#'
make.plot <- function(data, I){
  upper <- as.numeric(I[,"Maxs"])
  lower <- as.numeric(I[,"Mins"])
  N <- length(upper)

  for(i in 1:N){
    data[[i]] <- data[[i]]/upper[i]
  }

  # Basic plot to update
  p <-  GGally::ggparcoord(data, columns = 1:N,
                           groupColumn = (N + 1),
                           scale = "globalminmax",
                           shadeBox = NULL) +
    coord_cartesian(ylim = c(0,1))

  p <- p + scale_colour_brewer(palette = "YlGnBu")

  # Start with a basic theme
  p <- p + theme_minimal()

  # Decrease amount of margin around x, y values
  p <- p + scale_y_continuous(expand = c(0.02, 0.02))
  p <- p + scale_x_discrete(expand = c(0.02, 0.02))

  # Remove axis ticks and labels
  p <- p + theme(axis.ticks = element_blank())
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(axis.text.y = element_blank())

  # Clear axis lines
  p <- p + theme(panel.grid.minor = element_blank())
  p <- p + theme(panel.grid.major.y = element_blank())

  # Removing the legend
  p <- p + theme(legend.position="none")

  # Adding a border
  p <- p + theme(panel.border = element_rect(colour = "darkgrey",
                                             fill=NA, size=0.5))

  # Calculate label positions for each veritcal bar
  lab_x <- rep(1:(N), times = 2) # 2 times, 1 for min 1 for max
  lab_y <- rep(c(0, 1), each = (N))

  # min and max values from original dataset
  lab_z <- c(rep(0, N), upper)

  # Convert to character for use as labels
  lab_z <- as.character(lab_z)

  # Add labels to plot
  p <- p + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)

  # Display parallel coordinate plot
  print(p)
}


## To test vdiffr
## https://cran.r-project.org/web/packages/vdiffr/index.html
