make.zoom.plot = function(data, I, Min, Max){
  upper <- as.numeric(I[,"Maxs"])
  lower <- as.numeric(I[,"Mins"])
  N <- length(upper)

  # The Lims makes it zoom!!!!!
  Lims = Round(Min = Min, Max = Max, N = N)

  for(i in Lims[1]:Lims[2]){
    data[[i - Lims[1] + 1]] <- data[[i]]/upper[i]
  }

  C = colnames(data)[c(Lims[1]:Lims[2], N+1)]
  data = data[,c(1:(Lims[2] - Lims[1] + 1), N+1)]
  colnames(data) = C

  # Basic plot to update
  p = ggparcoord(data, columns = 1:(Lims[2] - Lims[1] + 1), groupColumn = dim(data)[2],
                 scale = "globalminmax", shadeBox = NULL) + coord_cartesian(ylim = c(0,1))

  ## Using the colours that we like
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

  ## Updating N for the number of columns in the zoom graph
  ## So that the labels are right and don't force the graph
  ## to make extra columns
  N = (Lims[2] - Lims[1] + 1)

  # Calculate label positions for each veritcal bar
  lab_x <- rep(1:(N), times = 2) # 2 times, 1 for min 1 for max
  lab_y <- rep(c(0, 1), each = (N))

  # min and max values from original dataset
  lab_z <- c(rep(0, N), upper[Lims[1]:Lims[2]])

  # Convert to character for use as labels
  lab_z <- as.character(lab_z)

  # Add labels to plot
  p <- p + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)

  # Display parallel coordinate plot
  print(p)
}
