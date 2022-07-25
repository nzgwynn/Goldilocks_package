#' @import shiny
#' @import ggplot2


goldilocks <- function(){
  shinyApp(ui = ui(),
         server = server,
         enableBookmarking = "url")
}
