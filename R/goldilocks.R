#' @import shiny
#' @import ggplot2
NULL

#' Run Shiny app
#'
#' TODO documentation here!
#'
#' @export
#' @examples
#'  \dontrun{goldilocks()}
goldilocks <- function(){
  shiny::shinyApp(ui = ui(),
         server = server,
         enableBookmarking = "url")
}
