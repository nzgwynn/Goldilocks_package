#' @import shiny
#' @import ggplot2
NULL

#' This function takes the ui and server from `{goldilocks}` and runs it locally.
#'
#' @export
#' @examples
#'  \dontrun{goldilocks()}
goldilocks <- function(){
  shiny::shinyApp(ui = ui(),
         server = server,
         enableBookmarking = "url")
}
