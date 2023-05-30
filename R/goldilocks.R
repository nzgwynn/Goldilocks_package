#' @import shiny
#' @import ggplot2
NULL

#' This function takes the ui and server from `{goldilocks_app}`
#' and runs it locally.
#'
#' @export
#' @examples
#'  \dontrun{goldilocks_app()}
goldilocks_app <- function(){
  shiny::shinyApp(ui = ui(),
         server = server,
         enableBookmarking = "url")
}
