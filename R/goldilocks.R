#' @import shiny
#' @import ggplot2
NULL

#' This function takes the ui and server from `{goldilocksTwoArm}`
#' and runs it locally.
#'
#' @export
#' @examples
#'  \dontrun{goldilocksTwoArm()}
goldilocksTwoArm <- function(){
  shiny::shinyApp(ui = ui(),
         server = server,
         enableBookmarking = "url")
}

