#' @import shiny


goldilocks <- function(){
  shinyApp(ui = ui(),
         server = server,
         enableBookmarking = "url")
}
