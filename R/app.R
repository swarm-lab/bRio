#' @export
brio <- function(...) {
  app_path <- paste0(find.package("bRio"), "/app/brio")

  shiny::runApp(app_path, ...)
}
