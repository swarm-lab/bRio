#' @export
brio <- function(...) {
  app_path <- paste0(find.package("bRio"), "/app/bRio")

  shiny::runApp(app_path, ...)
}
