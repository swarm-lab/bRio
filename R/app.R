#' @title Brio Application
#'
#' @description \code{brio} launches the `brio` shiny application.
#'
#' @param ... Optional parameters to be passed to \code{\link[shiny]{runApp}}.
#'
#' @return This function launches the app and returns nothing.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
brio <- function(...) {
  app_path <- paste0(find.package("bRio"), "/app/bRio")

  shiny::runApp(app_path, ...)
}
