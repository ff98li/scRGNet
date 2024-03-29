#' Launch Shiny webtool
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#' scRGNet::runscRGNet()
#' }
#'
#' @references
#' \insertRef{shiny}{scRGNet}
#'
#' @export
#' @importFrom shiny runApp
runscRGNet <- function() {
    appDir <- system.file("shiny-scripts", package = "scRGNet")
    shiny_fsize <- options(shiny.maxRequestSize = 1000*1024^2)
    on.exit(options(shiny_fsize), add = TRUE)
    shiny::runApp(appDir, display.mode = "normal")
    return()
}

# [END]
