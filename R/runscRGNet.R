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
runTestingPackage <- function() {
    appDir <- system.file("shiny-scripts", package = "scRGNet")
    shiny::runApp(appDir, display.mode = "normal")
    return()
}

# [END]
