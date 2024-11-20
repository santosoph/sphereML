#' A shiny application of sphereML
#'
#' In this package, a machine learning based analysis can be performed to predict students' performance outcomes in physics.
#' @return A user interface of shiny application.
#' @export sphereML
#' @import shinydashboard
#' @import spheredata
#' @import lavaan
#' @import semPlot
#' @import CTT
#' @import mirt
#' @import shinycssloaders
#' @import FSelectorRcpp
#' @import randomForest
#' @import caret
#' @import caTools
#' @import pROC
#' @import GA
#' @import psych
#' @import readxl
#' @examples
#' library(sphereML)
#' sphereML()

sphereML <- function() {

  sphereML_pack <- c("shiny", "shinydashboard", "spheredata", "DT", "lavaan", "semPlot", "CTT", "mirt", "psych",
                     "shinycssloaders", "FSelectorRcpp", "randomForest", "caret", "caTools", "pROC", "GA", "readxl")

  for (i in 1:length(sphereML_pack)){
    requireNamespace(sphereML_pack[i], quietly = TRUE)
  }

  appDir <- system.file("shiny-apps", package = "sphereML")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing Statsomat.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
