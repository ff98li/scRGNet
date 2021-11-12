#' Main function of running scGNN framework for analysis
#'
#' Main
#'
#' @param datasetName A character vector representing the name of the dataset used
#' @param outputDir Path to the output directory
#' @param LTMGregularized Whether to use LTMG tags as regularizer
#' @param LTMGpath Path to the LTMG sparse mtx file
#' @param hyperParams A list of hyperparameter to tune the model. Optional.
runSCGNN <- function(datasetName,
                     outputDir,
                     LTMGregularized = FALSE,
                     LTMGpath        = NULL,
                     hyperParams     = list(
                         "EM_iteration" = 10,
                         "regu_epochs" = 500,
                         "EM_epochs" = 200,
                         "k" = 7,
                         "GAEepochs" = 200,
                         "L1" = 1.0,
                         "L2" = 0.0
                     )
                     ) {

}
