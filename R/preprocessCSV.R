#' Pre-process scRNA-seq Count Matrix in CSV File
#'
#' A function that reads a CSV file containing scRNA-seq counts
#' and pre-process the scRNA-seq matrix with the given criteria.
#' before feeding it to the scGNN framework for analysis.
#'
#' @param path A string the represents the path to the input file, where
#'     directories in the path are separated by "/", and the input data file
#'     should have extension .csv or .gz
#' @param log_transform A Boolean value indicating whether to perform natural
#'     logarithmic transformation for every value of the data at the end of
#'     pre-processing or not. If TRUE, the log(x+1) of the original expression
#'     level will be calculated.
#' @param cell_ratio A double-precision value strictly greater than 0 and
#'     less than 1. It defines the maximum ratio of genes with zeros in cells.
#'     Default is 0.99, which means cells with more than 99% genes that are
#'     zeros will be filtered out.
#' @param gene_ratio A double-precision value strictly greater than 0 and
#'     less than 1. It defines the maximum ratio of zeros in genes.
#'     Default is 0.99, which means genes with more than 99% zero values
#'     will be filtered out.
#' @param geneSelectNum A positive integer that defines how many most variant
#'     genes to keep. The default is 2000, which 2000 genes, ranked from
#'     most variant to least by their variances of expression values in each
#'     sample, will be retained in the final pre-processed data and be used
#'     for later analysis.
#' @param LTMG A Boolean value indicating whether to discretise regulatory
#'     signal using Left-Trunctruncated-Mixed-Gaussian(LTMG) model.
#'     If TRUE, LTMG inferring will be performed and can take 10-15 minutes.
#'     Although default is FALSE, it is highly recommended to perform LTMG
#'     inferring prior to analysis.
#' @param transpose A Boolean value telling the function whether the input
#'     scRNA-seq matrix is given in a transposed manner, i.e.
#'     cells as rows and genes as columns.
#'     If TRUE, a transpose operation will be performed. Default is FALSE.
#' @param toCSV A Boolean value indicating whether to save the pre-processed
#'     expression matrix to a CSV file. If TRUE a CSV file with <savename> will
#'     be saved to /output directory under user's current working directory.
#' @param savename A string representing the name of the csv file that saves
#'     pre-processed scRNA-seq data. There is no strict rule for this. However,
#'     it is highly recommended to give it a meaningful name.
#'
#' @return Returns a matrix with the pre-processed scRNA-seq expression values,
#'     where row names are genes and column names are cells.
#'
#' @references
#' \insertRef{scGNN}{scRGNet}
#'
#' @export
#' @import R.utils
#' @importClassesFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom data.table fread transpose
#' @importFrom stats var
#' @importFrom utils write.csv
preprocessCSV <- function(path,
                          log_transform = TRUE,
                          cell_ratio    = 0.99,
                          gene_ratio    = 0.99,
                          geneSelectNum = 2000L,
                          LTMG          = FALSE,
                          transpose     = FALSE,
                          toCSV         = TRUE,
                          savename      = "preprocessedCSV") {

    ## TODO: add dependency R.utils
    ## check whether given data file is in csv format
    if (!grepl(pattern = "\\.(csv|gz)$", path)) {
        stop("Input file is not a valid csv file.")
    } else {
        ## check whether file exists in directory
        if (!file.exists(path))
            stop(paste("No such file found in the path:"), path)
    }

    message("CSV file found. Start processing data...")
    start_time <- Sys.time() ## measure CSV processing time
    message("Step 1: filtering data by input criteria...")

    ## Load scRNA-seq matrix if file exists
    counts_raw <- as.matrix(data.table::fread(path), rownames = 1)
    rownames(counts_raw) <- counts_raw[ , 1]

    ## transpose data table if raw matrix given in transposed manner
    if (transpose)
        counts_raw <- data.table::transpose(counts_raw)

    ## Find genes with zero ratio over gene_ratio
    zeroGene <- apply(
        counts_raw,
        MARGIN = 1,
        function(x){
            if (mean(as.logical(x)) >= gene_ratio) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    )

    ## filters out genes with more than (gene_ratio x 100%) genes that are zeros
    counts_filtered_gene <- counts_raw[-which(zeroGene), ]

    ## free up memory
    rm(list = c("zeroGene", "counts_raw"))
    invisible(gc())

    message(
        sprintf(
            "After pre-processing, %i genes remaining",
            nrow(counts_filtered_gene)
            )
        )

    zeroCell <- apply(
        counts_filtered_gene,
        MARGIN = 2,
        function(x) {
            if (mean(as.logical(x)) >= cell_ratio) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    )

    ## filters out cells with more than (cell_ratio x 100%) genes that are zeros
    counts_filtered_cell <- counts_filtered_gene[, -which(zeroCell)]


    ## free up memory
    rm(list = c("counts_filtered_gene", "zeroCell"))
    invisible(gc())

    ## select <geneSelectNum> of most variant genes
    varGene <- apply(counts_filtered_cell, MARGIN = 1, FUNC = stats::var)
    varGene <- names(sort(varGene, decreasing = TRUE)[1:geneSelectNum])
    counts_processed <- counts_filtered_cell[varGene, ]

    ## free up memory
    rm(list = c("varGene", "counts_filtered_cell"))
    invisible(gc())

    if (log_transform)
        counts_processed <- log1p(counts_processed)

    if (LTMG) {
        ; ##TODO: Implement LTMG module
    }

    if (toCSV) {
        output_dir <- paste0(getwd(),"/output")
        if (!dir.exists(output_dir))
            dir.create(output_dir)
        utils::write.csv(counts_processed,
                         file = paste0(output_dir, "/", savename))
    }

    finish_time <- Sys.time()
    process_time <- finish_time - start_time
    message(sprintf("Time taken for pre-processing data: %f", process_time))

    return(counts_processed)
}


# [END]
