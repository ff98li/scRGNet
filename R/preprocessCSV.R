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
#' @param cell_zero_ratio A double-precision value strictly greater than 0 and
#'     less than 1. It defines the maximum proportion of genes with zeros in cells.
#'     Default is 0.99, which means cells with more than 99% genes that are
#'     zeros will be filtered out.
#' @param gene_zero_ratio A double-precision value strictly greater than 0 and
#'     less than 1. It defines the maximum proportion of zeros in genes.
#'     Default is 0.99, which means genes with more than 99% zero values
#'     will be filtered out.
#' @param geneSelectNum A positive integer that defines how many most variant
#'     genes to keep. The default is 2000, which 2000 genes, ranked from
#'     most variant to least by their variances of expression values in each
#'     sample, will be retained in the final pre-processed data and be used
#'     for later analysis.
#' @param transpose A Boolean value telling the function whether the input
#'     scRNA-seq matrix is given in a transposed manner, i.e.
#'     cells as rows and genes as columns.
#'     If TRUE, a transpose operation will be performed. Default is FALSE.
#' @param toCSV A Boolean value indicating whether to save the pre-processed
#'     expression matrix to a CSV file. If TRUE a CSV file with <savename> will
#'     be saved to /output directory under user's current working directory.
#' @param outdir_path A character vector that designates the path to the directory where
#'     the output CSV will be saved if toCSV is TRUE. This argument won't be
#'     used if toCSV is FALSE. There must be no path separator at the end.
#'     If the designated does not exist in the parent directory,
#'     it will create one automatically. Default is a directory named "output"
#'     under user's current R working directory.
#' @param savename A character vector representing the name of the csv file that saves
#'     pre-processed scRNA-seq data. There is no strict rule for this. However,
#'     it is highly recommended to give it a meaningful name.
#'
#' @return Returns a matrix with the pre-processed scRNA-seq expression values,
#'     where row names are genes and column names are cells.
#'
#' @examples
#' # Example 1:
#' # Accessing the demo gene_counts_small dataset available with the package
#' inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
#' # Preprocess the raw counts
#' counts <- preprocessCSV(path = inputCountsPath, toCSV = FALSE, geneSelectNum = 50)
#'
#' # Example 2:
#' # Preprocess the whole scRNA-seq dataset
#' \dontrun{
#' inputCountsPath <- system.file("extdata", "GSE138852_counts.csv.gz", package = "scRGNet")
#' counts <- preprocessCSV(path = inputCountsPath, savename = "GSE138852_preprocessed")
#' # Take ~40 seconds to process. If log_transform = FALSE then ~20 seconds
#' }
#'
#' @references
#' \insertRef{scGNN}{scRGNet}
#'
#' @export
#' @import R.utils
#' @importClassesFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom data.table fread transpose
#' @importFrom utils write.csv
preprocessCSV <- function(path,
                          log_transform   = TRUE,
                          cell_zero_ratio = 0.99,
                          gene_zero_ratio = 0.99,
                          geneSelectNum   = 2000L,
                          transpose       = FALSE,
                          toCSV           = TRUE,
                          outdir_path     = file.path(getwd(), "output"),
                          savename        = "preprocessedCSV") {

    if (!(is.character(path) & (nchar(path) > 0))) {
        stop("Invalid file path. Must be a string of non-zero length.")
    } else {
        if (file.exists(path)) {
            ## check whether given data file is in csv format
            if (!grepl(pattern = "\\.(csv|gz)$", path))
                stop("Input file is not a valid csv file.")
        } else {
            stop(paste("File not found:", path, "does not exist."))
        }
    }

    if (is.numeric(gene_zero_ratio)) {
        if (gene_zero_ratio <= 0 || gene_zero_ratio >= 1)
            stop("gene_zero_ratio out of range: Must be within 0 to 1")
    } else {
        stop("Invalid input for gene_zero_ratio: Must be numeric")
    }
    if (is.numeric(cell_zero_ratio)) {
        if (cell_zero_ratio <= 0 || cell_zero_ratio >= 1)
            stop("cell_zero_ratio out of range: Must be within 0 to 1")
    } else {
        stop("Invalid input for cell_zero_ratio: Must be numeric")
    }

    if (is.numeric(geneSelectNum)) {
        geneSelectNum <- as.integer(geneSelectNum)
    } else {
        stop("Invalid input for geneSelectNum: Must be numeric")
    }

    if (!is.logical(log_transform))
        stop("Invalid input for log_transform: Must be logical")
    if (!is.logical(transpose))
        stop("Invalid input for transpose: Must be logical")

    if (is.logical(toCSV)) {
        if (toCSV) {
            if (!is.character(savename))
                stop("Invalid input for savename: Must be a string")
            if (!is.character(outdir_path))
                stop("Invalid input for output directory path: Must be a string")
        }
    } else {
        stop("Invalid input for toCSV: Must be logical")
    }


    message("CSV file found. Start pre-processing data...")
    start_time <- Sys.time() ## measure CSV processing time

    ## Load scRNA-seq matrix using data.table for faster loading time
    counts_raw <- data.table::fread(path)
    ## transpose data table if raw matrix given in transposed manner
    if (transpose)
        counts_raw <- data.table::transpose(counts_raw)

    ## using matrix for consequential processing
    ## since data.table doesn't support row names indexing
    counts_raw <- as.matrix(counts_raw, rownames = 1)

    ## Find genes with zero ratio over gene_ratio
    valid_gene <- apply(
        counts_raw,
        MARGIN = 1,
        function(x){
            ## Proportion of non-zero counts for a gene
            nonzero_ratio <- mean(as.logical(x))
            if (nonzero_ratio >= (1 - gene_zero_ratio)) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    )

    ## filters out genes with more than (gene_ratio x 100%) genes that are zeros
    counts_filtered_gene <- counts_raw[which(valid_gene), ]

    n_gene <- dim(counts_filtered_gene)[1] ## number of genes after filtering
    if (n_gene == 0) {
        stop("No genes left after filtering. Consider using a higher gene_zero_ratio.")
    } else {
        message(
        sprintf(
            "After preprocessing, %i genes with at most %f zero count remaining",
            n_gene,
            gene_zero_ratio
            )
        )
    }

    ## free up memory
    rm(list = c("valid_gene", "counts_raw"))
    invisible(gc())

    valid_cell <- apply(
        counts_filtered_gene,
        MARGIN = 2,
        function(x) {
            ## Proportion of non-zero genes in a cell
            cell_non_zero_gene_ratio <- mean(as.logical(x))
            if (cell_non_zero_gene_ratio >= (1 - cell_zero_ratio)) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    )

    ## filters out cells with more than (cell_ratio x 100%) genes that are zeros
    counts_filtered_cell <- counts_filtered_gene[, which(valid_cell)]

    ## number of cell samples remaining after filtering
    n_cell <- dim(counts_filtered_cell)[2]
    if (n_cell == 0) {
        stop("No cell left after filtering. Consider using a higher cell_zero_ratio.")
    } else {
        message(
        sprintf(
            "After preprocessing, %i cells have at most %f zero gene counts",
            n_cell, cell_zero_ratio
            )
        )
    }

    ## free up memory
    rm(list = c("counts_filtered_gene", "valid_cell"))
    invisible(gc())

    ## Manually compute gene-wise variance rather than using apply + stats::var
    ## 200x faster than apply(counts_filtered_cell, 1, stats::var)
    ## Improved computation run-time
    ## Source:
    ## https://stackoverflow.com/questions/25099825/row-wise-variance-of-a-matrix-in-r
    gene_means <- rowMeans(counts_filtered_cell)
    n_cell     <- dim(counts_filtered_cell)[2]
    varGene    <- rowSums((counts_filtered_cell - gene_means)^2)/(n_cell - 1)

    ## Check how many genes available to select
    n_gene <-dim(counts_filtered_cell)[1]
    if (geneSelectNum < n_gene) {
        message(
            sprintf("Remaining genes are fewer than %i. All remaining %i genes will be selected.",
                    geneSelectNum, n_gene)
            )
        geneSelectNum <- n_gene
    }

    ## select <geneSelectNum> of most variant genes
    varGene <- names(sort(varGene, decreasing = TRUE)[1:geneSelectNum])
    counts_processed <- counts_filtered_cell[varGene, ]

    ## free up memory
    rm(list = c("gene_means", "varGene", "counts_filtered_cell", "n_cell", "n_gene"))
    invisible(gc())

    if (log_transform)
        counts_processed <- log1p(counts_processed)

    if (toCSV) {
        if (!dir.exists(outdir_path))
            dir.create(outdir_path)

        utils::write.csv(counts_processed,
                         file = file.path(outdir_path,
                                          paste(savename, "csv", sep = ".")
                                          )
                         )
    }

    finish_time  <- Sys.time()
    process_time <- finish_time - start_time
    message(sprintf("Time taken for pre-processing data: %f", process_time))

    return(counts_processed)
}

#' Get discretised regulatory signals from LTMG model
#'
#' A wrapper function for running scGNNLTMG that uses
#' the Left-Trunctruncated-Mixed-Gaussian(LTMG) model to
#' translate the input gene expression data pre-processed by preprocessCSV into
#' a discretised regulatory signal as the regularizer for
#' the feature autoencoder of scGNN.
#' It will take around 10-15 minutes for inferring LTMG tags, depending on
#' the dataset.
#' This step is optional but highly recommended prior to scGNN analysis.
#'
#' @param expr_mat A matrix or a data frame with row names as genes
#'     and column names as cell samples containing scRNA-seq expression values
#'     pre-processed by preprocessCSV. If reading from a expression file,
#'     this argument needs not be provided.
#' @param fromFile A Boolean value indicating whether to read RNA-seq matrix
#'     from a expression file. If TRUE, expr_mat needs not be provided, and
#'     it will check whether the expression file exists in readPath.
#'     If FALSE, expr_mat must be provided.
#' @param readPath A character vector representing path to the expression file.
#'     If fromFile is TRUE, it must be provided. Otherwise optional.
#' @param toFile   A Boolean value indicating whether to write the returned
#'     LTMG object to file. If TRUE, outdir_path and savename must be provided.
#' @param outdir_path A character vector representing the path to the parent
#'     directory where the object will be saved as a file. If toFile is TRUE,
#'     it must be provided. Otherwise optional.
#' @param savename A character vector representing the name of the saved
#'     LTMG object file. If toFile is TRUE, it must be provided.
#'     Otherwise optional.
#'
#' @return Returns an LTMG object containing the inferred LTMG tags of expr_mat.
#'
#' @examples
#' # Example 1:
#' # Tested examples. Not run for fast package compiling
#' \dontrun{
#' # Accessing the demo gene_counts_small dataset available with the package
#' inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
#' # Preprocess the raw counts
#' counts <- preprocessCSV(path = inputCountsPath, toCSV = FALSE, geneSelectNum = 50)
#' LTMG <- runLTMG(expr_mat = counts, toFile = FALSE)
#' # ~75 seconds
#'}
#' # Example 2:
#' # Preprocess the whole scRNA-seq dataset
#' \dontrun{
#' inputCountsPath <- system.file("extdata", "GSE138852_counts.csv.gz", package = "scRGNet")
#' counts <- preprocessCSV(path = inputCountsPath, savename = "GSE138852_preprocessed")
#' LTMG <- runLTMG(expr_mat = counts)
#' # Take 10-14 minutes to finish running
#' }
#'
#' @references
#' \insertRef{LTMG}{scRGNet}
#' \insertRef{scGNN}{scRGNet}
#'
#' @export
#' @importFrom Rdpack reprompt
#' @import scGNNLTMG
#' @importClassesFrom data.table data.table
#' @importFrom data.table fread
runLTMG <- function(expr_mat    = NULL,
                    fromFile    = FALSE,
                    readPath    = NULL,
                    toFile      = FALSE,
                    outdir_path = file.path(getwd(), "output"),
                    savename    = "ltmg") {
    ## checking validity for inputs
    if (is.logical(fromFile) && !is.na(fromFile)) {
        if (fromFile) {
            if (is.null(readPath)) {
                stop("No path to data file provided for argument readPath.")
            } else if (is.character(readPath)) {
                stop("Invalid argument for readPath. Must be a character vector.")
            } else {
                if (file.exists(readPath)) {
                    expr_mat = as.matrix(data.table::fread(readPath),
                                         rownames = 1)
                } else {
                    stop(paste("File not found:", readPath, "does not exist."))
                }
            }
        } else {
            if (is.null(expr_mat))
                stop("No expression matrix or data frame provided for argument expr_mat.")
        }
    } else {
        stop("Invalid argument fromFile. Must be a boolean value.")
    }

    if (is.logical(toFile) & !is.na(toFile)) {
        if (toFile) {
            ## validate save path and save name only if user choose to save file.
            if (!(is.character(outdir_path) & (nchar(outdir_path) > 0)))
                stop("Invalid argument outdir_path. Must be a string of non-zero length.")
            if (!(is.character(savename) & (nchar(savename) > 0)))
                stop("Invalid argument savename. Must be a string of non-zero length.")
        }
    }

    message("Starting inferring LTMG from expression matrix...")
    start_time <- Sys.time()
    LTMGObject <- scGNNLTMG::CreateLTMGObject(expr_mat)
    LTMGObject <- scGNNLTMG::RunLTMG(LTMGObject, Gene_use = 'all')
    if (toFile) {
        message("Writing LTMG object to file...")
        scGNNLTMG::WriteSparse(LTMGObject, gene.name=FALSE, cell.name=FALSE)
    }

    finish_time  <- Sys.time()
    process_time <- finish_time - start_time
    message(sprintf("Time taken for inferring LTMG: %f", process_time))

    return(LTMGObject)
}

# [END]
