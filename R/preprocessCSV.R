#' @export
#' @import data.table
preprocessCSV <- function(path,
                          log_transform = TRUE,
                          cell_ratio    = 0.99,
                          gene_ratio    = 0.99,
                          geneSelectNum = 2000,
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
