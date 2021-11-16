#' Main function of running scGNN feature auto-encoder
#'
#' The main function for performing dimensional reduction on the preprocessed scRNA-seq data
#'
#' @param scDataset A scDataset object containing scRNA-seq data
#' @param LTMG_mat LTMG sparse matrix. If provided, then LTMG regularisation will be applied. Optional.
#' @param hyperParams A list of hyperparameter to tune the model. Optional.
#' @param hardwareSetup A list of parameters to setup the hardware on which the model runs. Optional.
#'
#' @return A matrix representing the encoded space of the scRNA-seq matrix.
#'
#' @examples
#' # Example 1:
#' # Tested examples. Not run for fast package compiling
#' \dontrun{
#' # Accessing the demo gene_counts_small dataset available with the package
#' inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
#' # Preprocess the raw counts
#' counts <- preprocessCSV(path = inputCountsPath)
#' ltmg <- runLTMG(counts)
#' hyperParams <- setHyperParams(regu_epochs = 5L)
#' hardwareSetup <- setHardware(coresUsage = 1L)
#' z <- runFeatureAE(scDataset = counts, LTMG_mat = ltmg, hyperParams, hardwareSetup)
#'}
#'
#' @references
#' \insertRef{scGNN}{scRGNet}
#' \insertRef{LTMG}{scRGNet}
#' \insertRef{torch}{scRGNet}
#' \insertRef{progress}{scRGNet}
#' \insertRef{matrix}{scRGNet}
#'
#' @export
#' @importFrom Rdpack reprompt
#' @importFrom methods is
#' @importFrom coro loop
#' @import torch
#' @import progress
#' @import Matrix
runFeatureAE <- function(scDataset,
                         LTMG_mat       = NULL,
                         hyperParams    = list(
                         "batch_size"   = 1L, ## default set to 1
                         "regu_epochs"  = 5L,
                         "L1"           = 0.5,
                         "L2"           = 0.5,
                         "regu_alpha"   = 0.9,
                         "reduction"    = "sum"),
                         hardwareSetup = list(
                             "CUDA"      = F,
                             "coresUage" = 5L ## reset to 1 on submission
                         )
                        ) {

    if (is.null(scDataset)) {
        stop("Must provide a scDataset object to perform analysis.")
    } else {
        if (!methods::is(scDataset, "scDataset"))
            stop("Invalid input. Must provide a scDataset object.")
    }

    if(!is.null(LTMG_mat)) {
        if (!is.matrix(LTMG_mat)) {
            stop("Invalid argument for LTMG_mat. Must be a matrix.")
        }
    }

    #if (dir.exists(outputDir) == FALSE)
    #    dir.create(outputDir)

    if (hardwareSetup$CUDA){
        device <- torch::torch_device(type = "cuda")
    } else {
        device <- torch::torch_device(type = "cpu")
        torch::torch_set_num_threads(hardwareSetup$coresUage)
    }

    sample_list <- scDataset$features@Dimnames[[1]]
    gene_list   <- scDataset$features@Dimnames[[2]]

    train_loader <- torch::dataloader(dataset     = scDataset,
                                      batch_size  = hyperParams$batch_size,
                                      shuffle     = FALSE,
                                      pin_memory  = ifelse(hardwareSetup$CUDA, TRUE, FALSE),
                                      num_workers = ifelse(hardwareSetup$CUDA, 1, 0))


    useLTMG <- FALSE
    if (!is.null(LTMG_mat)) {
        useLTMG  <- TRUE
        LTMG_mat <- torch::torch_tensor(LTMG_mat)
    }

    ## TODO: Add new function to use variational feature auto-encoder
    model     <- AE(scDataset$features@Dim[2])$to(device = device)
    optimiser <- torch::optim_adam(params = model$parameters, lr = 1e-3)
    message("Torch Model Ready...")

    message("Start training...")

    #stateStart <- list(
    #    "state_dict" = model$state_dict(),
    #    "optimiser"  = optimiser$state_dict()
    #)

    #rdaFileStart <- file.path(outputDir, paste(datasetName, "rda",sep = "."))
    #torch::torch_save(stateStart, rdaFileStart)

    train_output <- list() ## Initialised to save model output

    pb <- progress_bar$new(
        format = "  Training [:bar] :current/:total (:percent) Averaged Loss: :risk Time taken: :elapsedfull",
        total  = hyperParams$regu_epoch, clear = FALSE, show_after = 0, width = 95)
    for (epoch in seq(hyperParams$regu_epoch)) {
        train_output <- train(epoch        = epoch,
                              train_loader = train_loader,
                              model        = model,
                              optimiser    = optimiser,
                              regu_mat     = LTMG_mat,
                              hyperParams  = hyperParams,
                              device       = device,
                              EMflag       = FALSE)
        pb$tick(token = list(risk = as.character(format(round(train_output$risk, 2), nsmall = 3))))
    }

    #zOut <- train_output$z$detach()$cpu()

    ## Store reconOri for imputation (currently not available)
    #rdaStatus <- model$state_dict()
    #recon           <- Matrix::as.matrix(train_output$recon)
    #rownames(recon) <- sample_list
    #colnames(recon) <- gene_list
#
    #original           <- Matrix::as.matrix(train_output$original)
    #rownames(original) <- sample_list
    #colnames(original) <- gene_list

    z           <- Matrix::as.matrix(train_output$z)
    rownames(z) <- sample_list

    ## Proceed to inferring cell-type...
    #return(
    #    list(
    #        "recon"    = recon,
    #        "original" = original,
    #        "z"        = z,
    #        "risk"     = train_output$risk
    #    )
    #)
    return(z)
}

#' Hyper-parameter setter
#'
#' A function to set up the hyper-parameter to tune the auto-encoder
#' @param batch_size Batch size used to train the auto-encoder
#' @param regu_epochs Epoch of trainning; that is, the number of time the dataset is visited.
#' @param L1 Intensity of L1 penalty on loss if LTMG matrix is used. Takes value in range >=0 and >= 1
#' @param L2 Intensity of L2 penalty on loss if LTMG matrix is used. Takes value in range >=0 and >= 1
#' @param regu_alpha Intensity of LTMG regularisation. Takes value in range >=0 and >= 1
#' @param reduction Type of reduction used in loss functions:
#'     available options: "mean", "sum", or "none", meaning not using reduction method.
#' @return A list of hyper-parameters for runFeatureAE
#'
#' @references
#' \insertRef{scGNN}{scRGNet}
#'
#' @export
setHyperParams <- function(
    batch_size   = 1L, ## default set to 1
    regu_epochs  = 5L,
    L1           = 0.5,
    L2           = 0.5,
    regu_alpha   = 0.9,
    reduction    = "sum"
) {

    if (batch_size < 1) {
        stop("Invalid argument for batch_size. Must be at least 1.")
    } else {
        batch_size <- as.integer(batch_size)
    }

    if (regu_epochs < 1) {
        stop("Invalid argument for regu_epochs. Must be at least 1.")
    } else {
        regu_epochs <- as.integer(regu_epochs)
    }

    if (L1 < 0 | L1 > 1) {
        stop("Invalid argument for L1. Only in range 0 <= L1 <= 1 will be accepted.")
    }

    if (L2 < 0 | L2 > 1) {
        stop("Invalid argument for L2. Only in range 0 <= L2 <= 1 will be accepted.")
    }

    if (regu_alpha < 0 | regu_epochs > 1) {
        stop("Invalid argument for regu_alpha. Only in range 0 <= regu_alpha <= 1 will be accepted.")
    }

    if (!(reduction %in% c('mean', 'sum', 'none'))) {
        stop("Invalid argument for reduction methods. Available methods: mean, sum, or none")
    }

    hyperParams <- list(
        "batch_size"   = batch_size,
        "regu_epochs"  = 5L,
        "L1"           = 0.5,
        "L2"           = 0.5,
        "regu_alpha"   = 0.9,
        "reduction"    = "sum"
    )

    return(hyperParams)
}

#' A hardware setter to run the model
#'
#' Setting up your hardware to train the model faster and get a better result (if you have a powerful one)
#'
#' @param coresUsage The number of CPU cores you would like to use to train the model.
#'     For Mac user, please do not change the default value of 1. This is a special case for Mac OS (sorry)
#' @param CUDA Whther to use Nvidia GPU to train the model via CUDA.
#'     Make sure you have CUDA 10.0 or 11.1 version installed.
#'     Higher version of CUDA is currently not supported by torch.
#'     If CUDA is set to TRUE, coresUsage will not be used. Default FALSE.
#'
#' @return A list of parameters to set up hardware
#'
#' @references
#' \insertRef{scGNN}{scRGNet}
#'
#' @export
setHardware <- function(
    coresUsage = 1L,
    CUDA       = FALSE
) {

    if (!is.logical(CUDA)) {
        stop("Invalid argument for CUDA. Must be logical.")
    }

    if (!is.numeric(coresUsage)) {
        stop("Invalid argument for coresUsage. Must be an integer number.")
    } else {
        if (!CUDA) {
            if (coresUsage < 1) {
                stop("Invalid argument for coresUsage. Must use at least 1 core.")
            } else {
                coresUsage <- as.integer(coresUsage)
            }
        }
    }

    hardwareSetup = list(
        "CUDA"      = CUDA,
        "coresUage" = coresUsage
    )

    return(hardwareSetup)
}
