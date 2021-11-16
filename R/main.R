#' Main function of running scGNN feature auto-encoder for analysis
#'
#' Main
#'
#' @param scDataset A scDataset object containing scRNA-seq data
#' @param LTMG_mat LTMG sparse matrix. If provided, then LTMG regularisation will be applied. Optional.
#' @param hyperParams A list of hyperparameter to tune the model. Optional.
#' @param hardwareSetup A list of parameters to setup the hardware on which the model runs. Optional.
#'
#' @export
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
#' @param L1 Intensity of L1 penalty on loss if LTMG matrix is used.
#' @param L2 Intensity of L2 penalty on loss if LTMG matrix is used.
#' @param regu_alpha Intensity of LTMG regularisation.
#' @param reduction Type of reduction used in loss functions:
#'     available options: "mean", "sum", or "none", meaning not using reduction method.
#' @return A list of hyper-parameters for runFeatureAE

#' @export
setHyperParams <- function(
    batch_size   = 1L, ## default set to 1
    regu_epochs  = 5L,
    L1           = 0.5,
    L2           = 0.5,
    regu_alpha   = 0.9,
    reduction    = "sum"
) {
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
