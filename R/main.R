#' Main function of running scGNN framework for analysis
#'
#' Main
#'
#' @param scDataset A scDataset object containing scRNA-seq data
#' @param outputDir Path to the output directory
#' @param LTMG_mat LTMG sparse matrix. If provided, then LTMG regularisation will be applied. Optional.
#' @param hyperParams A list of hyperparameter to tune the model. Optional.
#' @param hardwareSetup A list of parameters to setup the hardware on which the model runs. Optional.
#'
#' @export
#' @importFrom coro loop
#' @import torch
runSCGNN <- function(scDataset,
                     outputDir,
                     LTMG_mat        = NULL,
                     hyperParams     = list(
                         "batch_size" = 12800L, ## must let user define this...
                         "EM_iteration" = 10L,
                         "regu_epochs" = 500L,
                         "EM_epochs" = 200L,
                         "K" = 7L,
                         "GAEepochs" = 200L,
                         "L1" = 1.0,
                         "L2" = 0.0
                    ),
                    hardwareSetup = list(
                        "CUDA"      = FALSE,
                        "coresUage" = 5 ## reset to 1 on submission
                    )
                ) {

    if (hardwareSetup$CUDA){
        device <- torch::torch_device(type = "cuda")
    } else {
        device <- torch::torch_device(type = "cpu")
        torch::torch_set_num_threads(hardwareSetup$coreUsage)
    }

    train_loader <- torch::dataloader(dataset     = scDataset,
                                      batch_size  = hyperParams$batch_size,
                                      shuffle     = FALSE,
                                      pin_memory  = ifelse(hardwareSetup$CUDA, TRUE, FALSE),
                                      num_workers = ifelse(hardwareSetup$CUDA, 1, 0))

    start_time <- Sys.time() ## count the time to process data.

    useLTMG <- FALSE
    if (!is.null(LTMG_mat)) {
        useLTMG <- TRUE ## TODO: if time permits, implement LTMG regularisation
    }

    ## TODO: Add option to use variational feature auto-encoder
    model     <- AE(length(scDataset))$to(device = device)
    optimiser <- torch::optim_adam(params = model$parameters(), lr = 1e-3)
    message("Torch Model Ready...")

    message("Start training...")

    stateStart <- list(
        "state_dict" = model$state_dict(),
        "optimiser"  = optimiser$state_dict()
    )

    for (epoch in seq(hyperParams$regu_epoch)) {
        train_ouput <- train(epoch, train_loader, model, optimiser, device)
    }

}
