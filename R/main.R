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
runFeatureAE <- function(scDataset,
                         LTMG_mat       = NULL,
                         #outputDir     = file.path(getwd(), "outputDir"),
                         #datasetName   = "scData",
                         hyperParams    = list(
                         "batch_size"   = 1L, ## default set to 1
                         "EM_iteration" = 10L,
                         "regu_epochs"  = 2L,
                         "EM_epochs"    = 20L,
                         #"GAEepochs"    = 200L,
                         "L1"           = 1.0,
                         "L2"           = 0.0,
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

    train_loader <- torch::dataloader(dataset     = scDataset,
                                      batch_size  = hyperParams$batch_size,
                                      shuffle     = FALSE,
                                      pin_memory  = ifelse(hardwareSetup$CUDA, TRUE, FALSE),
                                      num_workers = ifelse(hardwareSetup$CUDA, 1, 0))


    useLTMG <- FALSE
    if (!is.null(LTMG_mat)) {
        useLTMG <- TRUE ## TODO: if time permits, implement LTMG regularisation
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
        format = "  Training [:bar] :current/:total (:percent) :elapsedfull",
        total  = hyperParams$regu_epoch, clear = FALSE, width= 60, show_after = 0)
    for (epoch in seq(hyperParams$regu_epoch)) {
        pb$tick(0)
        train_output <- train(epoch        = epoch,
                              train_loader = train_loader,
                              model        = model,
                              optimiser    = optimiser,
                              regu_mat     = LTMG_mat,
                              hyperParams  = hyperParams,
                              device       = device,
                              EMflag       = FALSE)
        #message(sprintf("Epoch: %i / %i   Average Loss: %f",
        #                epoch, hyperParams$regu_epoch,
        #                train_ouput$risk))
    }

    #zOut <- train_output$z$detach()$cpu()

    ## Store reconOri for imputation (currently not available)
    #rdaStatus <- model$state_dict()

    ## Proceed to inferring cell-type...
    return(train_output)
}
