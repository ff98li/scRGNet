#' Main Training Model
#'
#' Train in main
#'
#' @param epoch epoch
#' @param train_loader A torch dataloader loaded with an scDataset object
#' @param model A feature auto-encoder object
#' @param optimiser Optimiser
#' @param regu_mat LTMG regulation matrix
#' @param device hardware to train the model
#' @param EMflag Emflag
#' @param EMreguTag whether to use regu_mat in EM process
#'
#' #' @references
#' \insertRef{scGNN}{scRGNet}
#'
#' @importFrom coro loop
train <- function(epoch, train_loader, model, optimiser, regu_mat = NULL, device, EMflag = FALSE, EMreguTag = FALSE) {
    regu <- !(is.null(regu_mat))
    model$train()
    loss       <- 0
    batch_idx  <- 0
    coro::loop(
        for (batch in train_loader) {
            batch_idx <- batch_idx + 1

            if (regu) {
                regu_mat_batch <- regu_mat[, batch_idx]
                regu_mat_batch <- regu_mat_batch$to(device)
            }

            optimiser$zero_grad()
            ## if (is(model, "AE")) ## for later VAE option
            train_output <- model(batch$sample)

            if (EMflag & !EMreguTag) {
                loss <- loss_function_graph(recon_x = train_output$recon,
                                            x = batch$sample$view(c(-1, train_output$sample$shape[2])))
            } else {
                loss
            }
        }
    )
}
