# server.R
library(shiny)
library(shinyjs)
library(shinybusy)

hide_UI <- function(output_ids){
    lapply(output_ids, function(output_id){
        shinyjs::hide(id = output_id)
    })
}

show_UI <- function(output_ids){
    lapply(output_ids, function(output_id){
        shinyjs::show(id = output_id, anim = TRUE)
    })
}

server <- function(input, output, session) {

    is_local <- Sys.getenv('SHINY_PORT') == "" # disable hardware UI on server

    scRGNet_data <- reactiveValues(
        counts        = NULL,
        LTMG_mat      = NULL,
        hardwareSetup = NULL,
        encoded       = NULL,
        k             = NULL,
        net           = NULL
    )

    # Initialise default reactive hardware values
    hardware_args <- reactiveValues(
        CUDA       = FALSE,
        coresUsage = 1
    )

    # Initialise default hyperparams
    hyper_params <- reactiveValues(
        ltmg        = FALSE,
        batch_size  = 1,
        regu_epochs = 5,
        L1          = 0.5,
        L2          = 0.5,
        regu_alpha  = 0.5,
        reduction   = "sum"
    )

    # ===== FILE UPLOAD HANDLING STARTS ========================================
    inFile <- reactiveValues(
        upload_state = NULL
    )
    output$upload_ui <- renderUI({
        input$reset ## Create a dependency with the reset button
        input$demo
        fileInput(inputId  = "raw_counts",
                  label    = "Upload csv or csv.gz file containing scRNA-seq data",
                  multiple = FALSE,
                  accept   = c(".csv",
                               "text/csv",
                               "text/comma-separated-values",
                               ".csv.gz")
        )
    })

    observeEvent(input$raw_counts, {
        inFile$upload_state <- 'uploaded'
    })
    observeEvent(input$reset, {
        inFile$upload_state <- 'reset'
    })

    observeEvent(input$demo, {
        inFile$upload_state <- "demo"
    })

    file_input <- reactive({
        if (is.null(inFile$upload_state)) {
            return(NULL)
        } else if (inFile$upload_state == 'uploaded') {
            return(input$raw_counts$datapath)
        } else if (inFile$upload_state == 'reset') {
            return(NULL)
        } else if (inFile$upload_state == 'demo') {
            return(system.file("extdata", "GSE138852_small.csv",
                               package = "scRGNet"))
        }
    })

    output$choose <- reactive({
        if (is.null(inFile$upload_state) || inFile$upload_state == "reset")
        {
            paste(
                "Use built-in dataset to run a demo by clicking",
                "Use demo data",
                "without uploading a file."
            )

        } else if (inFile$upload_state == "demo") {
            "Now you can run scRGNet with the built-in demo data."
        } else if (inFile$upload_state == "uploaded") {
            "File format validated. Now you can run scRGNet with your own data."
        }
    })
    output$upload_summary <- renderText({
        if (is.null(inFile$upload_state) || inFile$upload_state == "reset") {
            return("No file uploaded.")
        } else if (inFile$upload_state == "demo") {
            return("Demo dataset loaded.")
        } else if (inFile$upload_state == "uploaded") {
            return(paste("Uploaded file:", input$raw_counts$name))
        }
    })
    # ===== FILE UPLOAD HANDLING ENDS ==========================================


    # ===== PREPROCESS GENE COUNTS STARTS ======================================
    observe({
        if (is.null(file_input())) {
            hide_UI(c("preprocess",
                      "transpose",
                      "log_transform",
                      "cell_zero_ratio",
                      "gene_zero_ratio"))
            #shinyjs::hide("transpose")
            #shinyjs::hide("log_transform")
            #shinyjs::hide("cell_zero_ratio")
            #shinyjs::hide("gene_zero_ratio")
            #shinyjs::hide("preprocess")
        } else {
            show_UI(c("preprocess",
                      "transpose",
                      "log_transform",
                      "cell_zero_ratio",
                      "gene_zero_ratio"))
        }
    })


    counts <- reactive({
        scRGNet::preprocessCSV(
            path            = file_input(),
            transpose       = input$transpose,
            log_transform   = input$log_transform,
            cell_zero_ratio = input$cell_zero_ratio,
            gene_zero_ratio = input$gene_zero_ratio
        )
    })

    observeEvent(input$preprocess, {
        shinybusy::show_modal_spinner(spin  = "cube-grid",
                                      color = "#E95420",
                                      text  = "Preprocessing scRNA-seq counts...")
        # console message to shiny: https://stackoverflow.com/a/30490698
        withCallingHandlers({
            shinyjs::html("preprocess_result", "")
            tryCatch({
                scRGNet_data$counts <- counts()
            },
            warning = function(warn) {
                scRGNet_data$counts <- NULL
                showNotification(paste(warn), type = 'warning')
            },
            error = function(err) {
                scRGNet_data$counts <- NULL
                showNotification(paste(err), type = 'err')
            })
        },
        message = function(m) {
            shinyjs::html(id   = "preprocess_result",
                          html = m$message,
                          add  = TRUE)
        })
        shinybusy::remove_modal_spinner()
    })

    # ===== PREPROCESS GENE COUNTS ENDS ========================================

    # ===== HARDWARE SETUP STARTS ==============================================

    ## Hardware setting UI
    output$hardware_ui <- renderUI({
        verticalLayout(
            numericInput(
                inputId = "coresUsage",
                label   = "CPU cores usage",
                value   = 1
            ),
            checkboxInput(
                inputId = "CUDA",
                label   = "Enable CUDA",
                value   = FALSE
            ),
            fluid = TRUE
        )
    })


    observe({
        if (is.null(file_input()) | is.null(scRGNet_data$counts)) {
            shinyjs::hide("hardware_ui")
        } else {
            if (is_local) {
                shinyjs::show("hardware_ui")
            }
        }
    })

    set_hardware <- reactive({
        scRGNet::setHardware(coresUsage = hardware_args$coresUsage,
                             CUDA       = hardware_args$CUDA)
    })

    if (is_local) {
        observe({
            if (!is.null(input$CUDA))
                hardware_args$CUDA <- input$CUDA
            if (!is.null(input$coresUsage))
                hardware_args$coresUsage <- input$coresUsage
        })
    }

    observe({
        if (is.null(scRGNet_data$counts)) {
            scRGNet_data$hardwareSetup <- NULL
        } else {
            tryCatch({
                scRGNet_data$hardwareSetup <- set_hardware()
            },
            warning = function(warn) {
                showNotification(paste(warn), type = 'warning')
            },
            error = function(err) {
                showNotification(paste(err), type = 'err')
            })
        }
    })

    # ===== HARDWARE SETUP ENDS ================================================

    # ===== HYPERPARAMETERS SETUP STARTS =======================================
    observe({
        if (is.null(scRGNet_data$counts)) {
            output$choose_k <- renderUI({
                numericInput(inputId = "k",
                             label   = "k (default best heuristic)",
                             value   = 1)
            })
        } else {
            output$choose_k <- renderUI({
                numericInput(
                    inputId = "k",
                    label   = "k (default best heuristic)",
                    value   = floor(sqrt(
                        length(scRGNet_data$counts)
                    )),
                    min = 1,
                    max = length(scRGNet_data$counts)
                )
            })
        }
    })

    observe({
        if (is.null(file_input()) || is.null(scRGNet_data$counts)) {
            hide_UI(c("ltmg",
                      "batch_size",
                      "regu_epochs",
                      "L1",
                      "L2",
                      "regu_alpha",
                      "reduction",
                      "choose_k"))
        } else {
            show_UI(c("ltmg",
                      "batch_size",
                      "regu_epochs",
                      "L1",
                      "L2",
                      "regu_alpha",
                      "reduction",
                      "choose_k"))
        }
#
    })

    observe({
        if (!is.null(input$ltmg))
            hyper_params$ltmg <- input$ltmg
        if (!is.null(input$batch_size))
            hyper_params$batch_size <- input$batch_size
        if (!is.null(input$regu_epochs))
            hyper_params$regu_epochs <- input$regu_epochs
        if (!is.null(input$L1))
            hyper_params$L1 <- input$L1
        if (!is.null(input$L2))
            hyper_params$L2 <- input$L2
        if (!is.null(input$regu_alpha))
            hyper_params$regu_alpha <- input$regu_alpha
        if (!is.null(input$reduction))
            hyper_params$reduction <- input$reduction
    })

    set_hyper <- reactive({
        scRGNet::setHyperParams(batch_size  = hyper_params$batch_size,
                                regu_epochs = hyper_params$regu_epochs,
                                L1          = hyper_params$L1,
                                L2          = hyper_params$L2,
                                regu_alpha  = hyper_params$regu_alpha,
                                reduction   = hyper_params$reduction)
    })

    # ===== HYPERPARAMETERS SETUP ENDS =========================================

    observeEvent(input$print, {
        print(hardware_args$CUDA)
        print(hardware_args$coresUsage)
    })

    # ===== MODAL TRAINING ENDS ================================================

    run_ltmg <- reactive({
        scRGNet::runLTMG(scDataset = scRGNet_data$counts)
    })

    # ===== MODAL TRAINING ENDS ================================================
    # ===== GENERATING NETWORK STARTS ==========================================
    # ===== GENERATING NETWORK ENDS ============================================
    # ===== PLOTTING STARTS ====================================================
    # ===== PLOTTING ENDS ======================================================


}

# [END]
