# server.R
library(shiny)
library(shinyjs)
library(shinybusy)
library(scRGNet)
library(visNetwork)

hideUI <- function(input_ids){
    # Purpose:
    #     Hides multiple UI widgets by their input IDs
    # Parameters:
    #     input_ids: A character vector of widgets' input ID
    # Value:
    #     result: hide multiple widgets
    lapply(input_ids, function(input_id){
        shinyjs::hide(id = input_id, anim = TRUE)
    })
}

showUI <- function(input_ids){
    # Purpose:
    #     Makes multiple UI widgets reappear
    # Parameters:
    #     input_ids: A character vector of widgets' input ID
    # Value:
    #     result: show multiple widgets
    lapply(input_ids, function(input_id){
        shinyjs::show(id = input_id, anim = TRUE)
    })
}

## This is one of those few cases where vectorised operations don't apply
## reactiveValues doesn't actually work like a list so I have to do for-loop
inputToReactive <- function(input_var, react_var, ids) {
    # Purpose:
    #     A helper function to save multiple input values into reactive values
    # Parameters:
    #     input_var: the input object
    #     react_var: a reactiveValues object
    #     ids: a character vector of IDs
    # Value:
    #     result: save multiple input values to reactive by matching IDs
    for (i in seq_along(ids)) {
        if (!is.null(input_var[[ids[i]]]))
            react_var[[ids[i]]] <- input_var[[ids[i]]]
    }
}

server <- function(input, output, session) {

    is_local <- Sys.getenv('SHINY_PORT') == "" # disable hardware UI on server

    scRGNet_data <- reactiveValues(
        counts        = NULL,
        LTMG_mat      = NULL,
        hardwareSetup = NULL,
        hyperParams   = NULL,
        encoded       = NULL,
        k             = NULL,
        net           = NULL
    )

    # Initialise default reactive hardware values
    hardware_args <- reactiveValues(
        CUDA       = FALSE,
        coresUsage = 1
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
            hideUI(c("preprocess",
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
            showUI(c("preprocess",
                      "transpose",
                      "log_transform",
                      "cell_zero_ratio",
                      "gene_zero_ratio"))
        }
    })

    observeEvent(input$preprocess, {
        shinybusy::show_modal_spinner(spin  = "cube-grid",
                                      color = "#E95420",
                                      text  = "Preprocessing scRNA-seq counts...")
        # console message to shiny: https://stackoverflow.com/a/30490698
        withCallingHandlers({
            shinyjs::html("console", "")
            tryCatch({
                scRGNet_data$counts <- scRGNet::preprocessCSV(
                    path            = file_input(),
                    transpose       = input$transpose,
                    log_transform   = input$log_transform,
                    cell_zero_ratio = input$cell_zero_ratio,
                    gene_zero_ratio = input$gene_zero_ratio
                )
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
            shinyjs::html(id   = "console",
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

    if (is_local) {
        observe({
            inputToReactive(
                input_var = input,
                react_var = hardware_args,
                ids       = c("CUDA", "coresUsage")
            )
            if (is.null(scRGNet_data$counts)) {
                scRGNet_data$hardwareSetup <- NULL
            } else {
                tryCatch({
                    scRGNet_data$hardwareSetup <- scRGNet::setHardware(
                        coresUsage = hardware_args$coresUsage,
                        CUDA       = hardware_args$CUDA
                        )
                },
                warning = function(warn) {
                    scRGNet_data$hardwareSetup <- NULL
                    showNotification(paste(warn), type = 'warning')
                },
                error = function(err) {
                    scRGNet_data$hardwareSetup <- NULL
                    showNotification(paste(err), type = 'err')
                })
            }
        })

    } else {
        scRGNet_data$hardwareSetup <- scRGNet::setHardware(
            coresUsage = hardware_args$coresUsage,
            CUDA       = hardware_args$CUDA
            )
    }
    # ===== HARDWARE SETUP ENDS ================================================

    # ===== HYPERPARAMETERS SETUP STARTS =======================================
    observe({
        if (is.null(scRGNet_data$counts)) {
            ## Initialising widget
            output$choose_k <- renderUI({
                numericInput(inputId = "k",
                             label   = "k (default best heuristic)",
                             value   = 1)
            })
        } else {
            ## Change default value to calculated best heuristic from counts when available
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
            hideUI(
                c(
                    "ltmg",
                    "batch_size",
                    "regu_epochs",
                    "L1",
                    "L2",
                    "regu_alpha",
                    "reduction",
                    "choose_k",
                    "run"
                )
            )
        } else {
            showUI(
                c(
                    "ltmg",
                    "batch_size",
                    "regu_epochs",
                    "L1",
                    "L2",
                    "regu_alpha",
                    "reduction",
                    "choose_k",
                    "run"
                )
            )
        }
        #
    })

    observeEvent(input$run, {
        is_valid_k <- input$k >= 1 & input$k <= length(scRGNet_data$counts)
        if (!is_valid_k) {
            showNotification(
                paste("Invalid value for k. Must be between 1 and", length(scRGNet_data)),
                type = 'error'
            )
        }
        req(is_valid_k)
        tryCatch({
            scRGNet_data$hyperParams <- scRGNet::setHyperParams(
                batch_size  = input$batch_size,
                regu_epochs = input$regu_epochs,
                L1          = input$L1,
                L2          = input$L2,
                regu_alpha  = input$regu_alpha,
                reduction   = input$reduction
            )
        },
        warning = function(warn) {
            scRGNet_data$hyperParams <- NULL
            showNotification(paste(warn), type = 'warning')
        },
        error = function(err) {
            scRGNet_data$hyperParams <- NULL
            showNotification(paste(err), type = 'err')
        })
        # ===== HYPERPARAMETERS SETUP ENDS =====================================

        # ===== MODAL TRAINING STARTS ==========================================
        req(!is.null(scRGNet_data$hyperParams))
        if (input$ltmg) {
            shinybusy::show_modal_spinner(spin  = "cube-grid",
                                          color = "#E95420",
                                          text  = "Inferring LTMG signals...")
            tryCatch({
                scRGNet_data$LTMG_mat <-
                    scRGNet::runLTMG(scDataset = scRGNet_data$counts)
            },
            error = function(err) {
                scRGNet_data$LTMG_mat <- NULL
                showNotification(paste(err), type = 'err')
            })
            shinybusy::remove_modal_spinner()
        }
        shinybusy::show_modal_spinner(spin  = "self-building-square",
                                      color = "#E95420",
                                      text  = "Encoding Expression Values...")
        withCallingHandlers({
            shinyjs::html("console", "")
            tryCatch({
                scRGNet_data$encoded <- scRGNet::runFeatureAE(
                    scDataset     = scRGNet_data$counts,
                    LTMG_mat      = scRGNet_data$LTMG_mat,
                    hyperParams   = scRGNet_data$hyperParams,
                    hardwareSetup = scRGNet_data$hardwareSetup
                )
            },
            warning = function(warn) {
                scRGNet_data$encoded <- NULL
                showNotification(paste(warn), type = 'warning')
            },
            error = function(err) {
                scRGNet_data$encoded <- NULL
                showNotification(paste(err), type = 'err')
            })
        },
        message = function(m) {
            shinyjs::html(id   = "console",
                          html = m$message,
                          add  = TRUE)
        })
        shinybusy::remove_modal_spinner()
        # ===== MODAL TRAINING ENDS ========================================

        # ===== GENERATING NETWORK STARTS ==================================
        req(!is.null(scRGNet_data$encoded))
        shinybusy::show_modal_spinner(spin  = "rotating-plane",
                                      color = "#E95420",
                                      text  = "Calculating Cell Graph...")
        tryCatch({
            scRGNet_data$net <- scRGNet::generateNetwork(
                feature_mat   = scRGNet_data$encoded,
                k             = input$k,
                hardwareSetup = scRGNet_data$hardwareSetup
            )
        },
        warning = function(warn) {
            scRGNet_data$net <- NULL
            showNotification(paste(warn), type = 'warning')
        },
        error = function(err) {
            scRGNet_data$net <- NULL
            showNotification(paste(err), type = 'err')
        })
        shinybusy::remove_modal_spinner()
        output$network <- visNetwork::renderVisNetwork({
            scRGNet::plotCellNet(
                net            = scRGNet_data$net,
                group          = input$highlight_net_group,
                title          = input$net_title,
                show_select_by = input$sel_by,
                node_size      = input$node_size
            )
        })
        output$degree_plot <- renderPlot({scRGNet::plotDegree(net = scRGNet_data$net)})
        output$log_plot    <- renderPlot({scRGNet::plotLog(net = scRGNet_data$net)})
        # ===== GENERATING NETWORK ENDS ========================================
    })

    observeEvent(input$print, {
        print(scRGNet_data$encoded)
    })

    # ===== PLOTTING STARTS ====================================================

    observe({
        has_net <- !is.null(scRGNet_data$net)
        req(has_net)
        output$network <- visNetwork::renderVisNetwork({
            scRGNet::plotCellNet(
                net            = scRGNet_data$net,
                group          = input$highlight_net_group,
                title          = input$net_title,
                show_select_by = input$sel_by,
                node_size      = input$node_size
            )
        })
    })

    observeEvent(input$render_degree, {
        has_net <- !is.null(scRGNet_data$net)
        if (has_net) {
            output$degree_plot <- renderPlot({
                scRGNet::plotDegree(
                    net   = scRGNet_data$net,
                    title = input$degree_plot_title
                )
            })
        } else {
            showNotification("A network must be calculated first.",
                             type = "error")
        }
    })

    observeEvent(input$render_log, {
        has_net <- !is.null(scRGNet_data$net)
        if (has_net) {
            output$log_plot <- renderPlot({
                scRGNet::plotLog(
                    net   = scRGNet_data$net,
                    title = input$log_title
                )
            })
        } else {
            showNotification("A network must be calculated first.",
                             type = "error")
        }
    })
    # ===== PLOTTING ENDS ======================================================


}

# [END]
