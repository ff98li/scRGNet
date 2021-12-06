# server.R
library(shiny)
library(shinyjs)
library(shinybusy)

server <- function(input, output, session) {

    is_local <- Sys.getenv('SHINY_PORT') == ""

    scRGNet_data <- reactiveValues(
        counts  = NULL,
        encoded = NULL,
        net     = NULL
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
            shinyjs::hide("transpose")
            shinyjs::hide("log_transform")
            shinyjs::hide("cell_zero_ratio")
            shinyjs::hide("gene_zero_ratio")
            shinyjs::hide("preprocess")
        } else {
            shinyjs::show("transpose")
            shinyjs::show("log_transform")
            shinyjs::show("cell_zero_ratio")
            shinyjs::show("gene_zero_ratio")
            shinyjs::show("preprocess")
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
        shinybusy::show_modal_spinner(spin = "cube-grid",
                                      color = "#E95420",
                                      text = "Preprocessing scRNA-seq counts...")
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
            shinyjs::html(id = "preprocess_result",
                          html = m$message,
                          add = TRUE)
        })
        shinybusy::remove_modal_spinner()
    })

    observeEvent(input$print, {
        print(is.null(scRGNet_data$counts))
    })

    # ===== PREPROCESS GENE COUNTS ENDS ========================================

    # ===== HARDWARE SETUP STARTS ==============================================
    #print(counts())
    #print(scRGNet_data$counts)
    #output$hardware_ui <- renderUI({
    #    splitLayout(
    #        tags$b("Hardware")
    #    )
    #})
#
    #if (is.null(scRGNet_data$counts)) {
    #    shinyjs::hide("hardware_ui")
    #} else {
    #    if (is_local) {
    #        shinyjs::show("hardware_ui")
    #    }
    #}

    # ===== HARDWARE SETUP ENDS ================================================
    # ===== HYPERPARAMETERS SETUP STARTS =======================================
    # ===== HYPERPARAMETERS SETUP ENDS =========================================
    # ===== MODAL TRAINING ENDS ================================================
    # ===== MODAL TRAINING ENDS ================================================
    # ===== GENERATING NETWORK STARTS ==========================================
    # ===== GENERATING NETWORK ENDS ============================================
    # ===== PLOTTING STARTS ====================================================
    # ===== PLOTTING ENDS ======================================================


}

# [END]
