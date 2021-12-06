# server.R
library(shiny)
library(shinyjs)
library(shinybusy)

server <- function(input, output, session) {

    is_local <- Sys.getenv('SHINY_PORT') == ""

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


    # ===== PREPROCESS GENE COUNTS STARTS =======================================
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

    counts <- NULL
    observeEvent(input$preprocess, {
        ## https://stackoverflow.com/a/30490698/12461512
        withCallingHandlers({
            shinyjs::html("preprocess_result", "")
            counts <- scRGNet::preprocessCSV(
                path            = file_input(),
                transpose       = input$transpose,
                log_transform   = input$log_transform,
                cell_zero_ratio = input$cell_zero_ratio,
                gene_zero_ratio = input$gene_zero_ratio
            )
        },
        message = function(m) {
            shinyjs::html(id = "preprocess_result", html = m$message, add = TRUE)
        })
    })


    # ===== PREPROCESS GENE COUNTS ENDS ========================================


}

# [END]
