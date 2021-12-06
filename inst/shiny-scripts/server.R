# server.R
library(shiny)
library(shinyjs)

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
            return(input$raw_counts)
        } else if (inFile$upload_state == 'reset') {
            return(NULL)
        } else if (inFile$upload_state == 'demo') {
            return(system.file("extdata", "GSE138852_small.csv",
                               package = "scRGNet"))
        }
    })


    output$choose <- reactive({
        if (is.null(file_input()))
        {
            paste(
                "Use built-in dataset to run a demo by clicking",
                "Use demo data",
                "without uploading a file."
            )

        } else if (file_input() == system.file("extdata", "GSE138852_small.csv",
                                               package = "scRGNet")) {
            "Now you can run scRGNet with the built-in demo data."
        } else {
            "File format validated. Now you can run scRGNet with your own data."
        }
    })
    output$upload_summary <- renderText({
        if (is.null(inFile$upload_state) || inFile$upload_state == "reset") {
            return("Uploaded file:")
        } else if (inFile$upload_state == "demo") {
            return("Demo dataset loaded.")
        } else if (inFile$upload_state == "uploaded") {
            return(paste("Uploaded file:", file_input()$name))
        }
    })
    # ===== FILE UPLOAD HANDLING ENDS ==========================================


    # ===== PREPROCESS GENE COUNTS STARTS =======================================
    observe({
        if (is.null(file_input())) {
            shinyjs::disable("preprocess")
        } else {
            shinyjs::enable("preprocess")
        }
    })


    # ===== PREPROCESS GENE COUNTS ENDS ========================================


}

# [END]
