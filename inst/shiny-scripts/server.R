# server.R
library(shiny)

server <- function(input, output, session) {

    values <- reactiveValues(
        upload_state = NULL
    )

    # ===== FILE UPLOAD HANDLING STARTS =======================================
    output$upload_ui <- renderUI({
        input$reset ## Create a dependency with the reset button
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
        values$upload_state <- 'uploaded'
    })
    observeEvent(input$reset, {
        values$upload_state <- 'reset'
    })

    file_input <- reactive({
        if (is.null(values$upload_state)) {
            return(NULL)
        } else if (values$upload_state == 'uploaded') {
            return(input$raw_counts)
        } else if (values$upload_state == 'reset') {
            return(NULL)
        }
    })

    output$choose <- reactive({
        if (is.null(file_input()))
        {
            "Run a demo dataset without uploading a file"
        } else {
            "File format validated. Now you can run scRGNet with your own data."
        }
    })
    output$summary <- renderText({
        return(paste("Uploaded file:", file_input()$name))
    })
    # ===== FILE UPLOAD HANDLING ENDS =======================================

    observeEvent(input$run, {
        if (is.null(file_input())) {
            print(TRUE)
        } else {
            print(FALSE)
        }
    })



}

# [END]
