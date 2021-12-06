# ui.R
library(shiny)
library(shinyjs)
library(shinybusy)
library(shinythemes)

is_local <- Sys.getenv('SHINY_PORT') == ""

main_page <- tabPanel(
    title = "Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            # ===== FILE UPLOAD HANDLING STARTS ================================
            uiOutput(outputId = "upload_ui"),
            actionButton(inputId = "demo",
                         label   = "Use demo data"),
            textOutput("choose"),
            textOutput("upload_summary"),
            actionButton(inputId = 'reset',
                         label   = 'Clear loaded data'),
            # ===== FILE UPLOAD HANDLING ENDS ==================================
            # ===== PREPROCESS GENE COUNTS STARTS ==============================
            checkboxInput(inputId = "transpose",
                          label = "Transpose count matrix",
                          value = FALSE),
            checkboxInput(inputId = "log_transform",
                          label = "Log transform",
                          value = TRUE),
            numericInput(inputId = "cell_zero_ratio",
                         label = "Cell zero ratio",
                         value = 0.99,
                         min = 0,
                         max = 1),
            numericInput(inputId = "gene_zero_ratio",
                         label = "Gene zero ratio",
                         value = 0.99,
                         min = 0,
                         max = 1),
            actionButton(inputId = "preprocess",
                         label   = "Preprocess data")
            # ===== PREPROCESS GENE COUNTS ENDS ================================
        ),
        mainPanel = mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Preprocessing Result",
                    verbatimTextOutput("preprocess_result")
                ),
                tabPanel(
                    title = "Network")
            )
        )
    )
)

about_page <- tabPanel(
    title = "About scRGNet",
    titlePanel("About scRGNet"),
    "TODO"
    )

ui <- navbarPage(
    title = "scRGNet",
    theme = shinythemes::shinytheme('united'),
    main_page,
    about_page,
    shinyjs::useShinyjs()
)

# [END]
