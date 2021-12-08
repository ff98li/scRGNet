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
            width = 3,
            shinyjs::useShinyjs(),
            # ===== FILE UPLOAD HANDLING STARTS ================================
            uiOutput(outputId = "upload_ui"),
            actionButton(inputId = "demo",
                         label   = "Use demo data"),
            textOutput("choose"),
            textOutput("upload_summary"),
            actionButton(inputId = 'reset',
                         label   = 'Clear loaded data',
                         icon    = icon("sync")),
            # ===== FILE UPLOAD HANDLING ENDS ==================================

            # ===== PREPROCESS GENE COUNTS STARTS ==============================
            shinyjs::hidden(
                checkboxInput(
                    inputId = "transpose",
                    label   = "Transpose count matrix",
                    value   = FALSE
                ),
                checkboxInput(
                    inputId = "log_transform",
                    label   = "Log transform",
                    value   = TRUE
                ),
                numericInput(
                    inputId = "cell_zero_ratio",
                    label   = "Cell zero ratio",
                    value   = 0.99,
                    min     = 0,
                    max     = 1,
                    step    = 0.01
                ),
                numericInput(
                    inputId = "gene_zero_ratio",
                    label   = "Gene zero ratio",
                    value   = 0.99,
                    min     = 0,
                    max     = 1,
                    step    = 0.01
                ),
                actionButton(inputId = "preprocess",
                             label   = "Preprocess data")

            ),
            # ===== PREPROCESS GENE COUNTS ENDS ================================
            # ===== HARDWARE SETUP STARTS ======================================
            shinyjs::hidden(uiOutput(outputId = "hardware_ui")),
            # ===== HARDWARE SETUP ENDS ========================================

            # ===== HYPERPARAMETERS SETUP STARTS ===============================
            shinyjs::hidden(
                checkboxInput(
                    inputId = "ltmg",
                    label   = "Infer LTMG tags",
                    value   = TRUE
                ),
                numericInput(
                    inputId = "batch_size",
                    label   = "Batch size",
                    value   = 1,
                    min     = 1,
                    step    = 1
                ),
                numericInput(
                    inputId = "regu_epochs",
                    label   = "Epochs",
                    value   = 5,
                    min     = 1,
                    step    = 1
                ),
                numericInput(
                    inputId = "L1",
                    label   = "L1 Regularisation intensity",
                    value   = 0.5,
                    min     = 0,
                    max     = 1,
                    step    = 0.01
                ),
                numericInput(
                    inputId = "L2",
                    label   = "L2 Regularisation intensity",
                    value   = 0.5,
                    min     = 0,
                    max     = 1,
                    step    = 0.01
                ),
                numericInput(
                    inputId = "regu_alpha",
                    label   = "LTMG Regularisation intensity",
                    value   = 0.5,
                    min     = 0,
                    max     = 1,
                    step    = 0.01
                ),
                selectInput(
                    inputId = "reduction",
                    label   = "Reduction Method",
                    choices = list(sum  = "sum",
                                   mean = "mean",
                                   none = "none"),
                    multiple = FALSE,
                    selected = "sum"
                ),
                uiOutput("choose_k"),
                actionButton(
                    inputId = "run",
                    label   = "Start analysis"
                )
            ),
            # ===== HYPERPARAMETERS SETUP ENDS =================================
            actionButton(inputId = "print",
                         label   = "print")
            # ===== MODAL TRAINING ENDS ========================================
            # ===== MODAL TRAINING ENDS ========================================
            # ===== GENERATING NETWORK STARTS ==================================
            # ===== GENERATING NETWORK ENDS ====================================
            # ===== PLOTTING STARTS ============================================
            # ===== PLOTTING ENDS ==============================================

        ),
        mainPanel = mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Console Output",
                    verbatimTextOutput("console")
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
    about_page
)

# [END]
