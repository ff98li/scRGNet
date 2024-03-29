# ui.R
library(shiny)
library(shinyjs)
library(shinythemes)
library(visNetwork)

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
                    value   = 50,
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
                uiOutput("choose_k")
            ),
            # ===== HYPERPARAMETERS SETUP ENDS =================================
            # ===== MODAL TRAINING STARTS ======================================
            shinyjs::hidden(actionButton(inputId = "run",
                                         label   = "Start analysis"))
            # ===== MODAL TRAINING ENDS ========================================
        ),
        mainPanel = mainPanel(tabsetPanel(
            tabPanel(title = "Console Output",
                     verbatimTextOutput("console")),
            # ===== GENERATING NETWORK STARTS ==================================
            tabPanel(
                title = "Network",
                fluidRow(
                    column(
                        6,
                        textInput(
                            inputId = "net_title",
                            label   = "Title",
                            width   = "600px",
                            value   = "My Cell Network"
                        )
                    ),
                    column(
                        2,
                        checkboxInput(
                            inputId = "highlight_net_group",
                            label   = "Colour by group",
                            value   = TRUE
                        )
                    ),
                    column(
                        2,
                        radioButtons(
                            inputId = "sel_by",
                            label   = "Select by",
                            choices = list("Node"  = "node",
                                           "Group" = "group",
                                           "None"  = "none"),
                            selected = "group"
                        )
                    )
                ),
                shinyjs::hidden(
                    uiOutput(outputId = "download_link")
                ),
                sliderInput(
                    inputId = "node_size",
                    label   = "Node size",
                    min     = 1,
                    max     = 50,
                    value   = 25,
                    width   = "600px"
                ),
                hr(),
                fluidRow(visNetwork::visNetworkOutput("network", height = "900px"))
            ),
            # ===== GENERATING NETWORK ENDS ====================================

            # ===== PLOTTING STARTS ============================================
            tabPanel(
                title = "Connectivity",
                fluidRow(column(
                    8,
                    textInput(
                        inputId = "degree_plot_title",
                        label   = "Title",
                        width   = "600px",
                        value   = "Distribution of Vertices in the Cell Network"
                    )
                )),
                fluidRow(
                    column(2,
                           actionButton(inputId = "render_degree",
                                        label   = "Re-render")),
                    column(2,
                           shinyjs::hidden(
                               uiOutput("download_degree_button")
                           )
                        )
                ),
                hr(),
                fluidRow(plotOutput("degree_plot"))
            ),
            tabPanel(
                title = "Log-log",
                fluidRow(column(
                    8,
                    textInput(
                        inputId = "log_title",
                        label   = "Title",
                        width   = "600px",
                        value   = "A log-log Plot of Connectivities for Cell Network"
                    )
                )),
                fluidRow(
                    column(2,
                           actionButton(inputId = "render_log",
                                        label   = "Re-render")),
                    column(2,
                           shinyjs::hidden(
                               uiOutput("download_log_button")
                           )
                    )
                ),
                hr(),
                fluidRow(plotOutput("log_plot"))
                # ===== PLOTTING ENDS ==========================================
            )
        ))
    )
)

about_page <- tabPanel(title = "About scRGNet",
                       titlePanel("About scRGNet"),
                       "scRGNet is an R package for inferring cell-cell networks from encoded scRNA-seq data.
                       It uses a feature autoconder from the scGNN framework proposed by Wang et al.
                       to generate a feature matrix containing the low-dimensional representation
                       of gene expression in each cell, and build a Cell-Cell network from the feature matrix
                       using KNN and isolation forest. A tutorial for this tool has been included in the vignette
                       of the scRGNet package.
                       This project is created purely for studying purposes.
                       Since the torch R package is still in the experimental stage of the tidyverse life cycle
                       (not yet in the stable stage), while its modules make up the building block of this project,
                       there are still lots of problem that need to be addressed
                       (e.g. unusual high memory usage compared to pyTorch).",
                       br(),
                       "Should any questions or concerns arise, feel free to open an issue:",
                       issue_url <- a("GitHub tracker", href="https://github.com/ff98li/scRGNet/issues"),
                       h2("Contact Info"),
                       "<ff.li@mail.utoronto.ca>")

ui <- navbarPage(title = "scRGNet",
                 theme = shinythemes::shinytheme('united'),
                 main_page,
                 about_page)

# [END]
