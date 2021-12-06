# ui.R
library(shiny)
library(shinyjs)
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
            actionButton(inputId = "preprocess",
                         label   = "Preprocess data")
        ),
        mainPanel = mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Preprocessed data"
                ),
                tabPanel(
                    title = "LTMG"
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
