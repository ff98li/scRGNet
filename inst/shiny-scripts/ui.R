# ui.R
library(shiny)
library(shinythemes)

main_page <- tabPanel(
    title = "Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            uiOutput(outputId = "upload_ui"),
            textOutput("choose"),
            textOutput("summary"),
            actionButton(inputId = 'reset',
                         label   =  'Clear uploaded file'),
            actionButton(inputId = "run",
                         label   = "Run scRGNet")
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
    about_page
)

# [END]
