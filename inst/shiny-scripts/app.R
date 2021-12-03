library(shiny)
library(shinythemes)

main_page <- tabPanel(
    title = "Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            fileInput(inputId = "upload",
                      label = "Upload csv or csv.gz file containing scRNA-seq data",
                      accept = c(".csv",
                                 "text/csv",
                                 "text/comma-separated-values",
                                 ".csv.gz")
                      ),
            "...or run a demo dataset"
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

server <- function(input, output) {
}
shiny::shinyApp(ui = ui, server = server)

# [END]
