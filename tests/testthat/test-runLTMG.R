context("Checking for invalid user input")
test_that("Invalid user input", {
    ## Invalid scDataset object
    expect_error(runLTMG(scDataset = "scDataset"))
    ## Invalid input for file path
    inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
    counts          <- preprocessCSV(path = inputCountsPath)
    expect_error(runLTMG(scDataset = counts,
                         fromFile  = TRUE,
                         readPath  = "path"))
})
