context("Checking for invalid user input")
test_that("Invalid user input", {
    ## Checking invalid user input for net
    expect_error(plotCellNet(net = "show me a network"))
    expect_error(plotDegree(net = "gimme my degree!"))
    expect_error(plotLog(net = "what"))
    inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
    counts          <- preprocessCSV(path = inputCountsPath)
    z               <- runFeatureAE(counts)
    net             <- generateNetwork(feature_mat = z)

    ## Invalid input for group
    expect_error(plotCellNet(net = net, group = 114))
    ## Invalid input for title
    expect_error(plotCellNet(net = net, group = TRUE, title = 114))
    expect_error(plotDegree(net = net, title = 114))
    expect_error(plotLog(net = net, title = 114))
})
