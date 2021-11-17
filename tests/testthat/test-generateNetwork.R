context("Checking for invalid user input")
test_that("Invalid argument input", {
    ## Invalid input for feature_mat
    expect_error(generateNetwork(feature_mat = "feature_mat"))
    inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
    counts          <- preprocessCSV(path = inputCountsPath)
    z               <- runFeatureAE(counts)
    ## Invalid k value
    expect_error(generateNetwork(feature_mat = z, k = -1))
})
