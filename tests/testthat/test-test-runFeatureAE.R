context("Checking for invalid user input")
test_that("Invalid argument", {
    # Incorrect input type for scDataset
    expect_error(runFeatureAE(scDataset = matrix(1:4, 2, 2)))
    inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
    counts          <- preprocessCSV(path = inputCountsPath)
    # Incorrect input type for LTMG_mat
    expect_error(runFeatureAE(scDataset = counts,
                              LTMG_mat  = "boom"))
    ltmg <- runLTMG(scDataset = counts)
    # Incorrect input type for hyper-parameters
    expect_error(runFeatureAE(scDataset   = counts,
                              LTMG_mat    = ltmg,
                              hyperParams = "what is hyper params?"))
    hyperParams <- setHyperParams() ## using default value to test
    # Incorrect input type for hardwareSetup
    expect_error(runFeatureAE(scDataset     = counts,
                              LTMG_mat      = ltmg,
                              hyperParams   = hyperParams,
                              hardwareSetup = "Use my super powerful RTX9090!"))
    }
)
