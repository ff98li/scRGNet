context("Checking for invalid user input")
test_that("Invalid argument", {
    # Incorrect input type for file path
    expect_error(preprocessCSV(path = 1))
    inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
    # Incorrect input type for log_transform
    expect_error(preprocessCSV(path = inputCountsPath,
                               log_transform = 1))
    # Incorrect input type for cell zero ratio
    expect_error(preprocessCSV(path = inputCountsPath,
                               cell_zero_ratio = -1))
    }
)
