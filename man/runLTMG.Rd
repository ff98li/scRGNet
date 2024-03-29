% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessCSV.R
\name{runLTMG}
\alias{runLTMG}
\title{Get discretised regulatory signals from LTMG model}
\usage{
runLTMG(
  scDataset = NULL,
  fromFile = FALSE,
  readPath = NULL,
  toFile = FALSE,
  outdir_path = file.path(getwd(), "output"),
  savename = "ltmg"
)
}
\arguments{
\item{scDataset}{A scDataset object containing a matrix with row names as cell samples
and column names as genes containing scRNA-seq expression values
pre-processed by preprocessCSV. If reading from a expression file,
this argument needs not be provided.}

\item{fromFile}{A Boolean value indicating whether to read RNA-seq matrix
from a expression file. If TRUE, expr_mat needs not be provided, and
it will check whether the expression file exists in readPath.
If FALSE, expr_mat must be provided.}

\item{readPath}{A character vector representing path to the expression file.
If fromFile is TRUE, it must be provided. Otherwise optional.}

\item{toFile}{A Boolean value indicating whether to write the returned
LTMG object to file. If TRUE, outdir_path and savename must be provided.}

\item{outdir_path}{A character vector representing the path to the parent
directory where the object will be saved as a file. If toFile is TRUE,
it must be provided. Otherwise optional.}

\item{savename}{A character vector representing the name of the saved
LTMG object file. If toFile is TRUE, it must be provided.
Otherwise optional.}
}
\value{
Returns an LTMG matrix containing the inferred LTMG tags of expr_mat.
}
\description{
A wrapper function for running scGNNLTMG that uses
the Left-Trunctruncated-Mixed-Gaussian(LTMG) model to
translate the input gene expression data pre-processed by preprocessCSV into
a discretised regulatory signal as the regularizer for
the feature autoencoder of scGNN.
It will take around 10-15 minutes for inferring LTMG tags, depending on
the dataset.
This step is optional but highly recommended prior to scGNN analysis.
}
\examples{
# Example 1:
# Tested examples. Not run for fast package compiling
\dontrun{
# Accessing the demo gene_counts_small dataset available with the package
inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
# Preprocess the raw counts
counts <- preprocessCSV(path = inputCountsPath)
LTMG <- runLTMG(counts)
# ~30 seconds
}
# Example 2:
# Preprocess the whole scRNA-seq dataset
\dontrun{
inputCountsPath <- system.file("extdata", "GSE138852_counts.csv.gz", package = "scRGNet")
counts <- preprocessCSV(path = inputCountsPath)
LTMG <- runLTMG(counts)
# Take 10-14 minutes to finish running
}

}
\references{
\insertRef{LTMG}{scRGNet}
\insertRef{scGNN}{scRGNet}
}
