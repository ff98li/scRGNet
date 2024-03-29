% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessCSV.R
\name{preprocessCSV}
\alias{preprocessCSV}
\title{Pre-process scRNA-seq Count Matrix in CSV File}
\usage{
preprocessCSV(
  path,
  log_transform = TRUE,
  cell_zero_ratio = 0.99,
  gene_zero_ratio = 0.99,
  geneSelectNum = 2000L,
  transpose = FALSE,
  toCSV = FALSE,
  outdir_path = file.path(getwd(), "output"),
  savename = "preprocessedCSV"
)
}
\arguments{
\item{path}{A string the represents the path to the input file, where
directories in the path are separated by "/", and the input data file
should have extension .csv or .gz}

\item{log_transform}{A Boolean value indicating whether to perform natural
logarithmic transformation for every value of the data at the end of
pre-processing or not. If TRUE, the log(x+1) of the original expression
level will be calculated.}

\item{cell_zero_ratio}{A double-precision value strictly greater than 0 and
less than 1. It defines the maximum proportion of genes with zeros in cells.
Default is 0.99, which means cells with more than 99\% genes that are
zeros will be filtered out.}

\item{gene_zero_ratio}{A double-precision value strictly greater than 0 and
less than 1. It defines the maximum proportion of zeros in genes.
Default is 0.99, which means genes with more than 99\% zero values
will be filtered out.}

\item{geneSelectNum}{A positive integer that defines how many most variant
genes to keep. The default is 2000, which 2000 genes, ranked from
most variant to least by their variances of expression values in each
sample, will be retained in the final pre-processed data and be used
for later analysis.}

\item{transpose}{A Boolean value telling the function whether the input
scRNA-seq matrix is given in a transposed manner, i.e.
cells as rows and genes as columns.
If TRUE, a transpose operation will be performed. Default is FALSE.}

\item{toCSV}{A Boolean value indicating whether to save the pre-processed
expression matrix to a CSV file. If TRUE a CSV file with <savename> will
be saved to /output directory under user's current working directory.}

\item{outdir_path}{A character vector that designates the path to the directory where
the output CSV will be saved if toCSV is TRUE. This argument won't be
used if toCSV is FALSE. There must be no path separator at the end.
If the designated does not exist in the parent directory,
it will create one automatically. Default is a directory named "output"
under user's current R working directory.}

\item{savename}{A character vector representing the name of the csv file that saves
pre-processed scRNA-seq data. There is no strict rule for this. However,
it is highly recommended to give it a meaningful name.}
}
\value{
Returns a scDataset with the pre-processed scRNA-seq expression values,
where row names are cells and column names are genes.
}
\description{
A function that reads a CSV file containing scRNA-seq counts
and pre-process the scRNA-seq matrix with the given criteria.
before feeding it to the scGNN framework for analysis.
}
\examples{
# Example 2:
\dontrun{
# Accessing the demo gene_counts_small dataset available with the package
inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
# Preprocess the raw counts
counts <- preprocessCSV(path = inputCountsPath)
}
# Example 1:
\dontrun{
# Preprocess the whole scRNA-seq dataset
inputCountsPath <- system.file("extdata", "GSE138852_counts.csv.gz", package = "scRGNet")
counts <- preprocessCSV(path = inputCountsPath)
# Take ~20 seconds to process.
}
}
\references{
\insertRef{scGNN}{scRGNet}
}
