% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gene_counts_small}
\alias{gene_counts_small}
\title{A subset of GSE138852 scRNA-seq gene expression counts}
\format{
A matrix with columns:
\describe{
\item{AD}{Human Entorhinal Cortex of aged individuals with Alzheimer’s disease (AD)}
\item{Healthy}{Human Entorhinal Cortex of aged Healthy individuals, the control.}
}
}
\source{
Duke-NUS Medical School, Singapore.
}
\usage{
gene_counts_small
}
\description{
A single-cell atlas of the human cortex reveals drivers of transcriptional changes in Alzheimer’s disease in specific cell subpopulations. This subset has 1000 most variant genes, and cells with at most 85\% of zero expression values. It is intended to be used for a quick function demo rather than a full analysis.
}
\examples{
\dontrun{
 gene_counts_small
}
}
\references{
\preformatted{Grubman A, Chew G, Ouyang JF, Sun G et al. A single-cell atlas of entorhinal cortex from individuals with Alzheimer's disease reveals cell-type-specific gene expression regulation. Nat Neurosci 2019 Dec;22(12):2087-2097. PMID: 31768052
}
}
\keyword{datasets}
