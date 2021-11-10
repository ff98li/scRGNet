#' scRNA-seq gene expression counts from GSE138852
#'
#' A single-cell atlas of the human cortex reveals drivers of transcriptional changes in Alzheimer’s disease in specific cell subpopulations
#'
#' @source Duke-NUS Medical School, Singapore.
#'
#' @references
#' 	Grubman A, Chew G, Ouyang JF, Sun G et al. A single-cell atlas of entorhinal cortex from individuals with Alzheimer's disease reveals cell-type-specific gene expression regulation. Nat Neurosci 2019 Dec;22(12):2087-2097. PMID: 31768052
#'
#' @format A matrix with columns:
#' \describe{
#'  \item{AD}{Human Entorhinal Cortex of aged individuals with Alzheimer’s disease (AD)}
#'  \item{Healthy}{Human Entorhinal Cortex of aged Healthy individuals, the control.}
#' }
#' @examples
#' \dontrun{
#'  gene_counts
#' }
"gene_counts"

#' A subset scRNA-seq data of GSE138852
#'
#' A subset of scRNA-seq gene counts from data set GSE138852. It is a small subset of the data with only 500 genes for running a quick demo of data processing functions of the package.
#'
#' @source Duke-NUS Medical School, Singapore.
#'
#' @references
#' 	Grubman A, Chew G, Ouyang JF, Sun G et al. A single-cell atlas of entorhinal cortex from individuals with Alzheimer's disease reveals cell-type-specific gene expression regulation. Nat Neurosci 2019 Dec;22(12):2087-2097. PMID: 31768052
#'
#' @format A matrix with columns:
#' \describe{
#'  \item{AD}{Human Entorhinal Cortex of aged individuals with Alzheimer’s disease (AD)}
#'  \item{Healthy}{Human Entorhinal Cortex of aged Healthy individuals, the control.}
#' }
#' @examples
#' \dontrun{
#'  gene_counts_small
#' }
"gene_counts_small"
