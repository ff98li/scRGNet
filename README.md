
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scRGNet

## Description

<!-- badges: start -->
<!-- badges: end -->

scRGNet is an R package for inferring cell-cell networks from scRNA-seq
data. It is the first R package that attempts to perform dimensional
reduction using an feature(that is, gene) autoencoder from the novel
single cell graph neural network (scGNN) framework.([Wang et al.
2021](#ref-scGNN)) It generate a feature matrix containing the
low-dimensional representation of gene expression in each cell, and
build a Cell-Cell network from the feature matrix using KNN. In training
the feature autoencoder, discretized regulatory signals quantified from
gene expression modeled by a left-truncated mixture Gaussian (LTMG)
model can also be used as a regulariser.([Wan et al. 2019](#ref-LTMG))
It is unique from other R packages for scRNA-seq analysis in that
scRGNet offers an option to analyse scRNA-seq data without assuming any
statistical distribution or relationships for gene expression.

## Installation

To install the latest version of scRGNet:

``` r
require("devtools")
devtools::install_github("ff98li/scRGNet")
```

## Overview

``` r
library(scRGNet)
ls("package:scRGNet")
#>  [1] "gene_counts"       "gene_counts_small" "generateNetwork"  
#>  [4] "plotCellNet"       "plotDegree"        "plotLog"          
#>  [7] "preprocessCSV"     "runFeatureAE"      "runLTMG"          
#> [10] "setHardware"       "setHyperParams"
data(package = "scRGNet")
```

Note that there are two datasets included in this package `gene_counts`
and `gene_counts_small`. `gene_counts` is a raw scRNA-seq matrix from
experiment GSE138852([Grubman et al. 2019](#ref-GSE138852)).
`gene_counts_small` is a subset of the `gene_counts` data for a quick
demo of the package, containing only 48 cells and 1000 genes. For usage
of functions in the package, please refer to package vignettes for more
details:

``` r
browseVignettes(package = "scRGNet")
```

An overview of the package is illustrated below.

![](./inst/extdata/structure.png)

## Contributions

The author of the package is Feifei Li. The *runLTMG* function uses the
LTMG object and the function for inferring LTMG tags from
`scgnnltmg`([Wang et al. 2021](#ref-scGNN)). `data.table` R
package([Dowle and Srinivasan 2020](#ref-dt)) is used for fast reading
in a large size scRNA-seq raw matrix from csv. The `Matrix`([Bates et
al. 2021](#ref-matrix)) R package is used to store scRNA-seq data as a
sparse matrix to reduce memory useage, and used to convert a tensor
object to an R matrix. The *scDataset* object is an `R6` object([Chang
2021](#ref-r6)) inherited from class `dataset` from `torch`. The feature
autoencoder is also an R6 object inherited from the basic neural network
modules `nn_module` from `torch` R package, and makes use of its
functional modules `nnf_linear` and `nnf_relu`.([Falbel et al.
2021](#ref-torch)) Iteration of model training makes use of `coro::loop`
form the `coro` R package. The model training also uses `progress` R
package to inform users the model trainning progress. The
*generateNetwork* function makes use of `graph_from_data_frame` from
`igraph` R package to generate a plottable `igraph` object.([Csardi and
Nepusz 2006](#ref-igraph)) All plotting functions in this package make
use of `graphics` R package. `cluster_label_prop` and `degree` function
from `igraph` R package are used to compute the communities and degrees
of the network.

## Acknowledgements

This package was developed for BCB410H: Applied Bioinformatics,
University of Toronto, Toronto, CANADA, 2021-2022. `scRGNet` welcomes
issues, enhancement requests, and other contributions. To submit an
issue, use the [GitHub
issues](https://github.com/ff98li/scRGNet/issues).

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-matrix" class="csl-entry">

Bates, Douglas, Martin Maechler, Timothy A. Davis, Jens Oehlschlägel,
and Jason Riedy. 2021. “Matrix: Sparse and Dense Matrix Classes and
Methods.” <http://Matrix.R-forge.R-project.org/>.

</div>

<div id="ref-r6" class="csl-entry">

Chang, Winston. 2021. “R6: Encapsulated Object-Oriented Programming for
r.” <https://r6.r-lib.org>.

</div>

<div id="ref-igraph" class="csl-entry">

Csardi, Gabor, and Tamas Nepusz. 2006. “The Igraph Software Package for
Complex Network Research.” *InterJournal*. <https://igraph.org>.

</div>

<div id="ref-dt" class="csl-entry">

Dowle, M, and A Srinivasan. 2020. “Data. Table: Extension of’data.
Frame’.” <https://r-datatable.com>.

</div>

<div id="ref-torch" class="csl-entry">

Falbel, Daniel, Javier Luraschi, Dmitriy Selivanov, Athos Damiani,
Christophe Regouby, Krzysztof Joachimiak, and Hamada S. Badr. 2021.
“Torch for r.” RStudio. <https://torch.mlverse.org/>.

</div>

<div id="ref-GSE138852" class="csl-entry">

Grubman, Alexandra, Gabriel Chew, John F Ouyang, Guizhi Sun, Xin Yi
Choo, Catriona McLean, Rebecca K Simmons, et al. 2019. “A Single-Cell
Atlas of Entorhinal Cortex from Individuals with Alzheimer’s Disease
Reveals Cell-Type-Specific Gene Expression Regulation.” *Nature
Neuroscience* 22 (12): 2087–97.

</div>

<div id="ref-LTMG" class="csl-entry">

Wan, Changlin, Wennan Chang, Yu Zhang, Fenil Shah, Xiaoyu Lu, Yong Zang,
Anru Zhang, et al. 2019. “<span class="nocase">LTMG: a novel statistical
modeling of transcriptional expression states in single-cell RNA-Seq
data</span>.” *Nucleic Acids Research* 47 (18): e111–11.
<https://doi.org/10.1093/nar/gkz655>.

</div>

<div id="ref-scGNN" class="csl-entry">

Wang, Juexin, Anjun Ma, Yuzhou Chang, Jianting Gong, Yuexu Jiang,
Hongjun Fu, Cankun Wang, Ren Qi, Qin Ma, and Dong Xu. 2021. “scGNN Is a
Novel Graph Neural Network Framework for Single-Cell RNA-Seq Analyses.”
*Nature Communications*. <https://doi.org/10.1038/s41467-021-22197-x>.

</div>

</div>
