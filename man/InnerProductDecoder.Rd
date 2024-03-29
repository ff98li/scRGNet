% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GAE.R
\name{InnerProductDecoder}
\alias{InnerProductDecoder}
\title{Inner Product Decoder}
\usage{
InnerProductDecoder(dropout, act = torch::torch_sigmoid)
}
\arguments{
\item{dropout}{dropout}

\item{act}{The activation function used by the neuron. Default sigmoid.}
}
\value{
A new \code{InnerProductDecoder} object.
}
\description{
Create an inner product decoder object.
}
\details{
Decoder using inner product for prediction.

#' @references
\insertRef{GCN}{scRGNet}
}
\section{Fields}{

\describe{
\item{\code{dropout}}{dropout}

\item{\code{act}}{activation function of the neuron}
}}

