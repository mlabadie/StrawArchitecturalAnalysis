% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{distribution.axillary_production}
\alias{distribution.axillary_production}
\title{Table of axillary production at node scale}
\usage{
distribution.axillary_production(data)
}
\arguments{
\item{data}{Dataframe. Data issue of the extraction at node scale}
}
\value{
DataFrame. Data containing frequency (n) and relative frequency (prob) for each genotype
}
\description{
Contingency table of axillary production
}
\examples{
    load("data/node_data.rdata")
    distribution.axillary_production(data = node_data)
}
