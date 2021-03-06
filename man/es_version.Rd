% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{es_version}
\alias{es_version}
\title{Elasticsearch version}
\usage{
es_version()
}
\value{
A list with the \code{major}, \code{minor} and \code{build} numbers.
}
\description{
Returns the major, minor and build version numbers for an Elasticsearch cluster, given a valid
URL to an Elasticsearch cluster.
}
\examples{
\dontrun{
elastic("http://localhost:9200") \%info\% es_version()
$major
[1] 5

$minor
[1] 0

$build
[1] 1
}
}
