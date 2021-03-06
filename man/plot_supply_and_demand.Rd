% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{plot_supply_and_demand}
\alias{plot_supply_and_demand}
\title{Plot Supply and Demand}
\usage{
plot_supply_and_demand(supply_and_demand_system, consumer_surplus = TRUE)
}
\arguments{
\item{supply_and_demand_system}{A tibble that represents a system of linear equations for supply and demand created with \code{create_supply_and_demand()}.}

\item{consumer_surplus}{Whether you want to plot the consumer surplus or not. Default: TRUE}
}
\value{
A ggplot2 plot.
}
\description{
Given a tibble representing a system of supply and demand, it plots all of the curves.
}
\examples{
\dontrun{
if(interactive()){
 create_supply_and_demand() \%>\%
 shift_demand() \%>\%
 shift_supply() \%>\%
 plot_supply_and_demand()
 }
}
}
\seealso{
\code{\link[tidyr]{gather}}
 \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}}
 \code{\link[purrr]{map}}
}
