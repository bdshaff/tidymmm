% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delayed_adstock_weights.R
\name{delayed_adstock_weights}
\alias{delayed_adstock_weights}
\title{Delayed Adstock Weights}
\usage{
delayed_adstock_weights(
  decay = 0.5,
  delay = 2,
  max_carryover = 12,
  normalize = TRUE
)
}
\arguments{
\item{decay}{decay rate}

\item{delay}{delay in the peak of the adstock}

\item{max_carryover}{maximum carryover}

\item{normalize}{should the result be scaled?}
}
\value{
a numeric vector of weights of length (max_carryover)
}
\description{
delayed_adstock_weights is a function that is called to generate a vector of weights
that define how media impressions will be decayed in present and carried over into the future.
}
\examples{
delayed_adstock_weights()
delayed_adstock_weights(decay = 0.1, delay = 2)
delayed_adstock_weights(decay = 0.1, delay = 5)

}
