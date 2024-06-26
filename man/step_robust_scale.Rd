% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robust_scale.R
\name{step_robust_scale}
\alias{step_robust_scale}
\title{Robust centering and scaling using median and MAD}
\usage{
step_robust_scale(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  loc_scale = NULL,
  skip = FALSE,
  id = recipes::rand_id("robust_scale")
)
}
\arguments{
\item{recipe}{A recipe to which to add the step}

\item{...}{List of columns (tidyverse's select) to which apply the step}

\item{role}{For internal use}

\item{trained}{For internal use}

\item{loc_scale}{Once trained (\code{prep}-ed), this will contain location and scale information.
Use \code{broom::tidy()} to access the information}

\item{skip}{Whether to skip this step in \code{bake}-ing}

\item{id}{ID of the step for the recipe}
}
\value{
An updated recipe with the step added
}
\description{
Robust centering and scaling using median and MAD
}
