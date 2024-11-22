% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MplusBootstrap.R
\name{MplusBootstrap}
\alias{MplusBootstrap}
\title{Title Mplus Boostrapping}
\usage{
MplusBootstrap(
  Model,
  type,
  first_path,
  second_path = NULL,
  multi_level,
  iv = NULL,
  m = NULL,
  w = NULL,
  xw = NULL,
  dv = NULL,
  first_stage_mod = NULL,
  level_one_mod = NULL,
  output = "Untitled",
  rep = 20000,
  conf = 95
)
}
\arguments{
\item{Model}{a list object containing the parameters of the model
(from Mplus output file)}

\item{type}{should choose from "med" "mod-med" or "med-mod"}

\item{first_path}{the name of the first random path, left as 'fixed'
when the path is not random (must be specified by the user)}

\item{second_path}{the name of the second random path, left as 'fixed'
when the path is not random (must be specified by the user)}

\item{multi_level}{a binary variable indicating whether the specified is
multi-level (must be specified by the user)}

\item{iv}{the name of the independent variable}

\item{m}{the name of the mediator}

\item{w}{the name of the moderator}

\item{xw}{the name of the interaction term}

\item{dv}{the name of the dependent variable}

\item{first_stage_mod}{a binary variable indicating whether the moderation
effect is on the first path}

\item{level_one_mod}{a binary variable indicating whether the moderation
effect is on the first level}

\item{output}{the name of the output image file (.png)}

\item{rep}{the number of repetitions (default: 20000)}

\item{conf}{the critical value used to generate confidence interval}
}
\value{
An image containing the distribution of the indirect effect and
confidence interval
}
\description{
Title Mplus Boostrapping
}
