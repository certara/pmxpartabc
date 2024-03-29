% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{parframe2setup}
\alias{parframe2setup}
\title{Import NONMEM output for parameter tables}
\usage{
parframe2setup(
  run_dir,
  run_prefix,
  runno,
  bootstrap = NULL,
  run_dir.boot = NULL,
  runno.boot = NULL,
  conf.level = 0.95,
  min_suc = TRUE,
  read.boot = NULL,
  boot.obj = NULL,
  yaml.file = NULL,
  yaml.file.name = NULL
)
}
\arguments{
\item{run_dir}{NONMEM model directory}

\item{run_prefix}{NONMEM run number prefix}

\item{runno}{NONMEM run number}

\item{bootstrap}{flag for availability of bootstrap results}

\item{run_dir.boot}{bootstrap results directory}

\item{runno.boot}{bootstrap NONMEM run number, if changes from runno}

\item{conf.level}{bootstrap results confidence interval. By default set to 95\%}

\item{min_suc}{filter bootstrap results on minimization successful. By default set to TRUE}

\item{read.boot}{flag for reading directly the customized filtering bootstrap raw_results.csv}

\item{boot.obj}{customized filtered bootstrap raw_results.csv data frame passed by user}

\item{yaml.file}{flag for using the yaml.file option; all the non-compulsory fields
e.g., label, trans, units, type will be read from yaml file}

\item{yaml.file.name}{name of the yaml file}
}
\value{

}
\description{
Reads all relevant inputs (ie, parameter estimates, bootstrap, shrinkage, etc.) using directly model run or with the help of a YAML file.
The parameter name must be provided in the model file. Other fields are optional and can be provided either in (1) the model file or a (2) YAML file.
Few fields needs to be inputted directly from user to complete the table:
\itemize{
 \item{name (compulsory): }{name of parameter (eg, CL, Vd, nCL, nVd, etc.)}
 \item{label: }{description of parameter (eg, "Volume of distribution", "Apparent Clearance", etc.)}
 \item{units: }{parameter unit (eg, L/h, 1/hr, etc)}
 \item{trans: }{parameter transformation (ie, \%, exp, ilogit, sqrt, CV\%)}
 \item{type: }{Typical Values, Between Subject Variability, Inter Occasion Variability, Residual Error or Covariates (ie respectively, Structural, IIV, IOV, RUV or CovariateEffect)}
 }
}
