% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spca_project.R
\name{spca_project}
\alias{spca_project}
\title{spca_project: Project Principal Component Analysis in various spatio-temporal
scenarios in bash mode}
\usage{
spca_project(
  layers_stack,
  layers_to_proj_dirs,
  sv_dir,
  pca_proj_dir_names = NULL,
  pca_obj = NULL,
  layers_format = ".tif"
)
}
\arguments{
\item{layers_stack}{A RasterStack of environmental variables.}

\item{layers_to_proj_dirs}{Paths to directories of projection layers}

\item{sv_dir}{A directory where the PCs will be saved. If NULL the PCs will not be written.}

\item{pca_proj_dir_names}{A vector with directory names where PCs will be saved. For example, pca_ccsm4_2050_6}

\item{pca_obj}{An object of class \code{\link[stats]{prcomp}} (default NULL). Usefull when the user already
has done the PCa for \code{layers_stack}.}

\item{layers_format}{A raster format for writing PCA results (see \code{\link[raster]{writeFormats}}). Default = ".asc"}
}
\description{
A simple function to project Principal Component Analysis for Spatial Data
in various spatial-temporal scenarios.
}
\details{
proj_dir_names and layers_to_proj_dirs must have the same length.
If user does not provide names for projection directories, ntbox will create
directories for each projection scenario.

It uses the function \code{\link[ntbox]{spca}} to compute PCs
}
