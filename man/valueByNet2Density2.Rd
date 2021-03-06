% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BrandDensity2.R
\name{valueByNet2Density2}
\alias{valueByNet2Density2}
\title{Convert value by net_coord to density}
\usage{
valueByNet2Density2(df, value_col, resolution = 10, df_ranges = NULL,
  dens_ranges = NULL, h = 2, n = 50, tracker = "Some Brand",
  PY = 1)
}
\arguments{
\item{df}{dataframe with net coord and some value, 2 columns}

\item{value_col}{value, most likely transactions summed by net_coord}

\item{resolution}{how many categories for density will be created. default=10}

\item{df_ranges}{character vector that is simialar to the output of *cut* function. Each element represents a range closed on the right. NULL for current season, adjustment for PY}

\item{dens_ranges}{character vector that is simialar to the output of *cut* function. Each element represents a range closed on the right. NULL for current season, adjustment for PY}

\item{h}{comes from MASS::kde2d. kernel parameters. See MASS::kde2d for details}

\item{n}{comes from MASS::kde2d. size of the grid.See MASS::kde2d for details}

\item{tracker}{an optional string that can be supplied to be added to df_ranges and dens_ranges}

\item{PY}{an adjustment factor. use 1 for CY, adjust to decline CY/PY for PY}
}
\description{
Function outputs netcoord with density and density ranges that can be used to produce consisted densities year over year
}
