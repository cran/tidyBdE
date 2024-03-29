% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bde_tidy_palettes.R
\name{bde_tidy_palettes}
\alias{bde_tidy_palettes}
\title{BdE color palettes}
\usage{
bde_tidy_palettes(
  n = 6,
  palette = c("bde_vivid_pal", "bde_rose_pal", "bde_qual_pal"),
  alpha = NULL,
  rev = FALSE
)
}
\arguments{
\item{n}{The number of colors (\verb{>= 1}) to be in the palette.}

\item{palette}{A valid palette name.}

\item{alpha}{An alpha-transparency level in the range \verb{[0,1]} (\code{0} means
transparent and \code{1} means opaque). A missing, i.e., \code{alpha = NULL}, does
not add opacity codes (\code{"FF"}) to the individual color hex codes. See
\code{\link[ggplot2:reexports]{ggplot2::alpha()}}.}

\item{rev}{Logical indicating whether the ordering of the colors should be
reversed.}
}
\value{
A vector of colors.
}
\description{
Custom palettes based on the publications of BdE. These are manual palettes
with a maximum of 6 colors.
}
\examples{

# BdE vivid pal
scales::show_col(bde_tidy_palettes(palette = "bde_vivid_pal"),
  labels = FALSE
)

# BdE rose pal
scales::show_col(bde_tidy_palettes(palette = "bde_rose_pal"), labels = FALSE)

# BdE qual pal
scales::show_col(bde_tidy_palettes(palette = "bde_qual_pal"), labels = FALSE)
}
\seealso{
Other bde_plot: 
\code{\link{scales_bde}},
\code{\link{theme_tidybde}()}
}
\concept{bde_plot}
