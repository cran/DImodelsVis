% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimplexPath.R
\name{simplex_path}
\alias{simplex_path}
\title{Visualising the change in a response variable between two points in
the simplex space}
\usage{
simplex_path(
  model,
  starts,
  ends,
  add_var = list(),
  interval = c("confidence", "prediction", "none"),
  conf.level = 0.95,
  se = FALSE,
  pie_positions = c(0, 0.5, 1),
  pie_colours = NULL,
  pie_radius = 0.3,
  FG = NULL,
  facet_var = NULL,
  plot = TRUE,
  nrow = 0,
  ncol = 0
)
}
\arguments{
\item{model}{A Diversity Interactions model object fit by using the
`\code{\link[DImodels:DI]{DI()}}` function from the
`\code{\link[DImodels:DImodels-package]{DImodels}}` package.}

\item{starts}{A data-frame specifying the starting proportions of the
compositional variables.
If a model object is specified then this data should contain all the
variables present in the model object including any additional non-compositional variables.
If a coefficient vector is specified then data should contain same number of
columns as the number of elements in the coefficient vector and a one-to-one
positional mapping would be assumed between the data columns and the
elements of the coefficient vector.}

\item{ends}{A data-frame specifying the ending proportions of the
compositional variables.
If a model object is specified then this data should contain all the
variables present in the model object including any additional non-compositional variables.
If a coefficient vector is specified then data should contain same number of
columns as the number of elements in the coefficient vector and a one-to-one
positional mapping would be assumed between the data columns and the
elements of the coefficient vector.}

\item{add_var}{A list specifying values for additional variables
in the model other than the proportions (i.e. not part of the simplex design).
This would be useful to compare the predictions across
different values for a categorical variable.
One plot will be generated for each unique combination
of values specified here.}

\item{interval}{Type of interval to calculate:
\describe{
  \item{"none"}{No interval to be calculated.}
  \item{"confidence" (default)}{Calculate a confidence interval.}
  \item{"prediction"}{Calculate a prediction interval.}
}}

\item{conf.level}{The confidence level for calculating confidence/prediction
intervals. Default is 0.95.}

\item{se}{A boolean variable indicating whether to plot confidence intervals associated with
the effect of species increase or decrease}

\item{pie_positions}{A numeric vector with values between 0 and 1 (both inclusive)
indicating the positions along the X-axis at which to
show pie-glyphs for each curve. Default is c(0, 0.5, 1) meaning
that pie-glyphs with be shown at the start, midpoint and end
of each curve.}

\item{pie_colours}{A character vector indicating the colours for the slices
in the pie-glyphs. \cr
If left NULL, the colour blind friendly colours will be
for the pie-glyph slices.}

\item{pie_radius}{A numeric value specifying the radius (in cm) for the
pie-glyphs. Default is 0.3 cm.}

\item{FG}{A higher level grouping for the compositional variables in the
data. Variables belonging to the same group will be assigned with
different shades of the same colour. The user can manually specify
a character vector giving the group each variable belongs to.
If left empty the function will try to get a grouping
from the original \code{\link[DImodels:DI]{DI}} model object.}

\item{facet_var}{A character string or numeric index identifying the column
in the data to be used for faceting the plot into multiple
panels.}

\item{plot}{A boolean variable indicating whether to create the plot or return
the prepared data instead. The default `TRUE` creates the plot while
`FALSE` would return the prepared data for plotting. Could be useful
for if user wants to modify the data first and then call the plotting
function manually.}

\item{nrow}{Number of rows in which to arrange the final plot
(when `add_var` is specified).}

\item{ncol}{Number of columns in which to arrange the final plot
(when `add_var` is specified).}
}
\value{
A ggmultiplot (ggplot if single plot is returned) class object or data-frame (if `plot = FALSE`)
}
\description{
This function will prepare the underlying data and plot the results for visualising the
change in a response variable as we move across a straight line between two points
in the simplex space in a single function call. The two sets of points specified
by the `starts` and `ends` parameters are joined by a straight line across the
simplex space and the response is predicted for the starting, ending and intermediate
communities along this line. The associated uncertainty along this prediction is also shown.
The generated plot will show individual curves indicating the variation in the
response between the points. `\code{\link[PieGlyph:PieGlyph-package]{Pie-glyphs}}`
are used to highlight the compositions of the starting, ending and midpoint
of the straight line between the two points.
This is a wrapper function specifically for statistical models fit using the
\code{\link[DImodels:DI]{DI()}} function from the
\code{\link[DImodels:DImodels-package]{DImodels}} R package and would implicitly
call \code{\link{simplex_path_data}} followed by
\code{\link{simplex_path_plot}}. If your model object isn't fit using
DImodels, consider calling these functions manually.
}
\examples{
library(DImodels)
data(sim2)

# Fit model
mod <- DI(y = "response", prop = 3:6, DImodel = "AV", data = sim2)

# Create plot
# Move from p3 monoculture to p4 monoculture
simplex_path(model = mod,
             starts = data.frame(p1 = 0, p2 = 0, p3 = 1, p4 = 0),
             ends = data.frame(p1 = 0, p2 = 0, p3 = 0, p4 = 1))

# Move from each 70\% dominant mixtures to p1 monoculture
simplex_path(model = mod,
             starts = sim2[c(1, 5, 9, 13), 3:6],
             ends = data.frame(p1 = 1, p2 = 0, p3 = 0, p4 = 0))

# Move from centroid community to each monoculture
simplex_path(model = mod,
             starts = sim2[c(18),],
             ends = sim2[c(48, 52, 56, 60), ])

# Show change across multiple points simultaneously and show confidence bands
# using `se = TRUE`
simplex_path(model = mod,
             starts = sim2[c(1, 17, 22), ],
             ends = sim2[c(5, 14, 17), ], se = TRUE)

# Change pie_colours using `pie_colours` and show pie-glyph at different
# points along the curve using `pie_positions`
simplex_path(model = mod,
             starts = sim2[c(1, 17, 22), ],
             ends = sim2[c(5, 14, 17), ], se = TRUE,
             pie_positions = c(0, 0.25, 0.5, 0.75, 1),
             pie_colours = c("steelblue1", "steelblue4", "orange1", "orange4"))

# Facet based on existing variables
\donttest{
simplex_path(model = mod,
             starts = sim2[c(1, 17, 22), ],
             ends = sim2[c(5, 14, 17), ], se = TRUE, facet_var = "block",
             pie_colours = c("steelblue1", "steelblue4", "orange1", "orange4"))

# Add additional variables and create a separate plot for each
simplex_path(model = mod,
             starts = sim2[c(1, 17, 22), 3:6],
             ends = sim2[c(5, 14, 17), 3:6], se = TRUE,
             pie_colours = c("steelblue1", "steelblue4", "orange1", "orange4"),
             add_var = list("block" = factor(c(1, 3),
                                             levels = c(1, 2, 3, 4))))
}

## Specify `plot = FALSE` to not create the plot but return the prepared data
head(simplex_path(model = mod, plot = FALSE,
                  starts = sim2[c(1, 17, 22), 3:6],
                  ends = sim2[c(5, 14, 17), 3:6], se = TRUE,
                  pie_colours = c("steelblue1", "steelblue4",
                                  "orange1", "orange4"),
                  add_var = list("block" = factor(c(1, 3),
                                                  levels = c(1, 2, 3, 4)))))
}
