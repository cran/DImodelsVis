% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GradientChange.R
\name{gradient_change_data}
\alias{gradient_change_data}
\title{Calculate change in predicted response over diversity gradient}
\usage{
gradient_change_data(
  data,
  prop,
  add_var = list(),
  gradient = c("richness", "evenness"),
  prediction = TRUE,
  ...
)
}
\arguments{
\item{data}{A data-frame consisting of variable proportions and
any other necessary variables to make predictions from
`model` or `coefficients`.}

\item{prop}{A vector identifying the column-names or indices of the
columns containing the variable proportions in `data`.}

\item{add_var}{A list specifying values for additional predictor variables
in the model independent of the compositional predictor variables.
This could be useful for comparing the predictions across
different values for a non-compositional variable.
If specified as a list, it will be expanded to show a plot
for each unique combination of values specified, while if specified
as a data-frame, one plot would be generated for each row in the
data and they will be arranged in a grid according to the
value specified in `nrow` and `ncol`.}

\item{gradient}{Diversity gradient to show on the X-axis, one of
"richness" or "evenness". Defaults to "richness". See
`Details` for more information.}

\item{prediction}{A logical value indicating whether to pass the final
data to the `add_prediction` function and append the
predictions to the data. Default value is TRUE, but
often it would be desirable to make additional changes
to the data before making any predictions, so the user
can set this to FALSE and manually call the `add_prediction`
function.}

\item{...}{
  Arguments passed on to \code{\link[=add_prediction]{add_prediction}}
  \describe{
    \item{\code{model}}{A regression model object which will be used to make predictions
for the observations in `data`. Will override `coefficients`
if specified.}
    \item{\code{coefficients}}{If a regression model is not available (or can't be fit in R),
the regression coefficients from a model fit in some other
language can be used to calculate predictions. However, the
user would have to ensure there's an appropriate one-to-one
positional mapping between the data columns and the
coefficient values. Further, they would also have to provide
a variance-covariance matrix of the coefficients in the `vcov`
parameter if they want the associated CI for the prediction or
it would not be possible to calculate confidence/prediction
intervals using this method.}
    \item{\code{vcov}}{If regression coefficients are specified, then the variance-covariance
matrix of the coefficients can be specified here to calculate the
associated confidence interval around each prediction. Failure to do
so would result in no confidence intervals being returned. Ensure
`coefficients` and `vcov` have the same positional mapping with the data.}
    \item{\code{coeff_cols}}{If `coefficients` are specified and a one-to-one positional
mapping between the data-columns and coefficient vector is
not present. A character string or numeric index can be specified
here to reorder the data columns and match the corresponding
coefficient value to the respective data column. See the
"Use model coefficients for prediction" section in examples.}
    \item{\code{conf.level}}{The confidence level for calculating confidence/prediction
intervals. Default is 0.95.}
    \item{\code{interval}}{Type of interval to calculate:
\describe{
  \item{"none" (default)}{No interval to be calculated.}
  \item{"confidence"}{Calculate a confidence interval.}
  \item{"prediction"}{Calculate a prediction interval.}
}}
  }}
}
\value{
The data-frame with the following columns appended at the end
 \describe{
   \item{.Richness}{The richness (number of non-zero compositional variables)
                    within each observation.}
   \item{.Evenness}{The evenness (metric quantifying the relative abundance
                    of each compositional variable) within each observation.}
   \item{.Gradient}{An character string defining the diversity gradient used
                    for averaging the response.}
   \item{.add_str_ID}{An identifier column for grouping the cartesian product
                      of all additional columns specified in `add_var`
                      parameter (if `add_var` is specified).}
   \item{.Pred}{The predicted response for each obsvervation.}
   \item{.Lower}{The lower limit of the prediction/confidence interval
                 for each observation.}
   \item{.Upper}{The upper limit of the prediction/confidence interval
                 for each observation.}
   \item{.Avg}{The averaged value of the predicted response for each unique
               value of the selected diversity gradient.}
 }
}
\description{
Helper function for creating the data to visualise a scatter-plot of the
response over a diversity gradient. The "richness" and "evenness"
diversity gradients are currently supported. The average (predicted) response
is calculated from all communities present at a given level of the
chosen diversity gradient in `data`. The output of this
function can be passed to the \code{\link{gradient_change_plot}} function
to visualise results.
}
\details{
Currently two diversity gradients are supported
\itemize{
  \item{\strong{Richness}: A metric describing the number of non-zero compositional
  variables in an observation.}
  \item{\strong{Evenness}: A metric quantifying the relative abundances of all
  compositional variables in an observation. Defined as
  \deqn{(2s/(s-1)) \sum_{i, j = 1; i < j}^{s}{p_i * p_j}} where \eqn{s} is the
  total number of compositional variables and \eqn{p_i} and \eqn{p_j} are the
  proportions of the variables \eqn{i} and \eqn{j}.
  See Kirwan et al., 2007 <\doi{doi:10.1890/08-1684.1}> and Kirwan et al., 2009
  <\doi{doi:10.1890/08-1684.1}> for more
  information.}
}

Here's a small example of how these metrics are calculated for a few
observations. Suppose we have four compositional variables (i.e. \eqn{s = 4})
and have the following three observations
\itemize{
 \item{A = (0.5, 0.5, 0, 0)}
 \item{B = (0.25, 0.25, 0.25, 0.25)}
 \item{C = (1, 0, 0, 0)}
}
The richness values for these three observations would be as follows
\itemize{
 \item{A = 2 (Since two of the four compositional variables were non-zero)}
 \item{B = 4 (Since all four compositional variables were non-zero)}
 \item{C = 1 (Since one of the four compositional variables were non-zero)}
}
The evenness values would be calculated as follows
\itemize{
 \item{A = \eqn{2*4/(4-1)*(0.5*0.5+0.5*0+0.5*0+0.5*0+0.5*0+0*0) = 0.67}}
 \item{B = \eqn{2*4/(4-1)*(0.25*0.25+0.25*0.25+0..25*0.25+0.25*0.25+0.25*0.25+0.25*0) = 1}}
 \item{C = \eqn{2*4/(4-1)*(1*0+1*0+1*0+0*0+0*0+0*0) = 0}}
}
}
\examples{
library(DImodels)
library(dplyr)

## Load data
data(sim2)

## Fit model
mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4)^2, data = sim2)

## Create data
## By default response would be averaged on the basis of richness
head(gradient_change_data(data = sim2,
                          prop = c("p1", "p2", "p3", "p4"),
                          model = mod))

## Average response with respect to evenness
head(gradient_change_data(data = sim2,
                          prop = c("p1", "p2", "p3", "p4"),
                          model = mod,
                          gradient = "evenness"))

## Additional variables can also be added to the data by either specifying
## them directly in the `data` or by using the `add_var` argument
## Refit model
sim2$block <- as.numeric(sim2$block)
new_mod <- update(mod, ~. + block, data = sim2)
## This model has block so we can either specify block in the data
subset_data <- sim2[c(1,5,9,11), 2:6]
subset_data
head(gradient_change_data(data = subset_data,
                          prop = c("p1", "p2", "p3", "p4"),
                          model = mod,
                          gradient = "evenness"))
## Or we could add the variable using `add_var`
subset_data <- sim2[c(1,5,9,11), 3:6]
subset_data
head(gradient_change_data(data = subset_data,
                          prop = c("p1", "p2", "p3", "p4"),
                          model = new_mod,
                          gradient = "evenness",
                          add_var = list(block = c(1, 2))))
## The benefit of specifying the variable this way is we have an ID
## columns now called `.add_str_ID` which could be used to create a
## separate plot for each value of the additional variable


## Model coefficients can also be used, but then user would have
## to specify the data with all columns corresponding to each coefficient
coef_data <- sim2 \%>\%
               mutate(`p1:p2` = p1*p2, `p1:p3` = p1*p2, `p1:p4` = p1*p4,
                      `p2:p3` = p2*p3, `p2:p4` = p2*p4, `p3:p4` = p3*p4) \%>\%
               select(p1, p2, p3, p4,
                      `p1:p2`, `p1:p3`, `p1:p4`,
                      `p2:p3`, `p2:p4`, `p3:p4`) \%>\%
               slice(1,5,9,11)
print(coef_data)
print(mod$coefficients)
gradient_change_data(data = coef_data,
                     prop = c("p1", "p2", "p3", "p4"),
                     gradient = "evenness",
                     coefficients = mod$coefficients,
                     interval = "none")
}
