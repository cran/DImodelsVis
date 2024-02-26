#' @title Calculate change in predicted response over diversity gradient
#'
#' @description
#' Helper function for creating the data to visualise a scatter-plot of the
#' response over a diversity gradient. The "richness" and "evenness"
#' diversity gradients are currently supported. The average (predicted) response
#' is calculated from all communities present at a given level of the
#' chosen diversity gradient in `data`. The output of this
#' function can be passed to the \code{\link{gradient_change_plot}} function
#' to visualise results.
#'
#' @importFrom tools toTitleCase
#'
#' @param data A data-frame consisting of variable proportions and
#'             any other necessary variables to make predictions from
#'             `model` or `coefficients`.
#' @param prop A vector identifying the column-names or indices of the
#'             columns containing the variable proportions in `data`.
#' @param prediction A logical value indicating whether to pass the final
#'                   data to the `add_prediction` function and append the
#'                   predictions to the data. Default value is TRUE, but
#'                   often it would be desirable to make additional changes
#'                   to the data before making any predictions, so the user
#'                   can set this to FALSE and manually call the `add_prediction`
#'                   function.
#' @inheritParams gradient_change
#' @inheritDotParams add_prediction -data
#'
#' @return The data-frame with the following columns appended at the end
#'  \describe{
#'    \item{.Richness}{The richness (number of non-zero compositional variables)
#'                     within each observation.}
#'    \item{.Evenness}{The evenness (metric quantifying the relative abundance
#'                     of each compositional variable) within each observation.}
#'    \item{.Gradient}{An character string defining the diversity gradient used
#'                     for averaging the response.}
#'    \item{.add_str_ID}{An identifier column for grouping the cartesian product
#'                       of all additional columns specified in `add_var`
#'                       parameter (if `add_var` is specified).}
#'    \item{.Pred}{The predicted response for each obsvervation.}
#'    \item{.Lower}{The lower limit of the prediction/confidence interval
#'                  for each observation.}
#'    \item{.Upper}{The upper limit of the prediction/confidence interval
#'                  for each observation.}
#'    \item{.Avg}{The averaged value of the predicted response for each unique
#'                value of the selected diversity gradient.}
#'  }
#'
#' @inherit gradient_change details
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' library(dplyr)
#'
#' ## Load data
#' data(sim2)
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4)^2, data = sim2)
#'
#' ## Create data
#' ## By default response would be averaged on the basis of richness
#' head(gradient_change_data(data = sim2,
#'                           prop = c("p1", "p2", "p3", "p4"),
#'                           model = mod))
#'
#' ## Average response with respect to evenness
#' head(gradient_change_data(data = sim2,
#'                           prop = c("p1", "p2", "p3", "p4"),
#'                           model = mod,
#'                           gradient = "evenness"))
#'
#' ## Additional variables can also be added to the data by either specifying
#' ## them directly in the `data` or by using the `add_var` argument
#' ## Refit model
#' sim2$block <- as.numeric(sim2$block)
#' new_mod <- update(mod, ~. + block, data = sim2)
#' ## This model has block so we can either specify block in the data
#' subset_data <- sim2[c(1,5,9,11), 2:6]
#' subset_data
#' head(gradient_change_data(data = subset_data,
#'                           prop = c("p1", "p2", "p3", "p4"),
#'                           model = mod,
#'                           gradient = "evenness"))
#' ## Or we could add the variable using `add_var`
#' subset_data <- sim2[c(1,5,9,11), 3:6]
#' subset_data
#' head(gradient_change_data(data = subset_data,
#'                           prop = c("p1", "p2", "p3", "p4"),
#'                           model = new_mod,
#'                           gradient = "evenness",
#'                           add_var = list(block = c(1, 2))))
#' ## The benefit of specifying the variable this way is we have an ID
#' ## columns now called `.add_str_ID` which could be used to create a
#' ## separate plot for each value of the additional variable
#'
#'
#' ## Model coefficients can also be used, but then user would have
#' ## to specify the data with all columns corresponding to each coefficient
#' coef_data <- sim2 %>%
#'                mutate(`p1:p2` = p1*p2, `p1:p3` = p1*p2, `p1:p4` = p1*p4,
#'                       `p2:p3` = p2*p3, `p2:p4` = p2*p4, `p3:p4` = p3*p4) %>%
#'                select(p1, p2, p3, p4,
#'                       `p1:p2`, `p1:p3`, `p1:p4`,
#'                       `p2:p3`, `p2:p4`, `p3:p4`) %>%
#'                slice(1,5,9,11)
#' print(coef_data)
#' print(mod$coefficients)
#' gradient_change_data(data = coef_data,
#'                      prop = c("p1", "p2", "p3", "p4"),
#'                      gradient = "evenness",
#'                      coefficients = mod$coefficients,
#'                      interval = "none")
gradient_change_data <- function(data, prop, add_var = list(),
                                 gradient = c("richness", "evenness"),
                                 prediction = TRUE, ...){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble containing the
                            variable proportions which to use for calculating
                            the average change across a diversity gradient."))
  }

  #Sanity Checks
  if(missing(prop)){
    cli::cli_abort(c("{.var prop} cannot be empty.",
                     "i" = "Specify the column-names of the columns
                           containing the variable proportions in {.var data}."))
  }

  sanity_checks(data = data, prop = prop)

  # choose variable to group by
  gradient <- match.arg(gradient)

  # Ensure experimental structure are specified correctly
  dotArgs <- rlang::dots_values(...)
  model <- if (!is.null(dotArgs$model)) dotArgs$model else NULL
  if(!is.null(model)){
    add_var <- check_add_var(model = model, add_var = add_var)
  }

  # Add any experimental structures
  if(length(add_var) > 0){
    data <- add_add_var(add_var = add_var, data = data)
  }

  # Get names of columns containing variable proportions
  species_names <- data %>% select(all_of(prop)) %>% colnames()

  # Choose diversity gradient to group data on
  group_var <- paste0(".", tools::toTitleCase(gradient))

  # Get name of column containing predictions
  # dotArgs <- rlang::dots_values(...)
  # pred_var <- if (!is.null(dotArgs$pred_name)) dotArgs$pred_name else ".Pred"

  # Add the average change across specified gradient
  plot_data <- data %>%
    mutate(".Richness" = get_richness(.data, species_names),
           ".Evenness" = get_evenness(.data, species_names),
           ".Gradient" = group_var)

  # Add predictions if prediction is true
  if(isTRUE(prediction)){
    # Calculate predictions
    plot_data <- add_prediction(data = plot_data, ...)

    if(check_col_exists(data, ".add_str_ID")){
      plot_data <- plot_data %>%
        group_by(!! sym(group_var), .data$.add_str_ID) %>%
        mutate('.Avg' = mean(.data$.Pred)) %>%
        ungroup()
    } else {
      plot_data <- plot_data %>%
        group_by(!! sym(group_var)) %>%
        mutate('.Avg' = mean(.data$.Pred)) %>%
        ungroup()
    }
  }

  # Add attribute to identify prop cols
  attr(plot_data, "prop") <- prop

  cli::cli_alert_success("Finished data preparation")
  return(plot_data)
}


#' @title Visualise change in (predicted) response over diversity gradient
#'
#' @description
#' Helper function for plotting the average (predicted) response at each level
#' of a diversity gradient. The output of the \code{\link{gradient_change_data}}
#' function should be passed here to visualise a scatter-plot of the predicted
#' response (or raw response) over a diversity gradient. The points can be overlaid
#' with `\code{\link[PieGlyph:PieGlyph-package]{pie-glyphs}}` to show the relative
#' proportions of the compositional variables. The average change in any user-chosen
#' variable over the chosen diversity gradient can also be shown using the `y_var`
#' parameter.
#'
#' @param data A data-frame which is the output of the
#'             `\link{gradient_change_data}` function, consisting of the
#'             predicted response averaged over a particular diversity gradient.
#' @param pie_data A subset of data-frame specified in `data`, to visualise
#'                 the individual data-points as pie-glyphs showing the
#'                 relative proportions of the variables in the data-point.
#' @param prop A vector of column names or indices identifying the columns containing the
#'             species proportions in the data. Will be inferred from the data if
#'             it is created using the `\code{\link{gradient_change_data}}`
#'             function, but the user also has the flexibility of manually
#'             specifying the values.
#' @inheritParams gradient_change
#' @inheritParams gradient_change_data
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' library(dplyr)
#'
#' ## Load data
#' data(sim4)
#' sim4 <- sim4 %>% filter(treatment %in% c(50, 150))
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + p5 + p6)^2, data = sim4)
#'
#' ## Create data
#' ## By default response would be averaged on the basis of richness
#' plot_data <- gradient_change_data(data = sim4,
#'                                   prop = c("p1", "p2", "p3",
#'                                            "p4", "p5", "p6"),
#'                                   model = mod)
#'
#' ## Create plot
#' gradient_change_plot(data = plot_data)
#'
#' ## Average response with respect to evenness
#' plot_data <- gradient_change_data(data = sim4,
#'                                   prop = c("p1", "p2", "p3",
#'                                            "p4", "p5", "p6"),
#'                                   model = mod,
#'                                   gradient = "evenness")
#' gradient_change_plot(data = plot_data)
#'
#' ## Can also manually specify prop variables
#' ## Add grouped proportions to data
#' plot_data <- group_prop(plot_data,
#'                        prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                        FG = c("Gr", "Gr", "Le", "Le", "He", "He"))
#' ## Manually specify prop to show in pie-glyphs
#' gradient_change_plot(data = plot_data,
#'                      prop = c("Gr", "Le", "He"))
#'
#' ## Don't show line indicating the average change by using `average = FALSE` and
#' ## Change colours of the pie-slices using `pie_colours`
#' gradient_change_plot(data = plot_data,
#'                      average = FALSE,
#'                      pie_colours = c("darkolivegreen1", "darkolivegreen4",
#'                                      "orange1", "orange4",
#'                                      "steelblue1", "steelblue4"))
#'
#' ## Manually specify only specific communities to be shown as pie-chart
#' ## glyphs using `pie_data`.
#' ## Note: It is important for the data specified in
#' ## `pie_data` to have the .Pred and .Gradient columns.
#' ## So the best use case for this parameter is to accept
#' ## a subset of the data specified in `data`.#'
#' ## Also use `facet_var` to facet the plot on an additional variable
#' gradient_change_plot(data = plot_data,
#'                      pie_data = plot_data %>% filter(.Richness %in% c(1, 6)),
#'                      facet_var = "treatment")
#'
#' ## If `add_var` was used during the data preparation step then
#' ## multiple plots will be produced and can be arranged using nrow and ncol
#' \donttest{
#' new_mod <- update(mod, ~. + treatment, data = sim4)
#' plot_data <- gradient_change_data(data = sim4[c(seq(1, 18, 3), 19:47), -2],
#'                                   prop = c("p1", "p2", "p3",
#'                                            "p4", "p5", "p6"),
#'                                   model = new_mod,
#'                                   add_var = list("treatment" = c(50, 250)))
#' ## Create plot arranged in 2 columns
#' gradient_change_plot(data = plot_data,
#'                      pie_data = plot_data %>% filter(.Richness %in% c(1, 6)),
#'                      ncol = 2)
#'
#' ## Create plot for raw data instead of predictions
#' ## Create the data for plotting by specifying `prediction = FALSE`
#' plot_data <- gradient_change_data(data = sim4[sim4$treatment == 50, ],
#'                                   prop = c("p1", "p2", "p3",
#'                                            "p4", "p5", "p6"),
#'                                   prediction = FALSE)
#' ## This data will not have any predictions
#' head(plot_data)
#' ## Call the plotting function by specifying the variable you we wish to
#' ## plot on the Y-axis by using the argument `y_var`
#' ## Since this data wasn't created using `gradient_change_data`
#' ## `prop` should be manually specified
#' gradient_change_plot(data = plot_data, y_var = "response",
#'                      prop = c("p1", "p2", "p3",
#'                               "p4", "p5", "p6"))
#' }
gradient_change_plot <- function(data, prop = NULL,
                                 pie_data = NULL,
                                 pie_colours = NULL,
                                 pie_radius = 0.25,
                                 points_size = 3,
                                 average = TRUE,
                                 y_var = ".Pred",
                                 facet_var = NULL,
                                 nrow = 0, ncol = 0){

  # Ensure data is specified
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble (preferably the
                            output of {.help [{.fn col_green(gradient_change_data)}](DImodelsVis::gradient_change_data)})."))
  }

  # If add_var is specified then unique plot for each combination of add_var
  if(check_col_exists(data, ".add_str_ID")){
    ids <- unique(data$.add_str_ID)
    plots <- lapply(cli_progress_along(1:length(ids), name = "Creating plot",
                                       format = paste0(
                                         "{cli::pb_spin} Creating plot ",
                                         "[{cli::pb_current}/{cli::pb_total}]  ETA:{cli::pb_eta}"
                                       )),
                    function(i){
                      data_iter <- data %>% filter(.data$.add_str_ID == ids[i])
                      gradient_change_plot_internal(data = data_iter, prop = prop,
                                                    pie_colours = pie_colours,
                                                    pie_radius = pie_radius,
                                                    points_size = points_size,
                                                    facet_var = facet_var,
                                                    y_var = y_var,
                                                    average = average,
                                                    pie_data = pie_data)+
                        labs(subtitle = ids[i]) +
                        ylim(min(data[, y_var]), max(data[, y_var]))
                    })
    if(length(plots) > 1){
      plot <- new("ggmultiplot", plots = plots, nrow = nrow, ncol = ncol)
    } else {
      plot <- plots[[1]]
    }
    cli::cli_alert_success("Created all plots.")
    # Else return a single ggplot
  } else {
    plot <- gradient_change_plot_internal(data = data, prop = prop,
                                          pie_colours = pie_colours,
                                          pie_radius = pie_radius,
                                          points_size = points_size,
                                          facet_var = facet_var,
                                          y_var = y_var,
                                          average = average,
                                          pie_data = pie_data)
    cli::cli_alert_success("Created plot.")
  }
  return(plot)
}



#' @title Visualise change in (predicted) response over diversity gradient
#'
#' @description
#' A scatter-plot of the predicted response (or raw response) over a diversity
#' gradient for specific observations is shown. The points can be overlaid with
#' `\code{\link[PieGlyph:PieGlyph-package]{pie-glyphs}}` to show the relative
#' proportions of the compositional variables. The average change in any user-chosen
#' variable over the chosen diversity gradient can also be shown using the `y_var`
#' parameter. \cr
#' This is a wrapper function specifically for statistical models fit using the
#' \code{\link[DImodels:DI]{DI()}} function from the
#' \code{\link[DImodels:DImodels-package]{DImodels}} R package and it implicitly
#' calls \code{\link{gradient_change_data}} followed by
#' \code{\link{gradient_change_plot}}. If your model object isn't fit using
#' DImodels, the associated data and plot functions can instead be called manually.
#'
#' @importFrom dplyr mutate %>% group_by distinct across all_of
#' @importFrom ggplot2 ggplot geom_line aes geom_point position_dodge
#'                     position_identity labs theme theme_bw
#' @importFrom PieGlyph geom_pie_glyph
#' @importFrom rlang !!! !! sym .data
#' @importFrom DImodels DI_data_E_AV
#'
#' @param data A dataframe specifying communities of interest for which user
#'             wants to visualise the gradient. If left blank, the data used to
#'             fit the model will be used.
#' @param pie_data Showing all points on the graph as pie-glyphs could be resource
#'                 intensive. Hence a subset of data-frame specified in `data`,
#'                 can be specified here to visualise only specific points as
#'                 pie-glyphs.
#' @param gradient Diversity gradient to show on the X-axis, one of
#'                 "richness" or "evenness". Defaults to "richness". See
#'                 `Details` for more information.
#' @param average A boolean value indicating whether to plot a line indicating
#'                the average change in the predicted response with respect to
#'                the variable shown on the X-axis. The average is calculated at the
#'                median value of any variables not specified.
#' @param y_var A character string indicating the column name of the variable
#'              to be shown on the Y-axis. This could be useful for plotting
#'              raw data on the Y-axis. By default has a value of ".Pred"
#'              referring to the column containing model predictions.
#' @inheritParams prediction_contributions
#' @inheritParams model_diagnostics
#'
#' @inherit prediction_contributions return
#'
#' @details
#' Currently two diversity gradients are supported
#' \itemize{
#'   \item{\strong{Richness}: A metric describing the number of non-zero compositional
#'   variables in an observation.}
#'   \item{\strong{Evenness}: A metric quantifying the relative abundances of all
#'   compositional variables in an observation. Defined as
#'   \deqn{(2s/(s-1)) \sum_{i, j = 1; i < j}^{s}{p_i * p_j}} where \eqn{s} is the
#'   total number of compositional variables and \eqn{p_i} and \eqn{p_j} are the
#'   proportions of the variables \eqn{i} and \eqn{j}.
#'   See Kirwan et al., 2007 <\doi{doi:10.1890/08-1684.1}> and Kirwan et al., 2009
#'   <\doi{doi:10.1890/08-1684.1}> for more
#'   information.}
#' }
#'
#' Here's a small example of how these metrics are calculated for a few
#' observations. Suppose we have four compositional variables (i.e. \eqn{s = 4})
#' and have the following three observations
#' \itemize{
#'  \item{A = (0.5, 0.5, 0, 0)}
#'  \item{B = (0.25, 0.25, 0.25, 0.25)}
#'  \item{C = (1, 0, 0, 0)}
#' }
#' The richness values for these three observations would be as follows
#' \itemize{
#'  \item{A = 2 (Since two of the four compositional variables were non-zero)}
#'  \item{B = 4 (Since all four compositional variables were non-zero)}
#'  \item{C = 1 (Since one of the four compositional variables were non-zero)}
#' }
#' The evenness values would be calculated as follows
#' \itemize{
#'  \item{A = \eqn{2*4/(4-1)*(0.5*0.5+0.5*0+0.5*0+0.5*0+0.5*0+0*0) = 0.67}}
#'  \item{B = \eqn{2*4/(4-1)*(0.25*0.25+0.25*0.25+0..25*0.25+0.25*0.25+0.25*0.25+0.25*0) = 1}}
#'  \item{C = \eqn{2*4/(4-1)*(1*0+1*0+1*0+0*0+0*0+0*0) = 0}}
#' }
#'
#' @export
#'
#' @examples
#' ## Load DImodels package to fit the model
#' library(DImodels)
#' library(dplyr)
#'
#' ## Load data
#' data(sim4)
#' sim4 <- sim4 %>% filter(treatment == 50)
#'
#' ## Fit DI model
#' mod <- DI(prop = 3:8, DImodel = "AV", data = sim4, y = "response") %>%
#'          suppressWarnings()
#'
#' ## Create visualisation
#'
#' ## By default, 'richness' is the gradient and communities from the
#' ## raw data are used to calculate average response
#' gradient_change(model = mod)
#'
#' ## Specify custom data
#' gradient_change(model = mod, data = sim4 %>% filter(richness <= 4))
#'
#' ## Create plot for all equi-proportional communities at a
#' ## given level of richness
#' plot_data <- get_equi_comms(6, variables = paste0("p", 1:6))
#' gradient_change(model = mod, data = plot_data)
#'
#' ## Can also plot average response across evenness and
#' ## change colours of the pie-slices using `pie_colours`
#' gradient_change(model = mod, gradient = "evenness",
#'                 pie_colours = c("darkolivegreen1", "darkolivegreen4",
#'                                 "orange1", "orange4",
#'                                 "steelblue1", "steelblue4"))
#'
#' ## Manually specify only specific communities to be shown as pie-chart
#' ## glyphs using `pie_data` and `facet_var` to facet the plot on
#' ## an additional variable.
#' gradient_change(model = mod,
#'                 pie_data = sim4 %>% filter(richness %in% c(1, 6)),
#'                 facet_var = "treatment")
#'
#' ## Use `add_var` to add additional variables independent of the compositions
#' ## Multiple plots will be produced and can be arranged using nrow and ncol
#' ## Create plot arranged in 2 columns
#' \donttest{
#' gradient_change(model = mod,
#'                 data = sim4[, -2],
#'                 add_var = list("treatment" = c(50, 250)),
#'                 pie_data = sim4[, -2] %>% filter(richness %in% c(1, 6)),
#'                 ncol = 2)
#' }
#'
#' ## Specify `plot = FALSE` to not create the plot but return the prepared data
#' head(gradient_change(model = mod, plot = FALSE,
#'                      pie_data = sim4 %>% filter(richness %in% c(1, 6)),
#'                      facet_var = "treatment"))
gradient_change <- function(model, data = NULL,
                            gradient = c('richness', 'evenness'),
                            add_var = list(),
                            plot = TRUE,
                            average = TRUE,
                            y_var = ".Pred",
                            pie_data = NULL,
                            pie_colours = NULL,
                            pie_radius = 0.25,
                            points_size = 3,
                            facet_var = NULL,
                            nrow = 0, ncol = 0){
  # Ensure specified model is fit using the DI function
  if(missing(model) || (!inherits(model, "DI") && !inherits(model, "DImulti"))){
    model_not_DI(call_fn = "gradient_change")
  }

  # choose variable to plot on x-axis
  gradient <- match.arg(gradient)

  # Get original data used to fit the model
  original_data <- model$original_data

  # Get all species in the model
  model_species <- attr(model, "prop")

  # If species were specified as column indices extract names
  if(is.numeric(model_species)){
    model_species <- colnames(original_data)[model_species]
  }

  # If data is not specified then create data
  if(is.null(data)){
    data <- original_data
  }

  # Sanity checks for colours and data
  sanity_checks(data = data, colours = pie_colours)
  data <- as.data.frame(data)
  # Ensure experimental structure are specified correctly
  add_var <- check_add_var(model = model, add_var = add_var)
  # If model object is of type DImulti add info about EFs and timepoints
  if(inherits(model, "DImulti")) {
    add_var <- link_DImodelsMulti(model = model, add_var = add_var)
  }
  if(length(add_var) > 0){
    data <- add_add_var(data = data,
                        add_var = add_var)

    if(!is.null(pie_data)){
      pie_data <- add_add_var(data = pie_data,
                              add_var = add_var)
    }
  }

  # Create data for plotting
  plot_data <- rlang::try_fetch(gradient_change_data(data = data,
                                                     model = model,
                                                     prop = model_species,
                                                     gradient = gradient),
                                error = function(cnd)
                                  rlang::abort("The following error was encountered while preparing `data` for plotting.",
                                               parent = cnd))
  if(!is.null(pie_data)){
    pie_data <- as.data.frame(pie_data)
    pie_data <- rlang::try_fetch(suppressMessages(gradient_change_data(
                                                      data = pie_data,
                                                      model = model,
                                                      prop = model_species,
                                                      gradient = gradient,
                                                      )),
                                 error = function(cnd)
                                   rlang::abort("The following error was encountered while preparing `pie_data` for plotting.",
                                                parent = cnd))
  }
  if(is.null(pie_data)) {
    prop <- if(length(model_species) > 10 || nrow(plot_data) > 1024) NULL else model_species
  } else {
    prop <- model_species
  }

  # Get functional groups
  FG <- attr(model, "FG")

  # Colours for species
  if(is.null(pie_colours)){
    pie_colours <- get_colours(vars = model_species, FG = FG)
  }

  if(isTRUE(plot)){
    plot <- gradient_change_plot(data = plot_data,
                                 prop = prop,
                                 pie_data = pie_data,
                                 pie_colours = pie_colours,
                                 pie_radius = pie_radius,
                                 points_size = points_size,
                                 average = average,
                                 facet_var = facet_var,
                                 y_var = y_var,
                                 nrow = nrow, ncol = ncol)
    return(plot)
  } else {
    return(plot_data)
  }
}

#' @keywords internal
#' Internal function for the gradient change plot
#'
#' @usage NULL
NULL
gradient_change_plot_internal <- function(data, prop = NULL,
                                          pie_data = NULL,
                                          pie_colours = NULL,
                                          pie_radius = 0.25,
                                          points_size = 3,
                                          average = TRUE,
                                          y_var = ".Pred",
                                          facet_var = NULL){

  # When possible infer prop from data
  if(is.null(prop)){
    prop <- attr(data, "prop")
  }

  # Ensure inputs are appropriate
  sanity_checks(data = data, prop = prop, colours = pie_colours,
                booleans = list("average" = average),
                numerics = list("points_size" = points_size,
                                "pie_radius" = pie_radius))

  # If pie_data is specified then prop cannot be NULL
  if(!is.null(pie_data) && is.null(prop)){
    cli::cli_abort(c("{.var prop} cannot be {.pkg NULL} when {.var pie_data}
                     is specified.",
                     "i" = "Specify the column-names or indices of the columns
                         containing the variable proportions in {.var pie_data}
                         to show as pie-glyphs on the plot. "))
  }

  # If prop is specified without pie_data, assume data is pie_data
  if(!is.null(prop) && is.null(pie_data)){
    pie_data <- data
  }

  # Check prop is present in pie_data too
  sanity_checks(data = pie_data, prop = prop)

  check_plot_data(data = data,
                  cols_to_check = c(".Gradient", y_var,
                                    ".Richness", ".Evenness"),
                  calling_fun = "gradient_change")

  # Deciding whether to dodge points
  gradient_var <- data$.Gradient[1]
  position <- if(gradient_var == ".Richness") position_dodge(0.5) else position_identity()

  # Choose a proper scale for x-axis
  xlab <- strsplit(gradient_var, split = "")[[1]][-1] %>% paste0(collapse = "")
  pie_max <- ifelse(is.null(pie_data) || !check_col_exists(pie_data, ".Richness"),
                    -Inf, max(pie_data$.Richness))
  xmax <- max(max(data$.Richness), pie_max)
  x_scale <- if(gradient_var == ".Richness")
    ggplot2::scale_x_continuous(breaks = seq(1:xmax))
  else
    ggplot2::scale_x_continuous()

  # Caption
  # if(gradient_var == ".Richness"){
  #   caption <- paste0("The solid black line connects the average predicted
  #                     response of all points shown at a given level of ", tolower(xlab),
  #                     ".\n The points at a given level of richness are jittered according
  #                     to the relative abundances of compositional variables in an observation.")
  # } else {
  #   caption <- paste0("The solid black line connects the average predicted
  #                     response of all points shown at a given level of ", tolower(xlab), ".")
  # }

  # Create plot
  plot <- ggplot(data = data, aes(x = !! sym(gradient_var), y = !! sym(y_var)))+
    geom_point(aes(group = .data$.Evenness),
               fill = '#555555',
               colour = "#000000",
               pch = 21, stroke = 1,
               size = points_size,
               position = position)+
    labs(x = xlab,
         y = ifelse(y_var == ".Pred", "Predicted Response", y_var),
         fill = "Variables")+
    theme_DI()+
    x_scale

  # Add facet
  if(!is.null(facet_var)){
    plot <- add_facet(plot, data, facet_var, labeller = label_both)
  }


  if(average){
    if(check_col_exists(data, ".Avg") && is.null(facet_var)){
      plot <- plot +
        geom_line(aes(y = .data$.Avg),
                  linewidth = 1, linetype = 2)
    } else {
      if(is.null(facet_var)){
        avg_data <- data %>% group_by(!! sym(gradient_var)) %>%
          mutate('.Avg' = mean(!! sym(y_var))) %>%
          ungroup()
      } else {
        avg_data <- data %>% group_by(!! sym(gradient_var), !!sym(facet_var)) %>%
          mutate('.Avg' = mean(!! sym(y_var))) %>%
          ungroup()
      }

      plot <- plot +
        geom_line(aes(y = .data$.Avg), data = avg_data,
                  linewidth = 1, linetype = 2)
    }
  }

  # If pie_data is specified then ensure colours are appropriate
  if(!is.null(pie_data)){
    # Colours for the pie-glyph slices
    if(is.null(pie_colours)){
      pie_colours <- get_colours(prop)
    } else {
      # Ensure there are same number of colours as variables specified in prop
      if(length(pie_colours) != length(prop)){
        cli::cli_abort(c("The number of {.var pie_colours} specified should
                         be same as the number of variables specified in
                         {.var prop}.",
                         "i" = "There are {length(prop)} values in {.var prop}
                               but only {length(pie_colours)} colour{?s}
                               {?was/were} specified."))
      }
    }

    check_plot_data(data = pie_data,
                    cols_to_check = c(".Gradient", y_var,
                                      ".Richness", ".Evenness"),
                    calling_fun = "gradient_change",
                    data_name = "pie_data")

    plot <- plot +
      geom_pie_glyph(aes(group = .data$.Evenness),
                     slices = prop, radius = pie_radius,
                     colour = 'black', data = pie_data,
                     position = position)+
      scale_fill_manual(values = pie_colours, name = "Variables")
  }

  return(plot)
}
