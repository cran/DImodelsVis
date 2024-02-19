#' @title Conditional ternary diagrams
#'
#' @description
#' The helper function for preparing the underlying data for creating conditional
#' ternary diagrams, where we fix \eqn{n-3} variables to have a constant value
#' \eqn{p_1, p_2, p_3, ..., p_{n-3}} such that \eqn{P = p_1 + p_2 + p_3 + ... p_{n - 3}}
#' and \eqn{0 \le P \le 1} and vary the proportion of the remaining three variables
#' between \eqn{0} and \eqn{1-P} to visualise the change in the predicted response as a
#' contour map within a ternary diagram. The output of this function can be passed to the
#' \code{\link{conditional_ternary_plot}} function to plot the results. Viewing multiple
#' 2-d slices across multiple variables should allow to create an approximation of
#' how the response varies across the n-dimensional simplex.
#'
#' @param prop A character vector indicating the model coefficients
#'             corresponding to variable proportions. These variables should
#'             be compositional in nature (i.e., proportions should sum to 1).
#' @param FG A character vector specifying the grouping of the variables
#'           specified in `prop`. Specifying this parameter would call the
#'           grouped_ternary_data function internally. See \code{\link{grouped_ternary}}
#'           or \code{\link{grouped_ternary_data}} for more information.
#' @param values A numeric vector specifying the proportional split of the
#'               variables within a group. The default is to split the group
#'               proportion equally between each variable in the group.
#' @param tern_vars A character vector giving the names of the three variables
#'                  to be shown in the ternary diagram.
#' @param conditional A data-frame describing the names of the compositional variables
#'                    and their respective values at which to slice the
#'                    simplex space. The format should be, for example, as follows: \cr
#'                    \code{data.frame("p1" = c(0, 0.5), "p2" = c(0.2, 0.1))} \cr
#'                    One figure would be created for each row in `conditional` with
#'                    the respective values of all specified variables. Any
#'                    compositional variables not specified in `conditional` will
#'                    be assumed to be 0.
#' @inheritParams ternary_data
#' @inheritDotParams add_prediction -data
#'
#' @return A data-frame containing compositional columns with names specified
#'         in `prop` parameter along with any additional columns specified in
#'         `add_var` parameter. The first five columns of the data contain the
#'         three variables (specified in `tern_vars`) shown in the ternary along
#'         with their 2-d projection and should not be modified. The following
#'         additional columns could also be present in the data.
#'  \describe{
#'    \item{.x}{The x-projection of the points within the ternary.}
#'    \item{.y}{The y-projection of the points within the ternary.}
#'    \item{.add_str_ID}{An identifier column for grouping the cartesian product
#'                       of all additional columns specified in `add_var`
#'                       parameter (if `add_var` is specified).}
#'    \item{.Sp}{An identifier column specifying the variable(s) along which the
#'               high dimensional simplex is sliced.}
#'    \item{.Value}{The value(s) (between 0 and 1) along the direction of variable(s)
#'                  in `.Sp` at which the high dimensional simplex is sliced.}
#'    \item{.Facet}{An identifier column formed by combining `.Sp` and `.value`
#'                  to group observations within a specific slice of the
#'                  high dimensional simplex.}
#'    \item{.Pred}{The predicted response for each observation
#'                 (if `prediction` is \code{TRUE}).}
#'    \item{.Lower}{The lower limit of the prediction/confidence interval
#'                  for each observation.}
#'    \item{.Upper}{The upper limit of the prediction/confidence interval
#'                  for each observation.}
#'  }
#'
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' ## Load data
#' data(sim4)
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + p5 + p6)^2, data = sim4)
#'
#' ## Create data
#' ## Any species not specified in `tern_vars` or conditional will be assumed
#' ## to be 0, for example p5 and p6 here.
#' head(conditional_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                               tern_vars = c("p1", "p2", "p3"),
#'                               conditional = data.frame("p4" = c(0, 0.2, 0.5)),
#'                               model = mod,
#'                               resolution = 1))
#'
#' ## Can also condition on multiple species
#' cond <- data.frame(p4 = c(0, 0.2), p5 = c(0.5, 0.1), p6 = c(0, 0.3))
#' cond
#' head(conditional_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                               tern_vars = c("p1", "p2", "p3"),
#'                               conditional = cond,
#'                               model = mod,
#'                               resolution = 1))
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + p5 + p6)^2 + treatment,
#'            data = sim4)
#'
#' ## Can also add any additional variables independent of the simplex
#' ## Notice the additional `.add_str_ID` column
#' head(conditional_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                               tern_vars = c("p1", "p2", "p3"),
#'                               conditional = data.frame("p4" = c(0, 0.2, 0.5)),
#'                               add_var = list("treatment" = c(50, 150)),
#'                               model = mod,
#'                               resolution = 1))
#'
#' ## It could be desirable to take the output of this function and add
#' ## additional variables to the data before making predictions
#' ## Use `prediction = FALSE` to get data without any predictions
#' cond_data <- conditional_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                                       tern_vars = c("p1", "p2", "p3"),
#'                                       conditional = data.frame("p4" = c(0, 0.2, 0.5)),
#'                                       prediction = FALSE,
#'                                       resolution = 1)
#' ## The data can then be modified and the `add_prediction` function can be
#' ## called manually using either the model object or model coefficients
#' cond_data$treatment <- 50
#' head(add_prediction(data = cond_data, model = mod))
conditional_ternary_data <- function(prop, FG = NULL,
                                     values = NULL, tern_vars = NULL,
                                     conditional = NULL,
                                     add_var = list(),
                                     resolution = 3, prediction = TRUE, ...){

  # Sanity Checks
  if(missing(prop)){
    cli::cli_abort(c("{.var prop} cannot be empty.",
                     "i" = "Specify a character vector indicating the model
                            coefficients corresponding to variable
                            proportions."))
  }

  # If FG is specified use grouped_ternary
  if(!is.null(FG)){
    cond_data <- grouped_ternary_data(prop = prop, FG = FG, values = values,
                                      tern_vars = tern_vars,
                                      conditional = conditional,
                                      add_var = add_var, resolution = 3,
                                      prediction = TRUE, ...)
    return(cond_data)
  }
  if (!inherits(prop, "character")){
    cli::cli_abort("{.var prop} should be a character vector containing the
                   names of the variables whose proportions sum to 1.",
                   "i" = "{.var prop} was specified as a
                          {.cls {prop}}.")
  }

  if(length(prop) < 3){
    cli::cli_abort(c("Ternary diagrams can only be created for models with more
                      than or equal to 3 species.",
                     "i" = "Currently only {length(prop)} variables are specified in
                            {.var prop}."))
  }

  # Species to be shown in the ternary diagram
  if(is.null(tern_vars)){
    cli::cli_warn(c("No values were specified in {.var tern_vars}.",
                    "i" = "The first three values from {.var prop} which are not
                    present in the {.var conditional} paramter would be chosen
                    as variables to show in the ternary.",
                    "i" = "If this is not desirable specify a character vector
                    of length 3, indicating the three variables to be shown
                    in the ternary diagram."))
    tern_vars <- prop[!prop %in% names(conditional)][1:3]
    if(any(is.na(tern_vars))){
      cli::cli_abort(c("After accounting for all variables specified in
                       {.var conditional}, only {length(tern_vars[!is.na(tern_vars)])}
                       variable{?s} are left over to show in the ternary.",
                       "i" = "Drop {3 - length(tern_vars[!is.na(tern_vars)])} variable{?s}
                       from the {.var conditional} parameter."))
    }
  }

  if (!inherits(tern_vars, "character")){
    cli::cli_abort(c("{.var tern_vars} should be a character vector of length 3,
                   containing the names of the variables to be shown in the ternary.",
                   "i" = "{.var tern_vars} was specified as a
                          {.cls {class(tern_vars)}}."))
  }

  if(!all(tern_vars %in% prop)){
    cli::cli_abort(c("All values specified in {.var tern_vars} should be present
                    in {.var prop}.",
                    "i" = "{.val {tern_vars[! tern_vars %in% prop]}} {?is/are}
                            not present in {.var prop}."))
  }

  if(length(tern_vars) != 3){
    cli::cli_abort(c("{.var tern_vars} should have exactly three elements.",
                     "i" = "Currently {.val {length(tern_vars)}} elements are
                     specified in {.var tern_vars}."))
  }


  # If conditional is specified ensure it's in proper format
  if(!is.null(conditional)){
    conditional <- check_conditional_parameter(conditional, prop = prop,
                                               tern_vars = tern_vars)
  }

  # Create base ternary data to be plotted
  triangle <- ternary_data(prop = tern_vars,
                           add_var = add_var,
                           resolution = resolution,
                           prediction = FALSE)

  # Any add any species not specified in tern_vars or conditional and
  # assume them to be zero
  cond_names <- colnames(conditional)
  leftover <- prop[! prop %in% c(tern_vars, cond_names)]
  if(length(leftover) > 0){
    pOther <- matrix(0, ncol = length(leftover), nrow = nrow(triangle))
    colnames(pOther) <- leftover
    triangle <- cbind(triangle, pOther)
  }

  # If there are no species to condition on then i.e. conditional is NULL
  # our data is ready and we can make predictions from the model
  if (is.null(conditional)){
    if(prediction){
      cond_data <- add_prediction(data = triangle, ...)
    } else {
      cond_data <- triangle
    }
  # Add the values for variables on which to condition the simplex space
  } else {
    # Rescaling species to be shown in ternary for each species
    # in the conditional parameter according to the values specified
    # in the values parameter
    cond_data <- lapply(cli_progress_along(1:nrow(conditional), name = "Preparing data"), function(idx){

      cond_vals <- as.numeric(conditional[idx, ])
      x <- sum(cond_vals)

      # No need to scale if x is zero
      if(x == 0){
        scaled_data <- triangle
      } else {
        # Scale proportion of species within the ternary
        scaled_data <- triangle %>%
          mutate(!! tern_vars[1] := rescale(!!sym(tern_vars[1]),
                                            min = 0, max = 1-x),
                 !! tern_vars[2] := rescale(!!sym(tern_vars[2]),
                                            min = 0, max = 1-x),
                 !! tern_vars[3] := rescale(!!sym(tern_vars[3]),
                                            min = 0, max = 1-x))
      }

      # Add the values for species being conditioned on
      cond_data <- matrix(rep(cond_vals, times = nrow(scaled_data)),
                          ncol = length(cond_names),
                          nrow = nrow(scaled_data), byrow = TRUE) %>%
        `colnames<-`(cond_names)

      # Combine the two data
      scaled_data <- cbind(scaled_data, cond_data)

      # Add information about conditioned variables
      sp_data <- scaled_data %>%
        mutate(.Sp = paste0(cond_names, collapse = ", "),
               .Value = paste0(cond_vals, collapse = ", "),
               .Facet = paste0(cond_names, " = ", cond_vals,
                               collapse = "; \t")) %>%
        mutate(.Facet = fct_inorder(.data$.Facet))

      # To avoid any rounding issues & ensure all species proportions sum to 1
      # No need to fix if rowsum is one
      if(any(!near(rowSums(sp_data[, prop]), 1))){
        # Find the first conditional species which is non-zero
        fix_ind <- which(cond_vals != 0)[1]
        fix_sp <- cond_names[fix_ind]
        sp_data[, fix_sp] <- 1 - rowSums(sp_data[, prop[prop != fix_sp]])
      }

      if(prediction){
        sp_data <- add_prediction(data = sp_data, ...)
      }

      # # Add remaining species in the data
      # remaining_species <- matrix(0, ncol = length(conditional),
      #                             nrow = nrow(triangle))
      # colnames(remaining_species) <- cond_names
      # scaled_data <- cbind(scaled_data, remaining_species)

      # Update value of species which we are conditioning on
      # species_data <- lapply(cond_names, function(cond_sp){
      #   # Add identifier for grouping data for a particular species
      #   sp_data <- scaled_data %>%
      #                 mutate(.Sp = cond_sp,
      #                        .Value = x,
      #                        .Facet = paste0(cond_sp, ' = ', x))
      #
      #   # Predicting the response for the communities
      #   if(prediction){
      #     sp_data <- add_prediction(data = sp_data, ...)
      #   }
      #   # Need to return data via subset of rows as bind_rows fails otherwise
      #   sp_data
      #   }) %>% bind_rows()
      sp_data
    }) %>% bind_rows()
  }
  # Final formatting to pretty up data
  selection <- if(is.null(conditional)) NULL else c(colnames(conditional))
  cond_data <- cond_data %>%
    select(all_of(c(tern_vars, ".x", ".y", selection, names(add_var))), everything())

  # Add attributes for fetching data
  attr(cond_data, "prop") <- prop
  attr(cond_data, "tern_vars") <- tern_vars
  attr(cond_data, "x_proj") <- ".x"
  attr(cond_data, "y_proj") <- ".y"
  attr(cond_data, "add_var") <- names(add_var)

  cli::cli_alert_success("Finished data preparation.")
  return(cond_data)
}

#' @title Conditional ternary diagrams
#'
#' @description
#' The helper function for plotting conditional ternary diagrams. The output of
#' the `\code{\link{conditional_ternary_data}}` should be passed here to
#' visualise the n-dimensional simplex space as 2-d slices showing the change
#' in the response across any three variables, when the other variables are
#' conditioned to have fixed values.
#'
#' @importFrom ggplot2 facet_wrap
#'
#' @param data A data-frame which is the output of the
#'             `\link{conditional_ternary_data}` function.
#' @inheritParams ternary_plot
#'
#' @inherit ternary_plot return
#'
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' ## Load data
#' data(sim4)
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + p5 + p6)^2, data = sim4)
#'
#' ## Create data for slicing
#' ## We only condition on the variable "p3"
#' plot_data <- conditional_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                                       tern_vars = c("p1", "p2", "p4"),
#'                                       conditional = data.frame("p3" = c(0, 0.2, 0.5)),
#'                                       model = mod,
#'                                       resolution = 1)
#'
#' ## Create plot
#' conditional_ternary_plot(data = plot_data)
#'
#' ## Condition on multiple variables
#' cond <- data.frame(p4 = c(0, 0.2), p5 = c(0.5, 0.1), p6 = c(0, 0.3))
#' cond
#' plot_data <- conditional_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                                       tern_vars = c("p1", "p2", "p3"),
#'                                       conditional = cond,
#'                                       model = mod,
#'                                       resolution = 1)
#' ## Create plot
#' conditional_ternary_plot(data = plot_data)
#'
#' ## Create multiple plots for additional variables using `add_var`
#' ## Fit model
#' \donttest{
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + p5 + p6)^2 + treatment,
#'            data = sim4)
#'
#' ## Notice the additional `.add_str_ID` column
#' plot_data <- conditional_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"),
#'                                       tern_vars = c("p1", "p2", "p3"),
#'                                       conditional = data.frame("p4" = c(0, 0.2, 0.5)),
#'                                       add_var = list("treatment" = c(50, 150)),
#'                                       model = mod,
#'                                       resolution = 1)
#' ## Create plot
#' ## Use nrow to align plots
#' conditional_ternary_plot(data = plot_data, nrow = 2)
#' }
conditional_ternary_plot <- function(data,
                                     col_var = ".Pred",
                                     nlevels = 7,
                                     colours = NULL,
                                     lower_lim = NULL,
                                     upper_lim = NULL,
                                     tern_labels = colnames(data)[1:3],
                                     contour_text = TRUE,
                                     show_axis_labels = TRUE,
                                     show_axis_guides = FALSE,
                                     points_size = 2,
                                     axis_label_size = 4,
                                     vertex_label_size = 5,
                                     nrow = 0, ncol = 0){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble (preferably the
                            output of {.help [{.fn {col_green(\"conditional_ternary_data\")}}](DImodelsVis::conditional_ternary_data)})."))
  }
  # Check data for important columns
  check_plot_data(data = data,
                  cols_to_check = c(col_var, ".x", ".y"),
                  calling_fun = "conditional_ternary")

  if(check_col_exists(data, ".add_str_ID")){
    ids <- unique(data$.add_str_ID)
    # If user didn't specify lower limit assume it to be min of predicted response
    if(is.null(lower_lim)){
      lower_lim <- round(min(data[, ".Pred"]), 2)
    }

    # If user didn't specify upper limit assume it to be max of predicted response
    if(is.null(upper_lim)){
      upper_lim <- round(max(data[, ".Pred"]), 2)
    }

    plots <- lapply(cli_progress_along(1:length(ids), name = "Creating plot",
                                       format = paste0(
                                         "{pb_spin} Creating plot ",
                                         "[{pb_current}/{pb_total}]   ETA:{pb_eta}"
                                       )),
                    function(i){
                      data_iter <- data %>% filter(.data$.add_str_ID == ids[i])
                      plot <- conditional_ternary_plot_internal(data = data_iter,
                                                                col_var = col_var,
                                                                nlevels = nlevels,
                                                                colours = colours,
                                                                tern_labels = tern_labels,
                                                                lower_lim = lower_lim,
                                                                upper_lim = upper_lim,
                                                                contour_text = contour_text,
                                                                show_axis_labels = show_axis_labels,
                                                                show_axis_guides = show_axis_guides,
                                                                axis_label_size = axis_label_size,
                                                                vertex_label_size = vertex_label_size) +
                        labs(subtitle = ids[i])
                    })
    if(length(plots) > 1){
      plot <- new("ggmultiplot", plots = plots, nrow = nrow, ncol = ncol)
    } else {
      plot <- plots[[1]]
    }
    cli::cli_alert_success("Created all plots.")
  } else {
    plot <- conditional_ternary_plot_internal(data = data,
                                              col_var = col_var,
                                              nlevels = nlevels,
                                              colours = colours,
                                              tern_labels = tern_labels,
                                              lower_lim = lower_lim,
                                              upper_lim = upper_lim,
                                              contour_text = contour_text,
                                              show_axis_labels = show_axis_labels,
                                              show_axis_guides = show_axis_guides,
                                              axis_label_size = axis_label_size,
                                              vertex_label_size = vertex_label_size)
    cli::cli_alert_success("Created plot.")
  }

  return(plot)
}

#' @title Conditional ternary diagrams
#'
#' @description
#' We fix \eqn{n-3} variables to have a constant value \eqn{p_1, p_2, p_3, ... p_{n-3}}
#' such that \eqn{P = p_1 + p_2 + p_3 + ... p_{n - 3}} and \eqn{0 \le P \le 1} and
#' vary the proportion of the remaining three variables between \eqn{0} and \eqn{1-P}
#' to visualise the change in the predicted response as a contour map within a
#' ternary diagram. This is equivalent to taking multiple 2-d slices of the
#' high dimensional simplex space. Taking multiple 2-d slices across multiple
#' variables should allow to create an approximation of how the response varies
#' across the n-dimensional simplex.
#' This is a wrapper function specifically for statistical models fit using the
#' \code{\link[DImodels:DI]{DI()}} function from the
#' \code{\link[DImodels:DImodels-package]{DImodels}} R package and would implicitly
#' call \code{\link{conditional_ternary_data}} followed by
#' \code{\link{conditional_ternary_plot}}. If your model object isn't fit using
#' DImodels, consider calling these functions manually.
#'
#'
#' @importFrom metR geom_text_contour
#' @importFrom grDevices terrain.colors
#' @importFrom dplyr tibble between
#' @importFrom ggplot2 geom_raster scale_fill_stepsn geom_contour
#'                     theme_void guide_colorbar coord_fixed
#'
#' @param model A Diversity Interactions model object fit by using the
#'              \code{\link[DImodels:DI]{DI()}} function from the
#'              \code{\link[DImodels:DImodels-package]{DImodels}} package.
#' @inheritParams ternary_plot
#' @inheritParams conditional_ternary_data
#' @inheritParams model_diagnostics
#'
#' @inherit prediction_contributions return
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' library(dplyr)
#' data(sim2)
#' m1 <- DI(y = "response", data = sim2, prop = 3:6, DImodel = "FULL")
#'
#' #' ## Create data for slicing
#' ## We only condition on the variable "p3"
#' conditional_ternary(model = m1, tern_vars = c("p1", "p2", "p4"),
#'                     conditional = data.frame("p3" = c(0, 0.2, 0.5)),
#'                     resolution = 1)
#'
#' ## Slices for experiments for over 4 variables
#' data(sim4)
#' m2 <- DI(y = "response", prop = paste0("p", 1:6),
#'          DImodel = "AV", data = sim4) %>%
#'          suppressWarnings()
#'
#' ## Conditioning on multiple variables
#' cond <- data.frame(p4 = c(0, 0.2), p3 = c(0.5, 0.1), p6 = c(0, 0.3))
#' conditional_ternary(model = m2, conditional = cond,
#'                     tern_vars = c("p1", "p2", "p5"), resolution = 1)
#'
#' ## Create separate plots for additional variables not a part of the simplex
#' m3 <- DI(y = "response", prop = paste0("p", 1:6),
#'          DImodel = "AV", data = sim4, treat = "treatment") %>%
#'          suppressWarnings()
#'
#' ## Create plot and arrange it using nrow and ncol
#' \donttest{
#' conditional_ternary(model = m3, conditional = cond[1, ],
#'                     tern_vars = c("p1", "p2", "p5"),
#'                     resolution = 1,
#'                     add_var = list("treatment" = c(50, 150)),
#'                     nrow = 2, ncol = 1)
#' }
#'
#' ## Specify `plot = FALSE` to not create the plot but return the prepared data
#' head(conditional_ternary(model = m3, conditional = cond[1, ],
#'                          resolution = 1, plot = FALSE,
#'                          tern_vars = c("p1", "p2", "p5"),
#'                          add_var = list("treatment" = c(50, 150))))
conditional_ternary <- function(model,
                                FG = NULL,
                                values = NULL,
                                tern_vars = NULL,
                                conditional = NULL,
                                add_var = list(),
                                resolution = 3,
                                plot = TRUE,
                                nlevels = 7,
                                colours = NULL,
                                lower_lim = NULL,
                                upper_lim = NULL,
                                contour_text = TRUE,
                                show_axis_labels = TRUE,
                                show_axis_guides = FALSE,
                                axis_label_size = 4,
                                vertex_label_size = 5,
                                nrow = 0, ncol = 0){

  # Ensure specified model is fit using the DI function
  if(missing(model) || (!inherits(model, "DI") && !inherits(model, "DImulti"))){
    model_not_DI(call_fn = "conditional_ternary")
  }

  # Get data used to fit the model
  og_data <- model$original_data

  # Get all species in the model
  if(inherits(model, "DI") || inherits(model, "DImulti")){
    species <- attr(model, "prop")
  }

  # If model object is of type DImulti add info about EFs and timepoints
  if(inherits(model, "DImulti")) {
    add_var <- link_DImodelsMulti(model = model, add_var = add_var)
  }

  # Create data in appropriate format for plotting
  plot_data <- conditional_ternary_data(prop = species,
                                        FG = FG,
                                        values = values,
                                        tern_vars = tern_vars,
                                        model = model,
                                        conditional = conditional,
                                        add_var = add_var,
                                        resolution = resolution)

  # Labels for the ternary
  tern_labels <- colnames(plot_data)[1:3]

  if(isTRUE(plot)){
    plot <- conditional_ternary_plot(data = plot_data,
                                     nlevels = nlevels,
                                     colours = colours,
                                     tern_labels = tern_labels,
                                     lower_lim = lower_lim,
                                     upper_lim = upper_lim,
                                     contour_text = contour_text,
                                     show_axis_labels = show_axis_labels,
                                     show_axis_guides = show_axis_guides,
                                     axis_label_size = axis_label_size,
                                     vertex_label_size = vertex_label_size,
                                     nrow = nrow, ncol = ncol)

    return(plot)
  } else {
    return(plot_data)
  }
}

#' @keywords internal
#' Internal function for creating a ternary plot
#'
#' @usage NULL
NULL
conditional_ternary_plot_internal <- function(data,
                                              col_var = ".Pred",
                                              nlevels = 7,
                                              colours = NULL,
                                              lower_lim = NULL,
                                              upper_lim = NULL,
                                              tern_labels = colnames(data)[1:3],
                                              contour_text = TRUE,
                                              show_axis_labels = TRUE,
                                              show_axis_guides = FALSE,
                                              axis_label_size = 4,
                                              vertex_label_size = 5){

  # Create the simple ternary plot
  pl <- ternary_plot_internal(data = data,
                              col_var = col_var,
                              nlevels = nlevels,
                              colours = colours,
                              tern_labels = tern_labels,
                              lower_lim = lower_lim,
                              upper_lim = upper_lim,
                              contour_text = contour_text,
                              show_axis_labels = FALSE,
                              show_axis_guides = show_axis_guides,
                              axis_label_size = axis_label_size,
                              vertex_label_size = vertex_label_size)

  # Check if we need to condition on anything and get appropriate values
  if(check_col_exists(data, ".Facet")){
    conditional <- unique(data$.Sp)
    values <- unique(data$.Value)

    # Facet and create one panel for each species to condition on
    pl <- pl +
      facet_wrap(~ .Facet, ncol = length(values))

  } else {
    conditional <- ""
    values <- "0"
  }

  # Show appropriate labels for for the ternary axes
  if(show_axis_labels){
    # Labels for the ternary axes
    # (because they'll be scaled for conditional panels)
    axis_labels <- # lapply(conditional, function(sp){
      lapply(values, function(val){
        cond_sp <- strsplit(conditional, ", ")[[1]]
        cond_vals <- as.numeric(strsplit(val, ", ")[[1]])
        x <- sum(cond_vals)
        positions <- tibble(x1 = seq(0.2,0.8,0.2),
                            y1 = c(0,0,0,0),
                            x2 = .data$x1/2,
                            y2 = .data$x1*sqrt(3)/2,
                            x3 = (1-.data$x1)*0.5+.data$x1,
                            y3 = sqrt(3)/2-.data$x1*sqrt(3)/2,
                            label = .data$x1*(1-x),
                            rev_label = rev(.data$label),
                            .Facet = paste0(cond_sp, " = ", cond_vals,
                                            collapse = "; \t"),
                            !! sym(col_var) := 0)
      }) %>% bind_rows()

    pl <- pl +
      geom_text(data = axis_labels,
                aes(x=.data$x1, y=.data$y1, label=.data$label),
                nudge_y=-0.055, size = axis_label_size)+
      geom_text(data = axis_labels,
                aes(x=.data$x2, y=.data$y2, label=.data$rev_label),
                nudge_x=-0.055, nudge_y=0.055, size = axis_label_size)+
      geom_text(data = axis_labels,
                aes(x=.data$x3, y=.data$y3, label=.data$rev_label),
                nudge_x=0.055, nudge_y=0.055, size = axis_label_size)
  }

  return(pl)
}


check_conditional_parameter <- function(conditional, prop, tern_vars,
                                        cond_FGs = NULL, FG_flag = FALSE){
  if(! is.null(conditional) && !(inherits(conditional, "data.frame"))){
    cli::cli_abort("{.var conditional} should be a data-frame containing
                   the names and values for the variables at which
                   the simplex space should be sliced.",
                   "i" = "{.var conditional} was specified as a
                          {.cls {conditional}}.",
                   call = caller_env())
  }

  # If conditional is specified as a list convert it to a data-frame
  # If the elements in the conditional list don't all have the same length
  # then the elements with shorter lengths will be padded with zeroes to
  # ensure all elements have the same length
  # if(is.list(conditional)){
  #   lengths <- sapply(conditional, length)
  #   max_len <- max(lengths)
  #   fixed_cond <- lapply(1:length(conditional), function(idx){
  #     vec <- conditional[[idx]]
  #     if(length(vec) != max_len){
  #       vec <- c(vec, rep(0, max_len - length(vec)))
  #     }
  #     vec
  #   })
  #   names(fixed_cond) <- names(conditional)
  #   conditional <- as_tibble(fixed_cond)
  # }

  # From now on conditional will be a data.frame
  cond_names <- colnames(conditional)
  if(!all(cond_names %in% prop)){
    if(FG_flag){
      if(length(cond_FGs) < 2){
        if(length(cond_FGs) == 1){
          code_str <- glue::glue("data.frame({dQuote(cond_FGs[1])} = c(0.1, 0.3))")
        } else {
          code_str <- glue::glue("data.frame({dQuote(\"FG1\")} = c(0.1, 0.3),
                                  {dQuote(\"FG2\")} = c(0.2, 0.1))")
        }

      } else {
        code_str <- glue::glue("data.frame({dQuote({cond_FGs[1]})} = c(0.1, 0.3),
                                {dQuote({cond_FGs[2]})} = c(0.2, 0.1))")
      }
      cli::cli_abort(c("If specifying the {.var FG} parameter, {.var conditional}
                     should have names as values specified in {.var FG}.",
                       "i" = "Use values from {.val {cond_FGs}} as column name{?s} in
                    {.var conditional}.",
                       "i" = "For example specify {.var conditional} as

                       {.code {code_str}}"))
    } else {
      cli::cli_abort(c("All variables specified in {.var conditional} should be present
                    in {.var prop}.",
                       "i" = "{.val {cond_names[! cond_names %in% prop]}} {?is/are}
                            not specified in {.var prop}."))
    }
  }

  if(any(cond_names %in% tern_vars)){
    cli::cli_abort(c("The same variables can't be specified in both {.var conditional}
                    and {.var tern_vars}.",
                    "i" = "{.val {cond_names[cond_names %in% tern_vars]}} {?is/are}
                            specified in both {.var conditional} and {.var tern_vars}."))
  }

  nums <- apply(conditional, 2, is.numeric)
  if(!all(nums)){
    cli::cli_abort(c("The values specified for conditioning should all be {.cls numeric}.",
                    "i" = "{.val {cond_names[, !nums]}} {?is/are} are not {.cls numeric}."))
  }

  not_between_01 <- apply(conditional, 1, function(x){
    any(!between(x, 0, 1))
  })
  if(any(not_between_01)){
    cli::cli_abort(c("The values specified for conditioning a particular slice
                    should all be between 0 and 1 describing the values at which to condition
                     the n-dimensional simplex space.",
                    "i" = "The values specified for slice{?/s} {.val {as.data.frame(t(conditional[not_between_01, ]))}} {?is/are} not between 0 and 1."))
  }

  over1 <- apply(conditional, 1, function(x){
    ifelse(sum(x) > 1, TRUE, FALSE)
  })
  if(any(over1)){
    cli::cli_abort(c("The values specified for conditioning a particular slice
                    should have a sum less than 1 describing the values at which to condition
                     the n-dimensional simplex space.",
                    "i" = "The values specified for slice{?/s} {.val {as.data.frame(t(conditional[over1, ]))}}
                    sum over 1."))
  }

  return(conditional)
}
