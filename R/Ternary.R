#' @title Prepare data for showing contours in ternary diagrams.
#'
#' @description
#' The data preparation function for creating an equally spaced grid of three
#' compositional variables (i.e., the three variables sum to 1 at each point
#' along the grid). The projection of each point in the grid on the x-y plane is
#' also calculated. This data can be used with a relevant statistical model
#' to predict the response across the ternary surface. The output of this
#' function can then be passed to the \code{\link{ternary_plot}} function to
#' visualise the change in the response as a contour plot. \cr
#' \emph{Note:} This function works only for models with three compositional
#' predictors. For models with more than three compositional predictors see
#' \code{\link{conditional_ternary}}.
#'
#' @param prop A character vector specifying the columns names of compositional
#'             variables whose proportions to manipulate. Default is ".P1", ".P2",
#'             and ".P3".
#' @param add_var A list or data-frame specifying values for additional variables
#'                in the model other than the proportions (i.e. not part of the
#'                simplex design).
#'                This could be useful for comparing the predictions across
#'                different values for a non-compositional variable.
#'                If specified as a list, it will be expanded to show a plot
#'                for each unique combination of values specified, while if specified
#'                as a data-frame, one plot would be generated for each row in the
#'                data.
#' @param resolution A number between 1 and 10 describing the resolution of the
#'                   resultant graph.
#'                   A high value would result in a higher definition figure
#'                   but at the cost of being computationally expensive.
#' @param prediction A logical value indicating whether to pass the final data
#'                   to the `\link{add_prediction}` function and append the
#'                   predictions to the data. Default value is \code{TRUE}, but
#'                   often it would be desirable to make additional changes to
#'                   the data before making any predictions, so the user can set this to
#'                   \code{FALSE} and manually call the `\link{add_prediction}`
#'                   function.
#' @inheritDotParams add_prediction -data
#'
#' @return A data-frame with the following columns and any additional columns
#'         specified in `add_var` parameter
#'  \describe{
#'    \item{.x}{The x component of the x-y projection of the simplex point.}
#'    \item{.y}{The y component of the x-y projection of the simplex point.}
#'    \item{.P1}{The first variable whose proportion is varied across the simplex.}
#'    \item{.P2}{The second variable whose proportion is varied across the simplex.}
#'    \item{.P3}{The third variable whose proportion is varied across the simplex.}
#'    \item{.add_str_ID}{An identifier column for grouping the cartesian product
#'                       of all additional columns specified in `add_var`
#'                       parameter (if `add_var` is specified).}
#'    \item{.Pred}{The predicted response for each observation
#'                (if `prediction` is \code{TRUE}).}
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
#' library(dplyr)
#'
#' ## Load data
#' data(sim0)
#'
#' ## Fit model
#' mod <- lm(response ~ 0 + (p1 + p2 + p3)^2, data = sim0)
#'
#' ## Prepare data for creating a contour map of predicted response over
#' ## the ternary surface
#' ## Remember to specify prop with the same character values as the names
#' ## of the variables in the model containing the prop.
#' plot_data <- ternary_data(resolution = 1, model = mod,
#'                           prop = c("p1", "p2", "p3"))
#' ## Show plot
#' ternary_plot(data = plot_data)
#'
#' ## Can also add any additional variables independent of the simplex using
#' ## the `add_var` argument
#' sim0$treatment <-  rep(c("A", "B", "C", "D"), each = 16)
#' new_mod <- update(mod, ~. + treatment, data = sim0)
#' plot_data <- ternary_data(prop = c("p1", "p2", "p3"),
#'                           add_var = list("treatment" = c("A", "B")),
#'                           resolution = 1, model = new_mod)
#' ## Plot to compare between additional variables
#' \donttest{
#' ternary_plot(plot_data)
#' }
#'
#' ## It could be desirable to take the output of this function and add
#' ## additional variables to the data before making predictions
#' ## Use `prediction = FALSE` to get data without any predictions
#' contour_data <- ternary_data(prop = c("p1", "p2", "p3"),
#'                              model = mod,
#'                              prediction = FALSE,
#'                              resolution = 1)
#' head(contour_data)
#'
#' ## Manually add the treatment variable
#' contour_data$treatment <- "A"
#' ## Make predictions
#' head(add_prediction(data = contour_data, model = new_mod))
#'
#' ## Manually add the interaction terms
#' contour_data <- contour_data %>%
#'                   mutate(`p1:p2` = p1*p2,
#'                          `p2:p3` = p2*p3,
#'                          `p1:p3` = p1*p3)
#'
#' ## Add predictions using model coefficients
#' contour_data <- add_prediction(data = contour_data,
#'                                coefficient = mod$coefficient)
#' head(contour_data)
#'
#' ## Note: Add predictions via coefficients would not give confidence intervals
#' ## to get CIs using coefficients we need to specify the variance-covariance
#' ## matrix using `vcov`
#' contour_data <- add_prediction(data = contour_data,
#'                                coefficient = mod$coefficient,
#'                                vcov = vcov(mod),
#'                                interval = "confidence")
#' head(contour_data)
#' ## Show plot
#' \donttest{
#' ternary_plot(contour_data)
#' }
#' ## See `?ternary_plot` for options to customise the ternary_plot
ternary_data <- function(prop = c(".P1", ".P2", ".P3"),
                         add_var = list(),
                         resolution = 3, prediction = TRUE, ...){
  # Ensure inputs are proper
  sanity_checks(characters = list("prop" = prop),
                numerics = list("resolution" = resolution),
                booleans = list("prediction" = prediction),
                unit_lengths = list("resolution" = resolution,
                                    "prediction" = prediction))

  # Ensure prop has length three
  if(length(prop) != 3){
    cli::cli_abort(c("{.fn ternary_data} works only for models with three compositional
                     predictors.",
                     "i" = "See {.help [{.fn conditional_ternary}](DImodelsVis::conditional_ternary)}
                     for models with more than three compositonal predictors."))
  }

  if(!between(resolution, 0, 10)){
    cli::cli_warn(c("{.var resolution} should be a number with values
                    between 0 and 10",
                    "i" = "The value specified for {.var resolution} was
                          {as.character(resolution)}.",
                    "i" = "Reverting back to the default value of 3."))
    resolution <- 3
  }

  # Column variables containing the projections
  x <-  ".x"
  y <-  ".y"

  # Prepare data for creating conditional proportions
  base <- seq(0,1,l=100*2*resolution)
  high <- seq(0,sin(pi/3),l=86.6*2*resolution)
  triangle <- expand.grid(base = base, high = high)
  triangle <- subset(triangle, (((base*sin(pi/3)*2) >= high) &
                                  (((1-base)*sin(pi/3)*2) >= high)))

  # Extrapolate 2-d simplex coordinates to represent proportions
  # of the three species shown in the simplex
  triangle <- triangle %>%
    mutate(!! prop[1] := .data$high*2/sqrt(3),
           !! prop[3] := .data$base - .data$high/sqrt(3),
           !! prop[2] := 1 - .data$high*2/sqrt(3) -
                                    (.data$base - .data$high/sqrt(3))) %>%
    rename(!!x := base, !!y := high) %>%
    select(all_of(c(x, y, prop[1:3])))

  # Ensure experimental structure are specified correctly
  dotArgs <- rlang::dots_values(...)
  model <- if (!is.null(dotArgs$model)) dotArgs$model else NULL
  if(!is.null(model)){
    add_var <- check_add_var(model = model, add_var = add_var)
  }

  # Add any experimental structures
  if(length(add_var) > 0){
    triangle <- add_add_var(add_var = add_var, data = triangle)
  }

  if(prediction){
    triangle <- add_prediction(data = triangle, ...)
  }

  # Order columns
  triangle <- triangle %>% select(all_of(c(prop, x, y)), everything())

  attr(triangle, "prop") <- prop
  attr(triangle, "tern_vars") <- prop
  attr(triangle, "x_proj") <- ".x"
  attr(triangle, "y_proj") <- ".y"
  attr(triangle, "add_var") <- names(add_var)

  return(triangle)
}

#' @title Ternary diagrams
#'
#' @description
#' Create a ternary diagram showing the a scatter-plot of points across the surface
#' or a contour map showing the change in a continuous variable across the
#' ternary surface. The ternary surface can be created using the
#' \code{\link{ternary_data}} function.
#'
#' @param data A data-frame consisting of the x-y plane projection of the
#'             2-d simplex. This data could be the output of the
#'             `\link{ternary_data}` function, and contain the  predicted
#'             response at each point along the simplex to show the variation
#'             in response as a contour map.
#' @param prop A character vector specifying the columns names of compositional
#'             variables. By default, the function will try to automatically
#'             interpret these values from the data.
#' @param tern_labels A character vector containing the labels of the vertices
#'                    of the ternary. The default is the column names of the
#'                    first three columns of the data, with the first column
#'                    corresponding to the top vertex, second column corresponding
#'                    to the left vertex and the third column corresponding to
#'                    the right vertex of the ternary.
#' @param col_var The column name containing the variable to be used for
#'                colouring the contours or points. The default is ".Pred".
#' @param show A character string indicating whether to show data-points or contours
#'             on the ternary. The default is to show "contours".
#' @param show_axis_labels A boolean value indicating whether to show axis
#'                         labels along the edges of the ternary. The default
#'                         is \code{TRUE}.
#' @param show_axis_guides A boolean value indicating whether to show axis
#'                         guides within the interior of the ternary. The
#'                         default is \code{FALSE}.
#' @param axis_label_size A numeric value to adjust the size of the axis labels
#'                        in the ternary plot. The default size is 4.
#' @param vertex_label_size A numeric value to adjust the size of the vertex
#'                          labels in the ternary plot. The default size is 5.
#' @param points_size If showing points, then a numeric value specifying the size
#'                    of the points.
#' @param nlevels The number of levels to show on the contour map.
#' @param colours A character vector or function specifying the colours for the
#'                contour map or points. The number of colours should be same as
#'                `nlevels` if (`show = "contours"`). \cr
#'                The default colours scheme is the
#'                \code{\link[grDevices:terrain.colors]{terrain.colors()}} for
#'                continuous variables and an extended version of the Okabe-Ito
#'                colour scale for categorical variables.
#' @param lower_lim A number to set a custom lower limit for the contour
#'                  (if `show = "contours"`). The default is minimum of the prediction.
#' @param upper_lim A number to set a custom upper limit for the contour
#'                  (if `show = "contours"`). The default is maximum of the prediction.
#' @param contour_text A boolean value indicating whether to include labels on
#'                     the contour lines showing their values
#'                     (if `show = "contours"`). The default is \code{FALSE}.
#' @param nrow Number of rows in which to arrange the final plot
#'             (when `add_var` is specified).
#' @param ncol Number of columns in which to arrange the final plot
#'             (when `add_var` is specified).
#'
#' @inherit prediction_contributions return
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' library(dplyr)
#' library(ggplot2)
#'
#' ## Load data
#' data(sim0)
#'
#' ### Show raw data as points in ternary
#' ## `ternary_plot` shows contours by default, use `show = "points"` to show
#' ## points across the ternary
#' ternary_plot(data = sim0, prop = c("p1", "p2", "p3"), show = "points")
#'
#' ## The points can also be coloured using an additional variable by
#' ## specifying it in `col_var`
#' ternary_plot(data = sim0, prop = c("p1", "p2", "p3"),
#'              col_var = "response", show = "points")
#'
#' ## Categorical variables can also be shown
#' ## Also show axis guides using `show_axis_guides`
#' sim0$richness <- as.factor(sim0$richness)
#' ternary_plot(data = sim0, prop = c("p1", "p2", "p3"),
#'              col_var = "richness", show = "points",
#'              show_axis_guides = TRUE)
#'
#' ## Change colours by using `colours` argument
#' ## and increase points size using `points_size`
#' ternary_plot(data = sim0, prop = c("p1", "p2", "p3"),
#'              col_var = "richness", show = "points",
#'              colours = c("tomato", "steelblue", "orange"),
#'              points_size = 4)
#'
#' ### Show contours of response
#' ## Fit model
#' mod <- lm(response ~ 0 + (p1 + p2 + p3)^2, data = sim0)
#'
#' ## Create a contour map of predicted response over the ternary surface
#' ## Remember to specify prop with the same character values as the names
#' ## of the variables in the model containing the prop.
#' plot_data <- ternary_data(resolution = 1, model = mod,
#'                           prop = c("p1", "p2", "p3"))
#'
#' ## Create a contour plot of response across the ternary space
#' ternary_plot(plot_data)
#'
#' ## Change colour scheme
#' cols <- hcl.colors(7) # because there are 7 contour levels by default
#' ternary_plot(plot_data, colours = cols)
#'
#' \donttest{
#' ## Change number of contours using `nlevels`
#' ## and set custom upper and lower limits for the scale
#' ternary_plot(plot_data, nlevels = 10, colours = hcl.colors(10),
#'              lower_lim = 10, upper_lim = 35)
#'
#' ## Change ternary labels along with their font-size
#' ternary_plot(plot_data, tern_labels = c("Sp1", "Sp2", "Sp3"),
#'              vertex_label_size = 6, axis_label_size = 5)
#'
#' ## Add additional variables and create a separate plot for each
#' sim0$treatment <-  rep(c("A", "B", "C", "D"), each = 16)
#' new_mod <- update(mod, ~. + treatment, data = sim0)
#' tern_data <- ternary_data(resolution = 1, model = new_mod,
#'                           prop = c("p1", "p2", "p3"),
#'                           add_var = list("treatment" = c("A", "C")))
#' ## Arrange plot in 2 columns
#' ternary_plot(data = tern_data, ncol = 2)
#' }
ternary_plot <- function(data, prop = NULL,
                         col_var = ".Pred",
                         show = c("contours", "points"),
                         tern_labels = c("P1", "P2", "P3"),
                         show_axis_labels = TRUE,
                         show_axis_guides = FALSE,
                         axis_label_size = 4,
                         vertex_label_size = 5,
                         points_size = 2,
                         nlevels = 7,
                         colours = NULL,
                         lower_lim = NULL,
                         upper_lim = NULL,
                         contour_text = FALSE,
                         nrow = 0,
                         ncol = 0){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data-frame or tibble containing compositional variables,
                     preferably the output of
                     {.help [{.fn {col_green('ternary_data')}}](DImodelsVis::ternary_data)} or
                     a data-frame with a similar structure and column names."))
  }
  # Ensure prop is specified
  if(is.null(prop)){
    data_prop <- attr(data, "tern_vars")
    if(is.null(data_prop)){
      cli::cli_abort(c("{.var prop} was not specified and can not be inferred
                       from the {.var data} either.",
                       "i" = "Specify a character vector indicating the names of the three
                       variables to be shown within the ternary in {.var prop} or create your data
                       using the {.help [{.fn {col_green('ternary_data')}}](DImodelsVis::ternary_data)}
                       function."))
    } else {
      prop <- data_prop
    }
  }
  show <-  match.arg(show)

  # Multiple plots if additional variables were specified
  if(check_col_exists(data, ".add_str_ID")){
    ids <- unique(data$.add_str_ID)
    # If contours are shown then ensure we have same scale for all plots
    if(show == "contours"){
      check_presence(data = data, col = col_var,
                     message = c("The column name specified in {.var col_var} is
                               not present in the data.",
                                 "i" = "Specify the name of the column
                                     containing the predictions for the
                                     communities in the simplex in {.var col_var}."))

      # If user didn't specify lower limit assume it to be min of predicted response
      if(is.null(lower_lim)){
        # Ensure rounding includes all values in range
        lower_lim <- round(min(data[, col_var]), 2) - 0.01
      }

      # If user didn't specify upper limit assume it to be max of predicted response
      if(is.null(upper_lim)){
        # Ensure rounding includes all values in range
        upper_lim <- round(max(data[, col_var]), 2) + 0.01
      }
    }

    plots <- lapply(cli_progress_along(1:length(ids), name = "Creating plot",
                                       format = paste0(
                                         "{cli::pb_spin} Creating plot ",
                                         "[{cli::pb_current}/{cli::pb_total}]   ETA:{cli::pb_eta}"
                                       )),
                    function(i){
                      data_iter <- data %>% filter(.data$.add_str_ID == ids[i])
                      ternary_plot_internal(data = data_iter,
                                            prop = prop,
                                            col_var = col_var,
                                            show = show,
                                            points_size = points_size,
                                            nlevels = nlevels,
                                            colours = colours,
                                            lower_lim = lower_lim,
                                            upper_lim = upper_lim,
                                            tern_labels = tern_labels,
                                            contour_text = contour_text,
                                            show_axis_labels = show_axis_labels,
                                            show_axis_guides = show_axis_guides,
                                            axis_label_size = axis_label_size,
                                            vertex_label_size = vertex_label_size)+
                        labs(subtitle = ids[i])
                    })
    if(length(plots) > 1){
      plot <- new("ggmultiplot", plots = plots, nrow = nrow, ncol = ncol)
    } else {
      plot <- plots[[1]]
    }
    cli::cli_alert_success("Created all plots.")
  # Single plot otherwise
  } else {
    plot <- ternary_plot_internal(data = data,
                                  prop = prop,
                                  col_var = col_var,
                                  show = show,
                                  points_size = points_size,
                                  nlevels = nlevels,
                                  colours = colours,
                                  lower_lim = lower_lim,
                                  upper_lim = upper_lim,
                                  tern_labels = tern_labels,
                                  contour_text = contour_text,
                                  show_axis_labels = show_axis_labels,
                                  show_axis_guides = show_axis_guides,
                                  axis_label_size = axis_label_size,
                                  vertex_label_size = vertex_label_size)
    cli::cli_alert_success("Created plot.")
  }
  return(plot)
}

#' @keywords internal
#' Internal function for creating a ternary plot
#'
#' @importFrom ggplot2 scale_colour_gradientn scale_color_manual element_line element_rect
#'
#' @usage NULL
NULL
ternary_plot_internal <- function(data, prop,
                                  col_var = ".Pred",
                                  show = c("contours", "points"),
                                  nlevels = 7,
                                  colours = NULL,
                                  lower_lim = NULL,
                                  upper_lim = NULL,
                                  tern_labels = c("P1", "P2", "P3"),
                                  show_contours = TRUE,
                                  contour_text = FALSE,
                                  show_axis_labels = TRUE,
                                  show_axis_guides = FALSE,
                                  points_size = 2,
                                  axis_label_size = 4,
                                  vertex_label_size = 5){

  # Check all inputs are appropriate. Print informative error messages if not
    sanity_checks(data = data,
                  characters = list("tern_labels" = tern_labels),
                  numerics = list("nlevels" = nlevels,
                                  "axis_label_size" = axis_label_size,
                                  "vertex_label_size" = vertex_label_size,
                                  "points_size" = points_size),
                  booleans = list("contour_text" = contour_text,
                                  "show_axis_guides" = show_axis_guides,
                                  "show_axis_labels" = show_axis_labels,
                                  "show_contours" = show_contours),
                  colours = colours)
    show <- match.arg(show)

    if(show == "contours"){
      # If user messes up data attributes
      if(any(sapply(list(attr(data, "x_proj"), attr(data, "y_proj"), attr(data, "tern_vars")), is.null))){
        attr(data, "x_proj") <- ".x"
        attr(data, "y_proj") <- ".y"
        attr(data, "tern_vars") <- names(data)[1:3]

        if(check_col_exists(data, ".x") && check_col_exists(data,".y")){
          cli::cli_warn(c("!" = "Certain attributes of the data which are needed to prepare
                    the plot are missing. This could happen if any data manipulation
                    performed by the user messes up the {.cls data.frame} attributes.",
                          "i" = "The function will try to reconstruct the necessary attributes
                    and create the plot but it might not always be possible.",
                          "i" = "To avoid this, consider using the
                    {.help [{.fun copy_attributes}](DImodelsVis::copy_attributes)}
                    function on the data after performing any data manipulation operation
                          to ensure the it has the necessary attributes."))
        } else {
          cli::cli_abort(c("x" = "Certain attributes of the data which are needed for plotting
                    the response contours are missing. This could happen if any data manipulation
                    performed by the user messes up the {.cls data.frame} attributes.",
                          "i" = "If you intended to plot raw points set the {.var show} argument to {.val points}",
                          "i" = "If you indeed wish to plot contours, use the
                    {.help [{.fun copy_attributes}](DImodelsVis::copy_attributes)}
                    function on the data after performing any data manipulation operation
                          to ensure the it has the necessary attributes."))
        }

      }

      x <- attr(data, "x_proj")
      y <- attr(data, "y_proj")

      # Ensure column containing col_var is present in data
      check_presence(data = data, col = col_var,
                     message = c("The column name specified in {.var col_var} is
                               not present in the data.",
                                 "i" = "Specify the name of the column
                                     containing the predictions for the
                                     communities in the simplex in {.var col_var}."))

      # If user didn't specify lower limit assume it to be min of predicted response
      if(is.null(lower_lim)){
        lower_lim <- round(min(data[, col_var]), 2) - 0.01
      }

      # If user didn't specify upper limit assume it to be max of predicted response
      if(is.null(upper_lim)){
        upper_lim <- round(max(data[, col_var]), 2) + 0.01
      }
    } else {
      # Calculate x-y projection if it's not present in data
      x <- attr(data, "x_proj")
      y <- attr(data, "y_proj")
      if(is.null(x) || is.null(y)){
        x <- ".x"
        y <- ".y"
        data <- prop_to_tern_proj(data, prop = prop, x = x, y = y)
      }
      lower_lim <- upper_lim <- 0
    }


    # Further checks for appropriateness of parameters
    sanity_checks(numerics = list("upper_lim" = upper_lim,
                                  "lower_lim" = lower_lim),
                  unit_lengths = list("upper_lim" = upper_lim,
                                      "lower_lim" = lower_lim,
                                      "col_var" = col_var,
                                      "nlevels" = nlevels,
                                      "points_size" = points_size,
                                      "axis_label_size" = axis_label_size,
                                      "vertex_label_size" = vertex_label_size,
                                      "contour_text" = contour_text,
                                      "show_contours" = show_contours,
                                      "show_axis_guides" = show_axis_guides,
                                      "show_axis_labels" = show_axis_labels))

    # Labels for the ternary
    if(length(tern_labels) !=3){
      if(length(tern_labels) > 3){
        cli::cli_warn(c("More than three labels were specified for the ternary
                        diagram. The first three labels in {.var tern_labels}
                        will be chosen"))
        tern_labels <- tern_labels[1:3]
      } else {
        cli::cli_abort(c("Three labels are needed for the ternary, only
                         {length(tern_labels)} were specified."))
      }
    }

    if(show == "contours"){
      # Create colour-scale (legend) for plot
      # Create breaks between range of legend
      size <- nlevels + 1
      breaks <- round(seq(lower_lim, upper_lim, length.out= size), 2)

      # Choose colours
      # If user didn't specify colours then use the default terrain colours
      if(is.null(colours)){
        colours <- terrain.colors(nlevels, rev = T)
      }

      pl <- ggplot(data, aes(x = .data[[x]], y = .data[[y]],
                             z = .data[[col_var]])) +
        geom_raster(aes(fill = .data[[col_var]]))+
        scale_fill_stepsn(colours = colours, breaks = breaks,
                          labels = function(val){
                            val
                          },
                          limits = c(lower_lim, upper_lim),
                          show.limits = TRUE,
                          guide = guide_colorbar(
                            # Defaults from theme since they get overridden by theme_void()
                            theme = theme(legend.frame = ggplot2::element_rect(colour = "black", linewidth = 0.5),
                                          legend.ticks = ggplot2::element_line(colour = "black", linewidth = 0.5))
                          ),
                          oob = scales::censor)+
        geom_contour(breaks = breaks, colour = "black")+
        # guides(fill = guide_colorbar(theme = theme(legend.frame = ggplot2::element_rect(colour = "black"),
        #                                            legend.ticks = ggplot2::element_line(colour = "black"))))+
        theme_void()+
        theme(legend.key.size = unit(0.1, 'npc'),
              legend.key.height = unit(0.04, 'npc'),
              legend.title = element_text(size = 14, vjust = 0.9),
              plot.subtitle = element_text(hjust=0.5, size=14),
              strip.text = element_text(size =14, vjust = 0.5),
              legend.text = element_text(size = 12, angle = 45,
                                         vjust = 1.2, hjust = 1.2),
              legend.position = "bottom",
              # Default tick length from theme() in ggplot
              legend.ticks.length = unit(0.24, "lines")) +
        labs(fill = "Prediction")
    } else {
      # Base of plot
      pl <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
        theme_void()+
        theme(legend.key.size = unit(0.1, 'npc'),
              legend.key.height = unit(0.04, 'npc'),
              legend.title = element_text(size = 14, vjust = 1),
              plot.subtitle = element_text(hjust=0.5, size=14),
              strip.text = element_text(size =14, vjust = 0.5),
              legend.text = element_text(size = 12),
              legend.position = 'bottom')
    }

    # Labels for the ternary axes
    axis_labels <- tibble(x1 = seq(0.2,0.8,0.2),
                          y1 = c(0,0,0,0),
                          x2 = .data$x1/2,
                          y2 = .data$x1*sqrt(3)/2,
                          x3 = (1-.data$x1)*0.5+.data$x1,
                          y3 = sqrt(3)/2-.data$x1*sqrt(3)/2,
                          label = .data$x1,
                          rev_label = rev(.data$label),
                          !! col_var := 0)

    # Showing axis labels
    if(show_axis_labels){
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

    # Showing axis guides
    if(show_axis_guides){
      pl <- pl +
        geom_segment(data = axis_labels,
                     aes(x = .data$x1, y = .data$y1,
                         xend = .data$x2, yend = .data$y2), colour='grey',
                     linetype='dashed', linewidth=1, alpha = .75)+
        geom_segment(data = axis_labels,
                     aes(x = .data$x1, y = .data$y1,
                         xend = .data$x3, yend = .data$y3), colour='grey',
                     linetype='dashed', linewidth=1, alpha = .75)+
        geom_segment(data = axis_labels,
                     aes(x = .data$x2, y = .data$y2,
                         xend = rev(.data$x3), yend = rev(.data$y3)),
                     colour='grey',
                     linetype='dashed', linewidth=1, alpha = .75)
    }

    # Layering plot
    pl <- pl +
      geom_text(data = tibble(x = c(0.5, 0, 1), y = c(sqrt(3)/2, 0,  0),
                              label = tern_labels,
                              !! col_var := 0) %>%
                  mutate(nudge_x = c(0, -0.05, 0.05),
                         nudge_y = c(0.05, 0, 0)) %>%
                  mutate(x = .data$x + .data$nudge_x,
                         y = .data$y + .data$nudge_y),
                aes(x= .data$x, y= .data$y, label = .data$label),
                size = vertex_label_size, fontface='plain')+
      geom_segment(data = tibble(x = c(0, 0, 1), y = c(0,0,0),
                                 xend = c(1, 0.5, 0.5),
                                 yend = c(0, sqrt(3)/2, sqrt(3)/2),
                                 !! col_var := 0),
                   aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
                   linewidth = 1)+
      #facet_wrap(~ Value, ncol = length(values))+
      coord_fixed()

    # Show points
    if(show == "points"){
      if(check_col_exists(data, col_var)){
        pl <- pl + geom_point(aes(colour = .data[[col_var]]), size = points_size)

        # Add colours
        if(is.numeric(data[, col_var])){
          # Add colours
          if(is.null(colours)){
            colours <- terrain.colors(nlevels, rev = T)
          }
          pl <- pl + scale_colour_gradientn(colours = colours)
        } else {
          # Add colours
          if(is.null(colours)){
            colours <- get_colours(length(unique(data[, col_var])))
          }
          pl <- pl + scale_color_manual(values = colours)
        }
      } else {
        pl <- pl + geom_point(size = points_size)
      }
    }

    if(show == "contours" && contour_text){
      pl <- pl +
        geom_text_contour(skip=0, breaks = breaks,
                          label.placer = metR::label_placer_flattest(),
                          size=3.5, nudge_x = 0.015, nudge_y = 0.015)

    }
    return(pl)
  }
