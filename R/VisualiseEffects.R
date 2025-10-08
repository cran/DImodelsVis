#' @title Prepare data for effects plots of compositional predictors
#'
#' @description
#' The helper function to create the underlying data for visualising the effect
#' of increasing or decreasing (or both) the proportion of a variable from a
#' set of compositional predictors. This is a special case of the
#' \code{\link{simplex_path}} function where the end points are either the
#' monoculture (i.e. variable of interest = 1, while all others equal 0) of the
#' variable of interest (when increasing the proportion) or a community without
#' the variable of interest (when decreasing the proportion). The observations
#' specified in `data` are connected to the respective communities (monoculture of
#' the variable of interest or the community without the variable of interest) by a
#' straight line across the simplex; This has the effect of changing the proportion of
#' the variable of interest whilst adjusting the proportion of the other variables
#' but keeping the ratio of their relative proportions unchanged, thereby preserving
#' the compositional nature of the data. See examples for more information.
#' The output of this function can be passed to the
#' \code{\link{visualise_effects_plot}} function to visualise the results.
#'
#'
#' @param data A dataframe specifying the initial communities of interest for which
#'             to visualise the effect of increasing/decreasing a variable.
#'             If a model object is specified then this data should contain all the
#'             variables present in the model object including any additional variables
#'             not part of the simplex design.
#'             If a coefficient vector is specified then data should contain same number of
#'             columns as the number of elements in the coefficient vector and a one-to-one
#'             positional mapping would be assumed between the data columns and the
#'             elements of the coefficient vector.
#' @param prop A vector of column names or indices identifying the columns containing the
#'             variables proportions in the data.
#' @param var_interest A character vector specifying the variable for which to visualise
#'                     the effect of change on the response. If left blank,
#'                     all variables would be assumed to be of interest.
#' @param prop A vector of column names or indices identifying the columns containing the
#'             variable proportions (i.e., compositional columns) in the data.
#' @param add_var A list specifying values for additional variables
#'                in the model other than the proportions (i.e. not part of the simplex design).
#'                This would be useful to compare the predictions across
#'                different values for a categorical variable.
#'                One plot will be generated for each unique combination
#'                of values specified here.
#' @param prediction A logical value indicating whether to pass the final data to
#'                   `\link{add_prediction}` and add predictions to the data.
#'                   Default value is \code{TRUE}, but often it would be desirable
#'                   to make additional changes to the data before making any
#'                   predictions, so the user can set this to \code{FALSE} and
#'                   manually call the `\link{add_prediction}` function.
#' @param effect One of "increase", "decrease" or "both" to indicate whether to
#'               look at the effect of increasing the proportion, decreasing the
#'               proportion or doing both simultaneously, respectively on the response.
#'               The default in "increasing".
#' @inheritDotParams add_prediction -data
#'
#' @return A data frame with the following columns appended at the end
#'  \describe{
#'    \item{.Sp}{An identifier column to discern the variable of interest being
#'               modified in each curve.}
#'    \item{.Proportion}{The value of the variable of interest within the community.}
#'    \item{.Group}{An identifier column to discern between the different curves.}
#'    \item{.add_str_ID}{An identifier column for grouping the cartesian product
#'                       of all additional columns specified in `add_var`
#'                       parameter (if `add_var` is specified).}
#'    \item{.Pred}{The predicted response for each observation.}
#'    \item{.Lower}{The lower limit of the prediction/confidence interval for
#'                  each observation.}
#'    \item{.Upper}{The upper limit of the prediction/confidence interval for
#'                  each observation.}
#'    \item{.Marginal}{The marginal change in the response (first derivative)
#'                     with respect to the gradual change in the proportion of
#'                     the species of interest.}
#'    \item{.Threshold}{A numeric value indicating the maximum proportion of
#'                      the species of interest within a particular community
#'                      which has a positive marginal effect on the response.}
#'    \item{.MarEffect}{A character string entailing whether the increase/decrease
#'                      of the species of interest from the particular community
#'                      would result in a positive or negative marginal effect
#'                      on the response.}
#'    \item{.Effect}{An identifier column signifying whether considering the
#'                   effect of species addition or species decrease.}
#'  }
#'
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' ## Load data
#' data(sim1)
#'
#' ## Fit model
#' mod <- glm(response ~ p1 + p2 + p3 + p4 + 0, data = sim1)
#'
#' ## Create data for visualising effect of increasing the proportion of
#' ## variable p1 in data
#' ## Notice how the proportion of `p1` increases while the proportion of
#' ## the other variables decreases whilst maintaining their relative proportions
#' head(visualise_effects_data(data = sim1, prop = c("p1", "p2", "p3", "p4"),
#'                             var_interest = "p1", effect = "increase",
#'                             model = mod))
#'
#' ## Create data for visualising the effect of decreasing the proportion
#' ## variable p1 in data using `effect = "decrease"`
#' head(visualise_effects_data(data = sim1, prop = c("p1", "p2", "p3", "p4"),
#'                             var_interest = "p1", effect = "decrease",
#'                             model = mod))
#'
#' ## Create data for visualising the effect of increasing and decreasing the
#' ## proportion variable p3 in data using `effect = "both"`
#' head(visualise_effects_data(data = sim1, prop = c("p1", "p2", "p3", "p4"),
#'                             var_interest = "p3", effect = "decrease",
#'                             model = mod))
#'
#' ## Getting prediction intervals at a 99% confidence level
#' head(visualise_effects_data(data = sim1, prop = c("p1", "p2", "p3", "p4"),
#'                             var_interest = "p1", effect = "decrease",
#'                             model = mod, conf.level = 0.99,
#'                             interval = "prediction"))
#'
#' ## Adding additional variables to the data using `add_var`
#' ## Notice the new .add_str_ID column in the output
#' sim1$block <- as.numeric(sim1$block)
#' new_mod <- update(mod, ~ . + block, data = sim1)
#' head(visualise_effects_data(data = sim1[, 3:6], prop = c("p1", "p2", "p3", "p4"),
#'                             var_interest = "p1", effect = "both",
#'                             model = new_mod,
#'                             add_var = list("block" = c(1, 2))))
#'
#' ## Create data for visualising effect of decreasing variable p2 from
#' ## the original communities in the data but using model coefficients
#' ## When specifying coefficients the data should have a one-to-one
#' ## positional mapping with specified coefficients.
#' init_comms <- sim1[, c("p1", "p2", "p3", "p4")]
#' head(visualise_effects_data(data = init_comms, prop = 1:4,
#'                             var_interest = "p2",
#'                             effect = "decrease",
#'                             interval = "none",
#'                             coefficients = mod$coefficients))
#'
#' ## Note that to get confidence interval when specifying
#' ## model coefficients we'd also need to provide a variance covariance
#' ## matrix using the `vcov` argument
#' head(visualise_effects_data(data = init_comms, prop = 1:4,
#'                             var_interest = "p2",
#'                             effect = "decrease",
#'                             interval = "confidence",
#'                             coefficients = mod$coefficients,
#'                             vcov = vcov(mod)))
#'
#' ## Can also create only the intermediary communities without predictions
#' ## by specifying prediction = FALSE.
#' ## Any additional columns can then be added and the `add_prediction` function
#' ## can be manually called.
#' ## Note: If calling the `add_prediction` function manually, the data would
#' ## not contain information about the marginal effect of changing the species
#' ## interest
#' effects_data <- visualise_effects_data(data = init_comms, prop = 1:4,
#'                                        var_interest = "p2",
#'                                        effect = "decrease",
#'                                        prediction = FALSE)
#' head(effects_data)
#' ## Prediction using model object
#' head(add_prediction(data = effects_data, model = mod, interval = "prediction"))
#' ## Prediction using regression coefficients
#' head(add_prediction(data = effects_data, coefficients = mod$coefficients))
visualise_effects_data <- function(data, prop, var_interest = NULL,
                                   effect = c("increase", "decrease", "both"),
                                   add_var = list(),
                                   prediction = TRUE, ...){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble indicating the communities
                     for which to calculate the effect of increasing/decreasing of
                     a variable in {.var data}."))
  }

  #Sanity Checks
  if(missing(prop)){
    cli::cli_abort(c("{.var prop} cannot be empty.",
                     "i" = "Specify the column-names of the columns
                           containing the variable proportions in {.var data}."))
  }

  sanity_checks(data = data, prop = prop,
                booleans = list("prediction" = prediction))

  # Convert to data.frame
  data <- as.data.frame(data)

  # Get names of columns containing species proportions
  species_names <- data %>% select(all_of(prop)) %>% colnames()

  # If var_interest is not specified assume all species are of interest
  if(is.null(var_interest)){
    cli::cli_bullets(c("*" = "{.var var_interest} was not specified. Assuming all variables are of interest."))
    var_interest <- species_names
  } else {
    if (!inherits(var_interest, "character")){
      cli::cli_abort(c("{.var var_interest} should be a character vector
                     containing the names of the variables to show
                     effects for.",
                     "i" = "{.var var_interest} was specified as a
                            {.cls {class(var_interest)}}."))
    }

    if(!all(var_interest %in% species_names)){
      cli::cli_abort(c("All values specified in {.var var_interest} should be present
                       in {.var prop}.",
                       "i" = "{.val {var_interest[! var_interest %in% species_names]}} {?is/are}
                            not present in {.var prop}."))
    }
  }

  # Ensure effect is only one of 'increase', "decrease" or "both"
  effect <- match.arg(effect)

  # Get all combinations of experimental structures
  # No plots can be created if user is looking for effect of species decrease but none of the data specified contain
  # the species of interest
  if(effect == "decrease"){
    if(all(data[, var_interest] == 0)){
      cli::cli_abort(c("Can't visualise effect of decrease of a variable if data doesn't contain the variable of interest.",
      "i" = "Specify data which contain some non-zero proportion of the variable{?s} of
           interest (i.e., {.val {var_interest}}) to see the effect of {? its/their} decrease."))
    }
  }

  # Similarly no plots can be created if the user is trying to find the effect of adding a species to it's monoculture
  if(effect == "increase"){
    if(length(var_interest) == 1 & all(data[, var_interest] == 1)){
      cli::cli_abort(c("Can't visualise effect of increase of a variable if only observations with 100% of
                       variable of interest are present.",
                       "i" = "Specify data with rows other than observations with 100% of the variable of interest,
                       i.e., {.val {var_interest}} should have values other than 1."))
    }
  }

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

  # Progress bar
  #pro_bar <- cli_progress_bar("Preparing data", total = 100, clear = FALSE)

  # Create data to create effects plot
  plot_data <- lapply(cli_progress_along(var_interest, name = "Preparing data"), function(idx){
    x <- var_interest[idx]
    if(effect == "increase"){
      pothers <- data %>% filter(!(!!sym(x) %in% c(1)))
      # Not needed as error is throw above
      # if(nrow(pothers) < 1){
      #   cli::cli_text(c("The effect of increasing variable {.var {x}} can not be calculated as the given data only contains observations where {.var {x}} is 1 (i.e., only variable in the composition so it can't be increased)."))
      #   return(NULL)
      # }
    } else if(effect == "decrease") {
      # Not needed as error is throw above
      pothers <- data %>% filter(!(!!sym(x) %in% c(0)))
      # if(nrow(pothers) < 1){
      #   cli::cli_text(c("The effect of decreasing variable {.var {x}} can not be calculated as the given data only contains observations where {.var {x}} is 0 (i.e., not present so can't be decreased)."))
      #   return(NULL)
      # }
    } else {
      pothers <- data
    }
    pother_names <- colnames(pothers[, species_names[species_names!=x]])
    nOthers <- length(pother_names)
    # Filter any duplicate data
    pothers <- unique(pothers)

    # For each row in data create copies with species of interest having one value from pvals (0.01, 0.02, 0.03, ..., 0.98, 0.99, 1)
    data_expand <- lapply(seq_len(nrow(pothers)), function(i){
      # Intervals for calculating the effects of species decrease
      if(effect == "increase"){
        pvals <- seq(pothers[i, x]*100, 100, length.out = 101)/100
      } else if(effect == "decrease") {
        pvals <- seq(0, pothers[i, x]*100, length.out = 101)/100
      } else {
        if(pothers[i, x] %in% c(0, 1)){
          pvals <- seq(0, 100, length.out = 101)/100
        } else {
          pvals <- c(seq(0, pothers[i, x]*100, length.out = 51)/100,
                     seq(pothers[i, x]*100, 100, length.out = 51)[2:51]/100)
        }
      }

      pothers_expand <- pothers %>%
        slice(i) %>%
        slice(rep(1, each = length(pvals))) %>%
        mutate(!!sym(x) := pvals,
               ".Sp" = x,
               ".Proportion" = pvals,
               ".Group" = i) %>%
        select(all_of(species_names), everything())

      # Rescale the proportions of the other species accordingly to ensure the proportions sum to 1
      denom <- rowSums(pothers_expand[, pother_names])
      # Can't divide by 0, so if it's zero, all remaining species take up equal proportions
      if(all(denom == 0)){
        rescaled <- matrix(rep((1 - pvals)/nOthers, times = nOthers),
                           ncol = nOthers)
      } else {
        rescaled <- pothers_expand[, pother_names]*(1 - pvals)/denom
      }
      pothers_expand[, pother_names] <- rescaled

      # If any of the proportions are NaN make them 0
      # pothers_expand[, pother_names] <- apply(pothers_expand[, pother_names], 2, function(x){
      #   x[which(is.nan(x))] <- 0
      # })

      # Need to return data via subset of rows as bind_rows fails otherwise
      pothers_expand[1:nrow(pothers_expand),]

    }) %>% bind_rows()
    data_expand
  }) %>% bind_rows()


  #cli_progress_done(id = pro_bar)

  # Make prediction and get marginal effect
  if(prediction){
    dots <- list(...)
    dots$data <- plot_data
    dots$interval <- if (is.null(dots$interval)) "conf" else dots$interval
    plot_data <- do.call(add_prediction, as.list(dots))

    # Calculate the marginal effect of adding a species for producing the marginal plots
    plot_data <- plot_data %>% group_by(.data$.Sp, .data$.Group) %>%
      mutate(.dy = c((diff(.data$.Pred)/diff(.data$.Proportion)), 1)) %>%
      mutate('.Marginal' = c(.data$.dy[1:(length(.data$.dy) - 1)], .data$.dy[(length(.data$.dy) - 1)]),
             '.Threshold' = .data$.Proportion[abs(.data$.Marginal) == min(abs(.data$.Marginal))][1],
             '.MarEffect' = ifelse(!!sym(".Proportion") < .data$.Threshold, 'Negative', 'Positive')) %>%
      select(-all_of(c(".dy"))) %>%
      ungroup() %>%
      as.data.frame()

    # dy <- diff(plot_dat$.Pred)/diff(pvals)
    # pothers_expand <- pothers_expand %>%
    #   mutate('.Marginal' = c(dy, dy[length(dy)]),
    #          '.Threshold' = pvals[abs(.data$.Marginal) == min(abs(.data$.Marginal))][1],
    #          '.MarEffect' = ifelse(!!sym(x) < .data$.Threshold, 'Negative', 'Positive'))
  }

  # Add flag to identify whether we are looking a effect of increase or effect of decrease
  plot_data <- plot_data %>%
    mutate(.Effect = effect,
           .Sp = fct_inorder(.data$.Sp))

  # Add attribute to identify prop cols
  attr(plot_data, "prop") <- prop

  cli::cli_alert_success("Finished data preparation.")
  return(plot_data)
}

#' @title Effects plot for compositional predictors
#'
#' @description
#' The plotting function to create plots showing the effect of increasing or
#' decreasing the proportion of a variable from a set of compositional variables.
#' The output of the `\code{\link{visualise_effects_data}}` function (with any
#' desired modifications) should be passed here. The generated plot will show a
#' curve for each observation (whenever possible) in the data.
#' `\code{\link[PieGlyph:PieGlyph-package]{Pie-glyphs}}` are
#' used to highlight the compositions of the specified communities and the ending
#' community after the variable of interest either completes dominates the community
#' (when looking at the effect of increase) or completely vanishes from the community
#' (when looking at the effect of decrease) or both.
#'
#' @param data A data frame created using the \code{\link{visualise_effects_data}} function.
#' @param prop A vector of column names or indices identifying the columns containing the
#'             compositional variables in the data. Will be inferred from the data if
#'             it is created using the `\code{\link{visualise_effects_data}}`
#'             function, but the user also has the flexibility of manually
#'             specifying the values.
#' @param pie_colours A character vector indicating the colours for the slices
#'                    in the pie-glyphs. \cr
#'                    If left NULL, the colour blind friendly colours will be
#'                    for the pie-glyph slices.
#' @param pie_radius A numeric value specifying the radius (in cm) for the
#'                   pie-glyphs. Default is 0.3 cm.
#' @param se A boolean variable indicating whether to plot confidence intervals associated with
#'           the effect of species increase or decrease
#' @param average A boolean value indicating whether to add a line describing the "average"
#'                effect of variable increase or decrease. The average is calculated at the
#'                median value of any variables not specified.
#' @inheritParams prediction_contributions
#'
#' @inherit prediction_contributions return
#'
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' ## Load data
#' data(sim1)
#'
#' ## Fit model
#' mod <- glm(response ~ p1 + p2 + p3 + p4 + 0, data = sim1)
#'
#' ## Create data for visualising effect of adding species 1 to
#' ## the original communities in the data
#' plot_data <- visualise_effects_data(data = sim1[sim1$block == 1, ],
#'                                     prop = c("p1", "p2", "p3", "p4"),
#'                                     var_interest = "p1",
#'                                     effect = "increase", model = mod)
#'
#' ## Create plot
#' visualise_effects_plot(data = plot_data)
#'
#' ## Show specific curves with prediction intervals
#' subset <- custom_filter(plot_data, .Group %in% c(7, 15))
#' visualise_effects_plot(data = subset, prop = 1:4, se = TRUE)
#'
#' ## Do not show average effect line
#' visualise_effects_plot(data = subset,
#'                        se = TRUE, average = FALSE)
#'
#' ## Change colours of the pie-glyph slices
#' visualise_effects_plot(data = subset,
#'                        pie_colours = c("darkolivegreen", "darkolivegreen1",
#'                                    "steelblue4", "steelblue1"))
#'
#' #' ## Simultaneously create multiple plots for additional variables
#' sim1$block <- as.numeric(sim1$block)
#' new_mod <- update(mod, ~ . + block, data = sim1)
#' plot_data <- visualise_effects_data(data = sim1[c(1, 5, 9, 13), 3:6],
#'                                     prop = c("p1", "p2", "p3", "p4"),
#'                                     var_interest = "p3",
#'                                     model = new_mod, conf.level = 0.95,
#'                                     add_var = list("block" = c(1, 2)))
#'
#' visualise_effects_plot(data = plot_data,
#'                        average = FALSE,
#'                        pie_colours = c("darkolivegreen", "darkolivegreen1",
#'                                        "steelblue4", "steelblue1"))
visualise_effects_plot <- function(data, prop, pie_colours = NULL,
                                   pie_radius = 0.3,
                                   se = FALSE, average = TRUE,
                                   nrow = 0, ncol = 0){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble (preferably the
                            output of {.help [{.fn {col_green(\"visualise_effects_data\")}}](DImodelsVis::visualise_effects_data)})."))
  }

  # Ensure identifiers for columns in data giving species proportions are specified
  if(missing(prop)){
    # Read from data if prop is missing
    prop <- attr(data, "prop")

    if(is.null(prop)){
      cli::cli_abort(c("{.var prop} is {.pkg NULL} and cannot be inferred from data.",
                       "i" = "Specify a character vector giving
                     names of the columns containing the
                     compositional variables in {.var data}."))
    }
  }

  sanity_checks(data = data, prop = prop,
                colours = pie_colours,
                booleans = list("se" = se, "average" = average),
                numerics = list("nrow" = nrow, "ncol" = ncol),
                unit_lengths = list("nrow" = nrow, "ncol" = ncol,
                                    "pie_radius" = pie_radius))

  if(check_col_exists(data, ".add_str_ID")){
    ids <- unique(data$.add_str_ID)
    lwr_lim <- ifelse(check_col_exists(data, ".Lower"), min(data$.Lower), min(data$.Pred))
    upr_lim <- ifelse(check_col_exists(data, ".Upper"), max(data$.Upper), max(data$.Pred))
    plots <- lapply(cli_progress_along(1:length(ids), name = "Creating plot",
                                       format = paste0(
                                         "{cli::pb_spin} Creating plot ",
                                         "[{cli::pb_current}/{cli::pb_total}]   ETA:{cli::pb_eta}"
                                       )),
                    function(i){
                      data_iter <- data %>% filter(.data$.add_str_ID == ids[i])
                      visualise_effects_plot_internal(data = data_iter,
                                                      prop = prop,
                                                      pie_radius = pie_radius,
                                                      pie_colours = pie_colours,
                                                      se = se,
                                                      average = average)+
                        labs(subtitle = ids[i]) +
                        ylim(lwr_lim, upr_lim)
                    })
    if(length(plots) > 1){
      plot <- new("ggmultiplot", plots = plots, nrow = nrow, ncol = ncol)
    } else {
      plot <- plots[[1]]
    }
    cli::cli_alert_success("Created all plots.")
  } else {
    plot <- visualise_effects_plot_internal(data = data, prop = prop,
                                            pie_radius = pie_radius,
                                            pie_colours = pie_colours,
                                            se = se,
                                            average = average)
    cli::cli_alert_success("Created plot.")
  }
  return(plot)
}

#' @title DI specific wrapper of effects plot for compositional variables
#'
#' @description
#' This function will prepare the underlying data and plot the results for visualising
#' the effect of increasing or decreasing the proportion of a predictor variable
#' (from a set of compositional variables). The generated plot will
#' show a curve for each observation (whenever possible) in the data.
#' \code{\link[PieGlyph:PieGlyph-package]{Pie-glyphs}} are
#' used to highlight the compositions of the specified communities and the ending
#' community after the variable interest either completes dominates the community
#' (when looking at the effect of increase) or completely vanishes from the community
#' (when looking at the effect of decrease) or both.
#' This is a wrapper function specifically for statistical models fit using the
#' \code{\link[DImodels:DI]{DI()}} function from the
#' \code{\link[DImodels:DImodels-package]{DImodels}} R package and would implicitly
#' call \code{\link{visualise_effects_data}} followed by
#' \code{\link{visualise_effects_plot}}. If your model object isn't fit using
#' DImodels, users can call the data and plot functions manually, one by one.
#'
#' @importFrom ggplot2 geom_line labs scale_colour_manual geom_ribbon labeller
#'                     stat_summary scale_x_reverse facet_wrap geom_smooth
#' @importFrom dplyr %>% select filter all_of everything slice mutate bind_rows summarise
#' @importFrom PieGlyph geom_pie_glyph
#' @importFrom rlang :=
#'
#' @param model A Diversity Interactions model object fit by using the
#'              `\code{\link[DImodels:DI]{DI()}}` function from the
#'              `\code{\link[DImodels:DImodels-package]{DImodels}}` package.
#' @param data A dataframe specifying communities of interest for which user
#'             wants visualise the effect of species decrease or increase.
#'             If left blank, the communities from the original data used
#'             to fit the model would be selected.
#' @param interval Type of interval to calculate:
#'  \describe{
#'    \item{"none"}{No interval to be calculated.}
#'    \item{"confidence" (default)}{Calculate a confidence interval.}
#'    \item{"prediction"}{Calculate a prediction interval.}
#'  }
#' @param conf.level The confidence level for calculating confidence/prediction
#'                   intervals. Default is 0.95.
#' @inheritParams visualise_effects_data
#' @inheritParams visualise_effects_plot
#' @inheritParams prediction_contributions
#'
#' @inherit prediction_contributions return
#'
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' ## Load data
#' data(sim1)
#'
#' ## Fit model
#' mod <- DI(prop = 3:6, DImodel = "AV", data = sim1, y = "response")
#'
#' ## Get effects plot for all species in design
#' visualise_effects(model = mod)
#'
#' ## Choose a variable of interest using `var_interest`
#' visualise_effects(model = mod, var_interest = c("p1", "p3"))
#'
#' ## Add custom communities to plot instead of design communities
#' ## Any variable not specified will be assumed to be 0
#' ## Not showing the average curve using `average = FALSE`
#' visualise_effects(model = mod, average = FALSE,
#'                   data = data.frame("p1" = c(0.7, 0.1),
#'                                     "p2" = c(0.3, 0.5),
#'                                     "p3" = c(0,   0.4)),
#'                   var_interest = c("p2", "p3"))
#'
#' ## Add uncertainty on plot
#' visualise_effects(model = mod, average = TRUE,
#'                   data = data.frame("p1" = c(0.7, 0.1),
#'                                     "p2" = c(0.3, 0.5),
#'                                     "p3" = c(0,   0.4)),
#'                   var_interest = c("p2", "p3"), se = TRUE)
#'
#' ## Visualise effect of species decrease for particular species
#' ## Show a 99% confidence interval using `conf.level`
#' visualise_effects(model = mod, effect = "decrease",
#'                   average = TRUE, se = TRUE, conf.level = 0.99,
#'                   data = data.frame("p1" = c(0.7, 0.1),
#'                                     "p2" = c(0.3, 0.5),
#'                                     "p3" = c(0,   0.4),
#'                                     "p4" = 0),
#'                   var_interest = c("p1", "p3"))
#'
#' ## Show effects of both increase and decrease using `effect = "both"`
#' ## and change colours of pie-glyphs using `pie_colours`
#' visualise_effects(model = mod, effect = "both",
#'                   average = FALSE,
#'                   pie_colours = c("steelblue1", "steelblue4", "orange1", "orange4"),
#'                   data = data.frame("p1" = c(0.7, 0.1),
#'                                     "p2" = c(0.3, 0.5),
#'                                     "p3" = c(0,   0.4),
#'                                     "p4" = 0),
#'                   var_interest = c("p1", "p3"))
#'
#' # Add additional variables and create a separate plot for each
#' \donttest{
#' visualise_effects(model = mod, effect = "both",
#'                   average = FALSE,
#'                   pie_colours = c("steelblue1", "steelblue4", "orange1", "orange4"),
#'                   data = data.frame("p1" = c(0.7, 0.1),
#'                                     "p2" = c(0.3, 0.5),
#'                                     "p3" = c(0,   0.4),
#'                                     "p4" = 0),
#'                   var_interest = c("p1", "p3"),
#'                   add_var = list("block" = factor(c(1, 2),
#'                                                   levels = c(1, 2, 3, 4))))
#' }
#'
#' ## Specify `plot = FALSE` to not create the plot but return the prepared data
#' head(visualise_effects(model = mod, effect = "both",
#'                        average = FALSE, plot = FALSE,
#'                        pie_colours = c("steelblue1", "steelblue4",
#'                                        "orange1", "orange4"),
#'                        data = data.frame("p1" = c(0.7, 0.1),
#'                                          "p2" = c(0.3, 0.5),
#'                                          "p3" = c(0,   0.4),
#'                                          "p4" = 0),
#'                        var_interest = c("p1", "p3")))
visualise_effects <- function(model, data = NULL, var_interest = NULL,
                              effect = c("increase", "decrease", "both"),
                              add_var = list(),
                              interval = c("confidence", "prediction", "none"),
                              conf.level = 0.95,
                              se = FALSE, average = TRUE,
                              pie_colours = NULL,
                              pie_radius = 0.3,
                              FG = NULL, plot = TRUE,
                              nrow = 0, ncol = 0){

  # Sanity checks
  # Ensure model is a DImodels object
  # Ensure specified model is fit using the DI function
  if(missing(model) || (!inherits(model, "DI") && !inherits(model, "DImulti"))){
    model_not_DI(call_fn = "visualise_effects")
  }

  # Get original data used to fit the model
  original_data <- model$original_data

  # Get all species in the model
  model_species <- attr(model, "prop")

  # If the user has not specified neither of communities, equi or model_data.
  # Then default to plotting the design communities
  if(is.null(data)){
    design_points <- nrow(unique(original_data[, model_species]))
    # By default the model will plot all data in the original data.
    # However this might not be feasible when there are a lot of data points in the
    # design. If that's the case then plot only the first 100 rows
    if(design_points <= 100){
      data <- unique(original_data[, model_species])
    } else {
      data <- unique(original_data[, model_species])[1:100, ]
    }
  }

  # Create empty dataframe to append data to plot
  plot_data <- data.frame()

  # If user has manually specified the data then add those to data to be plotted
  if(!is.null(data)){
    # Ensure data are specified as a data-frame and in proper format
    if(!inherits(data, "data.frame")){
      cli::cli_abort(c("{.var data} was not a data.frame",
      "i"="Specify data to show effects for in the plot as a data frame
           with proportions of the different species"))
    } else {

      sp_abs <- !model_species %in% colnames(data)
      # If the column names of the data dataframe not match the species present in the
      # model then notify user asking them to change the column names
      if(all(sp_abs)){
        cli::cli_abort(c("The column names of the data frame should be same as
                         the names of the variables specified when fitting the model.",
                         "i" = "Update the column names in {.var data} to ensure they match
                               {.val {model_species}}"))
      }
      # If any species present in the model is not specified in the data, then
      # notify user and assume its proportion to be 0
      if(any(sp_abs)){
        cli::cli_bullets(c("*" = "The variable{?s} {.val {model_species[sp_abs]}} {?was/were} not
                       present in the data specified, assuming their proportions to be 0."))
      }
      data[, model_species[sp_abs]] <- 0

      plot_data <- rbind(plot_data, data)
    }
  }

  # Ensure effect is only one of 'increase' or 'decrease'
  effect <- match.arg(effect)
  interval <- match.arg(interval)

  # If model object is of type DImulti add info about EFs and timepoints
  if(inherits(model, "DImulti")) {
    add_var <- link_DImodelsMulti(model = model, add_var = add_var)
  }

  plot_data <- visualise_effects_data(model = model, data = plot_data,
                                      prop = model_species, effect = effect,
                                      var_interest = var_interest,
                                      add_var = add_var,
                                      interval = interval,
                                      conf.level = conf.level)

  # Get functional groups
  if(is.null(FG)){
      FG <- attr(model, "FG")
  }

  # Colours for species
  if(is.null(pie_colours)){
    pie_colours <- get_colours(vars = model_species, FG = FG)
  }

  if(isTRUE(plot)){
    plot <- visualise_effects_plot(data = plot_data,
                                   prop = model_species,
                                   pie_colours = pie_colours,
                                   pie_radius = pie_radius,
                                   se = se,
                                   average = average,
                                   nrow = nrow, ncol = ncol)
    return(plot)
  } else {
    return(plot_data)
  }
}

#' @keywords internal
#' Internal function for creating effects plot
#'
#' @usage NULL
NULL
visualise_effects_plot_internal <- function(data, prop, pie_colours = NULL,
                                            pie_radius = 0.3,
                                            se = FALSE, average = TRUE){

  # Check all columns necessary for plotting are present
  check_plot_data(data = data,
                  cols_to_check = c(".Sp", ".Effect", ".Group",
                                    ".Pred", ".Proportion"),
                  calling_fun = "visualise_effects")

  # Get names of columns containing species proportions
  species_names <- data %>% select(all_of(prop)) %>% colnames()

  # Get data for showing the pie-chart glyphs
  if(unique(data$.Effect) == "both"){
    pie_ids <- c(1, 51, 101)
  } else {
    pie_ids <- c(1, 101)
  }

  pie_data <- data %>%
    group_by(.data$.Sp, .data$.Group) %>%
    slice(pie_ids) %>%
    ungroup() %>%
    group_by(.data$.Sp) %>%
    # Filter out any overlapping pies to avoid overplotting
    distinct(!!! rlang::syms(species_names), .data$.Pred, .keep_all = T) %>%
    ungroup()


  # Colours for the pie-glyph slices
  if(is.null(pie_colours)){
    pie_colours <- get_colours(species_names)
  }

  # Create canvas for plot
  plot <- ggplot(data, aes(x = .data$.Proportion, y= .data$.Pred))+
    theme_DI()

  # Add ribbons for uncertainty of prediction
  if(se){
    # Check all columns necessary for plotting are present
    check_plot_data(data = data,
                    cols_to_check = c(".Lower", ".Upper"),
                    calling_fun = "visualise_effects")
    plot <- plot +
      geom_ribbon(aes(ymin = .data$.Lower, ymax = .data$.Upper,
                      group = .data$.Group),
                  colour = 'grey', alpha = 0.25)
  }

  # Add line tracing the effect of adding a particular species to the data
  plot <- plot +
    geom_line(aes(group = .data$.Group), colour = 'black', alpha = 0.4)

  # If user specified to chose to see average effect of adding species then add that to plot
  if(average){
    # Round species proportion to 2 decimal places and calculate average effect.
    # Rounding done because for species decrease because some proportions have recurring decimals
    # avg_data <- data %>%
    #   mutate(".Proportion" = round(.data$.Proportion, 2)) %>%
    #   group_by(.data$.Sp, .data$.Group, .data$.Proportion) %>%
    #   distinct(.data$.Proportion, .keep_all = TRUE) %>%
    #   ungroup() %>%
    #   group_by(.data$.Sp, .data$.Proportion) %>%
    #   summarise(.Avg = round(mean(.data$.Pred), 2)) %>%
    #   ungroup()

    # Add line showing average effect
    plot <- plot +
      geom_smooth(colour = "black", linewidth = 1.25,
                  aes(group = 1), method = "loess",
                  formula = y ~ x, se = FALSE)
    }

  # Add the pie-chart glyphs for identifying the data
  plot <- plot +
    geom_pie_glyph(aes(group = .data$.Sp), data = pie_data, radius = pie_radius,
                   slices = prop, colour = 'black')+
    scale_fill_manual(values = pie_colours,
                      labels = prop)

  # If looking at species decrease then reverse the x-axis
  # if(data$.Effect[1] == 'decrease'){
  #   plot <- plot + scale_x_reverse()
  # }

  # Finally facet plot by each species of interest to ensure better visibility
  labs <- paste("Variable:", unique(data$.Sp))
  names(labs) <- unique(data$.Sp)
  plot <- plot +
    facet_wrap(~.data$.Sp, labeller = ggplot2::labeller(.Sp = labs))

  # Adjust plot aesthetics
  plot <- plot +
    labs(fill = "Variable",
         x = "Proportion",
         y = "Predicted Response")+
    theme(legend.position = 'top')

  return(plot)
}



