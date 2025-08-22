#' @title Model term contributions to predicted response
#'
#' @description
#' The helper function for preparing the data to split the predicted response
#' from a regression model into contributions (predictor coefficient * predictor value)
#' by the terms in the model. The output of this function can be passed to the
#' `\link{prediction_contributions_plot}` function to visualise the results.
#'
#' @importFrom stats coef formula model.matrix
#' @importFrom dplyr bind_cols ends_with row_number pull
#' @importFrom rlang := abort try_fetch
#'
#' @inheritParams prediction_contributions
#' @inheritParams add_prediction
#'
#' @return A data-frame with the following columns. Any additional columns which
#' weren't used when fitting the model would also be present.
#'  \describe{
#'    \item{.Community}{An identifier column to discern each
#'                      observation in the data. These are the labels which
#'                      will be displayed for the bars in the plot.}
#'    \item{.add_str_ID}{An identifier column for grouping the cartesian product
#'                       of all additional columns specified in `add_var`
#'                       parameter (if `add_var` is specified).}
#'    \item{.Pred}{The predicted repsonse for each observation.}
#'    \item{.Lower}{The lower limit of the prediction interval for
#'                  each observation.}
#'    \item{.Upper}{The lower limit of the prediction interval for
#'                  each observation.}
#'    \item{.Contributions}{An identifier describing the name of the
#'                          coefficient contributing to the response.}
#'    \item{.Value}{The contributed value of the respective coefficient/group
#'                  to the total prediction.}
#'  }
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
#' prediction_contributions_data(data = sim2[c(1,5,9,11), ],
#'                               model = mod)
#'
#' ## Specific coefficients can also be grouped together
#' ## Either by their indices in the model coefficient vector
#' prediction_contributions_data(data = sim2[c(1,5,9,11), ],
#'                               model = mod,
#'                               groups = list("Interactions" = 5:10))
#' ## Or by specifying the coefficient names as character strings
#' prediction_contributions_data(data = sim2[c(1,5,9,11), ],
#'                               model = mod,
#'                               groups = list("p1_Ints" = c("p1:p2",
#'                                                           "p1:p3",
#'                                                           "p1:p4")))
#'
#' ## Additional variables can also be added to the data by either specifying
#' ## them directly in the `data` or by using the `add_var` argument
#' ## Refit model
#' sim2$block <- as.numeric(sim2$block)
#' new_mod <- update(mod, ~. + block, data = sim2)
#' ## This model has block so we can either specify block in the data
#' subset_data <- sim2[c(1,5,9,11), 2:6]
#' subset_data
#' head(prediction_contributions_data(data = subset_data,
#'                                    model = new_mod))
#' ## Or we could add the variable using `add_var`
#' subset_data <- sim2[c(1,5,9,11), 3:6]
#' subset_data
#' head(prediction_contributions_data(data = subset_data,
#'                                    model = new_mod,
#'                                    add_var = list(block = c(1, 2))))
#' ## The benefit of specifying the variable this way is we have an ID
#' ## columns now called `.add_str_ID` which would be used to create a
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
#' prediction_contributions_data(data = coef_data,
#'                               coefficients = mod$coefficients,
#'                               interval = "none")
#' ## To get uncertainity using coefficients vcov matrix would have to specified
#' prediction_contributions_data(data = coef_data,
#'                               coefficients = mod$coefficients,
#'                               vcov = vcov(mod))
#'
#' ## Specifying `bar_labs`
#' ## Our data has four rows so we'd need four labels in bar_labs
#' prediction_contributions_data(data = coef_data,
#'                               coefficients = mod$coefficients,
#'                               vcov = vcov(mod),
#'                               bar_labs = c("p1 Domm", "p2 Domm",
#'                                            "p3 Domm", "p4 Domm"))
prediction_contributions_data <- function(data, model = NULL, coefficients = NULL,
                                          coeff_cols = NULL, vcov = NULL,
                                          add_var = list(), groups = list(), conf.level = 0.95,
                                          interval = c("confidence", "prediction", "none"),
                                          bar_labs = rownames(data)){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble containing the model
                     terms needed to calculate their contributions to the predicted
                     response."))
  }

  # Allow user to specify labels for the bars
  if(is.null(bar_labs)){
    cli_bullets(c("!" = "No values were specified for the bar labels and the data
                  didn't have any rownames.",
                  ">" = "The bars are given numeric identifiers in the order they
                  appear in the data.",
                  ">" = "If this is not desirable consider providing labels for the
                bars using the {.var bar_labs} parameter."))
    bar_labs <- paste0("Obs", 1:nrow(data))
  } else {
    if (length(bar_labs) != 1 && (length(bar_labs) != nrow(data))){
      cli_abort(c("{.var bar_labs} should either be a character string/numeric index
                  identifying the column in the data containing labels for the bars or
                  be a character vector containing the labels for the bars with the
                  same length as the number of rows in the data.",
                  "i" = "The data has {nrow(data)} rows while {length(bar_labs)} labels were
                         specified in {.var bar_labs}"))
    }

    # If bar labs is specified as a column then fetch values from data
    if(length(bar_labs) == 1){
      bar_labs <- rlang::try_fetch(dplyr::pull(data, bar_labs),
                                   error = function(cnd)
                                   rlang::abort("The value specified in `bar_labs` is invalid.",
                                      parent = cnd))
    } else {
      # Leave things the way they are
      bar_labs <- bar_labs
    }
  }

  # Add bar labs
  data <- data %>%
    mutate(".Community" = paste0("Community ", seq_along(bar_labs)),
           ".x_labs" = bar_labs)

  # Add any experimental structures specified by user
  # Ensure experimental structure are specified correctly
  add_var <- check_add_var(model = model, add_var = add_var)

  if(length(add_var) > 0){
    data <- add_add_var(data = data,
                        add_var = add_var)
  }

  interval <- match.arg(interval)

  # Add predictions from model/coefficients to check things are fine
  # First temporarily remove the columns we added
  pred_data <- data %>% select(-all_of(c(".Community", ".x_labs")))
  if(check_col_exists(pred_data, ".add_str_ID")){
    pred_data <- data %>% select(-all_of(c(".add_str_ID")))
  }
  pred_data <- add_prediction(data = pred_data, model = model,
                              coeff_cols = coeff_cols,
                              coefficients = coefficients,
                              vcov = vcov,
                              interval = interval,
                              conf.level = conf.level) %>%
    # Add them back
    mutate(".Community" = data$.Community,
           ".x_labs" = data$.x_labs)
  if(check_col_exists(data, ".add_str_ID")){
    pred_data <- pred_data %>%
      mutate(.add_str_ID = data$.add_str_ID)
  }

  # browser()
  # Branch here if model is specified
  if(!is.null(model)){
    form <- formula(model)
    form[2] <- NULL
    if(!(inherits(model, "DI") && (attr(model, "DImodel") == "ADD"))){
      X_matrix <- model.matrix(form, data)
      coefficients <- coef(model)
    }
  }
  # Branch here if regression coefficients are specified
  else if(!is.null(coefficients)){

    coeff_data <- data %>% select(-all_of(c(".Community", ".x_labs")))
    if(check_col_exists(coeff_data, ".add_str_ID")){
      coeff_data <- coeff_data %>% select(-all_of(c(".add_str_ID")))
    }

    # If coefficients are named then order data columns accordingly
    if(!is.null(names(coefficients)) & is.null(coeff_cols)){
      # Same check runs in add_prediction as well, so if won't be needed
      # if(!all(names(coefficients) %in% colnames(coeff_data))){
      #   cli::cli_abort(c("All coefficient names should be present in the data as columns.",
      #                    "i" = "{.var {names(coefficients)[!names(coefficients) %in% colnames(data)]}}
      #                    {?was/were} not present in the data."))
      # }
      # Create X matrix
      X_matrix <- as.matrix(sapply(coeff_data[, names(coefficients)], as.numeric))
    }

    # if(is.null(coeff_cols)){
    #   X_matrix <- as.matrix(sapply(coeff_data[, colnames(coeff_data)], as.numeric))
  else if(!is.null(coeff_cols)){
    # This check too runs in add_prediction
    # if(length(coefficients) != length(coeff_cols)){
    #   cli::cli_abort(c("The number of values specified for selecting and reordering
    #                      data columns in {.var coeff_cols} should be same as the
    #                      number of coefficients specified in the {.var coefficients}
    #                      vector.",
    #                    "i" = "The were {length(coefficients)} coefficients
    #                      while {.var coeff_cols} specified {length(coeff_cols)}
    #                      column{?s} to select."))
    # }
      X_cols <- coeff_data %>% select(all_of(coeff_cols))
      # Created X_matrix
      X_matrix <- as.matrix(sapply(X_cols, as.numeric))
  }
  # If neither coefficients are named nor a selection provided then
  # assume the user has specified everything correctly and calculate predictions
  else {
      # Same for this one as well
      # if(ncol(coeff_data)!=length(coefficients)){
      #   cli::cli_abort(c("If coeficients are not named, then the number of columns in
      #                    {.var data} should be the same as the number of coefficients.",
      #                    "i" = "The were {length(coefficients)} coefficients
      #                    while data had {ncol(data)} columns.",
      #                    "i" = "Consider giving names to the coefficient vector
      #                    specified in {.var coefficients} corresponding to the
      #                    respective data columns or providing a selection of
      #                    columns in {.var coeff_cols} corresponding (in
      #                    sequential order) to each coefficient."))
      # }
      # Create X_matrix
      X_matrix <- as.matrix(sapply(coeff_data, as.numeric))
    }
  }

  # If model has theta then drop it from coefficients
  if(!is.null(attr(model, "theta_flag")) && isTRUE(attr(model, "theta_flag"))){
    coefficients <- coefficients[names(coefficients) != "theta"]
  }

  # Express the prediction into contribution from each coefficient in the model
  if (!is.null(attr(model, "DImodel")) && attr(model, "DImodel") == "FG"){
    nonFG_flag <- FALSE
  } else {
    nonFG_flag <- TRUE
  }
  if(!is.null(model) && inherits(model, "lm") && (nonFG_flag)) {
    contr_data <- as.data.frame(suppressWarnings(predict(model, newdata = data, type = "terms")))
  } else {
    contr_data <- as.data.frame(sweep(X_matrix, MARGIN = 2, coefficients, `*`))
  }

  # Add identifier for each row
  contr_data$.Community <- data$.Community
  contr_data$.x_labs <- data$.x_labs

  # Check coefficients can be properly grouped if groups are specified
  check_coeff_groupings(coefficients, groups)

  # Group contributions
  grouped_data <- data.frame(".Community" = contr_data$.Community)
  for(group in names(groups)){
    vars <- groups[[group]]
    grouped_data <- grouped_data %>%
      mutate(!!group := rowSums(contr_data %>% select(all_of(vars)), na.rm = TRUE))
  }
  # Add any variables left over after grouping the contributions
  grouped_data <- bind_cols(contr_data %>%
                              select(-".Community") %>%
                              select(-all_of(unlist(groups))),
                            grouped_data)

  # Name of the contributions prediction is split into
  contributions <- grouped_data %>%
    select(-all_of(c(".Community", ".x_labs"))) %>%
    colnames()

  # If the data has an identifier for exp str then add that in and
  # reset observation numbers
  if(check_col_exists(data ,".add_str_ID")){
    grouped_data$.add_str_ID <- data$.add_str_ID
    # grouped_data <- grouped_data %>%
    #                   group_by(.data$.add_str_ID) %>%
    #                   mutate(".Community" = paste0("Obs", row_number())) %>%
    #                   ungroup()
  }

  # Add all other columns
  miss <- setdiff(colnames(pred_data), colnames(grouped_data))
  # browser()
  if(check_col_exists(pred_data, ".add_str_ID")){
    grouped_data <- grouped_data %>%
      left_join(pred_data %>% select(all_of(c(".Community", ".add_str_ID", miss))),
                by = c(".Community", ".add_str_ID")) %>%
      mutate(.Community = fct_inorder(.data[[".Community"]])) %>%
      # Converting to long format so it's easier to plot
      pivot_longer(cols = all_of(contributions),
                   names_to = '.Contributions', values_to = '.Value')
  } else {
    grouped_data <- grouped_data %>%
      left_join(pred_data %>% select(all_of(c(".Community", miss))),
                by = ".Community") %>%
      mutate(.Community = fct_inorder(.data[[".Community"]])) %>%
      # Converting to long format so it's easier to plot
      pivot_longer(cols = all_of(contributions),
                   names_to = '.Contributions', values_to = '.Value')
  }

  cli::cli_alert_success("Finished data preparation.")
  return(grouped_data)
}

#' @title Visualise model term contributions to predicted response
#'
#' @description
#' The plotting function to visualise the predicted response from a
#' regression model as a stacked bar-chart showing the contributions
#' (predictor coefficient * predictor value) of each model term to the total
#' predicted value (represented by the total height of the bar).
#' Requires the output of the `\link{prediction_contributions_data}` as an
#' input in the `data` parameter.
#'
#' @param data A data-frame which is the output of the
#'             `\link{prediction_contributions_data}` function, consisting of
#'             the predicted response split into the contributions by the
#'             different coefficients.
#' @param colours A character vector specifying the colours for the
#'                contributions of the different coefficients.
#'                If not specified, a default colour-scheme would be chosen,
#'                however it could be uninformative and it is recommended to
#'                manually choose contrasting colours for each coefficient
#'                group to render a more informative plot.
#' @inheritParams prediction_contributions
#'
#' @inherit prediction_contributions return
#' @export
#'
#' @examples
#' library(DImodels)
#' library(dplyr)
#'
#' ## Load data
#' data(sim2)
#' sim2$AV <- DI_data_E_AV(data = sim2, prop = 3:6)$AV
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + AV), data = sim2)
#'
#' ## Create data for plotting
#' plot_data <- prediction_contributions_data(data = sim2[c(1,5,9,11,15,19,23), ],
#'                                            model = mod)
#' ## Create plot
#' prediction_contributions_plot(data = plot_data)
#'
#' ## Choose distinct colours for groups of coefficients for better visibility
#' ID_cols <- get_colours(4, FG = c("G", "G", "H", "H"))
#' int_cols <- "#808080"
#' cols <- c(ID_cols, int_cols)
#' ## Specify colours using `cols`
#' prediction_contributions_plot(data = plot_data, colours = cols)
#'
#' ## Show prediction intervals
#' prediction_contributions_plot(data = plot_data, colours = cols, se = TRUE)
#'
#' ## Change orientation of bars using `bar_orientation`
#' prediction_contributions_plot(data = plot_data, colours = cols,
#'                               se = TRUE, bar_orientation = "horizontal")
#'
#' ## Facet plot based on a variable in the data
#' prediction_contributions_plot(data = plot_data, colours = cols,
#'                               se = TRUE, bar_orientation = "horizontal",
#'                               facet_var = "block")
#'
#' ## If multiple plots are desired `add_var` can be specified during
#' ## data preparation and the plots can be arranged using nrow and ncol
#' sim2$block <- as.character(sim2$block)
#' new_mod <- update(mod, ~. + block, data = sim2)
#' plot_data <- prediction_contributions_data(data = sim2[c(1,5,9,11,15,19,23), c(3:6, 8)],
#'                                            model = new_mod,
#'                                            add_var = list("block" = c("1", "2")))
#' ## Arrange in two columns
#' prediction_contributions_plot(data = plot_data, ncol = 2)
prediction_contributions_plot <- function(data, colours = NULL, se = FALSE,
                                          bar_orientation = c("vertical", "horizontal"),
                                          facet_var = NULL,
                                          nrow = 0, ncol = 0){
  # Ensure data is specified
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble, preferably the
                            output of {.help [{.fn {col_green(\"prediction_contributions_data\")}}](DImodelsVis::prediction_contributions_data)}."))
  }

  if(check_col_exists(data, ".add_str_ID")){
    ids <- unique(data$.add_str_ID)
    plots <- lapply(cli_progress_along(1:length(ids), name = "Creating plot",
                                       format = paste0(
                                         "{cli::pb_spin} Creating plot ",
                                         "[{cli::pb_current}/{cli::pb_total}]   ETA:{cli::pb_eta}"
                                       )),
                    function(i){
                      data_iter <- data %>% filter(.data[[".add_str_ID"]] == ids[i])
                      plot <- prediction_contributions_plot_internal(data = data_iter,
                                                                     colours = colours,
                                                                     se = se, facet_var = facet_var,
                                                                     bar_orientation = bar_orientation) +
                        labs(subtitle = ids[i])
                    })
    if(length(plots) > 1){
      plot <- new("ggmultiplot", plots = plots, nrow = nrow, ncol = ncol)
    } else {
      plot <- plots[[1]]
    }
    cli::cli_alert_success("Created all plots.")
  } else {
    plot <- prediction_contributions_plot_internal(data = data, colours = colours,
                                                   se = se, facet_var = facet_var,
                                                   bar_orientation = bar_orientation)
    cli::cli_alert_success("Created plot.")
  }

  return(plot)
}

#' @title Model term contributions to predicted response
#'
#' @description
#' A stacked bar_chart is shown where the individual contributions
#' (parameter estimate * predictor value) for each term in a statistical model are stacked
#' on top of another. The total height of the stacked bar gives the value of the
#' predicted response. The uncertainty around the predicted response can also be shown
#' on the plot.
#' This is a wrapper function specifically designed for statistical models fit using the
#' \code{\link[DImodels:DI]{DI()}} function from the
#' \code{\link[DImodels:DImodels-package]{DImodels}} R package and it implicitly
#' calls \code{\link{prediction_contributions_data}} followed by
#' \code{\link{prediction_contributions_plot}}. If your model object isn't fit using
#' DImodels, the associated data and plot functions can instead be called manually.
#'
#' @importFrom dplyr mutate %>% group_by distinct across
#'                   all_of slice_head ungroup near
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_rev fct_inorder
#' @importFrom ggplot2 ggplot element_text aes geom_col labs geom_errorbar
#'                     theme theme_bw facet_grid guides guide_legend coord_flip
#'                     scale_fill_manual unit element_blank label_both
#'                     scale_x_continuous scale_x_discrete
#' @importFrom PieGlyph geom_pie_glyph
#' @importFrom rlang !!! !! sym .data
#' @importFrom stats predict
#' @importFrom cli cli_progress_along
#' @importFrom DImodels DI_data_FG
#'
#' @param model A Diversity Interactions model object fit by using the
#'              \code{\link[DImodels:DI]{DI()}} function from the
#'              \code{\link[DImodels:DImodels-package]{DImodels}} package.
#' @param data A user-defined data-frame containing values for compositional variables
#'             along with any additional variables that the user wishes to predict for.
#'             If left blank, a selection of observations (2 from each level of
#'             richness) from the original data used to fit the model would be selected.
#' @param FG A higher level grouping for the compositional variables in the
#'           data. Variables belonging to the same group will be assigned with
#'           different shades of the same colour. The user can manually specify
#'           a character vector giving the group each variable belongs to.
#'           If left empty the function will try to get a grouping
#'           from the original \code{\link[DImodels:DI]{DI}} model object.
#' @param add_var A list specifying values for additional predictor variables
#'                in the model independent of the compositional predictor variables.
#'                This could be useful for comparing the predictions across
#'                different values for a non-compositional variable.
#'                If specified as a list, it will be expanded to show a plot
#'                for each unique combination of values specified, while if specified
#'                as a data-frame, one plot would be generated for each row in the
#'                data and they will be arranged in a grid according to the
#'                value specified in `nrow` and `ncol`.
#' @param groups A list specifying groupings to arrange coefficients into.
#'               The coefficients within a group will be added together and
#'               shown as a single component on the respective bars in the plot.
#'               This could be useful for grouping multiple similar terms
#'               into a single term for better visibility.
#' @param conf.level The confidence level for calculating confidence or
#'                   prediction intervals.
#' @param plot A boolean variable indicating whether to create the plot or return
#'             the prepared data instead. The default `TRUE` creates the plot while
#'             `FALSE` would return the prepared data for plotting. Could be useful
#'             for if user wants to modify the data first and then call the plotting
#'             function manually.
#' @param se A logical value indicating whether to show prediction intervals
#'           for predictions in the plot.
#' @param colours A character vector specifying the colours for the
#'                contributions of the different coefficients.
#'                If not specified, a default colour-scheme would be chosen,
#'                however it might be uninformative in some situations
#'                (for examples when manual groupings are specified using `groups`
#'                parameter).
#' @param nrow Number of rows in which to arrange the final plot
#'             (when `add_var` is specified).
#' @param ncol Number of columns in which to arrange the final plot
#'             (when `add_var` is specified).
#' @param bar_labs The labels to be shown for each bar in the plot. The user
#'                 has three options:
#'                    - By default, the row-names in the data would be used as
#'                      labels for the bars.
#'                    - A character string or numeric index indicating an ID
#'                      column in data.
#'                    - A character vector of same length as the number of rows
#'                      in the data, which manually specifies the names for each bar.
#'                 If none of the three options are available, the function would
#'                 assign a unique ID for each bar.
#' @param bar_orientation One of "vertical" or "horizontal" indicating the
#'                        orientation of the bars. Defaults to a vertical
#'                        orientation.
#' @param facet_var A character string or numeric index identifying the column
#'                  in the data to be used for faceting the plot into multiple
#'                  panels.
#' @param interval Type of interval to calculate:
#'  \describe{
#'    \item{"none"}{No interval to be calculated.}
#'    \item{"confidence" (default)}{Calculate a confidence interval.}
#'    \item{"prediction"}{Calculate a prediction interval.}
#'  }
#'
#' @return A ggmultiplot (ggplot if single plot is returned) class object or data-frame (if `plot = FALSE`)
#' @export
#'
#' @examples
#' #' ## Load DImodels package to fit the model
#' library(DImodels)
#'
#' ## Load data
#' data(sim2)
#'
#' ## Fit DI model
#' model1 <- DI(prop = 3:6, DImodel = 'FULL', data = sim2, y = 'response')
#'
#' ## Create visualisation
#' ## If no communities are specified 2 communities at
#' ## each level of richness from the original data are used
#' prediction_contributions(model1)
#'
#' ## Can also manually specify communities of interest
#' my_comms <- data.frame(p1 = c(1, 0, 0,   0.5, 1/3, 0.25),
#'                        p2 = c(0, 0, 0.5, 0,   1/3, 0.25),
#'                        p3 = c(0, 1, 0.5, 0,   1/3, 0.25),
#'                        p4 = c(0, 0, 0,   0.5, 0,   0.25))
#'
#' prediction_contributions(model1, data = my_comms)
#'
#' ## Group contributions to show as a single component on the plot
#' prediction_contributions(model1, data = my_comms,
#'                          groups = list("Interactions" = c("`p1:p2`", "`p1:p3`",
#'                                                           "`p1:p4`", "`p2:p3`",
#'                                                           "`p2:p4`", "`p3:p4`")))
#'
#' ## Add a prediction interval using `se = TRUE` and show bars horizontally
#' prediction_contributions(model1, data = my_comms, se = TRUE,
#'                          bar_orientation = "horizontal",
#'                          groups = list("Interactions" = c("`p1:p2`", "`p1:p3`",
#'                                                           "`p1:p4`", "`p2:p3`",
#'                                                           "`p2:p4`", "`p3:p4`")))
#'
#' ## Facet the plot on any variable
#' my_comms$richness <- c(1, 1, 2, 2, 3, 4)
#' ## Use `facet_var`
#' prediction_contributions(model1, data = my_comms, facet_var = "richness",
#'                          bar_orientation = "horizontal",
#'                          groups = list("Interactions" = c("`p1:p2`", "`p1:p3`",
#'                                                           "`p1:p4`", "`p2:p3`",
#'                                                           "`p2:p4`", "`p3:p4`")))
#'
#' ## Can also add additional variables independent of the simplex design
#' ## to get a separate plot for unique combination of the variables
#' prediction_contributions(model1, data = my_comms,
#'                          add_var = list("block" = factor(c(1, 2),
#'                                                          levels = c(1, 2, 3, 4))))
#'
#' ## Manually specify colours and bar labels
#' ## Model has 10 terms but we grouped 6 of them into 1 term,
#' ## so we need to specify 5 colours (4 ungrouped terms + 1 grouped term)
#' ## Bar labels can be specified using `bar_labs`
#' ## Also, using nrow to arrange plots in rows
#' prediction_contributions(model1, data = my_comms,
#'                          colours = c("steelblue1", "steelblue4",
#'                                      "orange", "orange4",
#'                                      "grey"),
#'                          bar_labs = c("p1 Mono", "p3 Mono", "1/2 p2 p3",
#'                                       "1/2 p1 p4", "1/3 p1 p2 p3", "Centroid"),
#'                          add_var = list("block" = factor(c(1, 2),
#'                                                          levels = c(1, 2, 3, 4))),
#'                          nrow = 2,
#'                          groups = list("Interactions" = c("`p1:p2`", "`p1:p3`",
#'                                                           "`p1:p4`", "`p2:p3`",
#'                                                           "`p2:p4`", "`p3:p4`")))
#'
#' ## Specify `plot = FALSE` to not create the plot but return the prepared data
#' head(prediction_contributions(model1, data = my_comms, plot = FALSE,
#'                               facet_var = "richness",
#'                               bar_orientation = "horizontal"))
prediction_contributions <- function(model, data = NULL,
                                     add_var = list(), groups = list(),
                                     conf.level = 0.95, bar_labs = rownames(data),
                                     colours = NULL, se = FALSE, FG = NULL,
                                     interval = c("confidence", "prediction", "none"),
                                     bar_orientation = c("vertical", "horizontal"),
                                     facet_var = NULL, plot = TRUE,
                                     nrow = 0, ncol = 0){
  # Ensure specified model is fit using the DI function
  if(missing(model) || (!inherits(model, "DI") && !inherits(model, "DImulti"))){
    model_not_DI(call_fn = "prediction_contributions")
  }

  # Get original data used to fit the model
  original_data <- model$original_data

  # Get all species in the model
  model_species <- attr(model, "prop")

  # If data is missing then use the original data used to fit the model
  # and choose a subset of the data to plot (two communities at each richness level)
  if(is.null(data)){
    data <- original_data %>%
      distinct(across(all_of(model_species)), .keep_all = T) %>%
      add_ID_terms(model) %>%
      mutate(.Richness = get_richness(.data, model_species)) %>%
      group_by(.data$.Richness) %>%
      slice_head(n = 2) %>%
      ungroup() %>%
      distinct(across(all_of(model_species)), .keep_all = T) %>%
      mutate(.Community = as.character(1:length(.data$.Richness))) %>%
      as.data.frame()

  } else {

    sanity_checks(data = data, prop = model_species)

    data <- data %>%
      add_ID_terms(model) %>%
      mutate(.Richness = get_richness(.data, model_species),
             .Community = as.character(1:length(.data$.Richness)))
  }

  interval <- match.arg(interval)
  if(is.null(FG)){
    FG <- attr(model, "FG")
  }

  # Prepare data for plotting
  # Add interaction terms
  old <- ncol(data) # To find number of colours for interaction terms
  data <- add_interaction_terms(model = model, data = data)
  new <- ncol(data)

  ## Add any experimental structures or missing terms
  before_add_exp <- colnames(data)
  data <- add_exp_str(model = model, data = data)
  after_add_exp <- colnames(data)

  ## Drop any _add columns
  if(attr(model, "DImodel") == "ADD"){
    data <- data %>% select(-all_of(paste0(model_species, "_add")))
  }

  # Drop exp str specified in add_var as they'll be add on at a later stage
  added_exp_str <- setdiff(after_add_exp, before_add_exp)
  common_exp_str <- intersect(names(add_var), added_exp_str)
  if(length(common_exp_str) > 0){
    data <- data %>% select(-all_of(common_exp_str))
  }

  # Split the predicted response into respective contributions
  plot_data <- prediction_contributions_data(data = data, model = model,
                                             add_var = add_var, interval = interval,
                                             groups = groups, bar_labs = bar_labs,
                                             conf.level = conf.level)

  ## If no groups are specified then colour the components manually
  if(length(groups) < 1){
    # Colours for species
    ## Colours for ID effects
    mod_ids <- if (is.null(model$DIcall$ID)) model_species else attr(model, "ID")
    ID_cols <- get_colours(vars = mod_ids, FG = FG)

    ## Colours for interaction effects
    ## Number of interaction terms
    DImodel_tag <- attr(model, "DImodel")

    if(DImodel_tag == "FG"){
      n_ints <- ncol(DI_data_FG(prop = model_species, data = data, FG = attr(model, "FG"))$FG)
    } else {
      n_ints <- new - old
    }
    if(n_ints == 0){
      int_cols <-  NULL
    } else {
      int_cols <- get_shades(shades = n_ints)[[1]]
    }

    ## Colours for experimental structures
    ## Number of experimental structures
    coeffs <- coef(model)
    n_coeff <- ifelse(is.na(coeffs["theta"]), length(coeffs), length(coeffs) - 1)
    n_exp <- n_coeff - length(ID_cols) - n_ints
    if(n_exp == 0){
      exp_cols <- NULL
    } else {
      exp_cols <- get_shades(colours = "red", shades = n_exp)[[1]]
    }

    # Final colours
    if(is.null(colours)){
      colours <- c(ID_cols, int_cols, exp_cols)
    }
  # User has specified groups so just give them colours
  } else {
    if(is.null(colours)){
      colours <- get_colours(levels(factor(plot_data$.Contributions)))
    }
  }

  if(isTRUE(plot)){
    plot <- prediction_contributions_plot(data = plot_data,
                                          colours = colours,
                                          se = se, facet_var = facet_var,
                                          bar_orientation = bar_orientation,
                                          nrow = nrow, ncol = ncol)
    return(plot)
  } else {
    return(plot_data)
  }

}

#' @keywords internal
#' Utility function for creating prediction_contributions plot
#'
#' @usage NULL
NULL
prediction_contributions_plot_internal <- function(data, colours = NULL,
                                                   se = FALSE,
                                                   bar_orientation = c("vertical", "horizontal"),
                                                   facet_var = NULL){
  # Sanity checks before creating the plot
  sanity_checks(data = data, colours = colours,
                booleans = list("se" = se))
  check_plot_data(data = data,
                  cols_to_check = c(".Community", ".Value", ".Contributions"),
                  calling_fun = "prediction_contributions")

  if(any(data[, ".Value"] < 0)){
    cli::cli_warn(c("Some terms in the model had negative contributions to the
                    predictions.",
                    "i" = "The plot will be created but might not be
                    interpretable for this example."))
  }

  # Colours for the bar segments
  if(is.null(colours)){
    rlang::warn(c("No colours were specified for the response contributions.",
                  "i" = paste("The default colours might not result in an informative plot,",
                               "consider choosing specific colours to contrast the contributions",
                               "of different groups in the response.")),
                .frequency = "once", .frequency_id = "2")
    colours <- get_colours(levels(factor(data$.Contributions)))
  }

  bar_orientation <- match.arg(bar_orientation)

  # browser()
  x_labs <- data %>%
    group_by(.data$.Community) %>%
    slice(1) %>%
    ungroup() %>%
    pull(".x_labs") %>%
    `names<-`(unique(data$.Community))

  plot <- ggplot(data, aes(x = .data$.Community, y = .data$.Value,
                           fill = fct_rev(fct_inorder(.data$.Contributions))))+
    geom_col(colour = "black")+
    theme_DI()+
    scale_fill_manual(values = rev(colours))+
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_discrete(labels = x_labs) +
    labs(y = 'Predicted Response',
         x = 'Community',
         fill = 'Contributions')

  if(se){
    check_plot_data(data = data,
                    cols_to_check = c(".Pred", ".Lower", ".Upper"),
                    calling_fun = "prediction_contributions")

    plot <- plot +
      geom_errorbar(aes(y = .data$.Pred,
                        ymin = .data$.Lower,
                        ymax = .data$.Upper),
                    colour = "black")
  }

  if(bar_orientation == "horizontal"){
    plot <- plot + ggplot2::coord_flip()
  }

  if(!is.null(facet_var)){
    plot <- add_facet(plot, data, facet_var,
                      scales = ifelse(bar_orientation == "horizontal",
                                      "free_y", "free_x"),
                      labeller = label_both)
  }

  return(plot)
}
