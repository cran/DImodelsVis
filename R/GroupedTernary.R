#' @title Grouped ternary diagrams
#'
#' @description
#' The helper function for preparing the underlying data for creating grouped
#' ternary diagrams where the proportions of the compositional variables
#' are combined into groups and visualised on a ternary diagram.
#' These are very useful when we have multiple compositional variables that can
#' be grouped together by some hierarchical grouping structure. For example, grouping
#' species in a ecosystem based on the functions they perform, or grouping
#' political parties based on their national alliances. Grouping variables this
#' way allows us to reduce the dimensionality of the compositional data and
#' visualise it. This is akin to looking at a 2-d slice of the high
#' dimensional simplex. The relative proportions of each variable within a group
#' can be adjust to look at different slices of the simplex. Looking at multiple
#' such slices would enable us to create an approximation of how the response varies
#' across the original n-dimensional simplex. The output of this function can be passed to the
#' \code{\link{grouped_ternary_plot}} function to plot the results.
#'
#'
#' @param FG A character vector specifying the groupings of the variables specified in `prop`.
#' @param values A numeric vector specifying the proportional split of the variables within a group.
#'               The default is to split the group proportion equally between
#'               each variable in the group.
#' @inheritParams conditional_ternary_data
#' @inheritDotParams add_prediction -data
#'
#' @return A data-frame containing compositional columns with names specified
#'         in `FG` and `prop` parameters along with any additional columns
#'         specified in `add_var` parameter and the following columns appended
#'         at the end.
#'  \describe{
#'    \item{.x}{The x-projection of the points within the ternary.}
#'    \item{.y}{The y-projection of the points within the ternary.}
#'    \item{.add_str_ID}{An identifier column for grouping the cartesian product
#'                       of all additional columns specified in `add_var`
#'                       parameter (if `add_var` is specified).}
#'    \item{.Sp}{An identifier column specifying the functional group along
#'               which the high dimensional simplex is sliced (if there are
#'               more than 3 groups).}
#'    \item{.Value}{The value (between 0 and 1) along the direction of functional
#'                  group in `.Sp` at which the high dimensional simplex is sliced.}
#'    \item{.Facet}{An identifier column formed by combining `.Sp` and `.value`
#'                  to group observations within a specific slice of the
#'                  high dimensional simplex.}
#'    \item{.Pred}{The predicted response for each observation.
#'                 (if `prediction` is \code{TRUE})}
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
#' data(sim3)
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9)^2,
#'            data = sim3)
#'
#' ## Create data
#' ## We have nine (p1 to p9) variables here and using \code{\link{conditional_ternary}}
#' ## to visualise the simplex space won't be very helpful as there are too
#' ## variables to condition on
#' ## Instead we group the nine-variables into three groups called "G", "L" and "H"
#' head(grouped_ternary_data(model = mod,
#'                           prop = paste0("p", 1:9),
#'                           FG = c("G","G","G","G","G","L","L","H","H"),
#'                           resolution = 1))
#'
#' ## By default the variables within a group take up an equal share of the
#' ## group proportion. So for example, each point along the above ternary
#' ## would have a 50:50 split of the variables in the group "L" or "H", thus
#' ## the vertex where "L" is 1, would mean that p6 and p7 are 0.5 each,
#' ## similarly, the vertex "H" is made up of 0.5 of p8 and p9 while the "G"
#' ## vertex is comprised of 0.2 of each of p1, p2, p3, p4, and p5. The concepts
#' ## also extend to points along the edges and interior of the ternary.
#'
#' ## Change the proportional split of species within an FG by using `values`
#' ## `values` takes a numeric vector where the position of each element
#' ## describes the proportion of the corresponding species within the
#' ## corresponding FG
#' ## For examples this vector describes, 2-% each of p1, p2, p3, p4 and p5,
#' ## in G, 0% and 100% of p6 and p7, respectively in G2 and 30% and 70% of
#' ## p8 and p9, respectively in G3.
#' vals <- c(0.2, 0.2, 0.2, 0.2, 0.2,
#'           0, 1,
#'           0.3, 0.7)
#' head(grouped_ternary_data(prop = paste0("p", 1:9),
#'                           FG = c("G","G","G","G","G","L","L","H","H"),
#'                           values = vals,
#'                           resolution = 1,
#'                           model = mod))
#'
#' ## Can also add any additional experimental structures
#' ## Notice .add_str_ID in the data
#' head(grouped_ternary_data(prop = paste0("p", 1:9),
#'                           FG = c("G","G","G","G","G","L","L","H","H"),
#'                           add_var = list("treatment" = c("50", "150")),
#'                           values = vals,
#'                           model = mod,
#'                           resolution = 1))
#'
#' ## It could be desirable to take the output of this function and add
#' ## additional variables to the data before making predictions
#' ## Use `prediction = FALSE` to get data without any predictions
#' grouped_data <- grouped_ternary_data(prop = paste0("p", 1:9),
#'                                      FG = c("G","G","G","G","G","L","L","H","H"),
#'                                      values = vals,
#'                                      resolution = 1,
#'                                      prediction = FALSE)
#' grouped_data$treatment <- 250
#' # Add predictions
#' head(add_prediction(data = grouped_data, model = mod))
grouped_ternary_data <- function(prop, FG,
                                 values = NULL,
                                 tern_vars = NULL,
                                 conditional = NULL,
                                 add_var = list(),
                                 resolution = 3,
                                 prediction = TRUE, ...){

  #Sanity Checks
  if(missing(prop)){
    cli::cli_abort(c("{.var prop} cannot be empty.",
                     "i" = "Specify a character vector indicating the model
                     coefficients corresponding to variable proportions."))
  }

  # Ensure FG is specified
  if(missing(FG)){
      cli::cli_abort(c("The {.var FG} argument cannot be empty.",
                       "i" = "The {.var FG} argument should be specified as
                       a character vector of same length as the {.var prop}
                       argument, specifying the group to which each
                       variable in prop belongs."))
  }

  # Check inputs of function are appropriate and return default values
  def_vals <- FG_sanity_checks(prop = prop, FG = FG,
                               values = values,
                               tern_vars = tern_vars,
                               conditional = conditional)

  values <- def_vals$values
  all_FGs <- def_vals$all_FGs
  tern_vars <- def_vals$tern_vars
  conditional <- def_vals$conditional

  # Create base ternary data to be plotted
  triangle <- ternary_data(prop = tern_vars,
                           add_var = add_var,
                           resolution = resolution,
                           prediction = FALSE)

  # If there are only three functional groups in the model
  # our data is ready and we can make predictions from the model after accounting for species proportions
  if (length(all_FGs) == 3){
    triangle[, prop] <- 0
    focus <- tern_vars
    FG_mapping <- get_FG_value_mapping(FG, focus = focus)

    for (FGs in focus){
      ids <- FG_mapping[[FGs]]
      sp_props <- (triangle[, FGs] %o% values[ids])
      # Special condition for when there is only one species per FG
      # As value gets stored as matrix otherwise
      if(length(ids) == 1){
        sp_props <- as.numeric(sp_props)
      }
      triangle[, prop[ids]] <- sp_props
    }

    cond_data <- triangle

    if(prediction){
      cond_data <- add_prediction(data = cond_data, ...)
    }
    # cond_data <- add_prediction(data = triangle,
    #                             model = model,
    #                             coefficients = coefficients,
    #                             coeff_cols = coeff_cols,
    #                             pred_name = pred_name,
    #                             conf.level = conf.level)
  } else {
    # Rescaling species to be shown in ternary for each species
    # in the conditional parameter according to the values specified
    cond_names <- colnames(conditional)
    leftover <- all_FGs[! all_FGs %in% c(tern_vars, cond_names)]
    if(length(leftover) > 0){
      pOther <- matrix(0, ncol = length(leftover), nrow = nrow(triangle))
      colnames(pOther) <- leftover
      triangle <- cbind(triangle, pOther)
    }
    # Iterate over each group specified in conditional
    cond_data <- lapply(cli_progress_along(1:nrow(conditional), name = "Preparing data"), function(idx){
      cond_vals <- as.numeric(conditional[idx, ])
      x <- sum(cond_vals)

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
                               collapse = "; "))

      # To avoid any rounding issues & ensure all species proportions sum to 1
      # No need to fix if rowsum is one
      if(any(!near(rowSums(sp_data[, all_FGs]), 1))){
        # Find the first conditional species which is non-zero
        fix_ind <- which(cond_vals != 0)[1]
        fix_sp <- cond_names[fix_ind]
        sp_data[, fix_sp] <- 1 - rowSums(sp_data[, all_FGs[all_FGs != fix_sp]])
      }

      # # Add remaining species in the data
      # remaining_FG <- matrix(0, ncol=length(cond_FG), nrow= nrow(triangle))
      # colnames(remaining_FG) <- cond_FG
      # scaled_data <- cbind(scaled_data, remaining_FG)

      # Update value of species which we are conditioning on
      # species_data <- lapply(cond_FG, function(cond_sp){
      #   # Add identifier for grouping data for a particular species
      #   sp_data <- scaled_data %>%
      #     mutate(.Sp = cond_sp,
      #            .Value =  x,
      #            .Facet = paste0(cond_sp, ' = ', x))

        # To avoid any rounding issues and ensure all species proportions sum to 1
        # sp_data[, cond_sp] <- 1 - rowSums(sp_data[, tern_vars[1:3]])

        sp_data[, prop] <- 0
        focus <- all_FGs #c(tern_vars, cond_sp)
        FG_mapping <- get_FG_value_mapping(FG, focus = focus)

        for (FGs in focus){
          ids <- FG_mapping[[FGs]]
          # Converting to df because when there is only 1 prop, %o% returns
          # a vector which causes conflicts
          sp_data[, prop[ids]] <- (sp_data[, FGs] %o% values[ids]) %>%
                                    as.data.frame() %>%
                                    `colnames<-`(prop[ids])
        }

        # Predicting the response for the communities
        if(prediction){
          sp_data <- add_prediction(data = sp_data, ...)
        }

        # Need to return data via subset of rows as bind_rows fails otherwise
        sp_data
      # }) %>% bind_rows()
    }) %>% bind_rows()
  }

  cond_data <- cond_data %>%
    select(all_of(c(tern_vars, ".x", ".y", prop, all_FGs[!all_FGs %in% tern_vars], names(add_var))), everything())

  # Add attributes for fetching data
  attr(cond_data, "prop") <- prop
  attr(cond_data, "FG") <- all_FGs
  attr(cond_data, "tern_vars") <- tern_vars
  attr(cond_data, "x_proj") <- ".x"
  attr(cond_data, "y_proj") <- ".y"
  attr(cond_data, "add_var") <- names(add_var)

  cli::cli_alert_success("Finished data preparation.")
  return(cond_data)
}

#' @title Conditional ternary diagrams at functional group level
#'
#' @description
#' The helper function for plotting grouped ternary diagrams. The output of
#' the `\code{\link{grouped_ternary_data}}` with the compositional variables
#' combined into groups should be passed here to be visualised on a 2-d ternary diagram.
#' These are very useful when we have multiple compositional variables that can
#' be grouped together by some hierarchical grouping structure. For example, grouping
#' species in a ecosystem based on the functions they perform, or grouping
#' political parties based on their national alliances.
#'
#' @inheritParams conditional_ternary_plot
#'
#' @inherit ternary_plot return
#'
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' ## Load data
#' data(sim3)
#'
#' ## Fit model
#' mod <- glm(response ~ 0 + (p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9)^2,
#'            data = sim3)
#'
#' ## Create data
#' ## We have nine (p1 to p9) variables here and using conditional_ternary
#' ## to visualise the simplex space won't be very helpful as there are too
#' ## variables to condition on
#' ## Instead we group the nine-variables into three groups called "G", "L" and "H"
#' plot_data <- grouped_ternary_data(model = mod,
#'                                   prop = paste0("p", 1:9),
#'                                   FG = c("G","G","G","G","G","L","L","H","H"),
#'                                   resolution = 1)
#' grouped_ternary_plot(plot_data)
#'
#' ## By default the variables within a group take up an equal share of the
#' ## group proportion. So for example, each point along the above ternary
#' ## would have a 50:50 split of the variables in the group "L" or "H", thus
#' ## the vertex where "L" is 1, would mean that p6 and p7 are 0.5 each,
#' ## similarly, the vertex "H" is made up of 0.5 of p8 and p9 while the "G"
#' ## vertex is comprised of 0.2 of each of p1, p2, p3, p4, and p5. The concepts
#' ## also extend to points along the edges and interior of the ternary.
#'
#' ## Change the proportional split of species within an FG by using `values`
#' ## `values` takes a numeric vector where the position of each element
#' ## describes the proportion of the corresponding species within the
#' ## corresponding FG
#' ## For examples this vector describes, 2-% each of p1, p2, p3, p4 and p5,
#' ## in G, 0% and 100% of p6 and p7, respectively in G2 and 30% and 70% of
#' ## p8 and p9, respectively in G3.
#' vals <- c(0.2, 0.2, 0.2, 0.2, 0.2,
#'           0, 1,
#'           0.3, 0.7)
#' plot_data <- grouped_ternary_data(prop = paste0("p", 1:9),
#'                                   FG = c("G","G","G","G","G","L","L","H","H"),
#'                                   values = vals,
#'                                   resolution = 1,
#'                                   model = mod)
#' ## Change number of contours and colour scheme
#' grouped_ternary_plot(plot_data,
#'                      nlevels = 8,
#'                      colours = hcl.colors(8))
#'
#' ## Can also add any additional experimental structures
#' ## Notice .add_str_ID in the data
#' plot_data <- grouped_ternary_data(prop = paste0("p", 1:9),
#'                                   FG = c("G","G","G","G","G","L","L","H","H"),
#'                                   add_var = list("treatment" = c("50", "150")),
#'                                   values = vals,
#'                                   model = mod,
#'                                   resolution = 1)
#' grouped_ternary_plot(data = plot_data)
grouped_ternary_plot <- function(data,
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
                                 vertex_label_size = 5,
                                 nrow = 0, ncol = 0){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble (preferably the
                            output of {.help [{.fn {col_green(\"grouped_ternary_data\")}}](DImodelsVis::grouped_ternary_data)})."))
  }

  pl <- conditional_ternary_plot(data = data,
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
                                 vertex_label_size = vertex_label_size,
                                 nrow = nrow, ncol = ncol)
  return(pl)
}

#' @title Conditional ternary diagrams at functional group level
#'
#' @description
#' Grouped ternary diagrams are created by combining the proportions of the
#' compositional variables into groups and visualising these groups on a 2-d
#' ternary diagram. These are very useful when we have multiple compositional
#' variables that can be grouped together by some hierarchical grouping structure.
#' For example, grouping species in a ecosystem based on the functions they perform,
#' or grouping political parties based on their national alliances. Grouping
#' variables this way allows us to reduce the dimensionality of the compositional
#' data and visualise it. This is akin to looking at a 2-d slice of the high
#' dimensional simplex. The relative proportions of each variable within a group
#' can be adjusted to look at different slices of the simplex. Looking at multiple
#' such slices would enable us to create an approximation of how the response varies
#' across the original n-dimensional simplex.
#' This is a wrapper function specifically for statistical models fit using the
#' \code{\link[DImodels:DI]{DI()}} function from the
#' \code{\link[DImodels:DImodels-package]{DImodels}} R package and would implicitly
#' call \code{\link{grouped_ternary_data}} followed by
#' \code{\link{grouped_ternary_plot}}. If your model object isn't fit using
#' DImodels, consider calling these functions manually.
#'
#' @importFrom stats setNames
#'
#' @param model A Diversity Interactions model object fit by using the
#'              \code{\link[DImodels:DI]{DI()}} function from the
#'              \code{\link[DImodels:DImodels-package]{DImodels}} package.
#' @inheritParams grouped_ternary_data
#' @inheritParams ternary_plot
#' @inheritParams model_diagnostics
#'
#' @inherit prediction_contributions return
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' library(dplyr)
#' data(sim3)
#' m1 <- DI(y = "response", prop = paste0("p", 1:9),
#'          DImodel = "AV", data = sim3) %>%
#'          suppressWarnings()
#'
#' ## We have nine (p1 to p9) variables here and using `conditional_ternary`
#' ## to visualise the simplex space won't be very helpful as there are too
#' ## variables to condition on
#' ## Instead we group the nine-variables into three groups called "G", "L" and "H"
#' grouped_ternary(model = m1, FG = c("G","G","G","G","G","L","L","H","H"),
#'                 resolution = 1)
#' ## By default the variables within a group take up an equal share of the
#' ## group proportion. So for example, each point along the above ternary
#' ## would have a 50:50 split of the variables in the group "L" or "H", thus
#' ## the vertex where "L" is 1, would mean that p6 and p7 are 0.5 each,
#' ## similarly, the vertex "H" is made up of 0.5 of p8 and p9 while the "G"
#' ## vertex is comprised of 0.2 of each of p1, p2, p3, p4, and p5. The concepts
#' ## also extend to points along the edges and interior of the ternary.
#'
#' ## We can also manually specify the split of the species within a group
#' ## This would mean we are looking at a different slice of the simplex
#' ## For example this would mean the groups "L" group is made up of 100% of
#' ## p7 and doesn't contain any p6, while "H" group contains 30% of p8 and
#' ## 70% of p9, while "G" group still contains 20% of each p1 to p5.
#' grouped_ternary(m1, FG = c("G","G","G","G","G","L","L","H","H"),
#'                 resolution = 1,
#'                 values = c(0.2, 0.2, 0.2, 0.2, 0.2,
#'                            0, 1,
#'                            0.3, 0.7))
#'
#' ## If here are more than three groups then, we could condition some groups
#' ## to have a fixed value while three groups are manipulated within a ternary
#' ## The group "G" is now split into two groups "G1" and "G2"
#' ## We can create conditional ternary diagram at the grouped level as well
#' ## Notice the values going in `tern_vars` and `conditional` are names
#' ## of the groups and not the original compositional variables
#' grouped_ternary(m1, FG = c("G1","G1","G2","G2","G2","L","L","H","H"),
#'                 resolution = 1,
#'                 tern_vars = c("G1", "L", "H"),
#'                 conditional = data.frame("G2" = c(0, 0.25, 0.5)))
#'
#' ## Specify `plot = FALSE` to not create the plot but return the prepared data
#' head(grouped_ternary(m1, FG = c("G1","G1","G2","G2","G2","L","L","H","H"),
#'                      resolution = 1, plot = FALSE,
#'                      tern_vars = c("G1", "L", "H"),
#'                      conditional = data.frame("G2" = c(0, 0.25, 0.5))))
#'
#' ## All other functionality from \code{\link{condtional_ternary_plot}} is
#' ## available in this function too.
grouped_ternary <- function(model,
                            FG,
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
    model_not_DI(call_fn = "grouped_ternary")
  }

  # Get data used to fit the model
  og_data <- model$original_data

  # Get all species in the model
  species <- attr(model, "prop")

  # Ensure FG is specified
  if(missing(FG)){
    if (!is.null(attr(model, "FG"))){
      FG <- attr(model, "FG")
    } else {
      cli::cli_abort(c("The {.var FG} argument cannot be empty.",
                       "i" = "The {.var FG} argument should be specified as a
                       character vector of same length as the {.var prop} argument,
                       specifying the functional group to which each variable
                       in {.var prop} belongs."))
    }
  }

  # If model object is of type DImulti add info about EFs and timepoints
  if(inherits(model, "DImulti")) {
    add_var <- link_DImodelsMulti(model = model, add_var = add_var)
  }

  # Create data in appropriate format for plotting
  plot_data <- grouped_ternary_data(prop = species, FG = FG,
                                    model = model,
                                    values = values,
                                    tern_vars = tern_vars,
                                    conditional = conditional,
                                    add_var = add_var,
                                    resolution = resolution)

  # Labels for the ternary
  tern_labels <- colnames(plot_data)[1:3]

  if(isTRUE(plot)){
    plot <- grouped_ternary_plot(data = plot_data,
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
    return(plot)
  } else {
    return(plot_data)
  }
}

#' @usage NULL
NULL
get_FG_values <- function(FG){
  nFG <- length(FG)
  counts <- table(FG)
  values <- setNames(vector(mode = 'numeric', length = nFG), FG)
  for(prop in 1:nFG){
    values[prop] <- 1/counts[names(counts) == FG[prop]]
  }
  return(values)
}

#' @usage NULL
NULL
check_FG_values <- function(FG, values){
  groups <- unique(FG)
  sp_in_FG_sums <- sapply(groups, function(group){
    sum(values[FG == group])
  })
  names(sp_in_FG_sums) <- groups
  sum_to_1 <- ifelse(near(sp_in_FG_sums, 1), T, F)
  return(list('sums' = sp_in_FG_sums, 'bool' = sum_to_1))
}

#' @usage NULL
NULL
get_FG_value_mapping <- function(FG, focus = unique(FG)){

  FG_mapping <- lapply(focus, function(group){
    which(FG == group)
  })
  names(FG_mapping) <- focus
  return(FG_mapping)
}

#' @usage NULL
NULL
FG_sanity_checks <- function(prop, FG,
                             values = NULL,
                             tern_vars = NULL,
                             conditional = NULL){

  if(!is.character(FG)){
    cli::cli_abort(c("The {.var FG} argument should be specified as a character
                     vector of same length as the {.var prop} argument,
                     specifying the functional group to which each variable
                     in {.var prop} belongs.",
                     "i" = "{.var FG} was specified as a {.cls {class(FG)}} object."))
  }

  if(length(prop) != length(FG)){
    cli::cli_abort(c("The {.var FG} argument should be specified as a character
                     vector of same length as the {.var prop} argument,
                     specifying the functional group to which each variable
                     in {.var prop} belongs.",
                     "i" = "{.var FG} has length {length(FG)} while {.var prop}
                     has length {length(prop)}."))
  }

  if(any(prop %in% FG)){
    cli::cli_abort(c("The names specified for grouping in the {.var FG}
                    argument should be different than those specified in
                    the {.var prop} argument.",
                     "i" = "The group{?s} {.val {unique(FG)[unique(FG) %in% prop]}}
                     from {.var FG} {?is/are} present in {.var prop}.
                     Please change their names in {.var FG}."))
  }

  if(is.null(values)){
    values <- get_FG_values(FG)
    cli::cli_warn("The proportional split of species in the groups was not specified in {.var values},
                  assuming an equal split for species in each group.")
  } else {
    if(!is.numeric(values)){
      cli::cli_abort(c("{.var values} should be a numeric vector with values between 0 and 1 specifying the proportion of each species within a function group.",
                       "i" = "{.var values} was specified as a {.cls {class(values)}} object."))
    }

    if(!all(between(values, 0, 1))){
      cli::cli_abort(c("{.var values} should be a numeric vector with values between 0 and 1 specifying the proportion of each species within a function group.",
                       "i" = "{.var values} was specified with value{?s} {.val {as.character(values)}}."))
    }

    if(length(values) != length(FG)){
      cli::cli_abort(c("{.var values} should have the same length as the {.var FG} argument.",
                       "i" = "{.var values} has length {length(values)} while {.var FG} has length {length(FG)}."))
    }

    sp_props_in_FG <- check_FG_values(FG, values)

    if(!all(sp_props_in_FG$bool)){
      faults <- sp_props_in_FG$sums[sp_props_in_FG$bool == F]
      cli::cli_abort(c("The species proportions within a functional group should sum to 1.",
                       "i" = "The proportions for {names(faults)} equal {faults}. respectively."))
    }
  }

  # Get variables to be shown in the ternary diagram
  all_FGs <- unique(FG)

  # Can't show anything if there are less than 3 groups
  if(length(all_FGs) < 3){
    cli::cli_abort(c("Ternary diagrams cannot be created for less than 3 unique groups",
                     "i" = "Currently only {length(all_FGs)} unique groups are specified in {.var FG}."))
  }

  # If tern_vars is not specified
  msg <- c("There are more than three unique groups. Only
            three can be shown within a ternary diagram.",
           "i" = "The groups {.val {tern_vars}} are shown in the
                ternary diagram while the remaining groups are conditioned to be 0.",
           "i" = "Use {.var tern_vars} to change the groups shown in the ternary
                and {.var conditional} to condition the remaining groups to have
                specific values.")
  # If there are more than three groups and tern_vars and conditional is not specified
  if(length(all_FGs) > 3 && is.null(conditional) && is.null(tern_vars)){
    conditional <- matrix(0, nrow = 1,
                          ncol = length(all_FGs[4:length(all_FGs)])) %>%
      `colnames<-`(all_FGs[4:length(all_FGs)]) %>%
      as.data.frame()
    tern_vars <- all_FGs[1:3]
    cli::cli_warn(msg)
  } else if(length(all_FGs) > 3 && is.null(conditional)){
    conds <- all_FGs[!all_FGs %in% tern_vars]
    conditional <- matrix(0, nrow = 1,
                          ncol = length(conds)) %>%
      `colnames<-`(conds) %>%
      as.data.frame()
    cli::cli_warn(msg)
  } else if(length(all_FGs) > 3 && is.null(tern_vars)){
    if(!(inherits(conditional, "data.frame"))){
      cli::cli_abort("{.var conditional} should be a data-frame containing
                   the names and values for the variables at which
                   the simplex space should be sliced.",
                     "i" = "{.var conditional} was specified as a
                          {.cls {conditional}}.",
                     call = caller_env())
    }
    conds <- names(conditional)
    tern_vars <- all_FGs[!all_FGs %in% conds][1:3]
    if(length(all_FGs[!all_FGs %in% conds]) > 3){
      cli::cli_warn(c("After conditioning the simplex space at the specified values,
                      there are more than three unique groups for the ternary. Only
            three can be shown within a ternary diagram.",
                      "i" = "The groups {.val {tern_vars}} are shown in the
                ternary diagram while all the remaining groups are conditioned to be 0.",
                      "i" = "Use {.var tern_vars} to change the groups shown in the ternary
                and {.var conditional} to condition the remaining groups to have
                specific values."))
    }
  }

  if(length(all_FGs) == 3 && is.null(tern_vars)){
    tern_vars <- all_FGs
  }

  # Sanity checks for tern_vars
  if (!is.null(tern_vars) && !inherits(tern_vars, "character")){
    cli::cli_abort("{.var tern_vars} should be a character vector of length 3,
                   containing the names of the variables to be shown in the ternary.",
                   "i" = "{.var tern_vars} was specified as a
                          {.cls {tern_vars}}.")
  }

  if(!is.null(tern_vars) && any(!tern_vars %in% all_FGs)){
    cli::cli_abort(c("If specifying the {.var FG} parameter, {.var tern_vars}
                     should consist of values specified in {.var FG}.",
                    "i" = "Choose three values from {.val {all_FGs}} in
                    {.var tern_vars} to be shown in the ternary."))
  }
  if(!is.null(tern_vars) && length(tern_vars) > 3){
    cli::cli_abort(c("{.var tern_vars} cannot have more than three elements.",
                     "i" = "Currently {.val {length(tern_vars)}} elements are
                     specified in {.var tern_vars}."))
  }

  # Check for conditional
  if(!is.null(conditional)){
    conditional <- check_conditional_parameter(conditional = conditional,
                                               prop = all_FGs,
                                               tern_vars = tern_vars,
                                               cond_FGs = all_FGs[!all_FGs %in% tern_vars],
                                               FG_flag = TRUE)
  }

  return(list("values" = values, "all_FGs" = all_FGs, "tern_vars" = tern_vars,
              "conditional" = conditional))
}

