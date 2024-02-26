#' @keywords internal
#' Performing sanity checks to ensure user input is appropriate and return
#' informative error messages if anything is wrong
#'
#' @importFrom insight is_regression_model get_data
#' @importFrom cli cli_abort cli_warn cli_alert col_green
#' @importFrom rlang caller_env
#' @importFrom dplyr near
#'
#' @usage NULL
NULL
sanity_checks <- function(data = NULL, prop = NULL, responses = NULL,
                          facet = NULL, treatments = NULL, DImodel = NULL,
                          model = NULL, colours = NULL,
                          numerics = NULL, booleans = NULL,
                          characters = NULL, unit_lengths = NULL,
                          call = caller_env()){

  # Sanity checks for data
  if(!is.null(data)){
    if(!inherits(data, "data.frame")){
      cli::cli_abort(c("{.var data} should be a data frame or tibble.",
                       "x" = "You specified an object with class {.cls {class(data)}}"),
                     call = call)
    }
  }

  # Ensure model is a DImodel object
  if(!is.null(DImodel)){
    if (!inherits(DImodel, "DI") && !inherits(DImodel, "DImulti")){
      cli::cli_abort(c("{.var model} should be an object creating using the
                        {.help [{.fun DI}](DImodels::DI)} function from the
                       {.help [{.pkg DImodels}](DImodels::DImodels)} package or
                       an object extending the {.cls DI} class.",
                       "x" = "You specified a model of class {.cls {class(DImodel)}}."),
                     call = call)
    }
    if(is.null(data)){
      data <- DImodel$original_data
    }
  }

  # Ensure if model is specified it is a statistical regression model
  if(!is.null(model)){
    if(!insight::is_regression_model(model)){
      cli::cli_abort(c("{.var model} should be a statistical model obejct.",
                       "x" = "You specified an object with class {.cls {class(model)}}"),
                     call = call)
    }
  }

  # Get data columns to check other variables
  data_col_names <- colnames(data)

  # Sanity check for prop
  if(!is.null(prop)){
    # Ensure prop is numeric or character
    if(!all(is.character(prop)) & !all(is.numeric(prop))){
      cli::cli_abort(c("{.var prop} should be a character vector specifying
                       the column names of the columns containing the compositonal
                       variables.",
                       "x" = "You specified an object of class {.cls {class(prop)}}"),
                     call = call)
    }

    if(is.numeric(prop)){
      # Ensure valid indices are specified
      if(!all(prop %in% 1:length(data_col_names))){
        cli::cli_abort(c("The indices specified in {.var prop} should be valid
                         indices for extracting columns from the data.",
                         "x" = "Can't extract columns using
                         {.var {as.character(prop[!(prop %in% seq_along(data_col_names))])}}
                         as column ind{?ex/ices}."),
                       call = call)
      } else {
        prop <- data_col_names[prop]
      }
    }

    # Ensure all prop present in the data
    if(!all(prop %in% data_col_names)){
      cli::cli_abort(c("All values specified in {.var prop} should be present
                       in the data.",
                       "x" = "{.var {prop[!(prop %in% data_col_names)]}} {?is/are}
                       not present in the data."),
                     call = call)
    }

    # Ensure all prop columns are numeric
    if(!all(sapply(data[, prop], is.numeric))){
      cli::cli_abort(c("All columns specified in {.var prop} should be numeric.",
                       "x" = "The column{?s} {.var {prop[!sapply(data[, prop], is.numeric)]}} {?is/are} are not numeric."),
                     call = call)
    }

    # Ensure prop is between 0 and 1
    if(!all(sapply(data[, prop], function(x) all(between(x, 0, 1))))){
      cli::cli_abort(c("All columns specified in {.var prop} should have values between 0 and 1.",
                       "x" = "The column{?s} {.var {prop[!sapply(data[, prop], function(x) all(between(x, 0, 1)))]}}
                       {?do/does} not have values between 0 and 1."),
                     call = call)
    }

    # Ensure the prop specified sum to 1
    comm_sum <- get_comm_sum(data, prop)
    if(!all(dplyr::near(comm_sum, 1, tol = .Machine$double.eps^0.25))){
      if(any(comm_sum < 1)){
        cli::cli_abort(c("!" = "The columns containing the variable proportions
                         should sum to 1 for each row.",
                         "i" = "Certain rows sum less than 1 currently.
                         Do you want to specify any additional columns from the
                         design?"),
                       call = call)
      } else {
        cli::cli_abort(c("!" = "The columns containing the variables proportions
                         should sum to 1 for each row.",
                         "i" = "Certain ros sum more than 1 currently.
                         Have you specified any additional columns not from the
                         design?"),
                       call = call)
      }

    }
  }

  # Sanity checks if facet is specified
  if(!is.null(facet)){
    # Ensure facet is numeric or character
    if(!all(is.character(facet)) & !all(is.numeric(facet))){
      cli::cli_abort(c("{.var facet} should be of type numeric or character specifying
                       the column indices or names of columns containing the variable(s)
                       to facet the plot on.",
                       "x" = "You specified an object of class {.cls {class(facet)}}"),
                     call = call)
    }

    # Can't facet for more than two variables
    if(length(facet) > 2){
      cli::cli_abort(c("x" = "{.var facet} cannot contain more than two variables,
                       as facetting is not possible for more than two variables.",
                       "i" = "Drop elements from {.var facet} to ensure the
                       length is one or two."),
                     call = call)
    }

    # If facet were specified as indices instead of characters
    if(is.numeric(facet)){
      # Ensure valid indices are specified
      if(!all(facet %in% 1:length(data_col_names))){
        cli::cli_abort(c("The indices specified in {.var facet} should be valid
                         indices for extracting columns from the data.",
                         "x" = "Can't extract columns using
                         {.var {as.character(facet[!(facet %in%  seq_along(data_col_names))])}}
                         as column ind{?ex/ices}."),
                       call = call)
      } else {
        facet <- data_col_names[facet]
      }
    }

    # Ensure values specified in facet are present in the data
    if(!all(facet %in% data_col_names)){
      cli::cli_abort(c("All values specified in {.var facet} should be present in the data.",
                       "x" = "{.var {facet[!(facet %in% data_col_names)]}} {?is/are} not
                       present in the data."),
                     call = call)
    }
  }

  # Sanity check for responses
  if(!is.null(responses)){
    # Ensure responses is numeric or character
    if(!all(is.character(responses)) & !all(is.numeric(responses))){
      cli::cli_abort(c("{.var responses} should be of type numeric or character specifying
                       the column indices or names of the columns containing the responses.",
                       "x" = "You specified an object of class {.cls {class(responses)}}"),
                     call = call)
    }

    # If responses were specified as indices instead of characters
    if(is.numeric(responses)){
      # Ensure valid indices are specified
      if(!all(responses %in% 1:length(data_col_names))){
        cli::cli_abort(c("The indices specified in {.var responses} should be valid
                         indices for extracting columns from the data.",
                         "x" = "Can't extract columns using
                         {.var {as.character(responses[!(responses %in% seq_along(data_col_names))])}}
                         as column ind{?ex/ices}."),
                       call = call)
      } else {
        responses <- data_col_names[responses]
      }
    }

    # Ensure values specified in responses are present in the data
    if(!all(responses %in% data_col_names)){
      cli::cli_abort(c("All values specified in {.var responses} should be present
                       in the data.",
                       "x" = "{.var {responses[!(responses %in% data_col_names)]}}
                       {?is/are} not present in the data."),
                     call = call)
    }

    # Ensure all responses columns are numeric
    if(!all(sapply(data[, responses], is.numeric))){
      cli::cli_abort(c("All columns specified in {.var responses} should be numeric.",
                       "x" = "The column{?s}
                       {.var {responses[!sapply(data[, responses], is.numeric)]}}
                       {?is/are} not numeric."),
                     call = call)
    }
  }

  # Sanity check for treatments (if specified)
  if(!is.null(treatments)){
    # Ensure treatments is numeric or character
    if(!all(is.character(treatments)) & !all(is.numeric(treatments))){
      cli::cli_abort(c("{.var treatments} should be of type numeric or character specifying
                       the column indices or names of the columns containing the treatments.",
                       "x" = "You specified an object of class {.cls {class(treatments)}}"),
                     call = call)
    }

    # If treatments were specified as indices instead of characters
    if(is.numeric(treatments)){
      # Ensure valid indices are specified
      if(!all(treatments %in% 1:length(data_col_names))){
        cli::cli_abort(c("The indices specified in {.var treatments} should be valid
                         indices for extracting columns from the data.",
                         "x" = "Can't extract columns using
                         {.var {as.character(treatments[!(treatments %in% seq_along(data_col_names))])}}
                         as column ind{?ex/ices}."),
                       call = call)
      } else {
        treatments <- data_col_names[treatments]
      }
    }

    # Ensure values specified in treatments are present in the data
    if(!all(treatments %in% data_col_names)){
      cli::cli_abort(c("All values specified in {.var treatments} should be present in the data.",
                       "x" = "{.var {treatments[!(treatments %in% data_col_names)]}}
                       {?is/are} not present in the data."),
                     call = call)
    }
  }

  # Checks if colours specified are proper
  if(!is.null(colours)){
    if(!all(sapply(colours, areColours))){
      cli::cli_abort(c("x" = "The value{?s} of {.var {colours[!sapply(colours, areColours)]}}
                       {?is/are} not {?a/} valid colour{?s}."),
                     call = call)
    }
  }

  # Check if booleans specified are proper
  if(!is.null(booleans)){
    if(!all(sapply(booleans, is.logical))){
      cli::cli_abort(c("x" = "The value{?s} specified in
                       {.var {names(booleans)[!sapply(booleans, is.logical)]}}
                       {?is/are} not {?a/} boolean."),
                     call = call)
    }
  }

  # Check if strings specified are proper
  if(!is.null(characters)){
    if(!all(sapply(characters, is.character))){
      cli::cli_abort(c("x" = "The value{?s} specified in
                       {.var {names(characters)[!sapply(characters, is.character)]}}
                       {?is/are} not {?a/} valid character{?s}."),
                     call = call)
    }
  }

  # Check if numerics specified are proper
  if(!is.null(numerics)){
    if(!all(sapply(numerics, is.numeric))){
      cli::cli_abort(c("x" = "The value{?s} specified in
                       {.var {names(numerics)[!sapply(numerics, is.numeric)]}}
                       {?is/are} not numeric."),
                     call = call)
    }
  }

  # Check if parameters have a unit length
  if(!is.null(unit_lengths)){
    if(!all(sapply(unit_lengths, function(x) ifelse(length(x) == 1, TRUE, FALSE)))){
      cli::cli_abort(c("x" = "The value{?s} specified in
                       {.var {names(unit_lengths)[!sapply(unit_lengths, function(x) ifelse(length(x) == 1, TRUE, FALSE))]}}
                       cannot be {?a/} vector{?s} {?it/they} should be of length 1."),
                     call = call)
    }
  }
  return(TRUE)
}

#' @keywords internal
#' Ensure the model or coefficients specified in the data preparation
#' functions are appropriate
#'
#' @keywords internal
#' @usage NULL
NULL
check_data_functions <- function(model, coefficients,
                                 call = caller_env()){
  # One of model or coefficients should be specified
  if(is.null(model) && is.null(coefficients)){
    cli::cli_abort(c("Both {.var model} and {.var coefficients} cannot be empty.",
                     "i" = "Specify a regression model object fit using {.fn lm},
                     {.fn glm}, {.fn gls}, {.fn lmer}, etc. functions or a matrix with
                     regression coefficients for calculating the predictions.",
                     "i" = "If this error is encountered when calling any of the
                     data preparation (i.e., *_data) functions, then use
                     {.var prediction = FALSE} and manually call the
                     {.help [{.fn add_prediction}](DImodelsVis::add_prediction)}
                     function later to add the predictions."),
                   call = call)
  }

  # If both model and coefficients are specified then model take precedence
  if(!is.null(model) && !is.null(coefficients)){
    cli::cli_warn(c("Both {.var model} and {.var coefficients} were specified.",
                    "i" = "Ignoring {.var coefficients} and using {.var model} for
                    calculating predictions."),
                  call = call)
  }

  # Ensure if model is specified it is a statistical regression model
  if(!is.null(model)){
    if(!insight::is_regression_model(model)){
      cli::cli_abort(c("{.var model} should be a statistical model obejct.",
                       "x" = "You specified an object with class {.cls {class(model)}}"),
                     call = call)
    }
  }

  return(TRUE)
}

#' @keywords internal
#' Ensure the experimental structures specified in plotting functions are
#' present in the model and have the appropriate type and values
#'
#' @usage NULL
NULL
check_add_var <- function(model = NULL, add_var = NULL, call = caller_env()){
  if(!is.null(add_var) && !is.list(add_var)){
    cli::cli_abort(c("{.var add_var} should be a list containing the names and
                     values for any additional variables other than compositional
                     variables in the model.",
                     "i" = "An example list could be as follows \n
                     {.code list(\"add_var1\" = c(\"value1\", \"value2\"),
                                 \"add_var2\" = c(\"value3\"))}"),
                   call = call)
  }

  # If no experimental structures were specified then do not perform any checks
  if(length(add_var) == 0){
    return(add_var)
  }

  if(!is.null(model) && insight::is_regression_model(model)){
    if (inherits(model, "DI")) {
      model_data <- model$original_data
    } else if (inherits(model, "DImulti")) {
      model_data <- attr(model, "data")
    } else {
      model_data <- insight::get_data(model)
    }
    data_col_names <- colnames(model_data)
    exp_names <- names(add_var)

    if(!all(exp_names %in% colnames(model_data))){
      cli::cli_warn(c("The names for the additional variables specified in
                      {.var add_var} should be same as the names used when
                      fitting the model.",
                      "x" = "{.var {exp_names[!(exp_names %in% data_col_names)]}}
                      {?is/are} not present in the data and will be ignored."),
                    call = call)
      add_var <- add_var[exp_names != exp_names[!(exp_names %in% data_col_names)]]
    }

    add_var <- sapply(names(add_var), function(x){
      model_var <- model_data[, x]
      value <- add_var[[x]]

      # If variable in model was of type factor ensure levels are specified
      levels_not_match <- if(length(levels(model_var)) != length(levels(as.factor(value)))) TRUE else FALSE
      if(inherits(model_var, "factor") && levels_not_match){
        lvl_str <- paste0(dQuote(unique(value)), collapse = ', ')
        mod_lvl <- paste0(dQuote(levels(model_var)), collapse = ', ')
        cli::cli_abort(c("{.var {x}} was of type {.cls {class(model_var)}} in the
                            data used to fit the model and the levels of the variable
                            specified in `add_var` do not match the levels of the
                            variable used when fitting the model.",
                         "i" = "Specify {.var {x}}  in {.var add_var} as a
                          {.cls factor} with the same levels as in the orignal data or
                          the predictions could fail.",
                         "i" = "Here, {.val {x}} had levels {.val {levels(model_var)}}
                         in the data used to fit the model, but predictions are needed
                         only for levels {.val {unique(value)}}. However we'd still have
                         to specify all levels for {.val {x}} when specifying {.val {x}}
                          in {.var add_var}. Thus `add_var` would be specified as \n
                          {.code add_var({x} = factor(c({lvl_str}),
                                                      levels = c({mod_lvl})))}
                          "),
                       call = call)
      }
      # If variable in data was of type character or factor ensure add_var is too
      if(!all(class(model_var) == class(value))){
        cli::cli_warn(c("{.var {x}} was of type {.cls {class(model_var)}} in the
                      data used to fit the model but was specified as
                      {.cls {class(add_var[[x]])}} in {.var add_var}.",
                        "i" = "Converting {.var {x}} in {.var add_var} to
                        {.cls {class(model_var)}}."),
                      call = call)

        if(is.character(model_var) || is.factor(model_var)){
          value <- as.character(value)
        } else if(is.numeric(model_var)){
          value <- suppressWarnings(as.numeric(value))

          # Fallback if numeric conversion failed
          if(any(is.na(value))){
            cli::cli_abort(c("{.var {x}} was of type {.cls {class(model_var)}} in the data,
                             but the values specified in {.var add_var} for {.var {x}}
                             can't be converted to {.cls numeric}.",
                             "i" = "Specify a numeric vector for {.var {x}} in {.var add_var}."),
                           call = call)
          }
        }
      }
      value
    }, simplify = FALSE)

    check_values <- sapply(names(add_var), function(x){
      model_var <- model_data[, x]
      values <- add_var[[x]]
      # Throwing error for a factor variable if value specified was not in the orignal data
      if(!is.numeric(model_var) && !all(values %in% unique(model_var))){
        return_value <- values[!values %in% unique(model_var)]
      } else {
        return_value <- TRUE
      }
    }, simplify = FALSE)

    if(!all(sapply(check_values, is.logical))){
      cli::cli_warn(c("The values for categorical additional variable specified in
                      {.var add_var} cannot accept values not present in the data
                      used when fitting the model.",
                      "x" = "The values specified for
                      {.var {names(check_values)[!sapply(check_values, is.logical)]}}
                      are not present in the data and will be ignored."),
                    call = call)
      glue::glue("The following values are dropped from `add_var`,
               {paste(paste(\"\u2022\", names(check_values)), check_values, sep = \": \", collapse = \"\\n\")}")

      add_var <- sapply(names(add_var), function(x){
        add_var[[x]][!add_var[[x]] %in% check_values[[x]]]
      }, simplify = FALSE)
    }
  }
  return(add_var)
}

#' @keywords internal
#' Ensure the coefficient groupings specified for response contributions are appropriate
#'
#' @usage NULL
NULL
check_coeff_groupings <- function(coefficients, groups, call = caller_env()){
  if(length(groups) > 0){
    # Ensure groups are specified as a list
    if(!is.list(groups)){
      cli::cli_abort(c("x" = "The coefficients groupings in {.var groups} should
                       be specified as a list.",
                       "i" = "{.var groups} is specified as {.cls {class(groups)}}."),
                     call = call)
    }

    # Warn if the coefficient groups are not named
    if(is.null(names(groups))){
      cli::cli_warn(c("The coefficient groups should be given names.",
                      "i" = "Naming the group{?s} {paste0(\"Group\", 1:length(groups))}
                      {?respectively}.",
                      "i" = "Provide a named list in {.var groups} to add custom names."),
                    call = call)
    }

    # Ensure there are no common coefficients between any two groups
    elements <- unlist(groups)
    if(length(unique(elements)) != length(elements)){
      counts <- data.frame(table(elements))
      dups <- counts[counts$Freq > 1, 1]
      dup_group <- sapply(groups, function(x){
        ifelse(dups[1] %in% x, TRUE, FALSE)
      })
      cli::cli_abort(c("x" = "The same coefficient cannot be present in multiple groups.",
                       "i" = "{.var {dups[1]}} is present in {.var {names(groups)[dup_group]}}"),
                     call = call)
    }

    # Ensure the name/index specified for grouping is valid
    if(is.numeric(elements)){
      if(any(elements > length(coefficients))){
        locations <- elements[elements > length(coefficients)]
        cli::cli_abort(c("Can't group coefficients past the end.",
                         "i" = "Location{?s} {as.character(locations)} do{?es/}n't exist.",
                         "i" = "There are only {length(coefficients)} coefficients."),
                       call = call)
      }
    } else {
      if(any(!(elements %in% names(coefficients)))){
        cli::cli_abort(c("Can't group coefficients that don't exist.",
                         "i" = "Coefficient{?s} {.var {elements[!(elements %in% names(coefficients))]}}
                         do{?es/}n't exist."),
                       call = call)
      }
    }
  }
  return(TRUE)
}

#' @keywords internal
#' Utility function to ensure specified value is an appropriate colour
#'
#' @importFrom grDevices col2rgb
#'
#' @usage NULL
NULL
areColours <- function(colours) {
  sapply(colours, function(colour) {
    tryCatch(is.matrix(col2rgb(colour)),
             error = function(e) FALSE)
  })
}
