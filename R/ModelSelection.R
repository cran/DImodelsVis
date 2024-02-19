#' @title Visualising model selection
#' @description
#' This function helps to visualise model selection by showing a
#' visual comparison between the information criteria for different models.
#' It is also possible to visualise a breakup of the information criteria into
#' deviance (goodness-of-fit) and penalty terms for each model. This could aid in
#' understanding why a parsimonious model could be preferable over a more complex model.
#'
#' @importFrom ggplot2 geom_line geom_point geom_label aes scale_colour_identity
#'                     scale_colour_manual geom_hline geom_segment arrow
#' @importFrom dplyr %>% mutate rename arrange sym group_by ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom insight is_regression_model
#'
#' @param models List of statistical regression model objects.
#' @param metric Metric used for comparisons between models. Takes values
#'               from c("AIC", "BIC", "AICc", "BICc", "logLik").
#'               Can choose a single or multiple metrics for comparing the
#'               different models.
#' @param sort A boolean value indicating whether to sort the model from
#'             highest to lowest value of chosen metric.
#' @param breakup A boolean value indicating whether to breakup the metric value
#'                into deviance (defined as -2*loglikelihood) and
#'                penalty components. Will work only if a single metric out of
#'                "AIC", "AICc", "BIC", or "BICc" is chosen to plot.
#' @param model_names A character string describing the names to display
#'                    on X-axis for each model in order they appear in the
#'                    models parameter.
#' @param plot A boolean variable indicating whether to create the plot or return
#'             the prepared data instead. The default `TRUE` creates the plot while
#'             `FALSE` would return the prepared data for plotting. Could be useful
#'             if user wants to modify the data first and then call the plotting
#'
#' @return A ggplot object or data-frame (if `plot == FALSE`)
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' ## Load data
#' data(sim2)
#'
#' ## Fit different DI models
#' mod_AV <- DI(prop = 3:6, DImodel = "AV", data = sim2, y = "response")
#' mod_FULL <- DI(prop = 3:6, DImodel = "FULL", data = sim2, y = "response")
#' mod_FG <- DI(prop = 3:6, DImodel = "FG", FG = c("G","G","L","L"),
#'              data = sim2, y = "response")
#' mod_AV_theta <- DI(prop = 3:6, DImodel = "AV", data = sim2,
#'                    y = "response", estimate_theta = TRUE)
#' mod_FULL_theta <- DI(prop = 3:6, DImodel = "FULL", data = sim2,
#'                      y = "response", estimate_theta = TRUE)
#' mod_FG_theta <- DI(prop = 3:6, DImodel = "FG", FG = c("G","G","L","L"),
#'                    data = sim2, y = "response", estimate_theta = TRUE)
#'
#' models_list <- list("AV model" = mod_AV, "Full model" = mod_FULL,
#'                     "FG model" = mod_FG, "AV model_t" = mod_AV_theta,
#'                     "Full model_t" = mod_FULL_theta,
#'                     "FG model_t" = mod_FG_theta)
#'
#' ## Specific metric
#' model_selection(models = models_list,
#'                 metric = c("AIC"))
#'
#' ## Multiple metrics can be plotted together as well
#' model_selection(models = models_list,
#'                 metric = c("AIC", "BIC"))
#'
#' ## If single metric is specified then breakup of metric
#' ## between goodness of fit and penalty can also be visualised
#' model_selection(models = models_list,
#'                 metric = c("AICc"),
#'                 breakup = TRUE)
#'
#' ## Sort models
#' model_selection(models = models_list,
#'                 metric = c("AICc"),
#'                 breakup = TRUE, sort = TRUE)
#'
#' ## If multiple metrics are specified then sorting
#' ## will be done on first metric specified in list (AIC in this case)
#' model_selection(models = models_list,
#'                 metric = c("AIC", "BIC", "AICc", "BICc"), sort = TRUE)
#'
#' ## If the list specified in models is not named then
#' ## By default the labels on the X-axis for the models will be
#' ## created by assigning a unique ID to each model sequentially
#' ## in the order they appear in the models object
#' names(models_list) <- NULL
#' model_selection(models = models_list,
#'                 metric = c("AIC", "BIC", "AICc"), sort = TRUE)
#'
#' ## When possible the variables names of objects containing the
#' ## individual models would be used as axis labels
#' model_selection(models = list(mod_AV, mod_FULL, mod_FG,
#'                               mod_AV_theta, mod_FULL_theta, mod_FG_theta),
#'                 metric = c("AIC", "BIC"), sort = TRUE)
#'
#' ## If neither of these two situations are desirable custom labels
#' ## for each model can be specified using the model_names parameter
#' model_selection(models = list(mod_AV, mod_FULL, mod_FG,
#'                               mod_AV_theta, mod_FULL_theta, mod_FG_theta),
#'                 metric = c("AIC", "BIC"), sort = TRUE,
#'                 model_names = c("AV model", "Full model", "FG model",
#'                                 "AV theta", "Full theta", "FG theta"))
#'
#' ## Specify `plot = FALSE` to not create the plot but return the prepared data
#' head(model_selection(models = list(mod_AV, mod_FULL, mod_FG,
#'                                    mod_AV_theta, mod_FULL_theta, mod_FG_theta),
#'                      metric = c("AIC", "BIC"), sort = TRUE, plot = FALSE,
#'                      model_names = c("AV model", "Full model", "FG model",
#'                                      "AV theta", "Full theta", "FG theta")))
model_selection <- function(models,
                            metric = c("AIC", "BIC", "AICc", "BICc", "deviance"),
                            sort = FALSE,
                            breakup = FALSE,
                            plot = TRUE,
                            model_names = names(models)){

  # Ensure model_names is same length as models list
  if(!is.null(model_names) & (length(model_names) != length(models))){
    cli_abort(c("{.var model_names} should be a character vector with
                same length as the {.var models} list.",
                "i" = "{.var models} has {length(models)} models while
                       {.var model_names} has {length(model_names)} names."))
  }

  # Ensure all objects in model list are regression models
  if(!all(sapply(models, function(x) {insight::is_regression_model(x)}))){
    cli_abort(c("{.var models} should be a list of regression models.",
                "i" = "{.var {names(models)[!sapply(models, function(x)
                {insight::is_regression_model(x)})]}} {?is/are} not
                {?a/} regression model object{?s}."))
  }

  # Ensure all metrics specified by user match the choices
  # Manual check because partial mapping not working due to choices
  # being too similar
  if(!all(metric %in% c("AIC", "BIC", "AICc", "BICc", "deviance"))){
    cli_abort("{.var metric} should be one of
              {.val {c('AIC', 'BIC', 'AICc', 'BICc', 'deviance')}}")
  }

  # If user didn't choose anything then AIC is default
  if(missing(metric)){
    metric <- "AIC"
  }

  # If the user has not named the list of models, give them same
  # names as the variable names
  if(is.null(model_names)){
    p_tree <- substitute(models)
    # If possible then use variables names as X-axis labels
    if(length(p_tree) > 1) {
      cli_bullets(c(">" = "The list of models specified in {.var models} is not named.",
                    ">" = "The models are given the same names as the variables they
                    were stored in.",
                    ">" = "If this is not desirable consider providing names for
                    the models in the {.var model_names} parameter."))
      names(models) <- sapply(p_tree[-1], deparse)
      # Otherwise give a unique ID to models sequentially
    } else {
      cli_bullets(c(">" = "The list of models specified in {.var models} is not named.",
                    ">" = "They are given numeric identifiers in the order they appear
                in the {.var models} parameter.",
                    ">" = "If this is not desirable consider providing names for the
                models in the {.var model_names} parameter."))
      names(models) <- paste("Model", 1:length(models))
    }
  } else {
    names(models) <- model_names
  }

  # # To allow for partial matching
  # metric <- match.arg(metric,
  #                     choices = c("AIC", "BIC", "AICc", "BICc", "deviance"),
  #                     several.ok = TRUE)

  if(length(metric) > 1 && breakup){
    cli::cli_warn(c("!" = "Can't show the breakup into deviance
                  and penalty components when multiple metrics are chosen.",
                    "*" = "Choose one of {.val AIC}, {.val AICc},{.val BIC} or {.val BICc}
                    in {.var metric} to show the breakup for.",
                    "*" = "Showing all metrics without the breakup."))
    breakup <- FALSE
  }

  if(length(metric) == 1 && metric == "deviance" && breakup){
    cli::cli_warn(c("!" = "Can't show the breakup into deviance
                  and penalty components for {.val deviance} metric.",
                    "*" = "Choose one of {.val AIC}, {.val AICc},{.val BIC} or {.val BICc}
                    in {.var metric} to show the breakup for.",
                    "*" = "Showing deviance without the breakup."))
    breakup <- FALSE
  }
  # Create data frame for plotting
  plot_data <- data.frame(model_name = names(models))
  # Add the relevant metrics
  plot_data <- plot_data %>%
                  mutate("deviance" = deviance_vec(models) %>% round(2),
                         "logLik" = -2*logLik_vec(models) %>% round(2),
                         "AIC" = AIC_vec(models) %>% round(2),
                         "BIC" = BIC_vec(models) %>% round(2),
                         "AICc" = AICc_vec(models) %>% round(2),
                         "BICc" = BICc_vec(models) %>% round(2)) %>%
    mutate(model_name = forcats::fct_inorder(.data$model_name))

  # Dashed lines for rule of 2
  if(length(metric) == 1){
    rule_of_2 <- min(plot_data[, metric]) + c(0, 2)
  }


  # Show without breakup
  if(! breakup){
    # Sort models
    if(sort){
      plot_data <- plot_data %>%
        arrange((!!sym(metric[1]))) %>%
        mutate(model_name = forcats::fct_inorder(.data$model_name))
    }

    plot_data <- plot_data %>%
      pivot_longer(cols = all_of(metric), values_to = "Value",
                   names_to = "Metric") %>%
      mutate('Metric' = forcats::fct_inorder(.data$Metric))

    pl <- ggplot(plot_data,
                 aes(x = .data$model_name, y = .data$Value,
                     colour = .data$Metric, group = .data$Metric))

    # Rule of 2
    if(length(metric) == 1){
      pl <- pl +
        geom_hline(yintercept = rule_of_2, linetype = 3, linewidth = 0.75)
        # labs(caption = paste0("The dashed lines represent the range for `rule of 2`.
        #    Models having ", metric, " values within this
        #                       band are comparable to the best model."))
    }

    # Remainder of plot
    pl <- pl +
      geom_line(linewidth = 1.5)+
      geom_point(aes(fill = .data$Metric), colour = "black",
                 size = 4, pch = 21, stroke = 1)+
      # geom_label(aes(label = .data$Value),
      #            colour = 'black', nudge_y = 1.5)+
      labs(x = "Model")+
      theme_DI()+
      theme(legend.position = 'top')+
      guides(colour = guide_legend(override.aes = list(size=3)))+
      scale_colour_manual(values = colour_blind_friendly_cols(length(metric)))+
      scale_fill_manual(values = colour_blind_friendly_cols(length(metric)))

  # Show breakup into -2*log likelihood and penalty
  } else {
    plot_data <- plot_data %>%
      mutate("Goodness of fit" = -2*logLik_vec(models) %>% round(2),
             "Penalty" = (!!sym(metric) - .data$`Goodness of fit`) %>%
                            round(2)) %>%
      pivot_longer(cols = c("Goodness of fit", "Penalty"),
                   names_to = "Component", values_to = "Value")
    # Sort models
    if(sort){
      plot_data <- plot_data %>%
        arrange((!!sym(metric[1]))) %>%
        mutate(model_name = forcats::fct_inorder(.data$model_name))
    }

    pl <- ggplot(plot_data, aes(x = .data$model_name,
                                y = .data$Value,
                                fill = .data$Component, group = 1))+
      geom_col(aes(colour = "#00000000"))+
      labs(x = "Model", y = "Value", fill = "Component")+
      scale_colour_identity(name = paste0("Metric: ", metric),
                            guide = "legend", labels = "") +
      guides(colour = guide_legend(order = 1,
                                   override.aes = list(alpha = 0, size = 0.01)),
             fill = guide_legend(order = 2))+
      theme_DI()+
      theme(legend.position = 'top')+
      scale_fill_manual(values = c("#0072B2", "#f59424"),
                        labels = c("Deviance", "Penalty"))
  }

  if(isTRUE(plot)){
    return(pl)
  } else {
    return(plot_data)
  }
}
