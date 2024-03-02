#' Summarize Data
#'
#' Provides a summary for numeric and categorical variables in the dataset.
#' @param df Data frame to summarize.
#' @return A list containing summaries for numeric and categorical variables.
summarize_data <- function(df) {
  numerical_summary   <- df %>% select_if(is.numeric) %>% summary()
  categorical_summary <- df %>% select_if(is.character) %>% summary()
  list(numerical = numerical_summary, categorical = categorical_summary)
}


#' Plot Histograms for Numeric Variables
#'
#' Plots histograms for specified numeric variables in the dataset to explore distributions.
#'
#' @param df        Data frame containing numeric variables.
#' @param var_names Names of the variables to be plotted.
plot_histograms <- function(df, var_names) {
  numeric_vars <- select_if(df, is.numeric) %>% names()
  
  if (!all(var_names %in% numeric_vars)) {
    stop("All variables in 'var_names' must be numeric.")
  }
  
  df_long <- df %>%
    pivot_longer(cols = var_names, names_to = "Variables", values_to = "Sales") %>%
    mutate(Variables = factor(Variables, levels = var_names))
  
  p <- ggplot(df_long, aes(x = Sales, fill = Variables)) + 
    geom_histogram(bins = 20, color = "black", na.rm = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal() +
    facet_wrap(~Variables, scales = "free", ncol = length(var_names)) + 
    theme(legend.position = "none") +
    labs(title = "Distribution of Selected Variables")
  
  print(p)
}




#' Boxplots for Numeric Variables by Category
#'
#' Generates boxplots for numeric variables by a specified categorical variable.
#'
#' @param df Data frame containing the data.
#' @param responses the variables to be plotted as response in boxplots
#' @param category Name of the categorical variable.
boxplots_by_category <- function(df, responses, category) {
  numeric_vars <- select_if(df, is.numeric) %>% names()
  
  if (!all(responses %in% numeric_vars)) {
    stop("All variables in 'responses' must be numeric.")
  }
  
  # Convert the category to a factor based on its original order in the dataframe
  df[[category]] <- factor(df[[category]], levels = unique(df[[category]]))
  
  # Create a long format data frame suitable for faceting
  df_long <- df %>%
    pivot_longer(cols = responses, names_to = "Response", values_to = "Sales")
  
  p <- ggplot(df_long, aes(x = !!sym(category), y = Sales, fill = !!sym(category))) + 
    geom_boxplot(na.rm = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal() +
    facet_wrap(~Response, scales = "free_y", nrow = length(responses)) + 
    theme(legend.position = "bottom") + 
    labs(title = paste("Boxplot of responses by", category))
  
  print(p)
}



#' Summarize Sales by Dynamic Category
#'
#' Summarizes total sales by a specified category (e.g., Site, Intervention).
#' @param df Data frame containing the sales data.
#' @param category The name of the column to group by, as a string.
#' @return A data frame with the total sales summarized by the specified category.
summarise_sales <- function(df, category) {
  stat <- df %>%
    group_by(.data[[category]]) %>%
    summarise(Total_Sales = sum(Total, na.rm = TRUE), .groups = 'drop')
  return(stat)
}

#' Handle Missing Data 
#'
#' Provides an overview and simple strategies for handling missing data.
#' @param df Data frame with potential missing values.
#' @return Data frame with missing values handled.
handle_missing_data <- function(df) {
  # Overview of missing data
  missing_overview <- sapply(df, function(x) sum(is.na(x)))
  
  # Strategies for imputation or removal?
  
  return(list(MissingOverview = missing_overview, CleanedData = df))
}



#' Plot Scatter for Zero-Calorie vs Sugary Beverage Sales
#'
#' Creates a scatter plot to visualize the relationship between zero-calorie and sugary beverage sales, colored by site.
#'
#' @param df Data frame containing the beverage sales data.
#' @param x_var Name of the variable representing zero-calorie beverage sales.
#' @param y_var Name of the variable representing sugary beverage sales.
#' @param color_var Name of the variable to be used for point colors, typically site.
#'
#' @return A ggplot object representing the scatter plot.
plot_beverage_sales_comparison <- function(df, x_var, y_var, color_var) {
  df <- df %>%
    mutate(!!color_var := factor(!!sym(color_var), levels = unique(df[[color_var]]))) %>%
    drop_na(!!sym(x_var), !!sym(y_var), !!sym(color_var))
  
  p <- ggplot(df, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(color_var))) + 
    geom_point(alpha = 0.5, na.rm = T) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8) +
    theme_minimal() +
    theme(legend.position = "bottom") + 
    labs(title = paste("Comparison of", x_var, "and", y_var, "Sales"),
         x = x_var, 
         y = y_var)
  
  print(p)
}


#' Plot Time Series of Beverage Sales
#'
#' Creates a line plot to visualize the sales of zero-calorie and sugary beverages over time,
#' differentiated by site. Marks the start of each intervention with a dashed line and label.
#'
#' @param df Data frame containing the sales data.
plot_sales_time_series <- function(df) {
  interventions <- unique(df$Intervention)
  intervention_starts <- sapply(interventions, function(x) which(df$Intervention == x)[1])
  intervention_df <- data.frame(start = intervention_starts, label = interventions)
  intervention_df$label <- c("", "dismes", "", "dis", "", "follow", "cal", "exer", "both")
  
  df$Site <- factor(df$Site, levels = unique(df$Site))
  
  p <- ggplot(df, aes(x = Count)) +
    geom_line(aes(y = ZeroCal, color = "ZeroCal"), na.rm = TRUE) +
    geom_line(aes(y = Sugary, color = "Sugary"), na.rm = TRUE) +
    scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8) +
    geom_vline(data = intervention_df, aes(xintercept = start), linetype = "dashed") +
    geom_text(data = intervention_df, aes(x = start+ 10, y = 0, label = label), vjust = -5.5) +
    facet_wrap(~Site, ncol = 1, scales = "free_y") +
    theme_minimal() +
    labs(title = "Sales Over Time by Site", x = "Day", y = "Sales", color = "Beverage Type") +
    theme(legend.position = "top") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))
  
  print(p)
}




#' Plot Missing Data and Reflect Balance
#' 
#' Creates a plot visualizing missingness summaries and total number of 
#' observations faceted by site or intervention.
#' @param df Data frame containing the beverage sales data.
#' @param group_by_site Boolean reflecting desire to group by site variable. 
#' @param group_by_intervention Boolean reflecting desire to group by intervention variable. 
#' @param title Beginning of plot title.
#' @return A ggplot2 object representing missing data and balance between groups. 
plot_missing_data <- function(df, group_by_site= TRUE, group_by_intervention = TRUE,
                              title = "Missing Data and Observation Count by") {
  if(group_by_site) {
    df %>% vis_miss(facet=Site) + ggtitle(paste0(title, " ", "Site")) 
  } else if(group_by_intervention) {
    df %>% vis_miss(facet=Intervention) + ggtitle(paste0(title, " ", "Intervention")) 
  }
}


#' Plot correlation between variables
#' 
#' Creates a correlation plot based on the numerical variables within a dataframe 
#' @param df Data frame containing the beverage sales data.
#' @param index Boolean reflecting if an index variable exist within dataframe
#' @return A corrplot object representing correlation between all numeric variables
plot_corr_plot <- function(df, index = TRUE) {
  numeric_vars <- select_if(df, is.numeric) %>% names()
  if(index){
    corrplot.mixed(cor(df[select_if(beverage_sales, is.numeric) %>% names()][-1], use = "complete.obs"), order = 'AOE')
  } else {
    corrplot.mixed(cor(df[select_if(beverage_sales, is.numeric) %>% names()], use = "complete.obs"), order = 'AOE')
  }
  
}