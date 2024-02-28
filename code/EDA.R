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
#' Plots histograms for all numeric variables in the dataset to explore distributions.
#' @param df Data frame containing numeric variables.
plot_histograms <- function(df) {
  numeric_vars <- select_if(df, is.numeric) %>% names()
  
  for (var in numeric_vars) {
    print(ggplot(df, aes_string(x = var)) + 
      geom_histogram(bins = 20, fill = "blue", color = "black", na.rm = T) + 
      theme_minimal() + 
      labs(title = paste("Distribution of", var)))
  }
}


#' Boxplots for Numeric Variables by Category
#'
#' Generates boxplots for numeric variables by a specified categorical variable.
#' @param df Data frame containing the data.
#' @param category Name of the categorical variable.
boxplots_by_category <- function(df, category) {
  numeric_vars <- select_if(df, is.numeric) %>% names()
  
  for (var in numeric_vars) {
    print(ggplot(df, aes_string(x = category, y = var, fill = category)) + 
      geom_boxplot(na.rm = T) + 
      theme_minimal() + 
      labs(title = paste("Boxplot of", var, "by", category)))
  }
}


#' Scatter Plots for Numeric Variables
#'
#' Generates scatter plots for pairs of numeric variables.
#' @param df Data frame containing the data.
scatter_plots <- function(df) {
  numeric_vars <- select_if(df, is.numeric) %>% names()
  
  if (length(numeric_vars) > 1) {
    for (i in 1:(length(numeric_vars) - 1)) {
      for (j in (i + 1):length(numeric_vars)) {
        print(ggplot(df, aes_string(x = numeric_vars[i], y = numeric_vars[j])) +
          geom_point(alpha = 0.5, na.rm = T) +
          theme_minimal() +
          labs(title = paste("Scatter Plot of", numeric_vars[i], "vs", numeric_vars[j])))
      }
    }
  }
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
#' @param df Data frame containing the beverage sales data.
#' @param x_var Name of the variable representing zero-calorie beverage sales.
#' @param y_var Name of the variable representing sugary beverage sales.
#' @param color_var Name of the variable to be used for point colors, typically site.
#' @param title Plot title.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @return A ggplot object representing the scatter plot.
plot_beverage_sales_comparison <- function(df, x_var, y_var, color_var, title = "Comparison of Zero-Calorie and Sugary Beverage Sales",
                                           x_lab = "Zero-Calorie Beverage Sales", y_lab = "Sugary Beverage Sales") {
  ggplot(df, aes_string(x = x_var, y = y_var, color = color_var)) +
    geom_point() +
    theme_minimal() +
    labs(title = title, x = x_lab, y = y_lab)
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

