# =============================================================================
# Analysis of Broiler Performance Data
## =============================================================================
# =============================================================================
# User Defined Variables
# =============================================================================
# Set the working directory where your data file is located.
working_directory <- "your/working/directory/path"  # <-- Change this to your directory

# Specify your data file name. (This example uses an Excel file.)
data_file <- "your_data_file.xlsx"                 # <-- Change to your data file name

# Specify the name of the treatment (grouping) column in your dataset.
treatment_column <- "Treatment"                     # <-- Change if your treatment column has a different name

# Define the levels (order) of the treatment factor.
# Modify these levels to match your dataset.
treatment_levels <- c("CTR", "CtrDex", "JohnDex", "KitDex")  # <-- Change as needed

# Specify which columns to analyze.
# If you want to analyze all numeric columns (excluding the treatment column), leave as NULL.
# Alternatively, specify a vector of column names, e.g., c("Var1", "Var2", "Var3").
analysis_columns <- NULL

# Output file names for saving the summary results.
results_summary_file <- "analysis_summary.csv"
posthoc_summary_file   <- "posthoc_summary.csv"

# =============================================================================
# Load Necessary Libraries
# =============================================================================
library(readxl)      # For reading Excel files
library(ggplot2)     # For creating plots
library(dplyr)       # For data manipulation
library(tidyr)       # For tidying data
library(broom)       # To tidy up model outputs
library(car)         # For additional statistical tests
library(dunn.test)   # For Dunn's test post hoc comparisons

# =============================================================================
# Set Working Directory and Read Data
# =============================================================================
setwd(working_directory)

# Read the data from the Excel file.
data <- read_excel(data_file)

# Convert the treatment column to a factor with the specified levels.
data[[treatment_column]] <- factor(data[[treatment_column]], levels = treatment_levels)

# If analysis_columns is not defined, automatically select all numeric columns (excluding the treatment column).
if (is.null(analysis_columns)) {
  analysis_columns <- names(data)[sapply(data, is.numeric) & names(data) != treatment_column]
}

# =============================================================================
# Initialize Global Summary Data Frames
# =============================================================================
results_summary <- data.frame(
  Variable = character(), 
  p_value = numeric(), 
  Method = character(), 
  stringsAsFactors = FALSE
)

posthoc_summary <- data.frame(
  Variable = character(), 
  comparison = character(), 
  estimate = numeric(), 
  conf.low = numeric(), 
  conf.high = numeric(), 
  adj.p.value = numeric(), 
  comparison_type = character(), 
  stringsAsFactors = FALSE
)

# =============================================================================
# Define Helper Functions
# =============================================================================

# ---------------------------------------------------------------------------
# Function: perform_stat_test
# Description: Determines whether the data for a given variable is normally 
# distributed using the Shapiro-Wilk test. Depending on normality, it runs either:
#   - ANOVA if the data is normally distributed, or 
#   - Kruskal-Wallis test if the data is not normally distributed.
# The function returns a tidy summary of the test and the original model object.
# ---------------------------------------------------------------------------
perform_stat_test <- function(data, variable_name) {
  # Perform the Shapiro-Wilk test for normality.
  shapiro_test <- shapiro.test(data[[variable_name]])
  
  if (shapiro_test$p.value < 0.05) {
    # Data is not normally distributed; use the non-parametric Kruskal-Wallis test.
    test_result <- kruskal.test(as.formula(paste(variable_name, "~", treatment_column)), data = data)
    method_used <- "Kruskal-Wallis"
  } else {
    # Data is normally distributed; use ANOVA.
    test_result <- aov(as.formula(paste(variable_name, "~", treatment_column)), data = data)
    method_used <- "ANOVA"
  }
  
  # Tidy the test result into a data frame for easier handling.
  tidy_result <- tidy(test_result)
  tidy_result$method <- method_used
  
  return(list(test = tidy_result, model = test_result))
}

# ---------------------------------------------------------------------------
# Function: perform_post_hoc
# Description: Conducts post hoc comparisons based on the main test used.
#   - For ANOVA, Tukey's HSD test is applied.
#   - For Kruskal-Wallis, Dunn's test (with Bonferroni correction) is used.
# The function returns a tidy data frame of post hoc results.
# ---------------------------------------------------------------------------
perform_post_hoc <- function(data, variable_name, method_used, model) {
  if (method_used == "ANOVA") {
    # Perform Tukey's Honest Significant Difference test.
    posthoc_result <- TukeyHSD(model)
    posthoc_tidy <- broom::tidy(posthoc_result)
    
    # Rename and select columns for clarity.
    posthoc_tidy <- posthoc_tidy %>%
      rename(comparison = contrast) %>%
      select(-term)
    
    posthoc_tidy$comparison_type <- "TukeyHSD"
    
  } else if (method_used == "Kruskal-Wallis") {
    # Perform Dunn's test for non-parametric comparisons.
    posthoc_result <- dunn.test(x = data[[variable_name]], g = data[[treatment_column]], method = "bonferroni")
    
    # Process the comparison strings (e.g., "CTR-JohnDex") to create separate group labels.
    comp_pairs <- strsplit(posthoc_result$comparisons, "-")
    comp_first <- sapply(comp_pairs, `[`, 1)
    comp_second <- sapply(comp_pairs, `[`, 2)
    
    # Create a tidy data frame for the Dunn test results.
    posthoc_tidy <- data.frame(
      comparison = paste(comp_first, "vs", comp_second),
      estimate = NA,
      conf.low = NA,
      conf.high = NA,
      adj.p.value = posthoc_result$P.adjusted,
      comparison_type = "Dunn",
      stringsAsFactors = FALSE
    )
  } else {
    posthoc_tidy <- NULL
  }
  
  return(posthoc_tidy)
}

# ---------------------------------------------------------------------------
# Function: analyze_and_plot
# Description: For a given variable:
#   - Creates a boxplot with jittered data points.
#   - Runs the appropriate statistical test and, if necessary, a post hoc test.
#   - Annotates the plot with the p-value.
#   - Saves the plot in both PNG and PDF formats.
#   - Updates global summary data frames with test results.
# ---------------------------------------------------------------------------
analyze_and_plot <- function(var_name) {
  # Create a boxplot with individual data points for the variable.
  p <- ggplot(data, aes_string(x = treatment_column, y = var_name, fill = treatment_column)) +
    geom_boxplot(outlier.shape = NA, width = 0.4) +  # Draws the boxplot without outliers
    geom_jitter(width = 0.1, size = 1.5, shape = 21, color = "black") +  # Overlays individual data points
    labs(y = var_name, x = treatment_column) +        # Labels the axes
    theme_classic(base_size = 16) +                   # Applies a classic theme with a larger base font size
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_blank(),
          legend.position = "none")                 # Removes the legend
  
  # Run the appropriate statistical test.
  stat_results_list <- perform_stat_test(data, var_name)
  stat_results <- stat_results_list$test
  model <- stat_results_list$model
  
  # Update the global results summary with the main test results.
  results_summary <<- rbind(results_summary, 
                            data.frame(Variable = var_name, 
                                       p_value = stat_results$p.value[1], 
                                       Method = stat_results$method[1],
                                       stringsAsFactors = FALSE))
  
  # Run post hoc tests if applicable.
  posthoc_res <- perform_post_hoc(data, var_name, stat_results$method[1], model)
  if (!is.null(posthoc_res)) {
    posthoc_res$Variable <- var_name
    posthoc_res <- posthoc_res %>%
      select(Variable, comparison, estimate, conf.low, conf.high, adj.p.value, comparison_type)
    posthoc_summary <<- rbind(posthoc_summary, posthoc_res)
  }
  
  # Annotate the plot with the p-value.
  p <- p + annotate("text", x = 1, y = Inf, 
                    label = paste("p =", format(stat_results$p.value[1], digits = 3)),
                    vjust = 1.5, hjust = -0.1, color = "red", size = 6)
  
  # Save the plot as PNG and PDF files.
  ggsave(filename = paste0(var_name, ".png"), plot = p, width = 8, height = 8)
  ggsave(filename = paste0(var_name, ".pdf"), plot = p, width = 8, height = 8)
  
  # Print the test results to the console.
  message("Test used for ", var_name, ": ", stat_results$method[1])
  print(stat_results)
  
  if (!is.null(posthoc_res)) {
    cat("\nPost hoc results for", var_name, ":\n")
    print(posthoc_res)
  }
}

# =============================================================================
# Apply the Analysis to All Specified Variables
# =============================================================================
lapply(analysis_columns, analyze_and_plot)

# =============================================================================
# Save the Summary Results to CSV Files
# =============================================================================
write.csv(results_summary, results_summary_file, row.names = FALSE)

if (nrow(posthoc_summary) > 0) {
  write.csv(posthoc_summary, posthoc_summary_file, row.names = FALSE)
}


