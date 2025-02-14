---
title: "Tutorial: R Script for Broiler Performance Data Analysis "
layout: default
---
________________________________________

________________________________________

This guide will help you adapt and run the R script for any broiler feeding experiment performance dataset by modifying a few variables at the top of the script. Follow the instructions below to set up, understand, and execute your analysis.
________________________________________
1. Introduction
This generalized script is designed for flexibility so that you can apply it to various datasets and projects with minimal changes. It will:
•	Load your data: From an Excel file (or other formats with small modifications).
•	Preprocess your data: Convert a specified column (e.g., Treatment) to a factor.
•	Perform statistical tests: Decide between ANOVA and Kruskal-Wallis (plus post hoc comparisons) based on data normality.
•	Create plots: Generate boxplots with individual data points, annotated with p-values.
•	Save results: Output summary CSV files and save plots in both PNG and PDF formats.
________________________________________
2. Prerequisites
Before running the script, ensure you have the following:
•	R and RStudio installed: RStudio is recommended as it simplifies running and debugging scripts.
•	Required R packages: Install the packages using the commands below (only if you haven’t already):
•	install.packages("readxl")
•	install.packages("ggplot2")
•	install.packages("dplyr")
•	install.packages("tidyr")
•	install.packages("broom")
•	install.packages("car")
•	install.packages("dunn.test")
________________________________________
3. Script Overview and Structure
Below is an explanation of each section of the script. You only need to change the variables in the User Defined Variables section at the top to customize the script for your project.
3.1. User Defined Variables
At the very top of the script, you will find a section where you can define:
•	working_directory: The path to your data file.
•	data_file: The name of your data file (e.g., an Excel file).
•	treatment_column: The name of the column that represents the grouping variable (e.g., treatment groups).
•	treatment_levels: The order (levels) for the treatment groups.
•	analysis_columns: Which columns you want to analyze. If set to NULL, the script automatically selects all numeric columns (excluding the treatment column).
•	Output file names: For the summary CSV files.
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

3.2. Loading Libraries
The script then loads the required libraries. Each library provides functions for tasks such as reading data, plotting, data manipulation, and statistical tests.
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

3.3. Setting the Working Directory and Reading Data
This part sets the working directory to where your data is stored and reads the Excel file into a data frame.
# =============================================================================
# Set Working Directory and Read Data
# =============================================================================
setwd(working_directory)
# Read the data from the Excel file.
data <- read_excel(data_file)
3.4. Data Preprocessing
Here, the script converts the treatment column into a factor with the order defined by treatment_levels. It also automatically selects the numeric columns for analysis if you have not specified them manually.
# Convert the treatment column to a factor with the specified levels.
data[[treatment_column]] <- factor(data[[treatment_column]], levels = treatment_levels)

# If analysis_columns is not defined, automatically select all numeric columns (excluding the treatment column).
if (is.null(analysis_columns)) {
  analysis_columns <- names(data)[sapply(data, is.numeric) & names(data) != treatment_column]
}
3.5. Defining Helper Functions
There are three helper functions in the script:
1.	perform_stat_test:
o	Checks for normality using the Shapiro-Wilk test.
o	Runs ANOVA if the data is normal, or the Kruskal-Wallis test if not.
o	Returns a tidy summary of the test result and the original model.
2.	perform_post_hoc:
o	Runs Tukey’s HSD if ANOVA was used.
o	Runs Dunn’s test (with Bonferroni correction) if the Kruskal-Wallis test was used.
o	Returns a tidy data frame with the post hoc results.
3.	analyze_and_plot:
o	Creates a boxplot with overlaid jittered data points.
o	Calls the previous functions to perform the appropriate statistical tests.
o	Annotates and saves the plot.
o	Updates global data frames with test results.
You do not need to modify these functions unless you want to adjust the analysis steps.

3.6. Applying the Analysis and Saving Results
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
________________________________________
4. Step-by-Step Instructions for Running the Script
Step 1: Prepare Your Environment
•	Open RStudio: Launch RStudio on your computer.
•	Create a New Script: 
o	Go to File > New File > R Script.
o	Copy and paste the entire script (as provided above) into the new script file.

Step 2: Modify User Defined Variables
•	Set the Working Directory:
Change the working_directory variable to the folder where your data file is stored.
•	Set the Data File Name:
Update data_file with the name of your Excel (or CSV) file.
•	Specify the Treatment Column and Levels:
Make sure treatment_column matches your data’s grouping variable and adjust treatment_levels if needed.
•	Define Analysis Columns (Optional):
Leave analysis_columns as NULL to automatically analyze all numeric columns (except the treatment column) or specify a vector of column names to analyze.

Step 3: Install Required Packages (if necessary)
If you have not installed the necessary packages, run the following commands in the R console:
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("broom")
install.packages("car")
install.packages("dunn.test")

Step 4: Run the Script
•	Set the Working Directory:
You can use the setwd() function in the script or use RStudio’s menu:
Session > Set Working Directory > Choose Directory...
•	Execute the Script:
Click the “Source” button in the RStudio script editor or select all lines of code and press Ctrl+Enter (or Cmd+Enter on macOS) to run the script.
________________________________________
5. Reviewing the Outputs
After running the script, check the following:
•	Console Output:
The console will display messages indicating which test was used for each variable (ANOVA or Kruskal-Wallis) along with test results and any post hoc results.
•	Plots:
Look in the RStudio Plots pane to see the boxplots for each analyzed variable. Each plot is also saved in your working directory as both a PNG and a PDF file.
•	CSV Files:
The script saves two CSV files in your working directory: 
o	One with the overall test summary (analysis_summary.csv by default).
o	One with the post hoc test summary (posthoc_summary.csv by default), if any post hoc tests were performed.
________________________________________
6. Final Tips and Troubleshooting
•	Understanding Errors:
If you encounter errors, check that the variables (such as the working directory, file name, or column names) are correctly specified.
•	Adjusting the Script:
You can modify the helper functions if you want to change the analysis or plotting details.
•	Experiment:
Once you’re comfortable with the script, try changing plot themes, adding labels, or adjusting parameters to learn more about R’s capabilities.
•	Documentation:
Use ?function_name (for example, ?read_excel or ?aov) in RStudio to learn more about specific functions.


