install.packages("readxl")
install.packages("VIM")
install.packages("patchwork")
install.packages("randomForest")
install.packages("forcats")
install.packages("corrplot")
library(corrplot)
library(forcats)
library(randomForest)
library(readxl)
library(VIM)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


# Read the data from an Excel file, skip the first row, treat "NA" as missing values, and provide column names
data <- read_excel(file.choose(), skip = 2, na = "NA", col_names = FALSE)

#See dataset 
data

# Define column names
col_names_vector <- c("student_id", "school", "sex", "age", "family_size", "parent_status", "mother_education", "travel_time", "study_time", "class_failures", "school_support", "family_support", "extra_paid_classes", "higher_ed", "free_time", "health", "absences", "final_grade")

# Set column names
colnames(data) <- col_names_vector

# Display the structure of the data to verify column types
str(data)

# Display summary statistics
summary(data)

# Select relevant numeric variables for the box plot
numeric_vars <- c("age", "class_failures", "free_time", "health", "absences", "final_grade")

# Box plot for all numeric variables in one plot
boxplot(data[numeric_vars], main = "Box Plots of Numeric Variables", ylab = c("Age", "Class Failures", "Free_Time", "Health", "Absences", "Final_Grade"))


for (var in numeric_vars) {
  q <- quantile(data[[var]], c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(data[[var]], na.rm = TRUE)
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  outliers <- data[[var]] < lower_bound | data[[var]] > upper_bound
  data[[var]][outliers] <- NA
}

# Identify columns with missing values
boxplot(data[numeric_vars], main = "Box Plots of Numeric Variables", ylab = c("Age", "Class Failures", "Free_Time", "Health", "Absences", "Final_Grade"))

# Impute missing values using k-nearest neighbors from the VIM package
missing_cols <- colnames(data)[colSums(is.na(data)) > 0]
print(missing_cols)

# Calculate the percentage of missing values in each column
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100
print(missing_percentage)

# Replace imputed columns in the original dataset
data_imputed <- kNN(data[, missing_cols], k = 5)

# Replace imputed columns in the original dataset
data[, missing_cols] <- data_imputed[, missing_cols]

# Display the structure of the data after imputation
str(data)

# Display summary statistics after imputation
summary(data)

# Box plot for all numeric variables in one plot after imputation
boxplot(data[numeric_vars], main = "Box Plots of Numeric Variables (After Imputation)", ylab = c("Age", "Class Failures", "Free_Time", "Health", "Absences", "Final_Grade"))

# Check again the percentage of missing values in each column
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100
print(missing_percentage)

library(dplyr)
# Drop the 'student_id' column
data <- data[, -which(names(data) == "student_id")]

#drop class_failure cause it composed now of only 0s
data <- data[, -which(names(data) == "class_failures")]
########################Graphical presentation########

# **Quantitative Variables Univariate Analysis**

# We selected the quantitative variables to use for our analysis
quantitative_vars <- c("age", "free_time", "health", "absences", "final_grade")


# We implemented a function that loops through the list of quantitative variables and run 
# the Shapiro test to check the normality of our quantitative data and plots
# graphical representation for each one

analyze_quantitative_variables <- function(data, quantitative_vars) {
  plots_list <- list()

  for (var in quantitative_vars) {
    unique_values <- unique(data[[var]])
    is_continuous <- length(unique_values) > 10  # Adjust the threshold as needed

    if (length(unique(data[[var]])) > 1) {
      if (is_continuous) {
        # Create histogram for continuous variables
        plot <- ggplot(data, aes(x = .data[[var]])) +
          geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
          theme_minimal() +
          theme(legend.position = "none")
      } else {
        # Create bar plot for discrete variables
        plot <- ggplot(data, aes(x = factor(.data[[var]]))) +
          geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
          labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
          theme_minimal() +
          theme(legend.position = "none")
      }

      # Add the plot to the list
      plots_list[[var]] <- plot

      # Shapiro-Wilk test
      if (is_continuous) {
        shapiro_test <- shapiro.test(data[[var]])
        cat("Shapiro-Wilk Test for", var, ":\n")
        print(shapiro_test)
      } else {
        cat("Variable", var, "has identical values. Skipping normality test.\n\n")
      }
    }
  }

  # Arrange and display all histograms in one window
  multiplot_quantitative <- wrap_plots(plots_list, ncol = 2)
  print(multiplot_quantitative)
}


# Here we call our implemented function that take as a parameter the variables and the treated data.
analyze_quantitative_variables(data, quantitative_vars)


##################################################################################################
#################################################

# **Qualitative Variables Univariate Analysis**

# We selected the qualitative variables to use for our analysis in a For loop

qualitative_vars <- c("school", "sex", "family_size", "parent_status", "mother_education", 
                      "travel_time", "study_time", "school_support", "family_support", 
                      "extra_paid_classes", "higher_ed")

# Initialize an empty list to store plots
plots_list_qualitative <- list()

# Create bar charts and display frequency for qualitative variables
for (var in qualitative_vars) {
  # Print the frequency table
  cat("Frequency table for", var, ":\n")
  print(table(data[[var]]))
  
  # Create bar chart
  plot <- ggplot(data, aes(x = factor(.data[[var]]))) +
    geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = paste("Bar Chart of", var), x = var, y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Add the plot to the list
  plots_list_qualitative[[var]] <- plot
}

# Arrange and display all qualitative plots in one window
multiplot_qualitative <- wrap_plots(plots_list_qualitative, ncol = 2)
print(multiplot_qualitative)

######################mean and variance test#########################

##########Check how many samples i have 
num_samples <- nrow(data)

# Print the number of samples
cat("Number of samples in the dataset:", num_samples, "\n")

#########split my samples in two 
set.seed(123)  # Setting seed for reproducibility

# Generate a vector of indices to split the data into two parts
split_indices <- sample(1:nrow(data), size = nrow(data) / 2)

# Split the data into two parts
data_part1 <- data[split_indices, ]
data_part2 <- data[-split_indices, ]


nrow(data_part1)
nrow(data_part2)

 

############check the normality of each quantitave variable in the first part

check_normality <- function(data_part) {
  quantitative_vars <- sapply(data_part, is.numeric)
  
  for (var in names(quantitative_vars)[quantitative_vars]) {
    shapiro_test <- shapiro.test(data_part[[var]])
    cat("Shapiro-Wilk Test for", var, ":\n")
    print(shapiro_test)
    cat("\n")
  }
}

# Example usage
check_normality(data_part1)
check_normality(data_part2)

#the numerical  variables are not normal 

quantitative_vars1 <- sapply(data_part1, is.numeric)
quantitative_vars2 <- sapply(data_part2, is.numeric)

# Find the common numeric variables
common_vars <- intersect(names(quantitative_vars1)[quantitative_vars1], names(quantitative_vars2)[quantitative_vars2])

# Scale each common numeric variable in data_part1
scaled_data1 <- as.data.frame(lapply(data_part1[, common_vars], scale))

# Scale each common numeric variable in data_part2
scaled_data2 <- as.data.frame(lapply(data_part2[, common_vars], scale))

# Example usage
head(scaled_data1)
head(scaled_data2)

#recheck nomality
check_normality(scaled_data1)
check_normality(scaled_data2)


#//////compare mean and variances of numerical variables of the two samples except age


#mean
perform_wilcox_test <- function(data1, data2, numeric_vars) {
  results <- list()
  
  for (var in numeric_vars) {
    cat("Wilcoxon Rank Sum Test for", var, ":\n")
    result <- wilcox.test(data1[[var]], data2[[var]])
    print(result)
    results[[var]] <- result
    cat("\n-------------------------------------------\n")
  }
  
  return(results)
}

numeric_vars <- names(data_part1)[sapply(data_part1, is.numeric)]

wilcox_results <- perform_wilcox_test(data_part1, data_part2, numeric_vars)

#variance
perform_bartlett_test <- function(data1, data2, numeric_vars) {
  results <- list()
  
  for (var in numeric_vars) {
    cat("Bartlett's Test for Homogeneity of Variance for", var, ":\n")
    result <- stats::bartlett.test(list(data1[[var]], data2[[var]]))
    print(result)
    results[[var]] <- result
    cat("\n-------------------------------------------\n")
  }
  
  return(results)
}

numeric_vars <- names(data_part1)[sapply(data_part1, is.numeric)]

bartlett_results <- perform_bartlett_test(data_part1, data_part2, numeric_vars)

#//////compare proportions for qualitative  variables of the two samples except age
compare_proportions <- function(data_part1, data_part2, qualitative_vars) {
  results <- data.frame(
    Variable = character(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )

  for (var in qualitative_vars) {
    # Create contingency table
    table_part1 <- table(data_part1[[var]])
    table_part2 <- table(data_part2[[var]])

    # Combine tables with matching levels
    combined_levels <- union(names(table_part1), names(table_part2))
    combined_table <- data.frame(
      Level = factor(combined_levels),
      Count_Part1 = as.numeric(table_part1[combined_levels]),
      Count_Part2 = as.numeric(table_part2[combined_levels])
    )
    
    # Ensure that both counts and total counts have the same length
    combined_counts <- c(combined_table$Count_Part1, combined_table$Count_Part2)
    total_counts <- rep(c(sum(combined_table$Count_Part1), sum(combined_table$Count_Part2)), each = length(combined_levels))
    

    # Perform proportion test
    prop_test_result <- prop.test(combined_counts, total_counts)

    # Add results to the data frame
    results <- rbind(results, data.frame(Variable = var, P_Value = prop_test_result$p.value))
  }

  return(results)
}

comparison_results_proportions <- compare_proportions(data_part1, data_part2, qualitative_vars)
print(comparison_results_proportions)

#all pvalues are <0.05 so in our case the proportion of te first sample is smaller than the proportion of the second sample 
##############################Bivariate analysis : corrolation study############################
# For Quantitative variables
quantitative_vars <- c("age", "free_time", "health", "absences", "final_grade")

# For Quanlitative variables
categorical_vars <- c("school", "sex", "family_size", "parent_status", "mother_education", "travel_time", "study_time", "school_support", "family_support", "extra_paid_classes", "higher_ed")

##------------------ Categorical Data Encoding:
# Display unique values for each qualitative variable
for (var in categorical_vars) {
  unique_values <- unique(data[[var]])
  cat("Unique values for", var, ":", "\n")
  print(unique_values)
  cat("\n")
}
#=> We notice the existence of certain ordinal relationships among the categories so we are using label encoding

# Encode categorical variables using label encoding
encoded_data <- data[categorical_vars]

for (var in categorical_vars) {
  encoded_data[[var]] <- as.integer(fct_recode(factor(data[[var]]), .missing = "0"))
}

# Calculate correlation matrix for the encoded data
correlation_matrix_encoded <- cor(encoded_data)
correlation_matrix_encoded

# Customize the appearance of the correlation plot
corrplot(
  correlation_matrix_encoded, 
  method = "color",  # Specify the method for visualizing correlation ("color" for colors)
  type = "full",     # Show the full matrix (upper and lower triangles)
  addCoef.col = "black",  # Add color to the correlation coefficient labels
  tl.col = "black",   # Set color for text labels
  tl.srt = 45,        # Rotate text labels by 45 degrees for better readability
  diag = FALSE       # Exclude the diagonal (correlation of variables with themselves)
)

# Calculate correlation matrix for the quantitative variables
# Subset the data to include only quantitative variables
quantitative_data <- data[quantitative_vars]

# Calculate correlation between all pairs of quantitative variables
Scorrelation_matrix <- cor(quantitative_data)
correlation_matrix

corrplot(
  correlation_matrix, 
  method = "color",  # Specify the method for visualizing correlation ("color" for colors)
  type = "full",     # Show the full matrix (upper and lower triangles)
  addCoef.col = "black",  # Add color to the correlation coefficient labels
  tl.col = "black",   # Set color for text labels
  tl.srt = 45,        # Rotate text labels by 45 degrees for better readability
  diag = FALSE       # Exclude the diagonal (correlation of variables with themselves)
)

#Analysis:
#Age and Final Grade: There's a modest negative correlation between age and final_grade (-0.164). 
#This implies that as age increases, there's a slight tendency for the final_grade to decrease.

#Free Time and Final Grade: Free_time has a negligible correlation with final_grade (-0.003). 
#This indicates a very weak or no linear relationship between free_time and final_grade.

#Health and Final Grade: Health shows a negligible correlation with final_grade (-0.061), 
#suggesting a weak or limited linear relationship between health status and final_grade.

#Absences and Final Grade: Absences display a slight positive correlation with final_grade (0.084). 
#This indicates a small tendency for higher absences to be associated with slightly higher final grades.


#--------------------------Bivariate analysis (hypo testig) : quantitative variables----------------------


# Shapiro-Wilk test results for quantitative variables 

# Shapiro-Wilk test for normality
for (var in quantitative_vars) {
  shapiro_test <- shapiro.test(data[[var]])
  print(paste("Shapiro-Wilk Test for", var))
  print(shapiro_test)
}

shapiro_wilk_results <- list(
  age = c(W = 0.91462, p = 3.219e-14),
  free_time = c(W = 0.87437, p = 2.2e-16),
  health = c(W = 0.85008, p = 2.2e-16),
  absences = c(W = 0.84434, p = 2.2e-16),
  final_grade = c(W = 0.98091, p = 8.836e-13)
)

#=> None of the variables have p-values greater than 0.05, indicating that none of 
#them are considered normally distributed based on the Shapiro-Wilk 
#test at a significance level of 0.05.

#------------------Scaling quantiative var
# Scale the quantitative variables in your dataset
scaled_data <- data  # Make a copy of your original data

# Scaling the quantitative variables
scaled_data[quantitative_vars] <- scale(scaled_data[quantitative_vars])

# View the scaled data
head(scaled_data)

#----------------Retest the normality after scaling:
# Shapiro-Wilk test for normality

for (var in quantitative_vars) {
  shapiro_test <- shapiro.test(scaled_data[[var]])
  print(paste("Shapiro-Wilk Test for", var))
  print(shapiro_test)
}

# => In our case, this transformation didnt change the distribution's shape,
#it only changed the scale. So the data remain not Normal

#----------------Applying non parametric tests:

# For Quantitative variables without target feature:

quantitative_vars1 <- c("age", "free_time", "health", "absences")

# Perform Wilcoxon test for all quantitative variables against 'final_grade'
for (var in quantitative_vars1) {
  if (var != "final_grade") {
    # Print message indicating the test being performed
    print("----------------------------------------------------")
    cat("Wilcoxon Rank-Sum Test between", var, "and final_grade\n")
    
    # Perform Wilcoxon rank-sum test for each variable against 'final_grade'
    wilcox_test <- wilcox.test(data[[var]], data$final_grade)
    
    # Print results
    print(wilcox_test)
    
    # Interpretation based on p-value
    if (wilcox_test$p.value > 0.05) {
      print("Result: Accept H0 - No significant difference between the variables: the variable doesnt affects the target")
    } else {
      print("Result: Reject H0 - There is a significant difference between the variables, the variable affects the target")
    }
  }
}


#--------------------------Bivariate analysis (hypo testig) : qualitative variables----------------------

#=> Our target feature is quantitaive, so no need to perform qualitative variables hypo testig


#------------------------Bivariate analysis (hypo testig) : qualitative-quantitative variables


library(car)

#######################normality test for qualitative variables
qualitative_variables <- c("school", "sex", "family_size", "parent_status", "mother_education", 
                           "travel_time", "study_time", "school_support", "family_support", 
                           "extra_paid_classes", "higher_ed")

continuous_variable <- data$final_grade
for (variable in qualitative_variables) {
  cat("Shapiro-Wilk test for", variable, ":\n")
  for (group in unique(data[[variable]])) {
    subset_data <- continuous_variable[data[[variable]] == group]
    result <- shapiro.test(subset_data)
    cat("Group:", group, "- p-value:", result$p.value, "\n")
  }
  cat("\n")
}


#The normality test has proven that all qualitative variables 
#do not follow the normal distribution*

########################homogenity test for qualitative variables
for (variable in qualitative_variables) {
  cat(paste("Unique categories for", variable, ":", "\n"))
  cat(unique(data[[variable]]), "\n\n")
}
# Perform bartlett's test for each qualitative variable
for (variable in qualitative_variables) {
  grouping_variable <- data[[variable]]
  cat("homogenity 's test for", variable, ":\n")
  result <- bartlett.test(continuous_variable ~ grouping_variable)
  print(result)
  cat("\n")
}

# All the qualitative variables are homogtenous except the 
#variables :“school_support” and “extra_paid_classes”

###### hypothesis testing ######
perform_wilocox_kruskal <- function(data, continuous_variable, grouping_variable) {
  num_groups <- length(unique(data[[grouping_variable]]))

  # If either normality or homogeneity is not satisfied
  if (num_groups == 2) {
    cat("Performing Wilcoxon test:\n")
    result <- wilcox.test(data[[continuous_variable]] ~ data[[grouping_variable]])
  } else {
    cat("Performing Kruskal-Wallis test:\n")
    result <- kruskal.test(data[[continuous_variable]] ~ data[[grouping_variable]])
  }

  # Display the test result
  print(result)

  # Display the interpretation based on the p-value
  if (result$p.value > 0.05) {
    cat("p-value > 0.05, so we accept H0:\n")
    cat(paste("The variable", grouping_variable, "doesn't have an influence over the", continuous_variable, "\n\n"))
  } else {
    cat("p-value < 0.05, so we accept H1:\n")
    cat(paste("The variable", grouping_variable, "has an influence over the", continuous_variable, "\n\n"))
  }
}

# Perform this function that tests the qualitative variables 
# compared to our target variable final_grade. So, I want a function that
# performs ANOVA if the homogeneity and normality on all modalities of a variable 
# are verified. If either one of them is not verified, it checks the number of modalities. 
# If it's 2, it uses Wilcoxon for hypothesis testing; if it's greater, use Kruskal-Wallis.
#in our case will use only non parametric tests
qualitative_variables <- c("school", "sex", "family_size", "parent_status", "mother_education", 
                           "travel_time", "study_time", "school_support", "family_support", 
                           "extra_paid_classes", "higher_ed")

continuous_variable <- "final_grade"

for (variable in qualitative_variables) {
  cat("Testing for", variable, ":\n")
  perform_wilocox_kruskal(data, continuous_variable, variable)
  cat("\n-------------------------------------------\n")
}

#Thevariables have 2 modalities so we used wilcoxon test:
#the p_values >0.05 then the variables dont influence 
#the target variable except the variables:”sex”,”school_support” and “higher_ed”
#These variables have more than 2 modalities so we used kruskall test:
#the p_values >0.05 then the variables dont influence 
#the target variable except the variables:”mother_education”

############################research part##########################

# Choose a distance metric (e.g., Euclidean) and linkage method (e.g., complete linkage)
hac_model <- hclust(dist(data[, numeric_vars]), method = "complete")

# Define colors for each numeric variable
variable_colors <- rainbow(length(numeric_vars))

# Plot dendrogram with column names
plot(hac_model, main = "Dendrogram for HAC", xlab = "Students", sub = NULL)

# Add column names to the leaves of the dendrogram with different colors
rect.hclust(hac_model, k = length(numeric_vars), border = variable_colors)

# Add legend for column names and their corresponding colors
legend("topright", legend = numeric_vars, fill = variable_colors, title = "Numeric Variables")

#\\\\\\\\\\\\\\\\\\\\\\\part2\\\\\\\\\\\\\\\\\\\\\
# Install and load the randomForest package

# Create a random forest model
rf_model <- randomForest(final_grade ~ .-final_grade, data = data)

# Display the importance of each variable
print(rf_model)
summary(rf_model)
# Plot variable importance
varImpPlot(rf_model)

##The importance of a variable is measured by how much the 
#model’s predictions change when the variable is permuted. 
#The higher the mean decrease Gini, the more important the variable is.



