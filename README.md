# **Student Performance Analysis**

## **Dataset Description**

The dataset contains information about students with the following columns:

- `student_id`
- `school`
- `sex`
- `age`
- `family_size`
- `parent_status`
- `mother_education`
- `travel_time`
- `study_time`
- `class_failures`
- `school_support`
- `family_support`
- `extra_paid_classes`
- `higher_ed`
- `free_time`
- `health`
- `absences`
- `final_grade`

=> The dataset includes 395 rows of student data.

## **Data Pre-processing**

- **Outlier Detection:** Calculated Interquartile Range (IQR) and removed outliers.
- **Data Correction:** Corrected quantitative variables and qualitative data types.
- **Missing Values:** Imputed missing values using k-Nearest Neighbors (kNN).

## **Data Analysis**

- **Univariate Analysis:** Explored individual variables to understand their distributions and characteristics.
- **Bivariate Analysis:** Analyzed relationships between pairs of variables to identify potential correlations.

## **Statistical Modeling**

- **Hierarchical Ascending Classification (HAC):** Applied HAC, an unsupervised machine learning technique, to group similar data points into clusters, forming a hierarchical structure.

## **Technologies Used**

- **R Packages:**
  - `readxl` – For reading Excel files
  - `VIM` – For visualizing and imputing missing values
  - `patchwork` – For combining ggplot2 plots
  - `randomForest` – For random forest modeling
  - `forcats` – For working with categorical variables
  - `corrplot` – For correlation matrix visualization
  - `ggplot2` – For data visualization
  - `dplyr` – For data manipulation
  - `tidyr` – For data tidying


