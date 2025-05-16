library(readxl)
library(writexl)

# Function to replace NaN and 0 values with the column average
replace_with_avg <- function(data, column_name) {
  column <- data[[column_name]]
  
  # Convert NaN to NA for consistency
  column[is.nan(column)] <- NA
  
  # Calculate the average of the column, excluding NA and 0 values
  valid_mean <- mean(column[column != 0 & !is.na(column)], na.rm = TRUE)
  
  # Replace NA and 0 values with the calculated mean
  column[is.na(column) | column == 0] <- valid_mean
  
  # Update the column in the data
  data[[column_name]] <- column
  return(data)
}

# Read the Excel file
file_path <- "data.xlsx" # Update with your file's path
data <- read_excel(file_path)

column_to_process <- "avg_weight_of_siblings"
data <- replace_with_avg(data, column_to_process)

column_to_process <- "fruit_intake"
data <- replace_with_avg(data, column_to_process)

column_to_process <- "nuts_intake"
data <- replace_with_avg(data, column_to_process)

column_to_process <- "waist"
data <- replace_with_avg(data, column_to_process)

column_to_process <- "sleep"
data <- replace_with_avg(data, column_to_process)

column_to_process <- "travel_time"
data <- replace_with_avg(data, column_to_process)

column_to_process <- "cgpa"
data <- replace_with_avg(data, column_to_process)

column_to_process <- "tea_intake"
data <- replace_with_avg(data, column_to_process)

# Save the updated data back to an Excel file
write_xlsx(data, "data.xlsx")

print("Processing completed and updated file saved as 'data.xlsx'")


# Import data
data <- read_excel("data.xlsx")

# Remove the roll_no and phone_no columns
data <- data[, !(names(data) %in% c("roll_no", "phone_no"))]

# TASK 2: Descriptive Statistics (Mean, Median, Mode, Quartiles)

# Initialize a list to store results
results <- list()

#Loop through each column
for (col_name in names(data)) {
  # Extract the column
  col_data <- data[[col_name]]
  
  # Skip if the column is not numeric
  if (!is.numeric(col_data)) next
  
  # Calculate mean, median, mode, and quartiles
  column_stats <- list(
    mean = mean(col_data, na.rm = TRUE),
    median = median(col_data, na.rm = TRUE),
    mode = as.numeric(names(sort(table(col_data), decreasing = TRUE))[1]),
    Q1 = quantile(col_data, 0.25, na.rm = TRUE),
    Q2 = quantile(col_data, 0.5, na.rm = TRUE),  # Median
    Q3 = quantile(col_data, 0.75, na.rm = TRUE)
  )
  
  # Store the results
  results[[col_name]] <- column_stats
}

# Print the results
for (col_name in names(results)) {
  cat("\nColumn:", col_name, "\n")
  print(results[[col_name]])
}

# TASK 3: Box and Whisker Plot
par(mar = c(8, 4, 2, 2))  # Adjust margins for better x-axis label visibility
boxplot(data, 
        main = "Box and Whisker Plot for All Variables", 
        las = 2,  # Rotate x-axis labels
        col = rainbow(ncol(data)))  # Different colors for each variable

# TASK 4: Scatter Plots with Weight on the Y-axis
# Check if weight exists in data
if ("weight" %in% names(data)) {
  par(mfrow = c(ceiling(sqrt(ncol(data) - 1)), ceiling(sqrt(ncol(data) - 1))))  # Adjust layout for scatter plots
  for (col_name in names(data)) {
    if (col_name != "weight" && is.numeric(data[[col_name]])) {
      plot(data[[col_name]], data$weight, 
           xlab = col_name, ylab = "Weight", 
           main = paste("Weight vs", col_name), 
           col = "blue", pch = 16)
    }
  }
  par(mfrow = c(1, 1))  # Reset plotting layout
}

# TASK 5: MLRM for Weight with Significant Variables
mlrm_model <- lm(weight ~tea_intake +cgpa + sleep + fruit_intake + avg_weight_of_siblings + nuts_intake + travel_time + waist, data = data)

# Summary of the model
cat("\nMLRM Summary:\n")
print(summary(mlrm_model))

# Diagnostic Plots for MLRM
par(mfrow = c(2, 2))  # Set grid for plots
plot(mlrm_model)
par(mfrow = c(1, 1))  # Reset plotting layout
# 
# 
# #now make the model with only the significant variables
# mlrm_model <- lm(weight ~nuts_intake + travel_time + waist, data = data)
# 
# # Summary of the model
# cat("\nMLRM Summary:\n")
# print(summary(mlrm_model))
# 
# # Diagnostic Plots for MLRM
# par(mfrow = c(2, 2))  # Set grid for plots
# plot(mlrm_model)
# par(mfrow = c(1, 1))  # Reset plotting layout
