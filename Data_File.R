# Load Required Libraries
library(readxl)     # For reading Excel files
library(dplyr)      # For data wrangling
library(ggplot2)    # For visualization

# Load the Chemical-Level Emissions Data
chem_data <- read_excel("l_o_freq_request_data.xlsx", sheet = "Emissions from P&T Proc by Chem")

# Preview the first few rows
head(chem_data)

# Check actual column names for reference
colnames(chem_data)

# Rename columns for convenience (use your exact column names as found above)
chem_data <- raw_chem_data %>%
  filter(!is.na(`Fluorinated GHG Name`), !is.na(`Fluorinated GHG Emissions (metric tons)`)) %>%
  rename(
    year = `Reporting Year`,
    facility_id = `Facility ID`,
    facility_name = `Facility Name`,
    chemical_name = `Fluorinated GHG Name`,
    chemical_formula = `Chemical Formula`,
    emissions_mt = `Fluorinated GHG Emissions (metric tons)`
  ) %>%
  filter(!is.na(emissions_mtco2e))  # Keep only rows with emission data

# Confirm that you now have real numbers
summary(chem_data$emissions_mtco2e)
head(chem_data, 5)


# Quick summary of emissions
summary(chem_data$emissions_mt)
summary(chem_data$emissions_mtco2e)

# Check for unique chemicals and facilities
cat("Unique chemicals:", length(unique(chem_data$chemical_name)), "\n")
cat("Unique facilities:", length(unique(chem_data$facility_name)), "\n")

# Load necessary libraries (if not already loaded)
library(ggplot2)
library(dplyr)

# 1. Summary statistics for emissions
cat("Summary: Emissions (metric tons)\n")
print(summary(chem_data$emissions_mt))
cat("\nSummary: Emissions (mt CO2e)\n")
print(summary(chem_data$emissions_mtco2e))

# 2. Top 10 chemicals by total CO2-equivalent emissions
top_chemicals <- chem_data %>%
  group_by(chemical_name) %>%
  summarize(total_mtco2e = sum(emissions_mtco2e, na.rm = TRUE)) %>%
  arrange(desc(total_mtco2e)) %>%
  slice(1:10)

print(top_chemicals)

# 3. Bar plot: Top 10 chemicals by total emissions (mt CO2e)
ggplot(top_chemicals, aes(x = reorder(chemical_name, total_mtco2e), y = total_mtco2e)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Chemicals by Total Emissions (mt CO2e)",
    x = "Chemical Name",
    y = "Total Emissions (mt CO2e)"
  ) +
  theme_minimal()

# 4. Yearly trend: Total emissions per year (all chemicals combined)
yearly_trend <- chem_data %>%
  group_by(year) %>%
  summarize(total_emissions = sum(emissions_mtco2e, na.rm = TRUE))

ggplot(yearly_trend, aes(x = as.factor(year), y = total_emissions)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Total Chemical Emissions by Year",
    x = "Year",
    y = "Total Emissions (mt CO2e)"
  ) +
  theme_minimal()

# 5. Histogram: Distribution of chemical emissions (mt CO2e)
ggplot(chem_data, aes(x = emissions_mtco2e)) +
  geom_histogram(bins = 40, fill = "purple", color = "white") +
  labs(
    title = "Distribution of Chemical Emissions (mt CO2e)",
    x = "Emissions (mt CO2e)",
    y = "Count"
  ) +
  theme_minimal()


#model 

# Load necessary library
library(dplyr)

# Ensure predictors are factors
chem_data$chemical_name <- as.factor(chem_data$chemical_name)
chem_data$facility_name <- as.factor(chem_data$facility_name)
chem_data$year <- as.factor(chem_data$year)

# Optional: Check target variable distribution
hist(chem_data$emissions_mtco2e, breaks = 40, main = "Emissions (mt CO2e)", xlab = "mt CO2e")

# Optional: Log-transform target for skewness
chem_data$log_emissions <- log1p(chem_data$emissions_mtco2e)  # log1p handles zeros safely

# Fit multiple linear regression model
model <- lm(log_emissions ~ chemical_name + facility_name + year, data = chem_data)

# Show model summary (coefficients, p-values, R-squared, etc.)
summary(model)

# Predict (in log scale), then transform back to original scale
chem_data$predicted_log_emissions <- predict(model, newdata = chem_data)


chem_data$predicted_emissions <- expm1(chem_data$predicted_log_emissions)  # expm1 undoes log1p

# Model evaluation: RMSE
rmse <- sqrt(mean((chem_data$emissions_mtco2e - chem_data$predicted_emissions)^2, na.rm = TRUE))
cat("RMSE:", rmse, "\n")

# Visualization: Actual vs Predicted Emissions
library(ggplot2)
ggplot(chem_data, aes(x = emissions_mtco2e, y = predicted_emissions)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Emissions (mt CO2e)",
    x = "Actual Emissions (mt CO2e)",
    y = "Predicted Emissions (mt CO2e)"
  ) +
  theme_minimal()
