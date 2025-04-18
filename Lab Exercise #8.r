# ECO 231 Applied Business Statistics
# Lab 8 - Sales Data Analysis
# I'm using R to analyze sales data from 1985 to 2019 and do some regression stuff

# Install packages if I don't have them already
if (!require(lmtest)) {
  install.packages("lmtest", repos = "https://cloud.r-project.org")
}
if (!require(nlme)) {
  install.packages("nlme", repos = "https://cloud.r-project.org")
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}

# Load the libraries I need
library(lmtest)
library(nlme)
library(ggplot2)

# Set up my working directory
my_folder <- "/Users/nicolasdetroia/Documents/College"
if (!dir.exists(my_folder)) {
  dir.create(my_folder, recursive = TRUE)
  print(paste("Made a new folder at:", my_folder))
}

# Create the sales data (this is the data from the assignment)
sales_data <- data.frame(
  time_trend = 1:35,
  sales_millions = c(4.8, 4, 5.5, 15.6, 23.1, 23.3, 31.4, 46, 46.1, 41.9, 45.5, 
                     53.5, 48.4, 61.6, 65.6, 71.4, 83.4, 93.6, 94.2, 85.4, 86.2, 
                     89.9, 89.2, 99.1, 100.3, 111.7, 108.2, 115.5, 119.2, 125.2, 
                     136.3, 146.8, 146.1, 151.4, 150.9),
  year = 1985:2019
)

# I’ll rename columns to make it easier to work with
sales_data$time <- sales_data$time_trend
sales_data$sales <- sales_data$sales_millions
sales_data$year <- sales_data$year

# Plotting the sales over time to see what’s going on
sales_plot <- ggplot(sales_data, aes(x = year, y = sales)) +
  geom_line(color = "darkblue") +
  geom_point(color = "red") +
  labs(title = "Sales Over Time (1985-2019)", x = "Year", y = "Sales (Millions $)") +
  theme_minimal()
ggsave(file.path(my_folder, "sales_trend_plot.png"), plot = sales_plot, width = 7, height = 5)

# Answer for Question 1 (describing the plot)
q1_text <- paste(
  "The plot I made (sales_trend_plot.png) shows sales going up from 1985 to 2019.\n",
  "It’s mostly a steady increase, so there’s a clear trend.\n",
  "No seasonal patterns since it’s yearly data, not monthly.\n",
  "There are some ups and downs (like around 1994 and 2004) that might be economic cycles.\n",
  "Some small random wiggles are probably irregular stuff.\n"
)

# Run a simple linear regression (sales vs time)
simple_model <- lm(sales ~ time, data = sales_data)
model_summary <- summary(simple_model)

# Save residual plots to check the model
png(file.path(my_folder, "model_residuals.png"))
par(mfrow = c(2, 2))
plot(simple_model, which = 1:4)
dev.off()

# Make a residual plot with ggplot
resid_plot <- ggplot(data.frame(time = sales_data$time, resid = residuals(simple_model)), 
                     aes(x = time, y = resid)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time", x = "Time", y = "Residuals") +
  theme_minimal()
ggsave(file.path(my_folder, "time_vs_residuals.png"), plot = resid_plot, width = 7, height = 5)

# Get regression coefficients for Question 2
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- model_summary$r.squared

q2_text <- paste(
  paste("My regression gives: Intercept =", round(intercept, 3), 
        ", Slope =", round(slope, 3), ", R² =", round(r_squared, 3), "\n"),
  "The residual plots (model_residuals.png, time_vs_residuals.png) show a pattern.\n",
  "Residuals are positive at the start and end, negative in the middle.\n",
  "This probably means there’s autocorrelation in the data.\n",
  "The Q-Q plot has some weird tails, so maybe not perfectly normal.\n",
  "No obvious heteroscedasticity in residuals vs fitted plot.\n",
  "The ZINE assumptions (especially independence) are probably violated.\n"
)

# Do the Durbin-Watson test for autocorrelation
dw_test <- dwtest(simple_model)
dw_value <- dw_test$statistic
dw_p <- dw_test$p.value

q3_text <- paste(
  paste("Durbin-Watson test: DW =", round(dw_value, 3), ", p-value =", round(dw_p, 3), "\n"),
  "Since the p-value is less than 0.05, there’s evidence of autocorrelation.\n"
)

# Question 4: Why is autocorrelation a problem?
q4_text <- paste(
  "The regression assumes errors are independent (part of ZINE).\n",
  "The DW test shows autocorrelation (DW < 2 and p < 0.05).\n",
  "This messes up the standard errors, making t-tests unreliable.\n",
  "The estimates are still okay but not as efficient.\n"
)

# Try a GLS model to fix autocorrelation
gls_model <- gls(sales ~ time, data = sales_data, correlation = corAR1(), method = "ML")
gls_summary <- summary(gls_model)

q5_text <- paste(
  "I think we should use a GLS model with AR(1) autocorrelation correction.\n",
  "It’s better than OLS because it handles the autocorrelation problem.\n",
  "I used the nlme::gls function to do this.\n"
)

# Compare the two models for Question 6
ols_coefs <- model_summary$coefficients
ols_intercept <- ols_coefs[1, 1]
ols_slope <- ols_coefs[2, 1]
ols_se_intercept <- ols_coefs[1, 2]
ols_se_slope <- ols_coefs[2, 2]

gls_coefs <- gls_summary$tTable
gls_intercept <- gls_coefs[1, 1]
gls_slope <- gls_coefs[2, 1]
gls_se_intercept <- gls_coefs[1, 2]
gls_se_slope <- gls_coefs[2, 2]

# Print out the results to compare
cat("Regular OLS Model:\n")
cat("Intercept:", round(ols_intercept, 3), " (SE:", round(ols_se_intercept, 3), ")\n")
cat("Slope:", round(ols_slope, 3), " (SE:", round(ols_se_slope, 3), ")\n\n")
cat("GLS Model with AR(1):\n")
cat("Intercept:", round(gls_intercept, 3), " (SE:", round(gls_se_intercept, 3), ")\n")
cat("Slope:", round(gls_slope, 3), " (SE:", round(gls_se_slope, 3), ")\n")

# Calculate differences
intercept_diff <- gls_intercept - ols_intercept
slope_diff <- gls_slope - ols_slope
se_intercept_change <- ifelse(gls_se_intercept > ols_se_intercept, "got bigger", "got smaller")
se_slope_change <- ifelse(gls_se_slope > ols_se_slope, "got bigger", "got smaller")

q6_text <- paste(
  paste("OLS: Intercept =", round(ols_intercept, 3), ", SE =", round(ols_se_intercept, 3), 
        "; Slope =", round(ols_slope, 3), ", SE =", round(ols_se_slope, 3), "\n"),
  paste("GLS: Intercept =", round(gls_intercept, 3), ", SE =", round(gls_se_intercept, 3), 
        "; Slope =", round(gls_slope, 3), ", SE =", round(gls_se_slope, 3), "\n"),
  paste("Intercept changed by", round(intercept_diff, 3), ", SE", se_intercept_change, ".\n"),
  paste("Slope changed by", round(slope_diff, 3), ", SE", se_slope_change, ".\n"),
  "The GLS model is better because it accounts for autocorrelation.\n"
)

# Save all my answers to a file
sink(file.path(my_folder, "lab8_answers.txt"))
cat("Question 1:\n", q1_text, "\n")
cat("Question 2:\n", q2_text, "\n")
cat("Question 3:\n", q3_text, "\n")
cat("Question 4:\n", q4_text, "\n")
cat("Question 5:\n", q5_text, "\n")
cat("Question 6:\n", q6_text, "\n")
sink()

cat("Done! All results and plots are saved in", my_folder, "\n")
