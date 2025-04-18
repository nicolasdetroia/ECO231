# ECO 231 Applied Business Statistics

if (!require(lmtest)) install.packages("lmtest", repos = "https://cloud.r-project.org")
if (!require(nlme)) install.packages("nlme", repos = "https://cloud.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")

library(lmtest)  
library(nlme)    
library(ggplot2) 

base_path <- "/Users/nicolasdetroia/Documents/College "

if (!dir.exists(base_path)) {
  dir.create(base_path, recursive = TRUE)
  cat("Created directory:", base_path, "\n")
}

data <- data.frame(
  Trend = 1:35,
  SALES = c(4.8, 4, 5.5, 15.6, 23.1, 23.3, 31.4, 46, 46.1, 41.9, 45.5, 53.5, 48.4, 61.6, 65.6, 71.4, 83.4, 93.6, 94.2, 85.4, 86.2, 89.9, 89.2, 99.1, 100.3, 111.7, 108.2, 115.5, 119.2, 125.2, 136.3, 146.8, 146.1, 151.4, 150.9),
  Year = 1985:2019
)

data$T <- data$Trend
data$Sales <- data$SALES
data$Year <- data$Year

p1 <- ggplot(data, aes(x = Year, y = Sales)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Annual Sales (1985–2019)", x = "Year", y = "Sales ($ million)") +
  theme_minimal()
ggsave(file.path(base_path, "sales_plot.png"), plot = p1, width = 8, height = 6)

q1_answer <- paste(
  "The plot (sales_plot.png) shows a strong upward trend in sales from 1985 to 2019.\n",
  "No seasonal component exists due to annual data.\n",
  "Cyclical patterns are visible (e.g., dips around 1994, 2004, 2011), possibly due to economic cycles.\n",
  "Irregular components appear as short-term fluctuations around the trend.\n"
)

model1 <- lm(Sales ~ T, data = data)
summary_model1 <- summary(model1)

png(file.path(base_path, "residual_plots.png"))
par(mfrow = c(2, 2))
plot(model1, which = 1:4)
dev.off()

p2 <- ggplot(data.frame(T = data$T, Residuals = residuals(model1)), aes(x = T, y = Residuals)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Time", x = "Time (T)", y = "Residuals") +
  theme_minimal()
ggsave(file.path(base_path, "residuals_vs_time.png"), plot = p2, width = 8, height = 6)

b0_1 <- summary_model1$coefficients[1, 1]
b1_1 <- summary_model1$coefficients[2, 1]
r2_1 <- summary_model1$r.squared
q2_answer <- paste(
  sprintf("Regression results: Intercept = %.3f, Slope = %.3f, R² = %.3f.\n", b0_1, b1_1, r2_1),
  "Residual plots (residual_plots.png, residuals_vs_time.png) show a pattern in residuals vs. time,\n",
  "with positive residuals early and late, negative in the middle, suggesting autocorrelation.\n",
  "Q-Q plot shows slight deviations from normality at the tails.\n",
  "Residuals vs. fitted show no clear heteroscedasticity.\n",
  "ZINE assumptions are violated due to autocorrelation.\n"
)

dw_result <- dwtest(model1)
dw_stat <- dw_result$statistic
dw_pvalue <- dw_result$p.value

q3_answer <- paste(
  sprintf("DW statistic = %.3f, p-value = %.3f.\n", dw_stat, dw_pvalue),
  "Since p < 0.05, there is evidence of positive first-order autocorrelation.\n"
)

q4_answer <- paste(
  "The regression assumes independent errors (ZINE's Independence).\n",
  "The DW test (DW < 2, p < 0.05) indicates first-order autocorrelation,\n",
  "violating this assumption. This leads to biased standard errors,\n",
  "unreliable t-tests, and inefficient estimates.\n"
)

model2 <- gls(Sales ~ T, data = data, correlation = corAR1(), method = "ML")
summary_model2 <- summary(model2)

q5_answer <- paste(
  "I recommend a regression model (Sales = b0 + b1T) with first-order autocorrelation\n",
  "correction using GLS with an AR(1) error structure, implemented via nlme::gls.\n"
)

coef1 <- summary_model1$coefficients
b0_1 <- coef1[1, 1]  
b1_1 <- coef1[2, 1]  
se_b0_1 <- coef1[1, 2]  
se_b1_1 <- coef1[2, 2]  

xt
coef2 <- summary_model2$tTable
b0_2 <- coef2[1, 1]  
b1_2 <- coef2[2, 1]  
se_b0_2 <- coef2[1, 2]  
se_b1_2 <- coef2[2, 2]  


cat("Simple OLS (Model 1):\n")
cat(sprintf("Intercept: %.3f, SE: %.3f\n", b0_1, se_b0_1))
cat(sprintf("Slope: %.3f, SE: %.3f\n\n", b1_1, se_b1_1))
cat("GLS with AR(1) (Model 2):\n")
cat(sprintf("Intercept: %.3f, SE: %.3f\n", b0_2, se_b0_2))
cat(sprintf("Slope: %.3f, SE: %.3f\n", b1_2, se_b1_2))


delta_b0 <- b0_2 - b0_1
delta_b1 <- b1_2 - b1_1
se_b0_change <- ifelse(se_b0_2 > se_b0_1, "increased", "decreased")
se_b1_change <- ifelse(se_b1_2 > se_b1_1, "increased", "decreased")
q6_answer <- paste(
  sprintf("Simple OLS: Intercept = %.3f, SE = %.3f; Slope = %.3f, SE = %.3f.\n", b0_1, se_b0_1, b1_1, se_b1_1),
  sprintf("GLS with AR(1): Intercept = %.3f, SE = %.3f; Slope = %.3f, SE = %.3f.\n", b0_2, se_b0_2, b1_2, se_b1_2),
  sprintf("Intercept changed by %.3f, SE %s. Slope changed by %.3f, SE %s.\n", delta_b0, se_b0_change, delta_b1, se_b1_change),
  "GLS provides more reliable estimates by accounting for autocorrelation.\n"
)

sink(file.path(base_path, "lab8_results.txt"))
cat("Question 1 Answer:\n", q1_answer, "\n")
cat("Question 2 Answer:\n", q2_answer, "\n")
cat("Question 3 Answer:\n", q3_answer, "\n")
cat("Question 4 Answer:\n", q4_answer, "\n")
cat("Question 5 Answer:\n", q5_answer, "\n")
cat("Question 6 Answer:\n", q6_answer, "\n")
sink()
