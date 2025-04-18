# ECO231


Question 1:

Plot: Saved as sales_plot.png.
Answer: plot (sales_plot.png) shows sales going up from 1985 to 2019. no seasonal stuff since its yearly data. some dips like in 1994, 2004, 2011 - maybe economic stuff? theres random ups and downs around the trend.


Question 2:

Regression Results: Intercept = 0.402, Slope = 4.296, R² = 0.912 (matches your OLS output after adjusting T).
Answer: regression: intercept = 0.402, slope = 4.296, R² = 0.912. residual plots (residual_plots.png, residuals_vs_time.png) show a pattern. residuals are positive at start and end, negative in middle - looks like autocorrelation. Q-Q plot has some weird stuff at the ends, not perfectly normal. residuals vs fitted looks okay, no big patterns. ZINE stuff is broken bc of autocorrelation.


Question 3:

DW Test: DW = 0.766, p-value = 0.000.
Answer: DW stat = 0.766, p-value = 0.000. p < 0.05 so theres positive autocorrelation.


Question 4:

Answer: regression in Q2 assumes errors are independent (ZINE thing). DW test shows autocorrelation (DW < 2, p < 0.05). this messes up the standard errors, makes t-tests unreliable. estimates arent the best either.

Question 5:

Answer: I think we should use a model that fixes the autocorrelation. like Sales = b0 + b1T but with GLS and AR(1) errors using nlme::gls.

Question 6:

Results (using your output):
OLS model: intercept = 0.402, SE = 2.206; slope = 4.296, SE = 0.107.
GLS model: intercept = 0.405, SE = 3.812; slope = 4.296, SE = 0.181.
Answer: OLS model: intercept = 0.402, SE = 2.206; slope = 4.296, SE = 0.107. GLS model: intercept = 0.405, SE = 3.812; slope = 4.296, SE = 0.181. intercept changed by 0.003, SE went up, slope changed by 0.000, SE went up. GLS is better bc it handles the autocorrelation.
