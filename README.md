# Stress Test of JPMorgan

Stress Testing in Practice

The goal of the exercise is to develop models that link the relevant variables that determine changes in the tier 1 capital ratio (income and expenses) and forecast the evolution of the capital ratio over the course of 9 quarters under a severe economic scenario. This is how stress testing works in reality. A secondary dataset contains information detailing a severely adverse economic scenario and the historical data for the macroeconomic variables that are part of the scenario. In each of the exercises, you will be asked to model historical bank variables as a function of macroeconomic variables. The last part puts all the pieces together.

# Forecasting Net Interest Income
The objective of this section is to find a historical relationship between interest income and expense, and the economic conditions. Remember that Income Statement variables in the FR-Y9C data are flows reported year-to-date, so you have to make the necessary adjustments to have flows at the quarterly frequency. Notice that you do not need to make this adjustment for balance sheet data.

To find a relationship between interest income and the economic conditions, you have to run a regression with interest income on the left-hand side and macroeconomic variables that you believe are related to interest income on the right-hand side. You are free to use any combination of explanatory variables that you think is appropriate, transformations of the scenario variables, leads, lags, non-linear regressors, etc. The only rule is that it has to be a proper regression model, correctly specified, and your choice of regressors has to be thoroughly justified.
Once you have estimated the coefficients, use your model to forecast 8 quarters of interest income using the hypothetical severely adverse economic scenario provided. Show those forecasts in the report and discuss.
Do the same for interest expense. The macroeconomic variables do not need to be the same if you think that the drivers of expenses are not the same as those that drive income. You need to justify your choices. Show the forecasts in the report and discuss them too.
Now that you have forecasts for interest income and interest expense, compute the forecast for net interest income. Plot the historical and forecast net-interest income in a graph.
 

# Forecasting Non-Interest Income and Expense
This exercise is practically identical to the previous one, but for noninterest income and expenses:

Estimate a regression that relates noninterest income to macroeconomic variables that are part of the scenario provided. As before, you can transform the variables, use lags, leads, any specification that you believe explains better noninterest income. Your choice of model needs to be justified.
Estimate a separate regression for noninterest expense. Read carefully the FR-Y9C form to understand what is included in noninterest income and expense, in order to better justify your choice of macroeconomic variables.
Once both models are estimated, forecast noninterest income and expenses by using the severely adverse economic conditions on the right-hand side of your regressions.
In the same graph, plot historical and forecasted noninterest income and expenses. Discuss the results.
 

# Forecasting Losses
Losses are estimated typically at the portfolio level. Given the aggregate nature of the FR-Y9C data, we will estimate a model for losses in each type of loan. Losses behave different than income, so it is likely that you need to use different macroeconomic variables.  Different types of loans also behave differently. Think about what causes each type of loan losses in your bank. Estimate your regression models and use the severely adverse scenario provided to forecast losses for 8 quarters. You do not need to model each line of loans below. You can and should aggregate at the proper level (see Week 1 slides for guidance).

 
# Evolution of Capital
Forecast revenue (net interest income - net interest expense) for t+1 to t+8

Forecast loan losses for t+1 to t+8

Compute
Capital(t+i) = Capital(i) + Revenue(i+1) - Loan Losses(i+1)
repeat for i=1:8

Compare Capital/RWA with the regulatory minimum (5%)
Now, we have the revenue and loss forecast for the next 8 quarters, conditional on a severe crisis happening for each of the two banks. For this exercise, you need “Tier 1 Capital” and “Risk Weighted Assets” from the data file. Note that you only need those two variables for the very last quarter of the dataset (2022Q4), as the forecasts are computed off that starting point. Also note that you have Tier 1 capital ratio and risk-weighted assets, so you need to compute Tier 1 capital. (T1 Ratio = T1 capital / RWA)

Forecast the Tier 1 Capital for the next 8 quarters. As described above, the capital in the next period is equal to the capital in the previous period plus Net Income minus loan losses.
Assume that Risk Weighted Assets are constant over the forecasting period. Compute the evolution of Tier 1 Capital Ratio over the 8 quarters. Plot the forecasted ratio and also plot the 5% minimum ratio.

Did your BHC breach the 5% minimum at any point in the history provided? When? How was income in those periods?
If your BHC doesn't breach the minimum capital ratio, suggest a dividends distribution that allows the firm to keep a 10% Tier 1 capital ratio over the forecasting period. Plot the forecasted capital ratio without and with your dividends distribution policy for each bank.
What would you recommend to your CFO, given your estimates in the previous questions, to potentially increase the dividends distributions in the future? (Possibilities: decrease expenses, increase interest/noninterest income, focus on more/less risky assets, etc. It all depends on the sensitivity of your banks to the macroeconomy).
