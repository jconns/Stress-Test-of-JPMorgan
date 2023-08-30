# Stress Test of JPMorgan

Stress Testing in Practice

Stress tests access how big banks are likely to behave under hypothetical economic conditions. For this reason the task is to calculate JPMorgan's Tier 1 Capital Ratio under severe economic conditions. 

# Tier 1 Capital Ratio
The tier 1 capital ratio is the ratio of a bank’s core tier 1 capital—that is, its equity capital and disclosed reserves—to its total risk-weighted assets. It is a key measure of a bank's financial strength that has been adopted as part of the Basel III Accord on bank regulation.

# Data

Two datasets were used for this project. The first being the hypothetical economic environment and the other comes from the Federal Reserve and contains banking data on all major banks reviewed by the Fed. Both are available upon request.

# Approach

My goal was to model the behavior between JPMorgan's finances to the broader macro environment. The various components of the bank's income was evaluated seperarely to best capture each component on income individually. The same was done for expensese.

After creating a regression model for each component of income and expenses, they were forecasted out ove the 9 quarter horizon. From this data, net interest income and expenses are computed

# Evolution of Capital
Forecast revenue (net interest income - net interest expense) for t+1 to t+8

Forecast loan losses for t+1 to t+8

Compute
Capital(t+i) = Capital(i) + Revenue(i+1) - Loan Losses(i+1)
repeat for i=1:8

Compare Capital/RWA with the regulatory minimum (5%)

# Results

JPMorgan passed the stress test. Under adverse conditions, JPMorgan has constructed their balance sheet to withstand stress and meet the 5% minimum ratio.

# Forecasting Net Interest Income
The Income Statement variables in the FR-Y9C data are flows reported year-to-date, so they needed to be adjusted to have flows at the quarterly frequency. This adjustment did not have to be made for balance sheet data.

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
