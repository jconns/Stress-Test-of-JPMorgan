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
