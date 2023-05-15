#### INTEREST INCOME ####
library(tseries)
library(forecast)
### plot income + expense
jpm_quarterly %>%
  mutate(nim = qtr_interest_income - qtr_interest_expense) %>%
  ggplot() +
  geom_line(aes(date, qtr_interest_expense, color="Interest Expense"), size=1) +
  geom_line(aes(date, qtr_interest_income, color="Interest Income"), size=1) +
  geom_line(aes(date, nim, color="NIM"), size=1) +
  scale_color_manual(name="Variables", values=c("Interest Expense" = "red", "Interest Income" = "blue",
                                                "NIM" = "black")) +
  labs(title="JPMorgan Interest Income & Expense",
       x="Date", y="Amount") +
  theme_bw()

### interest income = log(interest_income)
adf.test(diff(hist_master$`10-year Treasury yield`))
`Market Volatility Index (Level)` <- diff(hist_master$`Market Volatility Index (Level)`)

ndiffs(hist_master$`House Price Index (Level)`)

### regression interest income
reg_interest_income <- lm(diff(qtr_interest_income[-1]) ~ diff(`Prime rate`[-1]) +
                            diff(diff(`House Price Index (Level)`)) + 
                            diff(`Market Volatility Index (Level)`[-1]), data=hist_master)

plot(reg_interest_income)

corrplot::corrplot(cor(hist_master[, 15:20]), method="color", tl.srt=45)

vif(reg_interest_income)
lmtest::bptest(reg_interest_income)

### predict given new values
### why does this prediction give me 72 values? / how can it
### questions
predictions_interest_income <- predict(reg_interest_income, newdata=macro_future)
predictions_interest_income

### make date frame for interest income
date <- seq(as.Date("2023-01-01"),length.out = nrow(predictions_interest_income), along.with=predictions_interest_income, by = "3 months")

### combine predictions and dates
predictions_interest_income <- data.frame(predictions_interest_income, date)
predictions_interest_income
### turn into tsibble
predictions_interest_income <- predictions_interest_income %>%
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index=date)

### column names
colnames(predictions_interest_income) <- c("qtr_interest_income", "date")
predictions_interest_income

##
### regression interest income table
stargazer(reg_interest_income,
          covariate.labels = c("Prime Rate", "House Price Index (Level)", "`Market Volatility Index (Level)"),
          dep.var.labels = "Interest Income",
          type="html",
          iqr=T,
          digits=2,
          out="interest_income.html")

### ridge regression
library(leaps)

### regsubsets
reg_sub <- regsubsets(log(qtr_interest_income[-(1:2)]) ~ diff(`Prime rate`[-1]) + diff(`BBB corporate yield`[-1]) + 
                        diff(`10-year Treasury yield`[-1])+ diff(`Commercial Real Estate Price Index (Level)`[-1]) +
                        `Market Volatility Index (Level)`[-(1:2)] + diff(`Unemployment rate`[-1]) +
                        `Real disposable income growth`[-(1:2)] + diff(diff(`House Price Index (Level)`)), 
                      data=hist_master, nvmax=9)

reg_sum <- summary(reg_sub)
reg_sum$rsq

### ridge regression
library(glmnet)
x <- model.matrix(log(qtr_interest_income[-(1:2)]) ~ diff(`BBB corporate yield`[-1]) + 
                    diff(`10-year Treasury yield`[-1])+ diff(`Commercial Real Estate Price Index (Level)`[-1]) +
                    `Market Volatility Index (Level)`[-(1:2)] + diff(`Unemployment rate`[-1]) +
                    `Real disposable income growth`[-(1:2)] + diff(diff(`House Price Index (Level)`)), 
                  hist_master)[, -1]

y <- log(hist_master$qtr_interest_income[-(1:2)])

grid <- 10^seq(10, -2, length = 100)

### ridge model
ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge_mod))

ridge_mod$lambda[50]

coef(ridge_mod)[, 50]

### ridge cv for p value
set.seed(123)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y_test <- y[test]
cv_ridge <- cv.glmnet(x[train, ], y[train], alpha = 0, lambda = grid)

plot(cv_ridge)
coef(cv_ridge, s = "lambda.min")

### best lambda
bestlam <- cv_ridge$lambda.min
bestlam

### predict using min lambda for mse score
ridge_pred <- predict(ridge_mod, s = bestlam, newx = x[test, ])

mean((ridge_pred - y_test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]


### INTEREST EXPENSE ###
library(tseries)
library(forecast)
adf.test(log(hist_master$qtr_interest_expense[-1]))
ndiffs(hist_master$qtr_interest_expense)

### regression interest expense
reg_interest_expense <- lm(diff(qtr_interest_expense) ~ diff(`Prime rate`) + 
                             diff(`House Price Index (Level)`), data=hist_master)

stargazer(reg_interest_expense,
          covariate.labels = c("Prime Rate", "House Price Index (Level)"),
          dep.var.labels = "Interest Expense",
          type="html",
          iqr=T,
          digits=2,
          out="interest_expense.html")

### predict
prediction_interest_expense <- predict(reg_interest_expense, macro_future)

# Add the dates column to the interest expense predictions data frame
date_exp <- seq(as.Date("2023-01-01"),length.out = nrow(prediction_interest_expense), along.with=prediction_interest_expense, by = "3 months")

prediction_interest_expense <- data.frame(prediction_interest_expense, date_exp)

### turn into tsibble
prediction_interest_expense <- prediction_interest_expense %>%
  mutate(date_exp = yearquarter(date_exp)) %>%
  as_tsibble(index=date_exp)

### change colnames to match hist_master
colnames(prediction_interest_expense) <- c("qtr_interest_expense", "date")
head(prediction_interest_expense)
### create master interest income + expense
interest_master <- merge(prediction_interest_expense, predictions_interest_income, on="date")

### interest master as tsibble
interest_master <- interest_master %>%
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index=date)
### merge w hist_master
interest_full <- bind_rows(interest_master, hist_master)

interest_master %>%
  mutate(nim = qtr_interest_income - qtr_interest_expense) %>%
  ggplot() +
  geom_line(aes(date, qtr_interest_income, color="Interest Income"), size=1) +
  geom_line(aes(date, qtr_interest_expense, color="Interest Expense"), size=1) +
  geom_vline(xintercept=as.numeric(as.Date("2022-12-31")), linetype="longdash", color="red", lwd=1) +
  labs(title="JPMorgan Interest Financials: Historical & Projected",
       y="Value", x="Date") +
  theme_bw()

### plot historical and predicted NIM
interest_master %>%
  mutate(nim = qtr_interest_income - qtr_interest_expense) %>%
  ggplot() +
  geom_area(aes(date, nim), fill=4, size=1) +
  geom_vline(xintercept=as.numeric(as.Date("2022-12-31")), linetype="longdash", color="red", lwd=1) +
  labs(title="JPMorgan Net Interest Margin: Historical & Projected",
       y="NIM", x="Date") +
  theme_bw()
