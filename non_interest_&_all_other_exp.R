#### non interest financials ####
### non interest income ###
adf.test(hist_master$auto_prod)
ndiffs(hist_master$`Market Volatility Index (Level)`)
`Market Volatility Index (Level)` <- diff(hist_master$`Market Volatility Index (Level)`)

### non interest income regression
reg_noninterest_income <- lm(diff(qtr_noninterest_income) ~  diff(`Market Volatility Index (Level)`) +
                               diff(`Real disposable income growth`) + diff(`Unemployment rate`), data=hist_master)
summary(reg_noninterest_income)
stargazer(reg_noninterest_income,
          covariate.labels = c("Market Volatility Index (Level)", "Real disposable income growth", "Unemployment rate"),
          dep.var.labels = "Non-Interest Income",
          type="html",
          iqr=T,
          digits=2,
          out="noninterest_income.html")

### predict non interest income 
noninterest_income_prediction <- predict(reg_noninterest_income, newdata=macro_future)

### non interest expense ###
adf.test(hist_master$auto_prod)
`Market Volatility Index (Level)` <- diff(hist_master$`Market Volatility Index (Level)`)

worse_noninterest_expense <- lm(diff(qtr_noninterest_expense) ~ diff(lead(`Unemployment rate`)),
                                data=hist_master) 

reg_noninterest_expense <- lm(diff(qtr_noninterest_expense) ~ diff(`Market Volatility Index (Level)`) +
                                diff(lead(`Unemployment rate`)), data=hist_master)

anova(reg_noninterest_expense, reg_non_exp)

### regression stargazer
stargazer(reg_noninterest_expense,
          covariate.labels = c("Market Volatility Index (Level)", "Lead Unemploymenr Rate"),
          dep.var.labels = "Non-Interest Expense",
          type="html",
          iqr=T,
          digits=2,
          out="noninterest_expense.html")

### predict non interest expense
noninterest_expense_prediction <- predict(reg_noninterest_expense, macro_future)

### combine non interest financial predictions
noninterest_predictions <- data.frame(noninterest_expense_prediction, noninterest_income_prediction)

### add dates
date_non <- seq(as.Date("2023-01-01"),length.out = nrow(noninterest_expense_prediction), along.with=noninterest_expense_prediction, by = "3 months")

## turn into tsibble
noninterest_predictions <- noninterest_predictions %>%
  mutate(date_non=yearquarter(date_non)) %>%
  as_tsibble(index=date_non)
colnames(noninterest_predictions) <- c("qtr_noninterest_expense", "qtr_noninterest_income", "date")

### combine with historical values to plot non interest financials 
noninterest_master <- merge(noninterest_expense_prediction, noninterest_income_prediction, on="date")
noninterest_master <- bind_rows(noninterest_predictions, hist_master)

### plot non interest financials
ggplot(noninterest_master) +
  geom_line(aes(date, qtr_noninterest_income, color="Non-Interest Income"), size=1) +
  geom_line(aes(date, qtr_noninterest_expense, color="Non-Interest Expense"), size=1) +
  geom_vline(xintercept=as.numeric(as.Date("2022-12-31")), linetype="longdash", lwd=1) +
  scale_color_manual(name="Variables", values=c("Non-Interest Income" = "green", "Non-Interest Expense" = "red")) +
  labs(title="JPMorgan Non-Interest Financials",
       y="Amount ($)" , x="Date") +
  theme_bw()

####  LOSSES PER CATEGORY ####
ggplot(hist_master, aes(date, color="variable")) +
  geom_line(aes(date, qtr_consumer_loans, color="Consumer_Loans"), size=1) +
  geom_line(aes(date, qtr_C_I_loans, color="C & I Loans"), size=1) +
  geom_line(aes(date, qtr_loans_to_depository_institutions_and_acceptances_of_other_banks, color="Loans to Depository Insts"), size=1) +
  geom_line(aes(date, real_estate_losses, color="Real Estate Losses"), size=1) +
  geom_line(aes(date, qtr_all_other_loans, color="All Other Loans"), size=1) +
  geom_line(aes(date, qtr_construction_domestic_offices, color="Construction US"), size=1) +
  scale_color_manual(name= "Y series", values=c("Consumer_Loans" = "blue", "C & I Loans" = "black", 
                                                "Construction US" ="green", "Real Estate Losses" = "red", "All Other Loans" = "maroon",
                                                "Loans to Depository Insts" = "pink")) +
  labs(title="JPMorgan Losses", y="Amount ($)", x="Date") +
  theme_bw()

### real estate regression
# plot real estate loans
### plot starting in 2014 because 2008 GFC contorts the data
hist_master %>%
  as_tsibble(index=date) %>%
  filter_index("2014 Q3" ~ .) %>%
  ggplot(aes(date, color="variable")) +
  geom_line(aes(date, qtr_x1_x4family_residential_construction, color="Fam 1-4"), size=1) +
  geom_line(aes(date, qtr_loans_secured_by_farmland, color="Farmland"), size=1) +
  geom_line(aes(date, qtr_loans_secured_by_farmland, color="Farmland"), size=1) +
  geom_line(aes(date, qtr_construction_domestic_offices, color="Construction US"), size=1) +
  geom_line(aes(date, qtr_multifamily_residential_properties_domestic_offices, color="Multifamily"), size=1) +
  geom_line(aes(date, qtr_nonfarm_nonresidential_domestic_offices, color="Nonfarm Nonresidential"), size=1) +
  geom_line(aes(date, qtr_real_estate_foreign_offices, color="Foreign Offices"),size=1) +
  scale_color_manual(name= "Y series", values=c("Fam 1-4" = "blue", "Farmland" = "black", 
                                                "Construction US" ="green", "Multifamily" = "red","Nonfarm Nonresidential" = "orange",
                                                "Foreign Offices" = "maroon")) +
  labs(title="Retail Losses by Type",
       y="Losses", x="Date") +
  theme_bw()

### REAL ESTATE LOSSES BY CATEGORY ###
hist_master <- hist_master %>%
  mutate(real_estate_losses = 
           qtr_x1_x4family_residential_construction + qtr_loans_secured_by_farmland +
           qtr_construction_domestic_offices + qtr_multifamily_residential_properties_domestic_offices +
           qtr_nonfarm_nonresidential_domestic_offices + 
           qtr_real_estate_foreign_offices)

### plot both actual and stationary
hist_master %>%
  mutate(change = qtr_x1_x4family_residential_construction - lag(qtr_x1_x4family_residential_construction)) %>%
  ggplot()+
  geom_line(aes(date, qtr_x1_x4family_residential_construction, color="Actual"), size=1) +
  geom_line(aes(date, change, color="Stationary"), size=1) +
  scale_color_manual(name="Y series", values = c("Actual" = "blue", "Stationary"="black")) +
  labs(title="Residential Construction Loans for Families 1-4", y="Losses ($ Thousands)") +
  theme_bw()

#### 1_4 FAMILY RESIDENTIAL CONSTRUCTION ####
reg_res_construction <- lm(diff(qtr_x1_x4family_residential_construction) ~ diff(lag(`CPI inflation rate`)) +
                             diff(`Commercial Real Estate Price Index (Level)`),
                           data=hist_master)
summary(reg_res_construction)

stargazer(reg_res_construction,
          type="html",
          covariate.labels = c("Lagged CPI", "Commercial Real Estate Price Index"),
          dep.var.labels = "Residential Construction Families 1-4",
          digits=2,
          iqr=T,
          out="res_construction_losses_x.html")

### predict + dates
library(lubridate)
res_construction_prediction <- predict(reg_res_construction, macro_future)

# Create a data frame with the date and prediction columns
res_construction_prediction <- data.frame(date, res_construction_prediction)

### add date
date_con <- seq(as.Date("2023-01-01"),length.out = nrow(res_construction_prediction), along.with=res_construction_prediction, by = "3 months")

res_construction_prediction <- data.frame(res_construction_prediction, date_con)

### data frame + tsibble
res_construction_prediction <-res_construction_prediction %>%
  mutate(date_con = yearquarter(date_con)) %>%
  as_tsibble(index=date_con)
colnames(res_construction_prediction) <- c("qtr_x1_x4family_residential_construction", "date")

### loans secured by farmland
### plot both actual and stationary
hist_master %>%
  mutate(change = qtr_loans_secured_by_farmland - lag(qtr_loans_secured_by_farmland)) %>%
  ggplot()+
  geom_line(aes(date, qtr_loans_secured_by_farmland, color="Actual"), size=1) +
  geom_line(aes(date, change, color="Stationary"), size=1) +
  scale_color_manual(name="Y series", values = c("Actual" = "blue", "Stationary"="black")) +
  labs(title="Loans Secured by Farmland", y="Losses ($ Thousands)") +
  theme_bw()

#### LOANS SECURED BY FARMLAND ####
reg_farmland <- lm(diff(qtr_loans_secured_by_farmland) ~ diff(lag(`10-year Treasury yield`))
                   + diff(lag(`Mortgage rate`)),
                   data=hist_master)

summary(reg_farmland)

stargazer(reg_farmland,
          type="html",
          covariate.labels = c("Lagged 10-Year Treasury Yield", "Mortgage Rate"),
          dep.var.labels = "Loans Secured by Farmlans",
          digits=2,
          iqr=T,
          out="reg_farmland.html")

### predict + dates
reg_farmland_prediction <- predict(reg_farmland, macro_future)

### add date
date_farm <- seq(as.Date("2023-01-01"),length.out = nrow(reg_farmland_prediction), along.with=reg_farmland_prediction, by = "3 months")

### data frame + tsibble
reg_farmland_prediction <- data.frame(reg_farmland_prediction, date)

reg_farmland_prediction <- reg_farmland_prediction %>%
  mutate(date_farm = yearquarter(date_farm)) %>%
  as_tsibble(index=date_farm)
colnames(reg_farmland_prediction) <- c("qtr_loans_secured_by_farmland", "date")

### construction in domestic offices
### plot both actual and stationary
hist_master %>%
  mutate(change = qtr_construction_domestic_offices - lag(qtr_construction_domestic_offices)) %>%
  ggplot()+
  geom_line(aes(date, qtr_construction_domestic_offices, color="Actual"), size=1) +
  geom_line(aes(date, change, color="Stationary"), size=1) +
  scale_color_manual(name="Y series", values = c("Actual" = "blue", "Stationary"="black")) +
  labs(title="Construction in Domestic Offices", y="Losses ($ Thousands)", x="Date") +
  theme_bw()

#### CONSTRUCTION IN DOMESTIC OFFICES ####
reg_dom_offices <- lm(diff(qtr_construction_domestic_offices) ~ diff(lag(`CPI inflation rate`))
                      ,data=hist_master)

summary(reg_dom_offices)

stargazer(reg_dom_offices,
          type="html",
          covariate.labels = c("Lagged CPI"),
          dep.var.labels = "Construction in Domestic Offices",
          digits=2,
          iqr=T,
          out="reg_dom_offices.html")

### predict + dates
reg_dom_prediction <- predict(reg_dom_offices, macro_future)

### data frame + tsibble
### add date
date_dom <- seq(as.Date("2023-01-01"),length.out = nrow(reg_dom_prediction), along.with=reg_dom_prediction, by = "3 months")

reg_dom_prediction <- data.frame(reg_dom_prediction, date_dom)

reg_dom_prediction <- reg_dom_prediction %>%
  mutate(date_dom = yearquarter(date_dom)) %>%
  as_tsibble(index=date_dom)
colnames(reg_dom_prediction) <- c("qtr_construction_domestic_offices", "date")


#### MULTIFAMILY RESIDENTIAL PROPERTIES #### 
### plot both actual and stationary
hist_master %>%
  mutate(change = qtr_multifamily_residential_properties_domestic_offices - lag(qtr_multifamily_residential_properties_domestic_offices)) %>%
  ggplot()+
  geom_line(aes(date, qtr_multifamily_residential_properties_domestic_offices, color="Actual"), size=1) +
  geom_line(aes(date, change, color="Stationary"), size=1) +
  scale_color_manual(name="Y series", values = c("Actual" = "blue", "Stationary"="black")) +
  labs(title="Multifamily Residential Properties", y="Losses ($ Thousands)", x="Date") +
  theme_bw()

#### multifamily residential properties in domestic offices ####
### not done ###
### graphs is 0 so we can't really grab a regression
library(tsibble)
hist_master <- hist_master %>%
  mutate(date = yearquarter(Date)) %>%
  as_tsibble(index=date)

stat <- lm(diff(qtr_multifamily_residential_properties_domestic_offices) ~ 
             diff(`Unemployment rate`), data=hist_master)

ggplot(hist_master) +
  geom_line(aes(date, qtr_multifamily_residential_properties_domestic_offices))

str(hist_master$Date)
plot(hist_master$qtr_multifamily_residential_properties_domestic_offices)

summary(lm(diff(qtr_multifamily_residential_properties_domestic_offices) ~ 
             diff(ns(`Unemployment rate`, 2)) + diff(`House Price Index (Level)`), data=hist_master))

anova(stat, poly)

step_model <- step(reg_multifam, direction = "forward")
summary(step_model)


### reg stargazer
stargazer(reg_multifam ,
          type="html",
          covariate.labels = c("Lagged CPI"),
          dep.var.labels = "Construction in Domestic Offices",
          digits=2,
          iqr=T,
          out="reg_multifam .html")

### predict + dates
reg_multifam_prediction <- predict(reg_multifam , macro_future)
### add date
date_multi <- seq(as.Date("2023-01-01"),length.out = nrow(reg_multifam_prediction), along.with=reg_multifam_prediction, by = "3 months")

### data frame + tsibble
reg_multifam_prediction <- data.frame(reg_multifam_prediction, date_multi)

reg_multifam_prediction <- reg_multifam_prediction %>%
  mutate(date_multi = yearquarter(date_multi)) %>%
  as_tsibble(index=date_multi)
colnames(reg_multifam_prediction) <- c("qtr_multifamily_residential_properties_domestic_offices", "date")

### non farm nonresidential domestic offices
### plot both actual and stationary
hist_master %>%
  mutate(change = qtr_nonfarm_nonresidential_domestic_offices - lag(qtr_nonfarm_nonresidential_domestic_offices)) %>%
  ggplot()+
  geom_line(aes(date, qtr_nonfarm_nonresidential_domestic_offices, color="Actual"), size=1) +
  geom_line(aes(date, change, color="Stationary"), size=1) +
  scale_color_manual(name="Y series", values = c("Actual" = "blue", "Stationary"="black")) +
  labs(title="Multifamily Residential Properties", y="Losses ($ Thousands)", x="Date") +
  theme_bw()

#### NON FARM NON RESIDENTIAL ####
### not done ###
reg_nonfarm  <- lm(diff(qtr_nonfarm_nonresidential_domestic_offices) ~ diff(lead(`Commercial Real Estate Price Index (Level)`)), 
                   data=hist_master)

### regression stargazer
stargazer::stargazer(reg_nonfarm,
                     type="html",
                     covariate.labels = c("Lead Commerical Real Estate Price Index"),
                     dep.var.labels = "Nonfarm Nonresidential Real Estate Losses",
                     digits=2,
                     iqr=T,
                     out="reg_nonfarm.html")

### predict + dates
nonfarm_predictions <- predict(reg_nonfarm, macro_future)

### data frame + tsibble
### add date
date_nonfarm <- seq(as.Date("2023-01-01"),length.out = nrow(nonfarm_predictions), along.with=nonfarm_predictions, by = "3 months")

nonfarm_prediction <- data.frame(nonfarm_prediction, date_nonfarm)

nonfarm_predictions <- nonfarm_predictions %>%
  mutate(date_nonfarm = yearquarter(date_nonfarm)) %>%
  as_tsibble(index=date_nonfarm)
colnames(nonfarm_predictions) <- c("qtr_nonfarm_nonresidential_domestic_offices", "date")

real_estate_agg <- merge(reg_dom_prediction, reg_farmland_prediction)
real_estate_agg <- merge(real_estate_agg, res_construction_prediction)
real_estate_agg <- merge(real_estate_agg, nonfarm_predictions)


#### Foreign Loans ####
##log wins
hist_master <- hist_master %>%
  mutate(log_foreign_govs = log(ifelse(qtr_loans_to_foreign_governments_and_official_institutions + 0.001 > 0, 
                                       qtr_loans_to_foreign_governments_and_official_institutions + 0.001, 1e-10)))

### regression foreign govs
reg_foreign_govs <- lm(qtr_loans_to_foreign_governments_and_official_institutions[-1] ~ diff(`Prime rate`) + diff(lag(`Dow Jones Total Stock Market Index (Level)`)), 
                       data=hist_master)

summary(lm(qtr_loans_to_foreign_governments_and_official_institutions[-1] ~ diff(`Prime rate`) + diff(lag(`Dow Jones Total Stock Market Index (Level)`)), 
           data=hist_master))

### regression stargazer
stargazer(reg_foreign_offices,
          type="html",
          covariate.labels = c("Lagged CPI"),
          dep.var.labels = "Construction in Domestic Offices",
          digits=2,
          iqr=T,
          out="reg_foreign_offices.html")

### predict + dates
foreign_offices_prediction <- predict(reg_foreign_offices, macro_future)

### add date
date_foreign <- seq(as.Date("2023-01-01"),length.out = nrow(foreign_offices_prediction), along.with=foreign_offices_prediction, by = "3 months")

### data frame + tsibble
foreign_offices_prediction <- data.frame(foreign_offices_prediction, date_foreign)

foreign_offices_prediction <- foreign_offices_prediction %>%
  mutate(date_foreign = yearquarter(date_foreign)) %>%
  as_tsibble(index=date_foreign)

colnames(foreign_offices_prediction) <- c("qtr_real_estate_foreign_offices", "date")


### Entire real estate regression which isn't necessary ####
### make log of real estate losses ###
### real estate losses regression
rs_cols <- c(55,11, 12, 13, 7, 8, 9, 10)
corrplot::corrplot(cor(na.omit(hist_master[, rs_cols])), method="color", tl.srt = 45, tl.col = "black")

#### LOANS TO DEPOSITORY INSTITUTIONS ####

## regression depository institutions
reg_loan_depository <- lm(diff(qtr_loan_depository) ~ diff(lag(`10-year Treasury yield`)), 
                          data=hist_master)

summary(reg_loan_depository)

### regression stargazer
stargazer(reg_loan_depository,
          type="html",
          covariate.labels = c("Lagged 10-Year Treasury Yield"),
          dep.var.labels = "Loan Depository Losses",
          digits=2,
          iqr=T,
          out="loan_depository.html")
# predict
loan_depository_predictions <- predict(reg_loan_depository, macro_future)
### add date
date_depos <- seq(as.Date("2023-01-01"),length.out = nrow(loan_depository_predictions), along.with=loan_depository_predictions, by = "3 months")

loan_depository_predictions <- data.frame(loan_depository_predictions, date_depos)

### loan depository tsibble
loan_depository_predictions <- loan_depository_predictions %>%
  mutate(date_depos = yearquarter(date_depos)) %>%
  as_tsibble(index=date_depos)

colnames(loan_depository_predictions) <- c("qtr_loan_depository", "date")

### FINANCE AGRO PRODUCTIONS ###
### log diff response
hist_master <- hist_master %>%
  mutate(loan_finance_agro_log = log(ifelse(qtr_loans_to_finance_agricultural_production + 0.001 > 0, 
                                            qtr_loans_to_finance_agricultural_production + 0.001, 1e-10)))

### regression finance agricultural production
reg_agro_fin <- lm(qtr_loans_to_finance_agricultural_production[-1] ~ diff(lag(`Unemployment rate`)) +
                     diff(lag(`BBB corporate yield`)), data=hist_master)

summary(reg_agro_fin)

## regression stargazer
stargazer(reg_agro_fin,
          type="html",
          covariate.labels = c("Lagged Unemployment Rate", "Lagged BBB Corporate Yield"),
          dep.var.labels = "Agroicultural Finance Losses",
          digits=2,
          iqr=T,
          out="agro_fin.html")
# predict
agro_fin_predictions <- predict(reg_agro_fin, macro_future)
### add date
date_agro <- seq(as.Date("2023-01-01"),length.out = nrow(agro_fin_predictions), along.with=agro_fin_predictions, by = "3 months")

agro_fin_predictions <- data.frame(agro_fin_predictions, date_agro)

### finance agricultural production tsibble
agro_fin_predictions <- agro_fin_predictions %>%
  mutate(date_agro = yearquarter(date_agro)) %>%
  as_tsibble(index=date_agro)
colnames(agro_fin_predictions) <- c("qtr_loans_to_finance_agricultural_production", "date")


#### C&I LOANS ####
##log
hist_master <- hist_master %>%
  mutate(log_ci_loans = log(ifelse(qtr_C_I_loans + 0.001 > 0, 
                                   qtr_C_I_loans + 0.001, 1e-10)))
### regression ci loans
reg_ci_loans <-  lm(qtr_C_I_loans[-1] ~ diff(`Prime rate`) + diff(`Commercial Real Estate Price Index (Level)`) +
                      diff(`Market Volatility Index (Level)`), 
                    data=hist_master)

summary(lm(qtr_C_I_loans[-1] ~ diff(`Prime rate`) + diff(`Commercial Real Estate Price Index (Level)`) +
             diff(lag(`Market Volatility Index (Level)`)), 
           data=hist_master))
## regression stargazer
stargazer(reg_ci_loans,
          type="html",
          covariate.labels = c("Prime Rate", "Commercial Real Estate Price Index"),
          dep.var.labels = "C&I Loans",
          digits=2,
          iqr=T,
          out="ci_loans.html")

# predict
ci_losses_prediction <- predict(reg_ci_loans, macro_future)

### add date
date_ci <- seq(as.Date("2023-01-01"),length.out = nrow(ci_losses_prediction), along.with=ci_losses_prediction, by = "3 months")

ci_losses_prediction <- data.frame(ci_losses_prediction, date_ci)

### c_i losses as tsibble
ci_losses_prediction <- ci_losses_prediction %>%
  mutate(date_ci = yearquarter(date_ci)) %>%
  as_tsibble(index=date_ci)
colnames(ci_losses_prediction) <- c("qtr_C_I_loans", "date")

#### Consumer Loans ####
ggplot(hist_master) +
  geom_line(aes(date, qtr_credit_card_loans_to_individuals, color="Credit Card Losses")) +
  geom_line(aes(date, qtr_automobile_loans_to_individuals, color="Auto Lossses")) +
  geom_line(aes(date, qtr_consumer_loans_excluding_credit_cards_and_automobiles,color="Excluding auto and creditcards")) +
  scale_y_continuous(labels=scales::comma) +
  scale_color_manual(name="Y Series", values=c("Credit Card Losses"="blue","Auto Lossses"="black", "Excluding auto and creditcards"="green")) +
  labs(title="Consumer Losses by Category", y="Losses ($ Thousands)", x="Date") +
  theme_bw()

#### consumer loan for credit cards ####
reg_credit_cards <- lm(diff(qtr_credit_card_loans_to_individuals) ~ diff(`10-year Treasury yield`) +
                         diff(lag(`Unemployment rate`)) + diff(`Real GDP growth`),
                       data=hist_master)

summary(reg_credit_cards)
stargazer(reg_credit_cards,
          type="html",
          covariate.labels = c("10-Year Treasury Yield", "Lagged Unemployment Rate", "Real GDP Growth"),
          dep.var.labels = "Credit Card Losses to Individuals",
          digits=2,
          iqr=T,
          out="creditcards.html")

credit_cards_prediction <- predict(reg_credit_cards, macro_future)

### credit cards create date column
### add date
date_cc <- seq(as.Date("2023-01-01"),length.out = nrow(credit_cards_prediction), along.with=credit_cards_prediction, by = "3 months")

credit_cards_prediction <- data.frame(credit_cards_prediction, date_cc)

### credit cards as tsibble
credit_cards_prediction <- credit_cards_prediction %>%
  mutate(date_cc = yearquarter(date_cc)) %>%
  as_tsibble(index=date_cc)

colnames(credit_cards_prediction) <- c("qtr_credit_card_loans_to_individuals", "date")

### consumer loans pt 2
### regression automobile loans consumer losses
reg_automobiles <- lm(diff(qtr_automobile_loans_to_individuals) ~ diff(lag(`Unemployment rate`)) + 
                        diff(lag(`10-year Treasury yield`)) + diff(lead(`Nominal disposable income growth`)),  
                      data=hist_master)


ggplot(hist_master, aes(date, qtr_automobile_loans_to_individuals)) +
  geom_line()

library(stargazer)
stargazer(reg_automobiles,
          type="html",
          covariate.labels = c("Lagged Unemployment Rate", "Lagged 10-Year Yield", "Lead Nominal Disp Income Growth"),
          dep.var.labels = "Automobile Losses to Individuals",
          digits=2,
          iqr=T,
          out="automobile.html")

### predict + dates
automobile_prediction <- predict(reg_automobiles, macro_future)
### add date
date_auto <- seq(as.Date("2023-01-01"),length.out = nrow(automobile_prediction), along.with=automobile_prediction, by = "3 months")

### data frame + tsibble
automobile_prediction <- data.frame(automobile_prediction, date_auto)

automobile_prediction <- automobile_prediction %>%
  mutate(date_auto = yearquarter(date_auto)) %>%
  as_tsibble(index=date_auto)
colnames(automobile_prediction) <- c("qtr_automobile_loans_to_individuals", "date")

### consumer loans pt 3
### regression consumer loans excluding credit cards and automobiles
reg_x_auto_cc <- lm(diff(qtr_consumer_loans_excluding_credit_cards_and_automobiles) ~ diff(`Commercial Real Estate Price Index (Level)`) +
                      diff(lag(`Nominal disposable income growth`)), data=hist_master)
library(ggplot2)
library(dplyr)
library(tsibble)
### regression stargazer
stargazer(reg_x_auto_cc,
          type="html",
          covariate.labels = c("Commercial Real Estate Price Index", "Lagged Nominal disposable income growth"),
          dep.var.labels = "Consumer Losses Excluding Credit Cards & Autos",
          digits=2,
          iqr=T,
          out="consumer_losses_x.html")

### predict + dates
x_auto_x_cc_prediction <- predict(reg_x_auto_cc, macro_future)
### add date
date_xx <- seq(as.Date("2023-01-01"),length.out = nrow(x_auto_x_cc_prediction), along.with=x_auto_x_cc_prediction, by = "3 months")


### data frame + tsibble
x_auto_x_cc_prediction <- data.frame(x_auto_x_cc_prediction, date_xx)

x_auto_x_cc_prediction <-x_auto_x_cc_prediction %>%
  mutate(date_xx = yearquarter(date_xx)) %>%
  as_tsibble(index=date_xx)
colnames(x_auto_x_cc_prediction) <- c("qtr_consumer_loans_excluding_credit_cards_and_automobiles", "date")

### consumer loans aggregate
consumer_losses <- merge(credit_cards_prediction, x_auto_x_cc_prediction)
consumer_losses <- merge(consumer_losses, automobile_prediction)

### master file for historical + future
consumer_loans_predictions <- bind_rows(consumer_losses, hist_master)

### plot consumer loans by its components
ggplot(consumer_loans_predictions) +
  geom_line(aes(date, qtr_credit_card_loans_to_individuals, color="Credit Card Losses")) +
  geom_line(aes(date, qtr_automobile_loans_to_individuals, color="Auto Lossses")) +
  geom_line(aes(date, qtr_consumer_loans_excluding_credit_cards_and_automobiles,color="Excluding auto and creditcards")) +
  geom_vline(xintercept = as.numeric(as.Date("2022-12-31")), linetype="dashed", color="red") +
  scale_y_continuous(labels=scales::comma) +
  scale_color_manual(name="Y Series", values=c("Credit Card Losses"="blue","Auto Lossses"="black", "Excluding auto and creditcards"="green")) +
  labs(title="Consumer Losses by Category", y="Losses ($ Thousands)", x="Date") +
  theme_bw()

### predict foreign gov losses
foreign_govs_prediction <- predict(reg_foreign_govs, macro_future)
### add date
date_gov <- seq(as.Date("2023-01-01"),length.out = nrow(foreign_govs_prediction), along.with=rforeign_govs_prediction, by = "3 months")

foreign_govs_prediction <- data.frame(foreign_govs_prediction, date_gov)

### turn into tsibble
foreign_govs_prediction <- foreign_govs_prediction %>%
  mutate(date_gov = yearquarter(date_gov)) %>%
  as_tsibble(index=date_gov)
colnames(foreign_govs_prediction) <- c("qtr_loans_to_foreign_governments_and_official_institutions", "date")

#### All Other Loans ####
##log wins
hist_master <- hist_master %>%
  mutate(log_all_other_loans = log(ifelse(qtr_all_other_loans + 0.001 > 0, 
                                          qtr_all_other_loans + 0.001, 1e-10)))


### regression all other loans
reg_other_loans <- lm(log_all_other_loans[-1] ~ diff(`House Price Index (Level)`) + diff(lag(`Prime rate`)), 
                      data=hist_master)
### reg stargazer
stargazer(reg_other_loans,
          type="html",
          covariate.labels = c("House Price Index", "Lagged Prime Rate"),
          dep.var.labels = "All Other Loans",
          digits=2,
          iqr=T,
          out="other_loans.html")

### predict all other loans
all_other_loans_prediction <- predict(reg_other_loans, macro_future)
### add date
date_other <- seq(as.Date("2023-01-01"),length.out = nrow(all_other_loans_prediction), along.with=all_other_loans_prediction, by = "3 months")

### dates
all_other_loans_prediction <- data.frame(all_other_loans_prediction, date_other)

### tsibble
all_other_loans_prediction <- all_other_loans_prediction %>%
  mutate(date_other = yearquarter(date_other)) %>%
  as_tsibble(index=date_other)
colnames(all_other_loans_prediction) <- c("qtr_all_other_loans", "date")
