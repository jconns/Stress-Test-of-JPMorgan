### loan losses
### total charge offs
charge_offs <- charge_offs %>%
  mutate_at(vars(starts_with("qtr_")), ~replace_na(.x, 0))

charge_offs <- charge_offs %>%
  mutate(total=
           qtr_credit_card_loans_to_individuals + qtr_all_other_loans +
           qtr_consumer_loans_excluding_credit_cards_and_automobiles + qtr_automobile_loans_to_individuals +
           qtr_loans_to_foreign_governments_and_official_institutions + qtr_loan_depository + qtr_loans_to_finance_agricultural_production)

### plot total charge offs
ggplot(charge_offs) +
  geom_area(aes(date, total), fill="darkblue") +
  geom_vline(xintercept = as.numeric(as.Date("2022-12-31")), color="red", linetype="dashed") +
  labs(title="Total Charge Offs", y="Losses ($ thousands", x="Date") +
  theme_bw()

#### TOTAL CHARGE OFFS ####
### repeat merge to combine charge off components
total_charge_off <- merge(ci_losses_prediction, consumer_losses, on="date")
total_charge_off <- merge(total_charge_off, all_other_loans_prediction, on="date")
total_charge_off <- merge(total_charge_off, foreign_govs_prediction,on="date")
total_charge_off <- merge(total_charge_off, x_auto_x_cc_prediction, on="date")
total_charge_off <- merge(total_charge_off, agro_fin_predictions, on="date")
total_charge_off <- merge(total_charge_off, loan_depository_predictions, on="date")
total_charge_off <- merge(total_charge_off, real_estate_agg, on="date")

total_charge_off <- total_charge_off %>%
  mutate(total_losses = qtr_all_other_loans + qtr_consumer_loans_excluding_credit_cards_and_automobiles +
           qtr_C_I_loans + qtr_construction_domestic_offices + qtr_credit_card_loans_to_individuals +
           qtr_automobile_loans_to_individuals + qtr_loan_depository + qtr_loans_secured_by_farmland +
           qtr_loans_to_finance_agricultural_production + qtr_x1_x4family_residential_construction + qtr_nonfarm_nonresidential_domestic_offices + qtr_loans_to_foreign_governments_and_official_institutions)
### predicted charge off values with its components
total_charge_off <- total_charge_off %>%
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index=date)

### plot total charge off
### plot total charge off by its components
### merge total_charge off w hist_master
### bind to create historical + prediction
charge_offs <- bind_rows(total_charge_off, hist_master)
### plot
ggplot(charge_offs) +
  geom_line(aes(date, qtr_credit_card_loans_to_individuals, color="Credit Cards"), size=1) +
  geom_line(aes(date, qtr_all_other_loans, color="All Other Loans"), size=1) +
  geom_line(aes(date, qtr_consumer_loans_excluding_credit_cards_and_automobiles, color="All Else Consumer Loans"), size=1) +
  geom_line(aes(date, qtr_automobile_loans_to_individuals, color="Auto Loan Losses"), size=1) +
  geom_line(aes(date, qtr_loans_to_foreign_governments_and_official_institutions, color="Foreign Govs"), size=1) +
  geom_line(aes(date, qtr_loan_depository, color="Loan Depository Losses"), size=1) +
  geom_line(aes(date, qtr_loans_to_finance_agricultural_production, color="Agricultural Production"), size=1) +
  geom_line(aes(date, qtr_C_I_loans, color="C&I Loans"), size=1) +
  geom_line(aes(date, qtr_construction_domestic_offices, color="Construction in Dom. Offices"), size=1) +
  geom_line(aes(date, qtr_x1_x4family_residential_construction, color="Family 1-4 Loans"), size=1) +
  geom_line(aes(date, qtr_nonfarm_nonresidential_domestic_offices, color="Nonfarm"), size=1) +
  
  geom_vline(xintercept=as.numeric(as.Date("2022-12-31")), color="red", linetype="dashed") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Total Charge Offs", y="Losses ($ thousands)", x="Date") +
  theme_bw()

##### CAPITAL #####

#### evolution of capital ####
### forecast ppnr
colnames(noninterest_predictions) <- c("qtr_noninterest_expense", "qtr_noninterest_income", "date")

### create ppnr data frame
ppnr <- merge(noninterest_predictions, interest_master)

### plot ppnr
ppnr %>%
  mutate(nim = qtr_interest_income - qtr_interest_expense,
         ppnr = nim + (qtr_noninterest_income - qtr_noninterest_expense),
         pos = ppnr >= 0) %>%
  ggplot(aes(date, ppnr, fill=pos)) +
  geom_col(position="identity") +
  geom_vline(xintercept = as.numeric(as.Date("2022-12-31")), color="red") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Pre-Provision Net Revenue", y="PPNR ($)", x="Date") +
  theme_bw()

ppnr <- ppnr %>%
  mutate(net_revenue = (qtr_interest_income - qtr_interest_expense) + (qtr_noninterest_income - qtr_noninterest_expense))

### if i want to look at historical + predicted
### capital
capital <- data.frame(jpm$tier1_capital_ratio, jpm$date)

### as tsibble
capital <- capital %>%
  mutate(date = yearquarter(jpm.date)) %>%
  as_tsibble(index=date)

### create capital forecast data frame of predictions
capital_forecast <- merge(ppnr, total_charge_off, by="date")
head(capital_forecast)
### create rwa and capital initial values
capital_forecast$rwa <- 386.9 * 10000000
capital_forecast$capital[1] <- 257341000

### build a loop function to calculate capital predictions
capital_forecast$capital_prediction <- NA 
capital_forecast$capital_prediction[1] <- capital_forecast$capital[1]
for (i in 2:nrow(capital_forecast)) {
  capital_forecast$capital_prediction[i] <- capital_forecast$capital_prediction[i-1] + 
    capital_forecast$net_revenue[i] - capital_forecast$total_losses[i]
}
capital_forecast %>%
  select(date, capital_prediction, net_revenue, total_losses, capital)

### calculate capital ratio
capital_forecast <- capital_forecast %>%
  mutate(capital_ratio = (capital_prediction / rwa)*100)

### plot capital ratio
ggplot(capital_forecast, aes(date, capital_ratio), size=1) +
  geom_line(aes(date, capital_ratio), size=1) +
  labs(title="Projected Capital Ratio", subtitle="5% minimum requirement", y="Percent", x="Date") +
  theme_bw()
