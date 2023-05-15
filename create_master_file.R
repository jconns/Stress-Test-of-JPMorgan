### stargazer macro + JPM ###
library(stargazer)
stargazer(as.data.frame(macro),
          summary=T,
          title = "Descriptive Statistics for Macro Data",
          type = "html",
          out = "macro.html",
          digits = 1,
          iqr = TRUE)

 ## stargazer for JPM
 stargazer(as.data.frame(jpm_quarterly),
      summary=T,
        covariate.labels = c("year", "interset income", "interest expense", 
          "noninterest income", "noninterest expense", "Construction Domestic",
          "1-4 family_residential_construction", "other_construction", 
          "loans_secured_by_farmland", "revolving open-ended loans by 1-4 family",
       "closed-end loans secured_by 1-4 family other", "closed_end_loans_secured_by_x1_x4_family_first_liens",
          "multifamily_residential_properties_domestic_offices", "onfarm_nonresidential_domestic_offices",
          "owner_occupied_nonfarm_nonresidential", "other_nonfarm_nonresidential", "real_estate_foreign_offices",
          "loans to depository institutions and acceptances of other_banks", "loans to US banks and other US_depository institutions",
          "loans to foreign banks", "loans to finance agricultural production", "C_I_loans", "consumer loans",
          "consumer loans excluding credit_cards", "credit card loans to individuals", "automobile loans to individuals",
          "consumer loans excluding creditcards andautomobiles", "loans to foreign governments and official institutions",
          "all_other_loans", "lease financing receivables", "leases to individuals", "all other leases", "total_net_chargeoffs",
          "total assets", "tier1 capital ratio"),
        title = "Descriptive Statistics for JPMorgan Financials",
        type = "html",
        out = "jpm.html",        digits = 1,
        iqr = TRUE)


### plot quarterly interest income + expense
ggplot(jpm_quarterly, aes(date, color="variable")) +
  geom_line(aes(year, qtr_interest_income, color="Interest_Income")) +
  geom_line(aes(year, qtr_interest_expense, color="Interest_Expense")) +
  scale_color_manual(name="Y series", values=c("Interest_Income" = "blue", "Interest_Expense" = "black"))

### jp_qtr tsibble
jpm_quarterly <- jpm_quarterly %>%
  mutate(date_qtr = paste(year, quarter, sep = "-"),
         date = yearquarter(date_qtr)) %>%
  as_tsibble(index=date)

### historical macro set
macro_hist <- macro_ts %>%
  filter(`Scenario Name` =="Actual") %>%
  filter_index("2004 Q3" ~ .)

### future macro set
macro_future <- macro_ts %>%
  filter(`Scenario Name` =="Supervisory Severely Adverse")

### joined at min date for jpmorgan
## hopefully will work for correlation
hist_master <- merge(macro_hist, jpm_quarterly, on="date")

hist_master <- hist_master %>%
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index=date) %>%
  filter_index("2005 Q1" ~ .) 

### correlation
cols <- c(4:17, 20, 21)
corrplot::corrplot(cor(hist_master[, cols]), 
                   method="color", tl.col = "black", tl.srt = 45, tl.pos = "lt")

### create master file
master <- merge(jp_ts, macro, on="date")
