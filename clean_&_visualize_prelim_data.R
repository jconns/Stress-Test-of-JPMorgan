### banking data
bhc <- read_dta("/Users/jackconnors/Downloads/historical_BHC_data (1).dta")

### macro data and 
macro <- readxl::read_excel("/Users/jackconnors/Downloads/2023-Table_1A_Historic_Domestic.xlsm", sheet="Sheet1")

### turn Date into yearquarter
library(tsibble)
library(fpp3)

### macro tsibble
macro_ts <- macro %>%
  mutate(date = yearquarter(Date)) %>%
  as_tsibble(index=date)

### banking tsibble
# pt 1
bhc_date<- bhc %>% 
  mutate(date = lubridate::ymd(DT)) %>%
  arrange(date)

# pt 2
bhc_date_filtered <- bhc_date %>%
  filter(!is.na(date))

# pt 3
jpmorgan <- bhc_date %>%
  filter(Name=="JPMORGAN CHASE & CO")

# pt 4
jp_ts <- jpmorgan%>%
  as_tsibble(index=date)


#### banking stress pt w week 5 data

### MACRO ENVIRONEMENT ###

### rates
macro_rates <- ggplot(macro_ts, aes(date, color="variable")) +
  geom_line(aes(date, `Unemployment rate`, color="Unemploy_Rate"), size=1)+
  geom_line(aes(date, `3-month Treasury rate`, color="3-Month"), size=1) +
  geom_line(aes(date, `Prime rate`, color="Prime rate"), size=1) +
  geom_line(aes(date, `10-year Treasury yield`, color="10-Year"),size=1) +
  geom_line(aes(date, `BBB corporate yield`, color="BBB-Yield"),size=1)+
  scale_color_manual(name = "Y series", values = c("Unemploy_Rate" = "darkblue", "3-Month" = "red", "10-Year"="green", "BBB-Yield"="black", "Prime rate" ="orange"))+
  geom_vline(xintercept=as.numeric(as.Date("2022-12-31")), linetype="longdash", lwd=1) +
  labs(title="Historical and Predicted Macro Environment",
       subtitle = "Predictions (2023 Q1 - 2026-Q1)",
       y="Rate", x="Date") +
  theme_bw()

### index
### dow is divided by 100
macro_index <- ggplot(macro_ts, aes(date, color="variable")) +
  geom_line(aes(date, `Market Volatility Index (Level)`, color="Market Volatility"), size=1)+
  geom_line(aes(date, `House Price Index (Level)`, color="Housing Index"), size=1) +
  geom_line(aes(date, `Dow Jones Total Stock Market Index (Level)`/100, color="DowJones"),size=1) +
  geom_line(aes(date, `Commercial Real Estate Price Index (Level)`, color="Commercial Real Estate Index"),size=1)+
  scale_color_manual(name = "Y series", values = c("Market Volatility" = "darkblue", "Housing Index" = "red", "DowJones"="green", "Commercial Real Estate Index"="black"))+
  geom_vline(xintercept=as.numeric(as.Date("2022-12-31")), linetype="longdash", lwd=1) +
  labs(title="Historical and Predicted Macro Environment",
       subtitle = "Predictions (2023 Q1 - 2026-Q1)",
       y="Index", x="Date") +
  theme_bw()


### growth
macro_growth <- ggplot(macro_ts, aes(date, color="variable")) +
  geom_line(aes(date, `Nominal GDP growth`, color="Nominal GDP"), size=1)+
  geom_line(aes(date, `Real GDP growth`, color="Real GDP"), size=1) +
  geom_line(aes(date, `Real disposable income growth`, color="Real disposable income growth"),size=1) +
  geom_line(aes(date, `Nominal disposable income growth`, color="Nominal disposable income growth"),size=1)+
  scale_color_manual(name = "Y series", values = c("Nominal GDP" = "darkblue", "Real GDP" = "red", "Real disposable income growth"="green", "Nominal disposable income growth"="black"))+
  geom_vline(xintercept=as.numeric(as.Date("2022-12-31")), linetype="longdash", lwd=1) +
  labs(title="Historical and Predicted Macro Environment",
       subtitle = "Predictions (2023 Q1 - 2026-Q1)",
       y="Growth", x="Date") +
  theme_bw()

View(macro_future)
### plot grid them all
library(cowplot)
plot_grid(macro_index, macro_rates, macro_growth, cols = 1, nrow=3)