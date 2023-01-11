### Data analysis for disaster management project ###
### Use China subnational data ###
### Preliminary results ###

### Library ###
library(foreign)
library(haven)
library(dplyr)

?### Input data ###
disaster_finance <- read_dta("database3.dta")

### Construct variables ###
disaster_finance_cleaned <- disaster_finance %>% 
          mutate (pcrevenue = revenue/population,
          pcexpenditure = expenditure/population,
          deficit = expenditure-revenue,
          pcdeficit = deficit/population,
          riskdefictgdp = deficit/gdp)




