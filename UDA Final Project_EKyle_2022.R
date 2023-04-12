
# UDA Final Project - Emily Kyle

library(tidyverse)
library(stargazer)
library(AER)
library(plm)
library(sandwich)
library(lmtest)
library(ggcorrplot)

# import data
kcec <- read.csv("WADOH_Environmental_Health_Disparities_Index_Calculated_for_King_County_wadohehdindex_area.csv")
med_hinc <- read.csv("ACSST5Y2019.S1901-2022-12-02T231054_medhouseholdincome.csv")

# trim to variables of interest
kcec <- kcec[c(7, 30, 46, 88, 106, 120, 127)]

# fix encoding name error in median household income
colnames(med_hinc)[1] = "census_tract"

# join kcec data with median household income per census tract
card_ind <- full_join(kcec, med_hinc, by = c("TRACT_FLT" = "census_tract"))

# save csv
write.csv(card_ind, "cardiovas_indicators.csv")

# run descriptive stats
stargazer(card_ind[-1], 
          title = "Summary Statistics for Cardiovascular Mortality Rate Indicators", 
          digits = 3,
          type = "latex", 
          out = "summarystats_card_ind.html")

# histogram for cardio disease death rate
hist(card_ind$cardio_mortality_rate,
     main = "Histogram of Cardiovascular Disease Mortality Rate",
     xlab = "Deaths per 100,000 people")

# hist of proximity to high traffic roadways
hist(card_ind$traffic_count)
hist(card_indtraffic_total_population)

# correlation matrix
cor_matrix <- round(cor(card_ind[-1], use = "complete.obs"), 2)
ggcorrplot(cor_matrix, hc.order = T, type = "lower", lab = T)

# simple regression
sim1 <- lm(cardio_mortality_rate ~ traffic_count, data = card_ind)

plot(cardio_mortality_rate ~ traffic_count, 
     main = "Cardiovascular Mortality Rate and Proximity to Traffic \nin the Greater Seattle Area",
     ylab = "Cardiovascular Mortality Rate",
     xlab = "Proximity to High Traffic Roadways",
     data = kcec)
abline(sim1)

sim1$rse <-sqrt(diag(vcovHC(sim1, type="HC1")))

# multivariate regression
multi <- lm(cardio_mortality_rate ~ traffic_count + education_percent + 
              unemployed_percent + ozone + PM25 + med_hh_inc, data = card_ind)

multi$rse <-sqrt(diag(vcovHC(multi, type="HC1")))

# multivariate with pm as primary, no traffic, and no ozone
multi2 <- lm(cardio_mortality_rate ~ PM25 + education_percent + 
              unemployed_percent + ozone + med_hh_inc, data = card_ind)

multi2$rse <-sqrt(diag(vcovHC(multi2, type="HC1")))

# stargazer table
stargazer(sim1, multi, multi2,
          se = list(sim1$rse, multi$rse, multi2$rse),
          type = "latex",
          column.labels = c("Bivariate", "MV - Traffic Prox", "MV - PM2.5"), 
          model.numbers = FALSE, 
          title = "Cardiovascular Mortality Rate & Environmental Indicators",
          out= "cardmort_environind_threemodels.html")


















