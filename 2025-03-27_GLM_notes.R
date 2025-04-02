# NMC 2025-03-27
# Generalzied Linear Models: 


# Logistic Regression: logistic regression is used to model a binary dependent variable or a depenent variablee that is bound between 0 and 1. 

# the math is what constrains your information between 0 and 1. 

# the reason what a logistic regression is called a glm, is becuase you can just rearrange the equation as a function of y and make the parameters linear. 

#library(tidyverse)
#load('data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData')

source('build_collapse_table.r')

glimpse(collapse)

glimpse(metadata)
unique(metadata$FisheryType)

# recall that we built in the sourced script

# is there chance that the stock has ever collapsed based on the Fishery Type.... ??
model_data = collapse %>% 
  group_by(stockid)%>%
  summarize(ever_collapsed = any(current_collapse)) %>% # collapsed each stock into a single row, did it ever collapse
  ungroup() %>%
  left_join(metadata)%>% 
  mutate(FisheryType = as.factor(FisheryType))

dim(collapse)
length(unique(collapse$stockid))
dim(model_data)

glimpse(model_data)
#Fishery Type is currently in text but we need to set them as factors to deem it as categorical variable

# Now, run the model. 
model_l = glm(ever_collapsed ~ FisheryType, data = model_data, family = "binomial")
summary(model_l)

model_data %>% distinct(FisheryType) %>% arrange(FisheryType)  

# now we make predictions: 

# prob of a collapse based on fishery type. 
FisheryType = unique(model_data$FisheryType)

FisheryType_2 = model_data %>% distinct(FisheryType)

model_l_predict = predict(model_l, newdata = FisheryType_2, se.fit = TRUE, type = "response")
model_l_predict

collapse_fishery_type_predictions = cbind(FisheryType_2, model_l_predict)
collapse_fishery_type_predictions

ggplot(data = collapse_fishery_type_predictions) + 
  geom_bar (aes(x=FisheryType, y = fit,fill = FisheryType),
            stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = fit - se.fit, ymax = fit + se.fit, x = FisheryType), width = 0.2) +
  ylab("Probability a stock will Collapse")+
  coord_flip()

## Poisson Models ## 
# gets a bit messed up if there's too many 0's
# over dispersion = when the varriances is higher than the mean

glimpse(timeseries_values_views)
# biomass / msy pref
# True pressure div / msy pref

u_summary = timeseries_values_views %>%
  filter(!is.na(UdivUmsypref), !is.na(BdivBmsypref)) %>%
  group_by(stockid, stocklong) %>%
  summarize(
    yrs_data = n(),
    ratio_yrs_overfished = sum(UdivUmgtpref > 1) / yrs_data,
    ratio_years_low_stock = sum(BdivBmgtpref > 1) / yrs_data  # Fixed parentheses
  ) %>%
  ungroup() %>%
  select(-yrs_data)%>%
  left_join(metadata %>% select(stockid,)

head(u_summary)

collapse_summary = collapse %>%
  group_by(stockid) %>%
  summarize(yrs_data = n(),
            yrs_collapsed = sum(currentt_collapse))%>%
  inner_join(u_summary)

head (collapse_summrary)

hist(collapse_summary$yrs_collapased)
table(collapse_summary$yrs_collapsed)

collapse_summary_zero_trunc = collapse_summary %>%
  filter(yrs_collapsed > 0)

model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_high_stock + FisheryType, 
              offset(log(yrs_data)),data = collapse_summary_zero_trunc, family = "poisson")

summary(model_p)

#variances is high, overdispersed

#library(AER)
#AER::dispersiontest(model_p)


