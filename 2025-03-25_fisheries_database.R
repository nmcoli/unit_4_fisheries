#NC: 2025-03-25
# 4.1:  Relational Database

library(tidyverse)
load('data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData')

head(timeseries)
dim(timeseries)
glimpse (tsmetrics)

timeseries_tsmetrics = timeseries %>%
  left_join (tsmetrics, by=c("tsid" = "tsunique"))
# left join - left column gets put first, right second. this is how we join two tables when the joining variable has different names. 

dim(timeseries)
dim(timeseries_tsmetrics) # good, we have the same amount of rows but we have additional columns. this is what we wanted. 

glimpse(timeseries_values_views)
glimpse(taxonomy)
glimpse(stock)

# ask Erin about this
fish = timeseries_values_views %>%
  left_join(stock, by = c ("stockid","stocklong")) %>% 
  left_join(taxonomy, by = c("tsn", "scientificname")) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup)
glimpse(fish)

dim(timeseries_values_views)
dim(fish)
head(fish)

#ggplot() +
 # geom_line(aes(x=year), y = TCbest, color = stocklong), data = fish %>% filter(TCbest > 3e6)) # + theme(legend.position = "none")

## filter by Cod

ggplot(fish %>% filter(TCbest > 3e6)) +
  geom_line(aes(x = year, y = TCbest, color = stocklong))

cod_can = fish %>% 
  filter(scientificname == "Gadus morhua", region == "Canada East Coast", !is.na(TCbest))

head(cod_can)
dim(cod_can)
unique(cod_can$region)

# now plot again!

ggplot() + 
  geom_line(aes(x =  year, y = TCbest, color = stocklong), data = cod_can)

cod_can_total = cod_can %>%
  group_by(year) %>% 
  summarize(total_catch_mt = sum(TCbest))
head(cod_can_total)

ggplot() + 
  geom_line(aes(x =  year, y = total_catch_mt), data = cod_can_total)

# did the stock officially collapse.

# quick fake dataset
dat = c(1,3,6,2,3,9,-1)
dat_max = cummax(dat)
dat_max
# cumulative max number

dat_sum = cumsum(dat)
test_cum = data.frame(dat, dat_max, dat_sum)
# keeps summing the row before 
test_cum

cod_collapse = cod_can_total %>% 
  mutate(historical_max_catch = cummax(total_catch_mt),
         collapse = total_catch_mt <= 0.1 * historical_max_catch)
cod_collapse
tail(cod_collapse)

# what year did cod collapse?

cod_collapse_year = cod_collapse %>%
  filter(collapse == TRUE) %>%
  summarize (year = min(year)) %>%
  pull(year) # this last line makes it just on number instead of a table with a year. 
cod_collapse_year
class(cod_collapse_year)

ggplot() + 
  geom_line(aes(x = year, y = total_catch_mt, color = collapse),
            data = cod_collapse) + 
  geom_vline(xintercept = cod_collapse_year)
      
collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax (TCbest),
         current_collapse = TCbest <= 0.1 * historical_max_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()

glimpse(collapse)
summarize(collapse)

collapse_yr = collapse %>% 
  group_by(stockid, stocklong, region) %>%
  filter(collapsed_yet == TRUE) %>%
  summarize (first_collapse_yr = min(year)) %>%
  ungroup()
glimpse(collapse_yr)

ggplot() + 
  geom_histogram(aes(x = first_collapse_yr), binwidth = 1, fill = "salmon", color = "black", data = collapse_yr)

n_stocks = length(unique(collapse$stockid))
n_stocks

collapse_ts = collapse_yr %>%
  count(first_collapse_yr) %>%
  mutate(cum_first_collapse_yr = cumsum(n),
         ratio_collapsed_yet = cum_first_collapse_yr / n_stocks)

head(collapse_ts)

ggplot() +
  geom_line(aes(x = first_collapse_yr, y = cum_first_collapse_yr), data = collapse_ts)

ggplot() +
  geom_line(aes(x = first_collapse_yr, y = ratio_collapsed_yet), data = collapse_ts)

## Exercise 2.1 ##

