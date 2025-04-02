# NMC 2025-04-01
# Principle Componenet Analysis: 

library(tidyverse)
library(palmerpenguins)
head(penguins)

# PCA is a way to compress your data.

pen_drop_na = penguins %>% 
  drop_na()

dim(penguins)
dim(pen_drop_na)
summary(pen_drop_na)
head(pen_drop_na)

pen_num = pen_drop_na %>%
  select(ends_with("mm"), body_mass_g)
head(pen_num)

pen_meta = pen_drop_na %>%
  select(species, sex, island, year)
head(pen_meta)

# Run PCA
# principle comp, we just wnat to normalize the data before putting it thought the PCA
pen_pca = prcomp(pen_num,scale. = TRUE, center = TRUE)
pen_pca
class(pen_pca)
typeof(pen_pca)
summary(pen_pca)

str(pen_pca)

# Proportion of Variance (very important)

summary(pen_pca)$importance
summary(pen_pca)$importance[2,]
(pen_pca$sdev)^2 / sum(pen_pca$sdev^2)

pen_pca$rotation
# the sign is the relationship (negative or positive)

pen_pca$x


## Scree Plot##
plot(pen_pca) # how does a plot respond, when you want to plot a PCA. This plot sucks

# first sort the data into a data frame
pca_scree = data.frame(pc = c(1:4), 
                       var = (pen_pca$sdev)^2 / sum(pen_pca$sdev^2))
pca_scree

ggplot(data = pca_scree, aes(x = pc, y = var)) +
  geom_col()+
  geom_point()+
  geom_line()+
  theme_bw()

# Bi-plot ## here we plot the PCA scores, which recall are stored in the variable x
# join with meta data from begining

pen_pca$x

# each row is each sample or an individual penguin
pen_pca_meta = cbind(pen_meta, pen_pca$x)
head(pen_pca_meta)

# want to reduce the dimensionality of data (the whole point of PCA)
# now we plot PC1 vs PC2

ggplot()+
  geom_point(aes(x = PC1, y = PC2, color = species,shape = sex), data = pen_pca_meta)+
  coord_fixed(ratio = 1)
# need to color point based on things you care about.
# allows the x axis to be equal on each side

libray(r)
#install(ggbiplot)
library(ggbiplot)

biplot(pen_pca)

ggbiplot(pen_pca)

## ggbiplot(pen_pca, scale = 1, obs.scale = 1, groups = pen_meta$species, elipse = TRUE)



# may be able to use PCA for seasonality and ENSO with project.
